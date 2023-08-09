class SpectrumWorker
  include Sidekiq::Worker
  include Rails.application.routes.url_helpers
  sidekiq_options :retry => true #, queue: 'critical'

  def perform(spectrum_id)
    start_time = Time.now
    spectrum = Spectrum.find(spectrum_id)
    submission = spectrum.submission
    spectrum.update!(status: 'profiling')

    FileUtils.mkdir_p(spectrum.sample_dir)
    if spectrum.spectrum_data.path =~/CDF$/i 
      suffix =  '.CDF'
    elsif spectrum.spectrum_data.path =~/mzXML$/i
      suffix = '.mzXML'
    else
      suffix = ''
      raise StandardError.new("Error: unknown format of sample file #{spectrum.spectrum_data.path}")
    end
    FileUtils.symlink(spectrum.spectrum_data.path, File.join(spectrum.sample_dir, "sample#{suffix}"))
    options = {infiledir: File.join(submission.input_dir),
              internalstd: submission.internal_standard,
              process: 'PROFILING',
              AlkaneRT: submission.alkane_rt.join(','),
              infoFileDir: submission.preprocessing_dir,
              outdir: File.join(spectrum.sample_dir),
              MFscore: submission.mf_score_threshold,
              log: spectrum.log_file,
              configfile: submission.config_file}

    if submission.database == 'upload'
      options[:userlib] = "#{Rails.application.config.apgcms_job_dir}/#{submission.secret_id}/input/user_library.csv"
      options[:usercal] = "#{Rails.application.config.apgcms_job_dir}/#{submission.secret_id}/input/user_calibration.csv"
    else
      options['lib.internal'] = submission.database.upcase
    end
    apgcms = APGCMS.new(options)
    if apgcms.success?
      spectrum.status = 'complete'
      # puts "spectrum.status => #{spectrum.status}"
      json_path = File.join(spectrum.sample_dir, 'sample_spectrum.json')   # Save JSON results
      spectrum.json_results = File.open(json_path)
      FileUtils.rm(json_path)                                              # Remove original JSON file
      spectrum.save!
      # puts "spectrum.status => #{spectrum.status}"
    else
      puts "apgcms.failed"
      spectrum.status = 'failed'
      spectrum.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end

    update_library(submission, spectrum)

  rescue StandardError => e
    puts "StandardError => #{e.message}"
    spectrum.status = "failed"
    spectrum.error =  "[Rescue from spectrum worker] There was a problem running GC-AutoFit. #{e.message}"
    spectrum.logger(e.message)
    spectrum.logger(e.backtrace.join("\n"))
  ensure
    spectrum.runtime = Time.now - start_time
    spectrum.save!
    # puts "spectrum => #{spectrum.inspect}"

  end

  # TODO: Make sure the column headers match with mixture file, mz_int file and lib_file
  def update_library(submission, spectrum)
    if submission.update_library
      mz_int_file = spectrum.mzint_for_db_file_path
      mixture_file = submission.profile_library.path
      lib_file = submission.lib_file_path
  
      # Read the mixture file
      mixture_data = CSV.read(mixture_file, headers: true)
  
      # Read the mzInt4DB file
      mz_int_data = CSV.read(mz_int_file, headers: true)
  
      # Read the library file
      lib_data = CSV.read(lib_file, headers: true)
  
      # Iterate through the mixture data and update the library file
      mixture_data.each do |row|
        rt = row['RT']
        matching_mz_int_row = mz_int_data.find { |r| r['rt'] == rt }
  
        # Add the information from the mixture and mzInt4DB files to the library file
        new_row = {
          'HMDB_ID' => row['HMDB_ID'],
          'Compound' => row['Compound name'],
          'CompoundwithTMS' => row['Compound name with TMS'],
          'TargetIon' => row['Target Ion'],
          'QIon' => row['Qualification Ion'],
          'RT' => rt,
          'RI' => matching_mz_int_row['RI'],
          'MZ' => matching_mz_int_row['m/z'],
          'Intensity' => matching_mz_int_row['Intensity']
        }
        lib_data << new_row
      end
  
      # Save the updated library file
      new_lib_file_path = lib_file.sub('.csv', "_#{Time.now.strftime('%Y%m%d')}.csv")
      CSV.open(new_lib_file_path, 'w') do |csv|
        csv << lib_data.headers
        lib_data.each do |row|
          csv << row
        end
      end
    end
  end

end
