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
    puts e.backtrace.join("\n")
    spectrum.status = "failed"
    spectrum.error =  "[Rescue from spectrum worker] There was a problem running GC-AutoFit. #{e.message}"
    spectrum.logger(e.message)
    spectrum.logger(e.backtrace.join("\n"))
  ensure
    spectrum.runtime = Time.now - start_time
    spectrum.save!
    # puts "spectrum => #{spectrum.inspect}"
  end

  def update_library(submission, spectrum)
    if submission.update_library
      mz_int_file = spectrum.mzint_for_db_file_path
      mixture_file = submission.profile_library.path
      lib_file = submission.lib_file_path
      tolerance = 0.1
  
      # Read the mixture file
      mixture_data = CSV.read(mixture_file, headers: true)
  
      # Read the mzInt4DB file
      mz_int_data = CSV.read(mz_int_file, headers: true)
  
      # Read the library file
      lib_data = CSV.read(lib_file, headers: true)
      original_headers = lib_data.headers

      # Find the last 'SeqIndex' in the lib_data and initialize a counter
      last_seq_index = lib_data['SeqIndex'].map(&:to_i).max || 1
  
      # Iterate through the mixture data and update the library file
      mixture_data.each do |row|
        rt = row['RT'].to_f
        # Find the matching row based on the tolerance
        matching_mz_int_row = mz_int_data.find do |r|
          (r['rt_min'].to_f - rt).abs <= tolerance  # Use absolute value for comparison
        end

        # If no RT matches between generated csv file and mixture file
        next if matching_mz_int_row.nil?
        # Increment the SeqIndex counter for each new row
        last_seq_index += 1
  
        # Create a new row with the original headers and add the additional information
        new_row = original_headers.map do |header|
          case header
          when 'SeqIndex' then last_seq_index
          when 'HMDB_ID' then row['HMDB_ID']
          when 'Compound' then row['Compound']
          when 'CompoundwithTMS' then row['CompoundwithTMS']
          when 'TargetIon' then row['TargetIon']
          when 'QIon' then row['QIon']
          when 'RT' then rt
          when 'RI' then matching_mz_int_row['RI']
          when 'MZ' then matching_mz_int_row['mz']
          when 'Intensity' then matching_mz_int_row['intensity']
          else nil
          end
        end
        lib_data << new_row
      end
  
      # Save the updated library file
      timestamp = Time.now.strftime('%Y%m%d%H%M')
      # Create a backup of the older lib file with the current timestamp
      backup_lib_file_path = lib_file.sub('.csv', "_#{timestamp}.csv")
      FileUtils.cp(lib_file, backup_lib_file_path)
      
      # Save the updated library file (overwrite the original lib_file)
      CSV.open(lib_file, 'w') do |csv|
        csv << original_headers
        lib_data.each do |row|
          csv << row
        end
      end
    end
  end
end
