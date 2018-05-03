class SpectrumWorker
  include Sidekiq::Worker
  include Rails.application.routes.url_helpers
  sidekiq_options :retry => true

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
    # FileUtils.symlink(submission.standards.spectrum_data.path, File.join(spectrum.sample_dir, 'Alkstd.mzXML'))
    # FileUtils.symlink(submission.blank.spectrum_data.path, File.join(spectrum.sample_dir, 'Blank.mzXML'))
    options = {infiledir: File.join(submission.input_dir),
              internalstd: submission.internal_standard,
              process: 'PROFILING',
              AlkaneRT: submission.alkane_rt.join(','),
              infoFileDir: submission.preprocessing_dir,
              outdir: File.join(spectrum.sample_dir),
              MFscore: submission.mf_score_threshold,
              log: spectrum.log_file}

    if submission.database == 'upload'
      options[:userlib] = "#{Rails.application.config.APGCMS_job_dir}/#{submission.secret_id}/input/user_library.csv"
      options[:usercal] = "#{Rails.application.config.APGCMS_job_dir}/#{submission.secret_id}/input/user_calibration.csv"
    else
      options['lib.internal'] = submission.database.upcase
    end
    apgcms = APGCMS.new(options)
    if apgcms.success?
      spectrum.status = 'complete'
      # Save JSON results
      json_path = File.join(spectrum.sample_dir, 'sample_spectrum.json')
      spectrum.json_results = File.open(json_path)
      # Remove original JSON file
      FileUtils.rm(json_path)
      spectrum.save!
    else
      spectrum.status = 'failed'
      spectrum.error = "There was a problem running GC-AutoFit: #{apgcms.errors} -- <a href=#{submission_spectrum_path(submission, spectrum, 'log')} target=_new>details</a>"
    end

  rescue StandardError => e
    spectrum.status = "failed"
    spectrum.error =  "[Rescue] There was a problem running GC-AutoFit. #{e.message}"
    spectrum.logger(e.message)
    spectrum.logger(e.backtrace.join("\n"))
  ensure
    spectrum.runtime = Time.now - start_time
    spectrum.save!
  end

end
