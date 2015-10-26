class SpectrumWorker
  include Sidekiq::Worker
  sidekiq_options :retry => false

  def perform(spectrum_id)
    start_time = Time.now
    spectrum = Spectrum.find(spectrum_id)
    submission = spectrum.submission
    spectrum.update!(status: 'profiling')

    FileUtils.mkdir_p(spectrum.sample_dir)
    FileUtils.symlink(spectrum.spectrum_data.path, File.join(spectrum.sample_dir, 'sample.mzXML'))
    # FileUtils.symlink(submission.standards.spectrum_data.path, File.join(spectrum.sample_dir, 'Alkstd.mzXML'))
    # FileUtils.symlink(submission.blank.spectrum_data.path, File.join(spectrum.sample_dir, 'Blank.mzXML'))

    apgcms = APGCMS.new(infiledir: File.join(spectrum.sample_dir),
                        'lib.internal': submission.database.upcase,
                        internalstd: submission.internal_standard,
                        process: 'PROFILING',
                        AlkaneRT: submission.alkane_rt.join(','),
                        infoFileDir: submission.preprocessing_dir,
                        outdir: File.join(spectrum.sample_dir),
                        log: spectrum.log_file)
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
      spectrum.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end

    spectrum.runtime = Time.now - start_time
    spectrum.save!

  end

end
