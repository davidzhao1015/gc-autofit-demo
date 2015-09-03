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
                        # 'lib.internal': 'SERUM',
                        'lib.internal': submission.database.upcase,
                        # internalstd: 'Ribitol',
                        internalstd: submission.internal_standard,
                        process: 'PROFILING',
                        infoFileDir: submission.preprocessing_dir,
                        outdir: File.join(spectrum.sample_dir))
    if apgcms.success?
      spectrum.status = 'complete'
      # Save JSON results
      spectrum.json_results = File.open(File.join(spectrum.sample_dir, 'sample_spectrum.json') )
      spectrum.save!
    else
      spectrum.status = 'failed'
      spectrum.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end

    spectrum.runtime = Time.now - start_time
    spectrum.save!

  end

end
