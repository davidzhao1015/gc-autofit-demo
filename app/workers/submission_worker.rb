class SubmissionWorker
  include Sidekiq::Worker
  sidekiq_options :retry => true

  def perform(submission_id)
    start_time = Time.now
    submission = Submission.find(submission_id)
    submission.update!(status: 'processing')
    options = {infiledir: File.join(submission.input_dir),
              internalstd: submission.internal_standard,
              process: 'PREPROCESSING',
              outdir: File.join(submission.preprocessing_dir),
              MFscore: submission.mf_score_threshold,
              log: submission.log_file}

    if submission.database == 'upload'
      options[:userlib] = "#{Rails.application.config.APGCMS_job_dir}/#{submission.secret_id}/input/user_library.csv"
      options[:usercal] = "#{Rails.application.config.APGCMS_job_dir}/#{submission.secret_id}/input/user_calibration.csv"
    else
      options['lib.internal'] = submission.database.upcase
    end
    apgcms = APGCMS.new(options)
    if apgcms.success?
      submission.status = 'complete'
      # Save Standards
      standards = submission.standards
      if standards
        standards.json_results = File.open(File.join(submission.preprocessing_dir, 'Alkstd_spectrum.json') )
        standards.plot = File.open(File.join(submission.preprocessing_dir, 'Plot_TIC_Alkstd.png') )
        standards.save!
      end
      # Save Blank
      blank = submission.blank
      if blank
        blank.json_results = File.open(File.join(submission.preprocessing_dir, 'Blank_spectrum.json') )
        blank.plot = File.open(File.join(submission.preprocessing_dir, 'Plot_TIC_Blank.png') )
        blank.save!
      end
      # Link Samples
      submission.samples.each do |sample|
        sample.plot = File.open(File.join(submission.preprocessing_dir, "Plot_TIC_Sample_#{sample.id}.png") )
        sample.save!
      end
    else
      submission.status = 'failed'
      submission.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end

  rescue StandardError => e
    submission.status = "failed"
    submission.error =  "[ from spectrum worker] There was a problem running GC-AutoFit. #{e.message}"
    submission.logger(e.message)
    submission.logger(e.backtrace.join("\n"))
  ensure
    submission.runtime = Time.now - start_time
    submission.save!
  end

end
