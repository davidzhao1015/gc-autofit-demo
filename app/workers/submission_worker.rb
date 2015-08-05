class SubmissionWorker
  include Sidekiq::Worker
  sidekiq_options :retry => false

  def perform(submission_id)
    submission = Submission.find(submission_id)
    submission.update!(status: 'processing')

    apgcms = APGCMS.new(infiledir: File.join(submission.input_dir),
                        'lib.internal': 'SERUM',
                        internalstd: 'Ribitol',
                        process: 'PREPROCESSING',
                        outdir: File.join(submission.preprocessing_dir))
    if apgcms.success?
      submission.status = 'complete'
    else
      submission.status = 'failed'
      submission.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end
    submission.save!

  end

end
