class SubmissionWorker
  include Sidekiq::Worker
  sidekiq_options :retry => false

  def perform(submission_id)
    submission = Submission.find(submission_id)
    submission.update!(status: 'processing')

    apgcms = APGCMS.new(infiledir: File.join(submission.working_dir, 'input'),
                        'lib.internal': 'SERUM',
                        internalstd: 'Ribitol',
                        plotonly: 'TRUE')
    if apgcms.success?
      submission.status = 'complete'
    else
      submission.status = 'failed'
      submission.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end
    submission.save!

  end

end
