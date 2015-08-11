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
      # Save Standards
      standards = submission.standards
      standards.json_results = File.open(File.join(submission.preprocessing_dir, 'Alkstd_spectrum.json') )
      standards.save!
      # Save Blank
      blank = submission.blank
      blank.json_results = File.open(File.join(submission.preprocessing_dir, 'Blank_spectrum.json') )
      blank.save!
      # Link Samples
      submission.samples.each do |sample|

      end
    else
      submission.status = 'failed'
      submission.error = "There was a problem running GC-AutoFit: #{apgcms.errors}"
    end
    submission.save!

  end

end
