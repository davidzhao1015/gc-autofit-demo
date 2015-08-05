class Submission < ActiveRecord::Base
  STATES = %w[ validating queued processing profiling complete failed ]
  FINALIZED_STATES = %w[ complete failed ]

  WORKING_DIR = Rails.env.test? ? Rails.root.join('tmp/working') : Rails.root.join('APGCMS_working')
  DAYS_TO_KEEP_SUBMISSIONS = 7
  SECRET_ID_LENGTH = 20

  DATABASES = {
    #db      #display
    'serum' => 'Serum',
    'urine' => 'Urine',
    'custom' => 'Custom',
    'upload' => 'Upload Your Library'

  }
  INTERNAL_STANDARDS = %w[ Ribitol Cholesterol ]

  has_many :spectra, dependent: :destroy
  has_one :standards, -> { where category: 'standards'}, class_name: Spectrum
  has_one :blank,     -> { where category: 'blank'}, class_name: Spectrum
  has_many :samples,  -> { where category: 'sample'}, class_name: Spectrum

  accepts_nested_attributes_for :spectra


  validates :secret_id, presence: true, uniqueness: true
  validates :status, inclusion: { in: STATES }
  validates :database, inclusion: { in: DATABASES.keys }

  before_validation :generate_secret
  after_create      :start_work
  after_destroy     :delete_working_dir

  serialize :custom_database, Array


  def self.delete_old_submissions
    old_submissions = Submission.where("created_at < ?", DAYS_TO_KEEP_SUBMISSIONS.days.ago)
    # puts "Submissions older than #{DAYS_TO_KEEP_SUBMISSIONS} days: #{old_submissions.count}"
    if old_submissions.count > 0
      # puts "Deleting..."
      old_submissions.destroy_all
      old_submissions = Submission.where("created_at < ?", DAYS_TO_KEEP_SUBMISSIONS.days.ago)
      # puts "Submissions older than #{DAYS_TO_KEEP_SUBMISSIONS} days: #{old_submissions.count}"
    end
  end

  def start_work
    create_working_dir
    self.update!(status: 'queued')
    # job_id = SubmissionWorker.perform_async(self.id)
    # self.update!(job_id: job_id)
    SubmissionWorker.new.perform(self.id)
  end

  def finalized?
    FINALIZED_STATES.include?(self.status)
  end

  def failed?
    self.status == 'failed'
  end

  def working_dir
    File.join(WORKING_DIR, 'Results', self.secret_id)
  end

  def input_dir
    File.join(self.working_dir, 'input')
  end

  def preprocessing_dir
    File.join(self.working_dir, 'preprocessing')
  end

  def create_working_dir
    FileUtils.mkdir_p(self.working_dir)
  end

  def delete_working_dir
    FileUtils.rm_r(self.working_dir) if File.exists?(self.working_dir)
  end

  def to_param
    self.secret_id
  end

  private

  # Generate private URL
  def generate_secret
    self.secret_id ||= SecureRandom.hex(SECRET_ID_LENGTH)
  end

end
