class Submission < ActiveRecord::Base
  # STATES = %w[ validating queued processing processed profiling complete failed ]
  STATES = %w[ validating queued processing complete failed ]
  # PROCESSED = %w[ processed profiling complete ]
  FINALIZED_STATES = %w[ complete failed ]

  WORKING_DIR = Rails.env.test? ? Rails.root.join('tmp/working') : Rails.root.join('APGCMS_working')
  DAYS_TO_KEEP_SUBMISSIONS = 7
  SECRET_ID_LENGTH = 20

  DATABASES = {
    #db      #display
    'serum' => 'Serum',
    'urine' => 'Urine',
    # 'saliva' => 'Saliva',
    'upload' => 'Upload Your Library'

  }
  INTERNAL_STANDARDS = %w[ Ribitol Cholesterol Other ]

  has_many :spectra, dependent: :destroy
  has_one :standards, -> { where category: 'standards'}, class_name: Spectrum
  has_one :blank,     -> { where category: 'blank'}, class_name: Spectrum
  has_many :samples,  -> { where category: 'sample'}, class_name: Spectrum

  accepts_nested_attributes_for :spectra

  has_attached_file :profile_library, path: ':input_dir/user_library.csv'
  has_attached_file :calibration, path: ':input_dir/user_calibration.csv'

  validates :secret_id, presence: true, uniqueness: true
  validates :status, inclusion: { in: STATES }
  validates :database, inclusion: { in: DATABASES.keys }

  before_validation :generate_secret
  # after_create      :start_work
  after_destroy     :delete_working_dir

  serialize :database_subset, Array


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
    job_id = SubmissionWorker.perform_async(self.id)
    self.update!(job_id: job_id)
  end

  def start_profiling
    if self.complete? && !self.profile?
      self.update!(profile: true)
      self.samples.each do |sample|
        # SpectrumWorker.new.perform(sample.id)
        sample.start_work
      end
    end
  end

  def finalized?
    FINALIZED_STATES.include?(self.status)
  end

  def samples_all_finalized?
    self.samples.all? { |s| s.finalized? }
  end

  def samples_any_finalized?
    self.samples.any? { |s| s.finalized? }
  end

  def failed?
    self.status == 'failed'
  end

  def complete?
    self.status == 'complete'
  end

  def processing?
    ['queued', 'processing'].include?(self.status)
  end

  # def processed?
  #   PROCESSED.include?(self.status)
  # end

  def working_dir
    File.join(WORKING_DIR, 'Results', self.secret_id)
  end

  def input_dir
    File.join(self.working_dir, 'input')
  end

  def preprocessing_dir
    File.join(self.working_dir, 'preprocessing')
  end

  def log_file
    File.join(preprocessing_dir, 'log.txt')
  end

  def profiling_dir
    File.join(self.working_dir, 'profiling')
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

  def display_status
    self.status.capitalize
  end

  def alkane_rt
    rt = []
    if self.standards.json_results.present?
      json_results = JSON.parse(File.read(self.standards.json_results.path))
      json_results['labels'].each do |label|
        rt << label['text'].strip.sub(/^\D+/, '')
      end
    end
    rt
  end

  private

  # Generate private URL
  def generate_secret
    self.secret_id ||= SecureRandom.hex(SECRET_ID_LENGTH)
  end

end
