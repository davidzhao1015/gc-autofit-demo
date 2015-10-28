class Spectrum < ActiveRecord::Base
  STATES = %w[ validating queued profiling complete failed ]
  FINALIZED_STATES = %w[ complete failed ]
  CATEGORIES = %w[ standards blank sample ]

  belongs_to :submission, inverse_of: :spectra

  has_attached_file :spectrum_data, path: ':input_dir/:sample_name.:extension'
  has_attached_file :json_results,  path: ':sample_dir/spectrum.:extension'
  has_attached_file :plot

  validates_attachment_file_name :spectrum_data, :matches => [/(CDF|mzXML)\Z/]
  validates_attachment_file_name :json_results, :matches => [/json\Z/]
  validates_attachment_file_name :plot, :matches => [/png\Z/]

  validates :status, inclusion: { in: STATES }, allow_blank: true
  validates :category, inclusion: { in: CATEGORIES }

  def sample_dir
    Rails.root.join(self.submission.profiling_dir, self.sample_name)
  end

  def input_dir
    self.submission.input_dir
  end

  def log_file
    File.join(sample_dir, 'log.txt')
  end

  def csv_file
    case self.category
    when 'blank'
      File.join(self.submission.preprocessing_dir, 'Blank_profiled.csv')
    when 'standards'
      File.join(self.submission.preprocessing_dir, 'Alkstd_alkanePeakList.csv')
    else
      File.join(sample_dir, 'sample_profiled.csv')
    end
  end

  def sample_name
    case self.category
    when 'blank'
      'Blank'
    when 'standards'
      'Alkstd'
    else
      "Sample_#{self.id}"
    end
  end

  def name
    self.spectrum_data_file_name
  end

  def display_runtime
    if self.runtime.nil?
      'NA'
    else
      min = self.runtime / 60
      sec = self.runtime % 60
      min > 0 ? "#{min} minutes #{sec} seconds" : "#{sec} seconds"
    end
  end

  def display_status
    text = self.status.capitalize
    text = text + " (#{self.display_queue_position})" if self.status == 'queued'
    text = text + '...' unless self.finalized?
    text
  end

  def display_queue_position
    position = self.queue_position
    if position.present?
      position += 1
      "#{position} " + 'submission'.pluralize(position) + " ahead of yours"
    else
      "You're next!"
    end
  end

  # Jobs are stored in the default queue
  # Jobs are added to the beginning of the queue
  # A value of nil means the job is not queued and should be processing
  # A value of 0 means the job is next
  def queue_position
    queue = Sidekiq::Queue.new
    jids = queue.map { |j| j.jid }.reverse
    jids.index(self.job_id)
  end

  def start_work
    self.update!(status: 'queued')
    job_id = SpectrumWorker.perform_async(self.id)
    self.update!(job_id: job_id)
  end

  def finalized?
    FINALIZED_STATES.include?(self.status)
  end

  def complete?
    self.status == 'complete'
  end

  def failed?
    self.status == 'failed'
  end

  def next
    spectra = self.submission.spectra
    index = spectra.index(self)
    if index >= (spectra.length - 1)
      spectra[0]
    else
      spectra[index + 1]
    end
  end

  def prev
    spectra = self.submission.spectra
    index = spectra.index(self)
    if index == 0
      spectra[spectra.length - 1]
    else
      spectra[index - 1]
    end
  end

end
