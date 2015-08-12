class Spectrum < ActiveRecord::Base
  STATES = %w[ validating queued profiling complete failed ]
  FINALIZED_STATES = %w[ complete failed ]

  belongs_to :submission

  has_attached_file :spectrum_data, path: ':input_dir/:sample_name.:extension'
  has_attached_file :json_results,  path: ':sample_dir/spectrum.:extension'
  has_attached_file :plot

  validates_attachment_file_name :spectrum_data, :matches => [/mzXML\Z/]
  validates_attachment_file_name :json_results, :matches => [/json\Z/]
  validates_attachment_file_name :plot, :matches => [/png\Z/]

  validates :status, inclusion: { in: STATES }, allow_blank: true

  def sample_dir
    Rails.root.join(self.submission.profiling_dir, self.sample_name)
  end

  def input_dir
    self.submission.input_dir
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

  def finalized?
    FINALIZED_STATES.include?(self.status)
  end

  def failed?
    self.status == 'failed'
  end

  Paperclip.interpolates :input_dir do |attachment, style|
    attachment.instance.input_dir
  end
  Paperclip.interpolates :sample_name do |attachment, style|
    attachment.instance.sample_name
  end
  Paperclip.interpolates :sample_dir do |attachment, style|
    attachment.instance.sample_dir
  end
end
