class Spectrum < ActiveRecord::Base

  belongs_to :submission

  has_attached_file :spectrum_data,
                    path: ':input_dir/:sample_name.:extension'

  has_attached_file :json_results,
                    path: ':sample_dir/spectrum.:extension'

  validates_attachment_file_name :spectrum_data, :matches => [/mzXML\Z/]
  validates_attachment_file_name :json_results, :matches => [/json\Z/]

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
