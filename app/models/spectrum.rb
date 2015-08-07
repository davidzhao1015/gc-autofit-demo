class Spectrum < ActiveRecord::Base

  belongs_to :submission

  has_attached_file :spectrum_data,
                    path: ':paperclip_dir/:paperclip_name.:extension'

  validates_attachment_file_name :spectrum_data, :matches => [/mzXML\Z/]

  def paperclip_dir
    Rails.root.join(self.submission.input_dir)
  end

  def paperclip_name
    case self.category
    when 'blank'
      'Blank'
    when 'standards'
      'Alkstd'
    else
      "Sample_#{self.id}"
    end
  end

  Paperclip.interpolates :paperclip_dir do |attachment, style|
    attachment.instance.paperclip_dir
  end
  Paperclip.interpolates :paperclip_name do |attachment, style|
    attachment.instance.paperclip_name
  end
end
