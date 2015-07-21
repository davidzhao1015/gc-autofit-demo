class Spectrum < ActiveRecord::Base
  belongs_to :submission

  has_attached_file :spectrum_data,
                    path: ':paperclip_dir/:filename'

  validates_attachment_file_name :spectrum_data, :matches => [/mzXML\Z/]

  def paperclip_dir
    Rails.root.join(self.submission.working_dir, 'input')
  end

  Paperclip.interpolates :paperclip_dir do |attachment, style|
    attachment.instance.paperclip_dir
  end
end
