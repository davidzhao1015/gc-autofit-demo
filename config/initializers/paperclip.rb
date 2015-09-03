require 'paperclip/media_type_spoof_detector'
module Paperclip
  class MediaTypeSpoofDetector
    def spoofed?
      false
    end
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
