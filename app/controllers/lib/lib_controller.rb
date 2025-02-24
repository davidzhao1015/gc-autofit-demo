
class Lib::LibController < ApplicationController
  
  def index
    db_dir = Rails.application.config.apgcms_mz_intensity_dir
    @url_lib_dict = get_lib_url_dict(db_dir, '/lib/db')
    @url_lib_keys = @url_lib_dict.keys.sort().select{|file| file == "Urine"}
    cali_dir = Rails.application.config.apgcms_calibration_dir
    @url_calibration_dict = get_lib_url_dict(cali_dir, '/lib/calibration')
    @url_calibration_keys = @url_calibration_dict.keys.sort()    
  end


  def mz_db
    db_dir = Rails.application.config.apgcms_mz_intensity_dir
    @url_lib_dict = get_lib_url_dict(db_dir, '/lib/db')
    @url_lib_keys = @url_lib_dict.keys.sort().select{|file| file == "Urine"}

  end

  def calibration_db
    cali_dir = Rails.application.config.apgcms_calibration_dir
    @url_calibration_dict = get_lib_url_dict(cali_dir, '/lib/calibration')
    @url_calibration_keys = @url_calibration_dict.keys.sort().select { |file| file.include? "Urine"  }
  end

  private
    def get_lib_url_dict(dir, url_head)
      file_list = Dir.glob("#{dir}/*.csv")
      file_hash = file_list.map do |f|
              f_b = File.basename(f)
              if f_b =~/lib_(\w+?)_CalibrationCurve_?(.*).csv/ # for calibration
                type = "#{$1}#{$2}".downcase
                ["#{$1} #{$2}".titleize, "#{url_head}?type=#{type}"]
              elsif f_b =~/lib_(\w+?).csv/ # for mz_intensity
                [$1.titleize, "#{url_head}?type=#{$1.downcase}"]
              end
            end.to_h        
    end

end
