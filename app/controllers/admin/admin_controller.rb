require 'digest'

class Admin::AdminController < ApplicationController
  before_action :authenticate

  def authenticate
    authenticate_or_request_with_http_basic('Administration') do |username, password|
      md5 = Digest::MD5.new 
      md5 << password
      username == Rails.application.config.admin_username && \
        md5.hexdigest.to_s == Rails.application.config.admin_password 
    end
  end

  def index
    db_dir = Rails.application.config.apgcms_mz_intensity_dir
    @url_lib_dict = get_lib_url_dict(db_dir, '/admin/db/csv')
    @url_lib_keys = @url_lib_dict.keys.sort()
    cali_dir = Rails.application.config.apgcms_calibration_dir
    @url_calibration_dict = get_lib_url_dict(cali_dir, '/admin/calibration/csv')
    @url_calibration_keys = @url_calibration_dict.keys.sort()
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
