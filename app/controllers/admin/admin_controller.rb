require 'digest'

class Admin::AdminController < ApplicationController
  before_filter :authenticate

  def authenticate
    authenticate_or_request_with_http_basic('Administration') do |username, password|
      md5 = Digest::MD5.new 
      md5 << password
      username == Rails.application.config.admin_username && md5.hexdigest.to_s == Rails.application.config.admin_password 
    end
  end

  def index
    @urine_lib_url = "/admin/db/urines?file=#{Rails.application.config.urine_lib_file}"
    @serum_lib_url = "/admin/db/serums?file=#{Rails.application.config.serum_lib_file}"
    @saliva_lib_url = "/admin/db/salivas?file=#{Rails.application.config.saliva_lib_file}"
    @alkane_lib_url = "/admin/db/alkanes?file=#{Rails.application.config.alkane_lib_file}"

    @saliva_calibration_lib_url = "/admin/calibration/salivas?file=#{Rails.application.config.saliva_calibration_lib_file}"
    @serum_calibration_lib_url = "/admin/calibration/serums?file=#{Rails.application.config.serum_calibration_lib_file}"
    @urinecholesterol_calibration_lib_url = "/admin/calibration/urinecholesterols?file=#{Rails.application.config.urinecholesterol_lib_file}"
    @urinesuccinicacidd4_calibration_lib_url = "/admin/calibration/urinesuccinicacidd4s?file=#{Rails.application.config.urinesuccinicacidd4_lib_file}"
    @urinetropicacid_calibration_lib_url = "/admin/calibration/urinetropicacids?file=#{Rails.application.config.urinetropicacid_lib_file}"

  end

end
