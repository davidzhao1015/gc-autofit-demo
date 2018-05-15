require 'digest'

class Admin::AdminController < ApplicationController
  before_filter :authenticate

  def authenticate
    authenticate_or_request_with_http_basic('Administration') do |username, password|
      md5 = Digest::MD5.new 
      md5 << password
      username == Rails.application.config.admin_username && \
        md5.hexdigest.to_s == Rails.application.config.admin_password 
    end
  end

  def index
    @urine_lib_url = "/admin/db/urines"
    @serum_lib_url = "/admin/db/serums"
    @saliva_lib_url = "/admin/db/salivas"
    @alkane_lib_url = "/admin/db/alkanes"

    @saliva_calibration_lib_url = "/admin/calibration/salivas"
    @serum_calibration_lib_url = "/admin/calibration/serums"
    @urinecholesterol_calibration_lib_url = "/admin/calibration/urinecholesterols"
    @urinesuccinicacidd4_calibration_lib_url = "/admin/calibration/urinesuccinicacidd4s"
    @urinetropicacid_calibration_lib_url = "/admin/calibration/urinetropicacids"

  end

end
