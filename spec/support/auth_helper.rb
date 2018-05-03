module AuthHelper
  def http_login
    user = ENV["ADMIN_USERNAME"]
    pw = ENV["ADMIN_PASSWORD"]
    request.env['HTTP_AUTHORIZATION'] = ActionController::HttpAuthentication::Basic.encode_credentials(user,pw)
  end  
end
