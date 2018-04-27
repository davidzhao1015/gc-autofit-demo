require "rails_helper"

RSpec.describe Admin::AdminController, type: :controller do
  # login to http basic auth
  include AuthHelper
  before(:each) do
    http_login
  end
  
  describe 'Index' do
	  it "index responds successfully" do
	    get :index
	    expect(response.status).to eq(200)
	  end
  end
end
