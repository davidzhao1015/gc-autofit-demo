require "rails_helper"

RSpec.configure do |config|
  config.filter_rails_from_backtrace!
end

RSpec.describe "Controller", :type => :controller do
  controller do
    def index
      #raise "Something went wrong."
    end
  end

  describe "GET index" do
    it "raises an error" do
      #get :index
    end
  end
end