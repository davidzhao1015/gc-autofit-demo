require "rails_helper"
require "support/render_views"

RSpec.describe Admin::CsvController, type: :controller do
  # login to http basic auth
  include AuthHelper
  before(:each) do
    http_login
  end
  
  #descendants = ObjectSpace.each_object(Class).select { |klass| klass < self }
  descendants = [Admin::Db::AlkanesController, 
                Admin::Db::SalivasController, 
                Admin::Db::SerumsController, 
                Admin::Db::UrinesController]

  descendants.each do |cls|

    describe cls, type: :controller do

      describe 'Index' do
        it "renders the index template" do
         get :index
         expect(response).to render_template("index")
        end

        it "renders the index template" do
          get :index
          expect(response.body).to match /<a.*Admin/m
        end

        it "index responds successfully" do
         get :index
         expect(response.status).to eq(200)
        end

        it "file not empyt" do
         file = ''
         get :index
         expect(assigns(:file)).not_to be_empty
        end

        it "csv file list is more than one" do
        # base file must exist
         csv_files = cls.model.get_csf_file_list(cls.model.csv_file)
         get :index
         expect(assigns(:csv_files).length).to be >= 1
        end
      end

      describe 'New' do 
        it "index = last_index + 1" do
         index  = cls.model.last_index.to_i + 1
         get :new
         assigns(:row)['SeqIndex'].should eq(index)
        end
      end

    end
  end

end
