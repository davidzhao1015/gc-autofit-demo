require "rails_helper"


RSpec.describe Admin::CsvModel, :type => :model do

  
  descendants = [Admin::Db::Alkane, 
                Admin::Db::Saliva, 
                Admin::Db::Serum, 
                Admin::Db::Urine,
                Admin::Calibration::Saliva,
                Admin::Calibration::Serum,
                Admin::Calibration::Urinecholesterol,
                Admin::Calibration::Urinesuccinicacidd4,
                Admin::Calibration::Urinetropicacid ]

  descendants.each do |cls|

	  describe "Check get_csf_file_list() function" do
	    it "equal to csv default alkane file's amount" do
	      csv = cls.csv_file
	      how_many = cls.get_csf_file_list(csv).length
	      how_many1 = Dir.glob("#{csv}*").length
	      how_many.should eq(how_many1)
	    end
	  end

	  if cls.header.include?('MZ')
		  describe "Test validate self.validate_fields function" do
		    it "Not valid with rows objects with invalid mz fileds" do
		      yaml = yaml_fixture_file("csv_models.yml")
		      row_dict = yaml['csv_model_with_no_MZ']
		      row_objs = [cls.new(row_dict)]
		      rn = cls.validate_fields(row_objs)
		      expect(rn).to eq(false)
		    end

		    it "Not valid with rows objects with invalid Intensity fileds" do
		      yaml = yaml_fixture_file("csv_models.yml")
		      row_dict = yaml['csv_model_with_no_Intensity']
		      row_objs = [cls.new(row_dict)]
		      rn = cls.validate_fields(row_objs)
		      expect(rn).to eq(false)
		    end

		    it "Not valid with rows objects with invalid Compound name" do
		      yaml = yaml_fixture_file("csv_models.yml")
		      row_dict = yaml['csv_model_with_no_Compound_name']
		      row_objs = [cls.new(row_dict)]
		      rn = cls.validate_fields(row_objs)
		      expect(rn).to eq(false)
		    end
		  end
	  elsif cls.header.include?('Slope')
	      describe "Test validate self.validate_fields function" do
		  	  it "Not valid with rows objects with invalid Compound name" do
			      yaml = yaml_fixture_file("csv_models.yml")
		      	  row_dict = yaml['csv_model_with_Slope_no_Compound_name']
			      row_objs = [cls.new(row_dict)]
			      rn = cls.validate_fields(row_objs)
			      expect(rn).to eq(false)
		      end
		  end
	  end
  end
end