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
		      row_dict = { SeqIndex: 1,
		                   HMDB_ID: '',
		                   Compound: 'xx',
		                   CompoundWithTMS: '',
		                   RT: '',
		                   RI: '',
		                   TargetIon: '',
		                   QIon: '',
		                   IonRatio: '',
		                   MZ: '',
		                   Intensity: 100,
		                   Source: '',
		                   'Note(mixtures)': '',
		                   To_do: '' }
		      row_objs = [cls.new(row_dict)]
		      rn = cls.validate_fields(row_objs)
		      expect(rn).to eq(false)
		    end

		    it "Not valid with rows objects with invalid Intensity fileds" do
		      row_dict = { SeqIndex: 1,
		                   HMDB_ID: '',
		                   Compound: 'xx',
		                   CompoundWithTMS: '',
		                   RT: '',
		                   RI: '',
		                   TargetIon: '',
		                   QIon: '',
		                   IonRatio: '',
		                   MZ: '100',
		                   Intensity: '',
		                   Source: '',
		                   'Note(mixtures)': '',
		                   To_do: '' }
		      row_objs = [cls.new(row_dict)]
		      rn = cls.validate_fields(row_objs)
		      expect(rn).to eq(false)
		    end

		    it "Not valid with rows objects with invalid Compound name" do
		      row_dict = { SeqIndex: 1,
		                   HMDB_ID: '',
		                   Compound: '',
		                   CompoundWithTMS: '',
		                   RT: '',
		                   RI: '',
		                   TargetIon: '',
		                   QIon: '',
		                   IonRatio: '',
		                   MZ: '100',
		                   Intensity: '100',
		                   Source: '',
		                   'Note(mixtures)': '',
		                   To_do: '' }
		      row_objs = [cls.new(row_dict)]
		      rn = cls.validate_fields(row_objs)
		      expect(rn).to eq(false)
		    end
		  end
	  elsif cls.header.include?('Slope')
	      describe "Test validate self.validate_fields function" do
		  	  it "Not valid with rows objects with invalid Compound name" do
			      row_dict = { SeqIndex: 1,
			                   HMDB_ID: '',
			                   Compound: '', 
			                   LOQ: '',
			                   Intercept: '',
			                   Slope: '',
			                   rsq: '',
			                   adjrsq: '',
			                   Src: '',
			                   RT: '',
			                   RI: '',
			                   To_do: ''}
			      row_objs = [cls.new(row_dict)]
			      rn = cls.validate_fields(row_objs)
			      expect(rn).to eq(false)
		      end
		  end
	  end
  end
end