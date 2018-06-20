require 'csv'


class  Lib::CsvModel
    
    include LibFileHash # in /lib
    include ClassLevelInheritableAttributes  # in /lib
    inheritable_attributes :flash, :csv_file_dir, :category
    @flash = {}
    @csv_file_dir = ''
    @category = ''

    attr_accessor :row, :mz, :intensity, :compound_name, :slope

    def self.all_rows(file)
        items = []
        CSV.foreach(file, headers: true) do |row|
          items << self.new(row.to_hash)
        end
        items
    end

    def self.header(file)
        CSV.read(file, headers: true).headers()
    end

    def self.csv_file(type)
        csv_file_dict = self.get_lib_file_dict(self.csv_file_dir)
        csv_file_dict = csv_file_dict.with_indifferent_access
        begin 
            csv_file_dict[type]
        rescue Exception => e  
            self.flash[:error] << e.message 
            ''
        else
            csv_file_dict[type]
        end
    end

    def initialize(row_dict)
    	self.row = row_dict
    end
      

end
