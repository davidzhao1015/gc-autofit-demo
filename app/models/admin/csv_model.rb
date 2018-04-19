require 'csv'

class  Admin::CsvModel

    class << self
        attr_accessor :csv_file
    end

    attr_accessor :row

    def self.save(rows)
        CSV.open(self.csv_file, "wb") do |csv|
            rows.each do |r|
                csv << r
            end
        end
    end

    def self.all_rows(file)
        items = []
        CSV.foreach(file, headers: true) do |row|
          items << self.new(row.to_hash)
        end
        items
    end

    def self.header
        CSV.read(self.csv_file, headers: true).headers
    end

    def self.csv
        self.csv_file
    end 

    def initialize(row_dict)
    	self.row = row_dict
    end

    

end
