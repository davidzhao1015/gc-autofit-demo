require 'csv'

class  Admin::CsvModel

    class << self
        attr_accessor :csv_file, :flash
    end

    attr_accessor :row


    def self.save(rows)
        # before save the file to lib. Make a 
        # copy with the current day. All the updates
        # will be made into the current file and the
        # dated file. Keep 20 past dated files for usage
        # Save today's file as copy first

        # check if the number of files is over the limit.
        # If yes, delete the oldest one.
        self.check_and_keep_copies()

        today_file = "#{self.csv_file}.#{DateTime.current().strftime('%Y%m%d')}"
        #save today's file and current db file
        [today_file, self.csv_file].each do |f|
          CSV.open(f, "wb") do |csv|
            rows.each do |r|
              csv << r
            end
          end
        end
        if self.flash.key?(:success)
            self.flash[:success] << " #{today_file} and #{self.csv_file} save!"
        else
            self.flash[:success] = "#{today_file} and #{self.csv_file} save!"
        end
    end


    def self.all_rows(file)
        items = []
        puts 'PPPPPPPP'
        puts file
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

     

    def self.check_and_keep_copies
        file_list = get_csf_file_list(self.csv_file)
        if file_list.length >= Rails.application.config.APGCMS_copy_number
          deleted_file_list = []
          begin
            # count the dated files, has to ignore the current base file.
            while (file_list.length - 1> Rails.application.config.APGCMS_copy_number)
                File.delete(file_list[-1])
                deleted_file_list << file_list[-1]
                file_list = file_list[0..-2]
            end
          rescue Exception => e  
            self.flash[:error] = e.message  
          else
            self.flash[:success] = "#{'File'.pluralize(deleted_file_list.length)} #{deleted_file_list.join(',')} deleted!<br/>"
          end
        end
    end

    def self.get_csf_file_list(base_file)
      file_list = Dir.glob("#{base_file}*")
      #reorder file list, the oldest is the last
      self.reorder_by_date(file_list, base_file)
    end 

    

    def self.reorder_by_date(csv_files, base_file)
        puts 'pppppppp'
        puts csv_files
      dict = {}
      tmp_list = csv_files.select do |f|
            f != base_file
      end
      tmp_list.each do |f|
        if f =~/\.(\d+)$/
          dict[$1] = f
        end
      end
      keys = dict.keys.sort().reverse
      values = keys.map do |k|
        dict[k]
      end 
      [base_file] + values
    end   

end
