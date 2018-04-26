require 'csv'

class  Admin::CsvModel
    include ActiveModel::Validations

    class << self
        attr_accessor :csv_file, :flash
    end

    attr_accessor :row, :mz, :intensity, :compound_name
    validates :compound_name, :mz, :intensity, presence: true
    validates_format_of :mz, :intensity, with: /\A[\d\s]+\z/i

    def self.save(row_objs, mode)
        # before save the file to lib. Make a 
        # copy with the current day. All the updates
        # will be made into the current file and the
        # dated file. Keep 20 past dated files for usage
        # Save today's file as copy first

        # check if the number of files is over the limit.
        # If yes, delete the oldest one.
        today_file = "#{self.csv_file}.#{DateTime.current().strftime('%Y%m%d')}"
        # Bypass validation if in destroy 
        unless mode == 'delete'
          unless self.validate_fields(row_objs)
            msg = "#{today_file} and #{self.csv_file} NOT saved!"
            self.flash['error'] += msg
            return false
          end
        end
        self.check_and_keep_copies()
        
        #save today's file and current db file
        h = self.header
        [today_file, self.csv_file].each do |f|
          CSV.open(f, "wb") do |csv|
            csv << h
            row_objs.each do |row_obj|
              r = h.map do |key|
                    row_obj.row[key]
              end
              csv << r
            end
          end
        end
        msg = "#{today_file} and #{self.csv_file} saved!"
        if self.flash.key?(:success)
            self.flash[:success] << " #{msg}"
        else
            self.flash[:success] = msg
        end
        return true
    end

    def self.validate_fields(row_objs)
      row_objs.each do |row_obj|
        row_obj.compound_name = row_obj.row['Compound']
        if row_obj.row.key?('MZ')
          row_obj.mz = row_obj.row['MZ']
          row_obj.intensity = row_obj.row['Intensity']
        end
        unless row_obj.valid?
          msg = "#{'Filed'.pluralize(row_obj.errors.messages.keys.length)} failed: " + 
                 row_obj.errors.messages.map{|k,v| "#{k}=#{v}"}.join('&') + ". "
          self.flash['error'] ||= msg
        end
      end
      unless self.flash['error']
        true 
      else
        false
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
        CSV.read(self.csv_file, headers: true).headers()
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
