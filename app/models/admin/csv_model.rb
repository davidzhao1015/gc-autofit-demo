require 'csv'


class  Admin::CsvModel
  
    include ActiveModel::Validations
    include LibFileHash # in /lib
    include ClassLevelInheritableAttributes  # in /lib
    inheritable_attributes :flash, :csv_file_dir, :category
    @flash = {}
    @csv_file_dir = ''
    @category = ''

    attr_accessor :row, :mz, :intensity, :compound_name, :slope, :ri, :rt
    validates :compound_name, presence: true
    validates :mz, :intensity, presence: true, if: :header_include_mz?
    validates_format_of :mz, :intensity, with: /\A[\d\se\+]+\z/i,  if: :header_include_mz?
    #validates :slope,  presence: true, if: :header_include_slope?
    #validates_format_of :slope, with: /\A[\d\.]+\z/i,  if: :header_include_slope?
    validates :ri, :rt,  presence: true
    #validates_format_of :ri, :rt, with: /\A[\d\.\+\-e]+\z/i

    def self.save(row_objs, file, mode)
        # before save the file to lib. Make a 
        # copy with the current day. All the updates
        # will be made into the current file and the
        # dated file. Keep 20 past dated files for usage
        # Save today's file as copy first
        # params: row_objs (data structure containing csv data)
        #         file  (row_objs data is going to save to this file)
        #         mode  (action)

        # check if the number of files is over the limit.
        # If yes, delete the oldest one.
        today_file = "#{file}.#{DateTime.current().strftime('%Y%m%d')}"
        # Bypass validation if in destroy 
        unless mode == 'delete'
          unless self.validate_fields(row_objs)
            msg = "#{today_file} and #{file} NOT saved!"
            self.flash[:error] << msg
            return false
          end
        end
        self.check_and_keep_copies(file)
        
        #save today's file and current db file
        h = self.header(file)
        [today_file, file].each do |f|
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
        msg = "#{today_file} and #{file} saved!"
        if self.flash.key?(:success)
            self.flash[:success] << " #{msg}"
        else
            self.flash[:success] = [msg]
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
        if row_obj.row.key?('Slope')
          row_obj.slope = row_obj.row['Slope']
          row_obj.intercept = row_obj.row['Intercept']
        end
        row_obj.ri = row_obj.row['RI']
        row_obj.rt = row_obj.row['RT']

        unless row_obj.valid?
          msg = "#{'Field'.pluralize(row_obj.errors.messages.keys.length)} failed: " + 
                row_obj.row['SeqIndex'].to_s + ' ' +
                "|#{row_obj.row['Intensity']}| " +
                row_obj.errors.messages.map{|k,v| "#{k}=#{v}"}.join('&') + ". "
          if ! self.flash.key?(:error)
            self.flash[:error] = []
          end
          self.flash[:error] << msg
        end
      end

      if ! self.flash.key?(:error)
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

    def header_include_mz?
      self.row.keys.include?('MZ')
    end

    def header_include_slope?
      self.row.keys.include?('Slope')
    end
     

    def self.check_and_keep_copies(file)
        file_list = get_csv_file_list(file)
        need_number = Rails.application.config.APGCMS_copy_number
        if file_list.length >= need_number
          deleted_file_list = []
          begin
            # count the dated files, has to ignore the current base file.
            while (file_list.length - 1> need_number)
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

    def self.get_csv_file_list(base_file)
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

    def self.last_index(file)
      self.all_rows(file).last ? self.all_rows(file).last.row['SeqIndex'] : 0
    end  

end
