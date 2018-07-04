require 'csv'


class  Makedb::CsvModel
    
    include ActiveModel::Validations
    include LibFileHash # in /lib
    include ClassLevelInheritableAttributes  # in /lib
    inheritable_attributes :flash, :template_file, :db_tmp_dir, :category
    @flash = {}
    @template_file = ''
    @db_tmp_dir = ''
    @category = ''

    attr_accessor :row, :mz, :intensity, :compound_name, :slope, :intercept, :ri, :rt
    validates :compound_name, presence: true
    validates :mz, :intensity, presence: true, if: :header_include_mz?
    validates_format_of :mz, :intensity, with: /\A[\d\se\+]+\z/i,  if: :header_include_mz?
    validates :slope, :intercept,  presence: true, if: :header_include_slope?
    validates_format_of :slope, :intercept, with: /\A[\d\.\+\-e]+\z/i,  if: :header_include_slope?
    validates :ri, :rt,  presence: true
    validates_format_of :ri, :rt, with: /\A[\d\.\+\-e]+\z/i

    def self.all_rows(file)
        items = []
        CSV.foreach(file, headers: true) do |row|
          items << self.new(row.to_hash)
        end
        items
    end

    def self.header()
        CSV.read(self.template_file, headers: true).headers()
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

    def self.save(row_objs, file, mode)
        # before save the file to lib. Make a 
        # copy with the current day. All the updates
        # will be made into the current file and the
        # dated file. Keep 20 past dated files for usage
        # Save today's file as copy first
        # params: row_objs (data structure containing csv data)
        #         file  (row_objs data is going to save to this file)
        #         mode  (action)

        
        # Bypass validation if in destroy 
        unless mode == 'delete'
          unless self.validate_fields(row_objs)
            msg = "#{file} NOT saved!"
            self.flash[:error] << msg
            return false
          end
        end
        
        #save file 
        h = self.header()
        
        CSV.open(file, "wb") do |csv|
            csv << h
            row_objs.each do |row_obj|
              r = h.map do |key|
                    row_obj.row[key]
              end
              csv << r
            end
        end
        
        msg = "#{file} saved!"
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
                row_obj.row['SeqIndex'] + ' ' +
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

    def header_include_mz?
      self.row.keys.include?('MZ')
    end

    def header_include_slope?
      self.row.keys.include?('Slope')
    end

    def initialize(row_dict)
    	self.row = row_dict
    end

    def self.last_index(file)
      self.all_rows(file).last ? self.all_rows(file).last.row['SeqIndex'] : 0
    end
      

end
