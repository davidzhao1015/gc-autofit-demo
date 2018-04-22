
class Admin::CsvController  < Admin::AdminController 
  
  class << self
     attr_accessor :model
  end

  def index
    @base_file = self.class.model.csv
    @file = params.key?(:file) ? params[:file] : @base_file
    @csv_files = self.class.model.get_csf_file_list(@base_file)
    @header = self.class.model.header
    @csv_rows = self.class.model.all_rows(@file)
  end

  def edit
    @file = self.class.model.csv
    @id = params[:id]
    csvs = self.class.model.all_rows(@file)
    @header = self.class.model.header
    @row = {}
    csvs.each do |mdl|
      if mdl.row['SeqIndex'] == @id
        @row = mdl.row
      end
    end
  end

  def update
    rows = []
    header = self.class.model.header
    rows << header
    fields = params[:form_fields]
    fields.keys.each do |k|
      unless fields[k].nil?
        fields[k] = fields[k].gsub(/\r/, '')
        fields[k] = fields[k].gsub(/\s+/, ' ')
        fields[k] = fields[k].strip
      end
    end
    
    self.class.model.all_rows(self.class.model.csv).each do |mdl|  
      if mdl.row['SeqIndex'].to_s == fields['SeqIndex'].to_s
        rows << header.map { |key| fields[key]}
      else
        rows << header.map { |key| mdl.row[key] }
      end
    end
    self.class.model.save(rows)
    flash[:notice] = 'Row upated!'
    update_flash()
    redirect_to action: "index"
  end

  def new
    @file = self.class.model.csv
    @header = self.class.model.header
    @row = {"SeqIndex" => self.last_index.to_i + 1}
  end

  def create
    rows = []
    header = self.class.model.header
    rows << header
    fields = params[:form_fields]
    fields.keys.each do |k|
      unless fields[k].nil?
        fields[k] = fields[k].gsub(/\r/, '')
        fields[k] = fields[k].gsub(/\s+/, ' ')
        fields[k] = fields[k].strip
      end
    end
    self.class.model.all_rows(self.class.model.csv).each do |mdl|  
        rows << header.map { |key| mdl.row[key] }
    end
    rows << header.map { |key| fields[key]}
    self.class.model.save(rows)
    flash[:notice] = 'Row generated!'
    update_flash()
    redirect_to action: "index"
  end

  def destroy
    rows = []
    header = self.class.model.header
    rows << header
    self.class.model.all_rows(self.class.model.csv).each do |mdl|  
      unless mdl.row['SeqIndex'] == params[:id]
        mdl.row['SeqIndex'] = mdl.row['SeqIndex'].to_i - 1 if mdl.row['SeqIndex'].to_i > params[:id].to_i
        rows << header.map { |key| mdl.row[key] }
      end
    end
    self.class.model.save(rows)
    flash[:notice] = "Row deleted!"
    update_flash()
    redirect_to action: "index"
  end

  def last_index
      self.class.model.all_rows(self.class.model.csv).last.row['SeqIndex']
  end

  def update_flash 
      flash.update(self.class.model.flash)
      # clean up model flash hash after flash picks up message.
      self.class.model.flash = {}
  end 

end
