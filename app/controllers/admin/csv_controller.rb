
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
    row_objs = []
    fields = params[:form_fields]
    fields.keys.each do |k|
      unless fields[k].nil?
        fields[k] = fields[k].gsub(/\r/, '')
        fields[k] = fields[k].gsub(/\s+/, ' ')
        fields[k] = fields[k].strip
      end
    end
    
    row_objs = self.class.model.all_rows(self.class.model.csv).map do |mdl|  
      if mdl.row['SeqIndex'].to_s == fields['SeqIndex'].to_s
        self.class.model.new(fields)
      else
        mdl
      end
    end
    if self.class.model.save(row_objs, 'update')
      flash[:notice] = 'Row upated!'
    else
      flash[:notice] = 'Row NOT upated!'
    end
      
    update_flash()
    redirect_to action: "index"
  end

  def new
    @file = self.class.model.csv
    @header = self.class.model.header
    @row = {"SeqIndex" => self.last_index.to_i + 1}
  end

  def create
    row_objs = []
    fields = params[:form_fields]
    fields.keys.each do |k|
      unless fields[k].nil?
        fields[k] = fields[k].gsub(/\r/, '')
        fields[k] = fields[k].gsub(/\s+/, ' ')
        fields[k] = fields[k].strip
      end
    end
    row_objs = self.class.model.all_rows(self.class.model.csv)
    row_objs << self.class.model.new(fields)

    if self.class.model.save(row_objs, 'create')
      flash[:notice] = 'Row generated!'
    else
      flash[:notice] = 'Row NOT generated!'
    end
    
    update_flash()
    redirect_to action: "index"
  end

  def destroy
    row_objs = []
    row_objs = self.class.model.all_rows(self.class.model.csv).map do |mdl|  
      unless mdl.row['SeqIndex'] == params[:id]
        # all indices which are bigger that param[:id] should sustract 1.
        mdl.row['SeqIndex'] = mdl.row['SeqIndex'].to_i - 1 if mdl.row['SeqIndex'].to_i > params[:id].to_i
        mdl
      else
        ''
      end
    end.select { |mdl| mdl != '' }

    self.class.model.save(row_objs, 'delete')
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
