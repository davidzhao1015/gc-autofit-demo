
class Admin::CsvController  < Admin::AdminController 
  
  class << self
     attr_accessor :model
  end

  def download
    file = params[:file]
    send_file file
  end

  def index
    @type = params.key?(:type) ? params[:type] : nil
    @base_file = self.class.model.csv_file(@type)
    @file = params.key?(:file) ? params[:file] : @base_file
    @csv_files = self.class.model.get_csv_file_list(@base_file)
    @header = self.class.model.header(@file)
    @csv_rows = self.class.model.all_rows(@file)
    update_flash()
  end

  def edit
    @type = params.key?(:type) ? params[:type] : nil
    @file = self.class.model.csv_file(@type)
    @id = params[:id]
    csvs = self.class.model.all_rows(@file)
    @header = self.class.model.header(@file)
    @row = {}
    csvs.each do |mdl|
      if mdl.row['SeqIndex'] == @id
        @row = mdl.row
      end
    end
  end

  def update
    row_objs = []
    @type = params.key?(:type) ? params[:type] : nil
    fields = params[:form_fields]
    fields.keys.each do |k|
      unless fields[k].nil?
        fields[k] = fields[k].gsub(/\r/, '')
        fields[k] = fields[k].gsub(/\s+/, ' ')
        fields[k] = fields[k].strip
      end
    end
    csv_file = self.class.model.csv_file(@type)
    row_objs = self.class.model.all_rows(csv_file).map do |mdl|  
      if mdl.row['SeqIndex'].to_s == fields['SeqIndex'].to_s
        self.class.model.new(fields)
      else
        mdl
      end
    end
    if self.class.model.save(row_objs, csv_file, 'update')
      flash[:notice] = 'Row upated!'
    else
      flash[:notice] = 'Row NOT upated!'
    end
      
    update_flash()
    redirect_to action: "index", type: @type
  end

  def new
    @type = params.key?(:type) ? params[:type] : nil
    id = params.key?(:id) ? params[:id] : nil
    @file = self.class.model.csv_file(@type)
    @header = self.class.model.header(@file)
    index = id ? id: self.class.model.last_index(@file).to_i + 1 
    @row = {"SeqIndex" => index}
  end

  def create
    @type = params.key?(:type) ? params[:type] : nil
    row_objs = []
    fields = params[:form_fields]
    fields.keys.each do |k|
      unless fields[k].nil?
        fields[k] = fields[k].gsub(/\r/, '')
        fields[k] = fields[k].gsub(/\s+/, ' ')
        fields[k] = fields[k].strip
      end
    end
    csv_file = self.class.model.csv_file(@type)
    row_objs = self.class.model.all_rows(csv_file)
    if fields['SeqIndex'] > row_objs[-1].row['SeqIndex']
      row_objs << self.class.model.new(fields)
    else
      row_objs_tmp = []
      row_objs.each do |e|
        if e.row['SeqIndex'] >= fields['SeqIndex']
          if e.row['SeqIndex'] == fields['SeqIndex']
            row_objs_tmp << self.class.model.new(fields)
          end
          e.row['SeqIndex'] = e.row['SeqIndex'].to_i + 1
        end
        row_objs_tmp << e
      end
      row_objs = row_objs_tmp
    end

    if self.class.model.save(row_objs, csv_file, 'create')
      flash[:notice] = 'Row generated!'
    else
      flash[:notice] = 'Row NOT generated!'
    end
    
    update_flash()
    redirect_to action: "index", type: @type
  end

  def destroy
    @type = params.key?(:type) ? params[:type] : nil
    row_objs = []
    csv_file = self.class.model.csv_file(@type)
    row_objs = self.class.model.all_rows(csv_file).map do |mdl|  
      unless mdl.row['SeqIndex'] == params[:id]
        # all indices which are bigger that param[:id] should sustract 1.
        mdl.row['SeqIndex'] = mdl.row['SeqIndex'].to_i - 1 if mdl.row['SeqIndex'].to_i > params[:id].to_i
        mdl
      else
        ''
      end
    end.select { |mdl| mdl != '' }

    self.class.model.save(row_objs, csv_file, 'delete')
    flash[:notice] = "Row deleted!"
    update_flash()
    redirect_to action: "index", type: @type
  end

  def update_flash 
      flash.update(self.class.model.flash)
      # clean up model flash hash after flash picks up message.
      self.class.model.flash = {}
  end 

end
