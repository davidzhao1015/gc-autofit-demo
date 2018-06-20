
class Makedb::CsvController  < Makedb::MakedbController 
  
  class << self
     attr_accessor :model
  end

  before_action ->(file=params[:file]) { check_file_exist file }, except:  %i|new|

  def download
    file = params[:file]
    send_file file
  end

  def index
    @type = params.key?(:type) ? params[:type] : nil
    @base_file = params.key?(:file) ? params[:file] : self.class.model.template_file
    @file =  @base_file
    @csv_files = [@base_file]
    @header = self.class.model.header()
    @csv_rows = self.class.model.all_rows(@file)
    update_flash()
  end

  def new
    @type = self.class.model.category
    @file = params.key?(:file) ? params[:file] : self.class.model.template_file
    is_template =  @file == self.class.model.template_file ?  true : false
    @header = self.class.model.header()
    last_index = is_template ? 0 : self.class.model.last_index(@file).to_i
    @row = {"SeqIndex" => last_index + 1}
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
    # clean up error from flash first. when render new in create.
    # flash error will be kept to next step.
    flash.delete(:error) if flash.key?(:error)
    
    if params.key?(:file)
      csv_file = params[:file]
      row_objs = self.class.model.all_rows(csv_file)
    else
      csv_file = "#{self.class.model.db_tmp_dir}/#{Time.now.to_i}.csv"
    end
    row_objs << self.class.model.new(fields)
    
    if self.class.model.save(row_objs, csv_file, 'create')
      flash[:notice] = 'Row generated!'
    else
      flash[:notice] = 'Row NOT generated!'
    end

    update_flash()
    if ! flash.key?(:error)
      redirect_to action: "index", type: @type, file: csv_file
    else
      @header = self.class.model.header()
      @row = fields
      @file = File.exist?(csv_file) ? csv_file : nil
      render :new
    end
  end

  def edit
    @type = params.key?(:type) ? params[:type] : nil
    @file = params.key?(:file) ? params[:file] : nil
    @id = params[:id]
    csvs = self.class.model.all_rows(@file)
    @header = self.class.model.header()
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
    csv_file = params.key?(:file) ? params[:file] : nil
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
    redirect_to action: "index", type: @type, file: csv_file
  end

  def destroy
    @type = params.key?(:type) ? params[:type] : nil
    row_objs = []
    csv_file = params.key?(:file) ? params[:file] : nil
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
    redirect_to action: "index", type: @type, file: csv_file
  end

  def update_flash 
      flash.update(self.class.model.flash)
      # clean up model flash hash after flash picks up message.
      self.class.model.flash = {}
  end 

  private 
    def check_file_exist(file)
      file_list = Dir.glob("#{self.class.model.db_tmp_dir}/*")
      if ! file_list.include?(file)
        flash[:error] = "No file #{file} in #{self.class.model.db_tmp_dir}/ !"
        redirect_to '/makedb'
      end
    end

end
