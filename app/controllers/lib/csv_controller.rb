
class Lib::CsvController  < Lib::LibController 
  
  class << self
     attr_accessor :model
  end

  def index
    @type = params.key?(:type) ? params[:type] : nil
    @base_file = self.class.model.csv_file(@type)
    @file =  @base_file
    @csv_files = [@base_file]
    @header = self.class.model.header(@base_file)
    @csv_rows = self.class.model.all_rows(@file)
    update_flash()
  end

  def update_flash 
      flash.update(self.class.model.flash)
      # clean up model flash hash after flash picks up message.
      self.class.model.flash = {}
  end 

end
