
class Admin::Db::Urine < Admin::CsvModel
    
   @csv_file = Rails.application.config.urine_lib_file 
end
