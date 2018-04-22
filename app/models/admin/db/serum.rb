
class Admin::Db::Serum < Admin::CsvModel

     @csv_file = Rails.application.config.serum_lib_file 
     @flash = {}
end
