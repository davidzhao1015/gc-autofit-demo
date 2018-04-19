
class Admin::Db::Saliva < Admin::CsvModel

    @csv_file = Rails.application.config.saliva_lib_file  
end
