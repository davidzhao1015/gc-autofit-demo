
class Admin::Calibration::Saliva < Admin::CsvModel

     @csv_file = Rails.application.config.saliva_calibration_lib_file   
     @flash = {} 
end
