
class Admin::Calibration::Serum < Admin::CsvModel
  @csv_file = Rails.application.config.serum_calibration_lib_file    
end
