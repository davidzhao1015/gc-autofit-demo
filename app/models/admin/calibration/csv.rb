

class Admin::Calibration::Csv < Admin::CsvModel

     @csv_file_dir= Rails.application.config.APGCMS_calibration_dir
     @category = 'calibration'
end