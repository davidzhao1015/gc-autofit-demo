

class Makedb::Calibration::Csv < Makedb::CsvModel
     @db_tmp_dir = Rails.application.config.APGCMS_calibration_tmp_dir
     @template_file = "#{@db_tmp_dir}/template.csv"
     @category = 'calibration'
end