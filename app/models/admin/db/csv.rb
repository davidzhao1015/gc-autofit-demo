


class Admin::Db::Csv < Admin::CsvModel

     @csv_file_dir = Rails.application.config.APGCMS_mz_intensity_dir
     @category = 'db'
end