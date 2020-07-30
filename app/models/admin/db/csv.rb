class Admin::Db::Csv < Admin::CsvModel
	@csv_file_dir = Rails.application.config.apgcms_mz_intensity_dir
	@category = 'db'
end