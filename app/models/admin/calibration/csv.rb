class Admin::Calibration::Csv < Admin::CsvModel
	@csv_file_dir= Rails.application.config.apgcms_calibration_dir
	@category = 'calibration'
end