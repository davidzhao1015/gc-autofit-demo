class Lib::Calibration::CsvController < Lib::CsvController
	@model  = Lib::Calibration::Csv


	def download
		file_location = params[:file][0]
		file_name = file_location.split("/").last
		file = File.open(file_location).read
		send_data(file, :filename => "#{file_name}")
	end

end