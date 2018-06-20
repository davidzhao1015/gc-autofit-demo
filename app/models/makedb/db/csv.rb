

class Makedb::Db::Csv < Makedb::CsvModel
    @db_tmp_dir = Rails.application.config.APGCMS_mz_intensity_tmp_dir
    @template_file = "#{@db_tmp_dir}/template.csv"
    @category = 'db'
end
