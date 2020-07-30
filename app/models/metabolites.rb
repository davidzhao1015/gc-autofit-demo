class Metabolites
  # - Metabolite file details:
  #   - should all contain columns of HMDB IDs and Compound Names
  #   - Comma separated
  #   - There should be a header

  # Directory where all metabolite related files are located
  METABOLITES_DIR = Rails.application.config.apgcms_metabolite_dir

  CSV_OPTIONS = {skip_lines: '#', skip_blanks: true, col_sep: ",", headers: true}

  # HMDB ID Column number (base-0)
  HMDB_ID_COLUMN = 1
  # Compound Name Column number (base-0)
  COMPOUND_NAME_COLUMN = 2

  def self.file_path_for_biofluid(biofluid)
    File.join(METABOLITES_DIR, "#{biofluid}.csv")
  end

  def self.available_for(biofluid_or_path)
    available = {}
    biofluid_path = self.file_path_for_biofluid(biofluid_or_path.downcase)
    path = File.exist?(biofluid_path) ? biofluid_path : biofluid_or_path
    CSV.foreach(path, CSV_OPTIONS) do |row|
      available[row[HMDB_ID_COLUMN]] = row[COMPOUND_NAME_COLUMN]
    end
    available
  end

end
