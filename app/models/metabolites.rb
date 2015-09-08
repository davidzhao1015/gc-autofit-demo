class Metabolites
  # - Metabolite file details:
  #   - should all contain columns of HMDB IDs and Compound Names
  #   - Comma separated
  #   - There should be a header

  # Directory where all metabolite related files are located
  METABOLITES_DIR = Rails.root.join('lib', 'APGCMS', 'DB')

  CSV_OPTIONS = {skip_lines: '#', skip_blanks: true, col_sep: ",", headers: true}

  # HMDB ID Column number (base-0)
  HMDB_ID_COLUMN = 1
  # Compound Name Column number (base-0)
  COMPOUND_NAME_COLUMN = 2

  def self.file_path_for_biofluid(biofluid)
    File.join(METABOLITES_DIR, "#{biofluid}.csv")
  end

  def self.available_for(biofluid)
    biofluid = biofluid.downcase
    available = {}
    CSV.foreach(self.file_path_for_biofluid(biofluid), CSV_OPTIONS) do |row|
      available[row[HMDB_ID_COLUMN]] = row[COMPOUND_NAME_COLUMN]
    end
    available
  end

end
