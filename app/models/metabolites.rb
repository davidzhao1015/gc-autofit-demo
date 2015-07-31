class Metabolites
  # - Metabolite file should all contain one column of HMDB IDs
  # - With one exception the AVAILABLE file must be a
  #   2 column (tab deliminated) with columns: HMDB ID & NAME
  # - There should be NO header 

  # Directory where all metabolite related files are located
  METABOLITES_DIR = Rails.root.join('lib', 'metabolites')
  # Name of tab deliminated file with list of all available metabolites
  AVAILABLE = 'available' # excluding suffix '.txt'
  # Name of file with list of all verified metabolites
  # (i.e. metabolites in CSF and Serum)
  VERIFIED  = 'verified'  # excluding suffix '.txt'
  # Names of biofluid files that should exist but are not in NmrRequest::BIOFLUID_TYPES
  EXTRA_BIOFLUIDS = %w( saliva )
  # The union of metabolites from these files should match the verified list
  VERIFIED_BIOFLUIDS = %w( serum csf )
  # Name of file with confidence scores
  CONFIDENCE = 'confidence_scores'

  CSV_OPTIONS = {skip_lines: '#', skip_blanks: true, col_sep: "\t"}
  # IDs that will be removed from all lists
  EXTRA_IDS = %w( HMDB00000 HMDB0000A )

  def self.file_path_for_biofluid(biofluid)
    File.join(METABOLITES_DIR, "#{biofluid}.txt")
  end

  # Returns a hash of available metabolites {HMDBID => NAME}
  def self.available
    @available ||= self.get_available
  end

  def self.available_ids
    self.available.keys
  end

  def self.get_available
    @available = {}
    CSV.foreach(self.file_path_for_biofluid(AVAILABLE), CSV_OPTIONS) do |row|
      @available[row[0]] = row[1]
    end
    @available
  end

  # Returns a hash of available metabolites confidence scores {HMDBID => CONFIDENCE}
  def self.confidence
    @confidence ||= self.get_confidence
  end

  def self.confidence_for(hmdb_id)
    self.confidence[hmdb_id]
  end

  def self.get_confidence
    @confidence = {}
    CSV.foreach(self.file_path_for_biofluid(CONFIDENCE), CSV_OPTIONS) do |row|
      @confidence[row[0]] = row[2]
    end
    @confidence
  end

  def self.verified
    @verified ||= self.list(VERIFIED)
  end

  def self.list(biofluid)
    biofluid = biofluid.downcase
    return [] unless File.exists?(self.file_path_for_biofluid(biofluid))
    ids = CSV.foreach(self.file_path_for_biofluid(biofluid), CSV_OPTIONS).map {|r| r[0]}
    ids - EXTRA_IDS
  end

  def self.name_for(hmdb_id)
    self.available[hmdb_id]
  end

  def self.status_for(hmdb_id)
    self.verified.include?(hmdb_id) ? 'Verified' : 'Experimental'
  end

  def self.is_verified?(hmdb_id)
    self.status_for(hmdb_id) == 'Verified'
  end

  def self.is_experimental?(hmdb_id)
    self.status_for(hmdb_id) == 'Experimental'
  end

  def self.biofluid_is_verified?(biofluid)
    (self.list(biofluid.downcase) - self.verified).empty?
  end

  def self.tool_tip(biofluid)
    tip = ''
    unless self.biofluid_is_verified?(biofluid)
      metabolites = self.list(biofluid)
      metabolites_missing = (metabolites - self.available_ids)
      metabolites_available = metabolites - metabolites_missing
      metabolites_experimental = metabolites_available - self.verified
      tip << "#{biofluid} has #{metabolites.size} metabolites"
      unless metabolites_missing.empty?
        tip << ", however, only #{metabolites_available.size} are available in the library"
      end
      unless metabolites_experimental.empty?
        tip << " and #{metabolites_experimental.size} of those are experimental"
      end
    end
    tip
  end

  # list of files (without suffix '.txt') that should exists and have 
  # ids that are within AVAILABLE
  def self.expected_files
    files = NmrRequest::BIOFLUID_TYPES.keys
    files.delete('custom')
    files += [AVAILABLE, VERIFIED, EXTRA_BIOFLUIDS].flatten
    files
  end





end
