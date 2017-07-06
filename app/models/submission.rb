class Submission < ActiveRecord::Base
  # STATES = %w[ validating queued processing processed profiling complete failed ]
  STATES = %w[ validating queued processing complete failed ]
  # PROCESSED = %w[ processed profiling complete ]
  FINALIZED_STATES = %w[ complete failed ]

  WORKING_DIR = Rails.env.test? ? Rails.root.join('tmp/working') : Rails.root.join('APGCMS_working')
  DAYS_TO_KEEP_SUBMISSIONS = 7
  SECRET_ID_LENGTH = 20

  DATABASES = {
    #db      #display
    'serum' => 'Serum',
    'urine' => 'Urine',
    'saliva' => 'Saliva',
    'upload' => 'Upload Your Library'

  }
  INTERNAL_STANDARDS = %w[ Ribitol Cholesterol Succinate-D4 Tropic\ acid Other ]

  has_many :spectra, dependent: :destroy
  has_one :standards, -> { where category: 'standards'}, class_name: Spectrum
  has_one :blank,     -> { where category: 'blank'}, class_name: Spectrum
  has_many :samples,  -> { where category: 'sample'}, class_name: Spectrum

  accepts_nested_attributes_for :spectra

  has_attached_file :profile_library, path: ':input_dir/user_library.csv'
  has_attached_file :calibration, path: ':input_dir/user_calibration.csv'
  has_attached_file :input_zip, path: ':input_dir/input.zip'
  validates_attachment_presence :profile_library, if: Proc.new { |sub| sub.database == 'upload' }
  validates_attachment_file_name :profile_library, :matches => [/csv\Z/]
  validates_attachment_file_name :calibration, :matches => [/csv\Z/]
  validates_attachment_file_name :input_zip, :matches => [/zip\Z/]


  validates :secret_id, presence: true, uniqueness: true
  validates :status, inclusion: { in: STATES }
  validates :database, inclusion: { in: DATABASES.keys }
  validates_numericality_of :mf_score_threshold,
                             greater_than_or_equal_to: 0,
                             less_than_or_equal_to: 999,
                             only_integer: true
  validates_presence_of :internal_standard
  validate :check_required_spectra
  validate :check_internal_standard
  validate :check_user_library
  #validate :check_user_calibration

  before_validation :generate_secret
  # after_create      :start_work
  after_destroy     :delete_working_dir

  serialize :database_subset, Array


  def self.delete_old_submissions
    old_submissions = Submission.where("created_at < ?", DAYS_TO_KEEP_SUBMISSIONS.days.ago)
    # puts "Submissions older than #{DAYS_TO_KEEP_SUBMISSIONS} days: #{old_submissions.count}"
    if old_submissions.count > 0
      # puts "Deleting..."
      old_submissions.destroy_all
      old_submissions = Submission.where("created_at < ?", DAYS_TO_KEEP_SUBMISSIONS.days.ago)
      # puts "Submissions older than #{DAYS_TO_KEEP_SUBMISSIONS} days: #{old_submissions.count}"
    end
  end

  def start_work
    create_working_dir
    self.update!(status: 'queued')
    job_id = SubmissionWorker.perform_async(self.id)
    self.update!(job_id: job_id)
  end

  def start_profiling
    if self.complete? && !self.profile?
      self.update!(profile: true)
      self.samples.each do |sample|
        # SpectrumWorker.new.perform(sample.id)
        sample.start_work
      end
    end
  end

  def finalized?
    FINALIZED_STATES.include?(self.status)
  end

  def samples_all_finalized?
    self.samples.all? { |s| s.finalized? }
  end

  def samples_any_finalized?
    self.samples.any? { |s| s.finalized? }
  end

  def failed?
    self.status == 'failed'
  end

  def complete?
    self.status == 'complete'
  end

  def processing?
    ['queued', 'processing'].include?(self.status)
  end

  def running?
    self.status == 'processing'
  end

  # def processed?
  #   PROCESSED.include?(self.status)
  # end

  def working_dir
    File.join(WORKING_DIR, 'Results', self.secret_id)
  end

  def input_dir
    File.join(self.working_dir, 'input')
  end

  def preprocessing_dir
    File.join(self.working_dir, 'preprocessing')
  end

  def log_file
    File.join(preprocessing_dir, 'log.txt')
  end

  def logger(text)
    File.open(self.log_path, 'a+') { |f| f.puts(text) }
  end

  def profiling_dir
    File.join(self.working_dir, 'profiling')
  end

  def create_working_dir
    FileUtils.mkdir_p(self.working_dir)
  end

  def delete_working_dir
    FileUtils.rm_r(self.working_dir) if File.exists?(self.working_dir)
  end

  def to_param
    self.secret_id
  end

  def display_status
    self.status.capitalize
  end

  def display_database
    #TEMP
    self.database.capitalize
  end

  def display_runtime
    if self.runtime.nil?
      'NA'
    else
      min = self.runtime / 60
      sec = self.runtime % 60
      min > 0 ? "#{min} minutes #{sec} seconds" : "#{sec} seconds"
    end
  end

  def alkane_rt
    rt = []
    if self.standards.json_results.present?
      json_results = JSON.parse(File.read(self.standards.json_results.path))
      json_results['labels'].each do |label|
        rt << label['text'].strip.sub(/^\D+/, '')
      end
    end
    rt
  end

  # Join CSV reports from all spectra into single CSV
  def csv_report_OLD_WAY
    all_concs = []
    hmdbids = {}
    self.samples.each do |spectrum|
      concentrations = {}
      if File.exist?(spectrum.json_results.path.to_s)
        json_results = JSON.parse(File.read(spectrum.json_results.path))
        json_results['labels'].each do |label|
          data = label['meta']['table_data']
          hmdbids[ data['HMDB ID'] ] = data['Name'] unless hmdbids[ data['id'] ].present?

          concentrations[ data['HMDB ID'] ] = data['Concentration (uM)']
        end
      end
      all_concs << concentrations
    end

    CSV.generate do |output|
      # self.settings.each { |s| output << s }
      output << ['# Concentration Units: uM']
      output << ["# Job ID: #{self.to_param}"]
      output << ['HMDB ID', 'Compound Name'] + self.samples.map(&:name)
      hmdbids.each do |hmdbid, name|
        output << [hmdbid, name] + all_concs.map { |d| "#{d[hmdbid]}" }
      end
    end
  end

  # Join CSV reports from all spectra into single CSV
  def csv_report
    samples_data = {}
    self.samples.each do |spectrum|
      concentrations = {}
      if File.exist?(spectrum.profile_results_path)
        results = File.readlines(spectrum.profile_results_path)
        ids = results[0].split('","').map { |i| i.gsub('"', '') }
        names = results[1].split('","').map { |i| i.gsub('"', '') }
        concs = results[2].split(',').map { |i| i.gsub('"', '') }
        ids.shift
        names.shift
        concs.shift
        ids.each_with_index do |id, i|
          concentrations[id] = { name: names[i], conc: concs[i] }
        end
      end
      samples_data[spectrum.name] = concentrations
    end

    if self.samples.count > 0
      first_sample_data = samples_data[self.samples.first.name]
      hmdb_ids = first_sample_data.keys
      names = hmdb_ids.map { |id| first_sample_data[id][:name] }
      CSV.generate do |output|
        # self.settings.each { |s| output << s }
        output << ['# Concentration Units: uM']
        output << ["# Job ID: #{self.to_param}"]
        output << ['HMDB_ID'] + hmdb_ids
        output << ['Compound'] + names
        self.samples.each do |sample|
          next if sample.failed?
          row = [sample.name]
          hmdb_ids.each do |id|
            row << samples_data[sample.name][id][:conc]
          end
          output << row
        end
      end
    end
  end

  def csv_filename
    "GC-AutoFit_Report_#{self.created_at.strftime('%Y-%m-%d')}.csv"
  end

  def unzip_spectra
    # return unless self.input_zip_file.present?
    return unless self.input_zip.queued_for_write[:original]
    Dir.mktmpdir do |dir|
      Zip::File.open(self.input_zip.queued_for_write[:original].path) do |zip_file|
        zip_file.each do |entry|
          ext = File.extname(entry.name).sub('.', '')
          name = File.basename(entry.name, '.*')
          next unless ext.downcase =~ /^(cdf|mzxml)$/
          next if name =~ /^\./
          case name.downcase
          when /alk/
            dest_path = File.join(dir, "Alkstd.#{ext}")
            # Rails.logger.debug dest_path
            entry.extract(dest_path)
            self.spectra.build(category: 'standards', spectrum_data: File.open(dest_path))
          when /blk/, /blank/
            dest_path = File.join(dir, "Blank.#{ext}")
            entry.extract(dest_path)
            self.spectra.build(category: 'blank', spectrum_data: File.open(dest_path))
          else
            dest_path = File.join(dir, "#{name}.#{ext}")
            entry.extract(dest_path)
            self.spectra.build(category: 'sample', spectrum_data: File.open(dest_path))
          end
        end
      end
    end
  end

  private

  # Generate private URL
  def generate_secret
    self.secret_id ||= SecureRandom.hex(SECRET_ID_LENGTH)
  end

  def check_required_spectra
    unless self.spectra.any? { |s| s.category == 'standards' }
      errors[:base] << "An alkane standards spectrum must be provided"
    end
    unless self.spectra.any? { |s| s.category == 'sample' }
      errors[:base] << "At least one sample spectrum must be provided"
    end
  end

  def check_internal_standard
    if self.internal_standard
      standard = self.internal_standard.downcase
      profile_file = self.profile_library.queued_for_write[:original]
      metabolites = {}
      if self.database == 'upload' 
        if profile_file
          metabolites = Metabolites.available_for(profile_file.path)
        end
      else
        metabolites = Metabolites.available_for(self.database)
      end
      ids = metabolites.keys.map { |i| i.downcase }
      names = metabolites.values.map { |n| n.downcase }
      valid_standard = (standard =~ /^hmdb/) ? ids.include?(standard) : names.include?(standard)
      unless valid_standard
        errors[:base] << "Internal standard must be present in the selected library"
      end
    end
  end

  def check_user_library
    # profile_file = self.profile_library.queued_for_write[:original]
    # if profile_file.path
    # end
  end

end
