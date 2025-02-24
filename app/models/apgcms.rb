class APGCMS
  @@apgcms_path = "#{Rails.application.config.apgcms_root}/scripts/apgcms_main.R"

  # The command that was run
  attr_reader :command
  # The status code returned from the process
  attr_reader :status
  # The standard out returned from the process
  attr_reader :stdout
  # An array of error messages
  attr_reader :errors

  @@debug=false

  # Create a ShellSession
  def initialize(options = {})
    @errors = []

    @command = "Rscript #{@@apgcms_path} "
    options.each do |key, value|
      @command += "--#{key}='#{value}' "
    end
    
    puts @command if @@debug
    
    
    @status, @stdout, @stderr = systemu(@command)
    if @@debug
      puts "@status, @stdout, @stderr"
      puts @status
      puts @stdout
      puts @stderr
    end

    # has to filter some info out from errors since xcms package 
    # has some info put into stderr but actually not errors. Those lines
    # are like this:
    # Create profile matrix with method 'bin' and step 1 ... OK
    @errors += @stderr.strip.split(/\n/).select { |e| e !~ /Create profile matrix with method.*?OK/ }

    # have to handle some files first
    output_json_file = "#{options[:outdir]}/#{File.basename(options[:outdir])}_spectrum.json"
    need_json_file = "#{options[:outdir]}/sample_spectrum.json"
    if not File.exist?(need_json_file) && File.exist?(output_json_file)
      FileUtils.ln_s(output_json_file, need_json_file)
    end

    if options[:log]
      File.open(options[:log], 'a') do |f|
        f.write("#{'-'*80}\nCOMMAND\n#{'-'*80}\n")
        f.write(@command)
        f.write("\n\n\n#{'-'*80}\nSTDOUT\n#{'-'*80}\n")
        f.write(@stdout)
        f.write("\n\n\n#{'-'*80}\nSTDERR\n#{'-'*80}\n")
        f.write(@errors)
      end
    end
   
  end


  def success?
    @status.success? && @errors.empty? 
  end

  private

    def app_failed(msg)
      @errors << msg
      Rails.logger.error(msg) if Rails.logger && @@debug
    end


end


