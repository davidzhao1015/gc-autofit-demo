class APGCMS
  @@apgcms_path = Rails.root.join('lib', 'APGCMS', 'APGCMS', 'apgcms_main.R')

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
      @command += "--#{key}='#{value}'"
    end

    # @command = "Rscript #{@@apgcms_path} --infiledir=#{options[:infiledir]} --lib.internal='SERUM' "
    # @command += "--internalstd='Ribitol' --plotonly=TRUE"
    puts @command if @@debug
    begin
      @status, @stdout, stderr = systemu(@command)
      @errors += stderr.strip.split(/\n/)
    rescue SystemCallError => error
      app_failed("There was a system call error (Is R and APGCMS installed?): #{error.to_s}")
    rescue Exception => error
      app_failed("There was an exception: #{error.to_s}")
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

