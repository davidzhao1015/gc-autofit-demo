class SubmissionsController < ApplicationController

  before_action :set_submission, only: [:show, :edit, :profile, :update, :save_alkane_standards, :destroy]
  before_action :set_upload_format, only: [:create, :update]

  # GET /submissions
  # GET /submissions.json
  def index
    @submissions = Submission.all
  end

  # GET /submissions/1
  # GET /submissions/1.json
  def show
    if ! @submission
      return "This case is deleted!"
    end
    if params[:spectrum]
      @spectrum = @submission.spectra.where(id: params[:spectrum]).first
    else
      # TODO: set to first working finalized spectrum
      @spectrum = @submission.spectra.first
    end
    
    respond_to do |format|
      format.csv do
        send_data(@submission.csv_report, type: 'text/csv', disposition: 'attachment', filename: @submission.csv_filename)
      end
      format.html
      format.js
    end
  end

  # GET /submissions/new
  def new
    @submission = Submission.new
    @submission.database = 'urine'
    @submission.internal_standard = 'Tropic acid'
    @submission.mf_score_threshold = 400
    @upload_spectra_format = 'separate'  # OR 'zip'
  end

  # GET /submissions/1/edit
  def edit
  end

  # POST /submissions
  # POST /submissions.json
  def create
    @submission = Submission.new(submission_params)
    @submission.status = 'validating'
    if @submission.internal_standard == 'Other'
      @custom_internal_standard = params[:custom_internal_standard]
      @submission.internal_standard = @custom_internal_standard
    end
    # Add zip file contents
    if @upload_spectra_format == 'zip' && params[:submission][:input_zip]
      @submission.unzip_spectra
    end

    respond_to do |format|
      if @submission.save
        @submission.start_work
        format.html { redirect_to @submission, notice: 'Submission was successfully created.' }
        format.json { render :show, status: :created, location: @submission }
      else
        format.html do
          render :new
        end
        format.json { render json: @submission.errors, status: :unprocessable_entity }
      end
    end
  end

  def profile
    @submission.start_profiling
    redirect_to submission_path(@submission, anchor: 'tab_profiling')
  end

  # PATCH/PUT /submissions/1
  # PATCH/PUT /submissions/1.json
  def update
    respond_to do |format|
      if @submission.update(submission_params)
        format.html { redirect_to @submission, notice: 'Submission was successfully updated.' }
        format.json { render :show, status: :ok, location: @submission }
      else
        format.html { render :edit }
        format.json { render json: @submission.errors, status: :unprocessable_entity }
      end
    end
  end

  def save_alkane_standards
    respond_to do |format|
      format.json do
        standards = JSON.parse(params[:alkane_standards])
        json_results = JSON.parse(File.read(@submission.standards.json_results.path))
        json_results['labels'] = standards
        File.open(@submission.standards.json_results.path, 'w') { |f| f.write(JSON.generate(json_results)) }
        head :no_content
      end
    end
  end

  # DELETE /submissions/1
  # DELETE /submissions/1.json
  def destroy
    @submission.destroy
    respond_to do |format|
      format.html { redirect_to submissions_url, notice: 'Submission was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  def example
    # puts "params => #{params.inspect}"
    # params => <ActionController::Parameters {"example_num"=>"1", "controller"=>"submissions", "action"=>"example"} permitted: false>
    @submission = get_example(params[:example_num])
    if @submission.save
      @submission.start_work
      redirect_to submission_path(@submission)
    else
      redirect_to new_submission_path, notice: 'Unknown example'
    end
  end

  def get_example(example_num)
    submission = Submission.new
    submission.status = 'validating'
    submission.mf_score_threshold = 400
    if example_num == '1'
      example_dir = File.join(Rails.application.config.apgcms_example_dir, "serum")
      submission.database = 'serum'
      submission.internal_standard = 'Ribitol'
      submission.spectra.build(category: 'standards',
                               spectrum_data: File.new(File.join(example_dir, 'ALKSTD.CDF')))
      submission.spectra.build(category: 'blank',
                               spectrum_data: File.new(File.join(example_dir, 'GSS-BLANK.CDF')))
      submission.spectra.build(category: 'sample',
                               spectrum_data: File.new(File.join(example_dir, 'GSS-1R.CDF')))
      submission.spectra.build(category: 'sample',
                               spectrum_data: File.new(File.join(example_dir, 'GSS-2R.CDF')))
    elsif example_num == '2'
      example_dir = File.join(Rails.application.config.apgcms_example_dir, 'urine')
      submission.database = 'urine'
      submission.internal_standard = 'Cholesterol'
      submission.spectra.build(category: 'standards',
                              spectrum_data: File.new(File.join(example_dir, 'Alkstd.mzXML')))
      submission.spectra.build(category: 'blank',
                              spectrum_data: File.new(File.join(example_dir, 'Blank.mzXML')))
      submission.spectra.build(category: 'sample',
                              spectrum_data: File.new(File.join(example_dir, 'C001.mzXML')))
      submission.spectra.build(category: 'sample',
                              spectrum_data: File.new(File.join(example_dir, 'C002.mzXML')))
    elsif example_num == '3'
      example_dir = File.join(Rails.application.config.apgcms_example_dir, 'saliva')
      submission.database = 'saliva'
      submission.spectra.build(category: 'standards',
                               spectrum_data: File.new(File.join(example_dir, 'ALKS.CDF')))
      submission.spectra.build(category: 'blank',
                               spectrum_data: File.new(File.join(example_dir, 'BLK2.CDF')))
      submission.spectra.build(category: 'sample',
                               spectrum_data: File.new(File.join(example_dir, 'S1.CDF')))
    end
    submission
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_submission
      @submission = Submission.find_by_secret_id(params[:id])
    end

    def set_upload_format
      @upload_spectra_format = params[:upload_spectra_format]
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def submission_params
      # Update submission params with spectral data
      if @upload_spectra_format == 'separate'
        spectra_attributes = []
        if params[:standards_spectrum]
          spectra_attributes << { category: 'standards', spectrum_data: params[:standards_spectrum] }
        end
        if params[:blank_spectrum]
          spectra_attributes << { category: 'blank', spectrum_data: params[:blank_spectrum] }
        end
        if params[:sample_spectra]
          params[:sample_spectra].each do |sample_spectrum|
            spectra_attributes << { category: 'sample', spectrum_data: sample_spectrum }
          end
        end
        params[:submission][:spectra_attributes] = spectra_attributes
        # Remove any zip file
        params[:submission][:zip_file] = nil
      end

      params.require(:submission).permit(:database, :internal_standard, :status, :mf_score_threshold,
                                         :profile_library, :calibration, :input_zip,
                                         database_subset: [],
                                         spectra_attributes: [ :spectrum_data, :category ])
    end
end
