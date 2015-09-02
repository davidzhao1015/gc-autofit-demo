class SubmissionsController < ApplicationController

  before_action :set_submission, only: [:show, :edit, :profile, :update, :destroy]

  # GET /submissions
  # GET /submissions.json
  def index
    @submissions = Submission.all
  end

  # GET /submissions/1
  # GET /submissions/1.json
  def show
    @spectrum = @submission.spectra.first
  end

  # GET /submissions/new
  def new
    @submission = Submission.new
  end

  # GET /submissions/1/edit
  def edit
  end

  # POST /submissions
  # POST /submissions.json
  def create
    @submission = Submission.new(submission_params)
    @submission.status = 'validating'

    respond_to do |format|
      if @submission.save
        @submission.start_work
        if params[:submission][:input_zip]
          # Process zip file and ignore aother files
        else
        end
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
    if example_num == '1'
      example_dir = Rails.root.join('lib', 'APGCMS', 'example', 'serum')
      submission.database = 'serum'
      submission.spectra.build(category: 'standards',
                               spectrum_data: File.new(File.join(example_dir, 'Alkstd.mzXML')))
      submission.spectra.build(category: 'blank',
                               spectrum_data: File.new(File.join(example_dir, 'Blank1.mzXML')))
      submission.spectra.build(category: 'sample',
                               spectrum_data: File.new(File.join(example_dir, 'Mix1-1.mzXML')))
    end
    if example_num == '2'
      example_dir = Rails.root.join('lib', 'APGCMS', 'example', 'urine')
      submission.database = 'serum'
      submission.spectra.build(category: 'standards',
                               spectrum_data: File.new(File.join(example_dir, 'Alkstd1.mzXML')))
      submission.spectra.build(category: 'blank',
                               spectrum_data: File.new(File.join(example_dir, 'Blank1.mzXML')))
      submission.spectra.build(category: 'sample',
                               spectrum_data: File.new(File.join(example_dir, 'Urine_ex1.mzXML')))
    end
    submission
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_submission
      @submission = Submission.find_by_secret_id(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def submission_params
      params.require(:submission).permit(:database, :internal_standard, :status,
                                         spectra_attributes: [ :spectrum_data, :category ])
    end
end
