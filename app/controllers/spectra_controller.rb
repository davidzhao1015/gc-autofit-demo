class SpectraController < ApplicationController
  before_action :set_spectrum, only: [:show, :edit, :update, :destroy]
  before_action :set_submission, only: [:index, :show ]

  # GET /spectra
  # GET /spectra.json
  def index
    redirect_to submission_path(@submission, anchor: 'tab_profiling')
  end

  # GET /spectra/1
  # GET /spectra/1.json
  def show
    # @submission = @spectrum.submission
    respond_to do |format|
      # format.csv { send_file(@spectrum.formatted_bayesil_path, filename: @spectrum.csv_filename, type: 'text/csv') }
      format.json { send_file(@spectrum.json_results.path, type: 'text/json') }
      format.js
      format.html { redirect_to submission_path(@submission, anchor: 'tab_results', spectrum: @spectrum.id)}
    end
  end

  # GET /spectra/new
  def new
    @spectrum = Spectrum.new
  end

  # GET /spectra/1/edit
  def edit
  end

  # POST /spectra
  # POST /spectra.json
  def create
    @spectrum = Spectrum.new(spectrum_params)

    respond_to do |format|
      if @spectrum.save
        format.html { redirect_to @spectrum, notice: 'Spectrum was successfully created.' }
        format.json { render :show, status: :created, location: @spectrum }
      else
        format.html { render :new }
        format.json { render json: @spectrum.errors, status: :unprocessable_entity }
      end
    end
  end

  # PATCH/PUT /spectra/1
  # PATCH/PUT /spectra/1.json
  def update
    respond_to do |format|
      if @spectrum.update(spectrum_params)
        format.html { redirect_to @spectrum, notice: 'Spectrum was successfully updated.' }
        format.json { render :show, status: :ok, location: @spectrum }
      else
        format.html { render :edit }
        format.json { render json: @spectrum.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /spectra/1
  # DELETE /spectra/1.json
  def destroy
    @spectrum.destroy
    respond_to do |format|
      format.html { redirect_to spectra_url, notice: 'Spectrum was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_spectrum
      @spectrum = Submission.find_by_secret_id(params[:submission_id]).spectra.find(params[:id])
    end

    def set_submission
      @submission = Submission.find_by_secret_id(params[:submission_id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def spectrum_params
      params.require(:spectrum).permit(:runtime)
    end
end
