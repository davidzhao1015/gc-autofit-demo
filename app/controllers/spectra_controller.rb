class SpectraController < ApplicationController
  before_action :set_spectrum, only: [:show, :edit, :update, :destroy]

  # GET /spectra
  # GET /spectra.json
  def index
    @spectra = Spectrum.all
  end

  # GET /spectra/1
  # GET /spectra/1.json
  def show
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
      @spectrum = Spectrum.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def spectrum_params
      params.require(:spectrum).permit(:runtime)
    end
end
