require 'test_helper'

class SpectraControllerTest < ActionController::TestCase
  setup do
    @spectrum = spectra(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:spectra)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create spectrum" do
    assert_difference('Spectrum.count') do
      post :create, spectrum: { runtime: @spectrum.runtime }
    end

    assert_redirected_to spectrum_path(assigns(:spectrum))
  end

  test "should show spectrum" do
    get :show, id: @spectrum
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @spectrum
    assert_response :success
  end

  test "should update spectrum" do
    patch :update, id: @spectrum, spectrum: { runtime: @spectrum.runtime }
    assert_redirected_to spectrum_path(assigns(:spectrum))
  end

  test "should destroy spectrum" do
    assert_difference('Spectrum.count', -1) do
      delete :destroy, id: @spectrum
    end

    assert_redirected_to spectra_path
  end
end
