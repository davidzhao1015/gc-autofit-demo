Rails.application.configure do
  # Settings specified here will take precedence over those in config/application.rb.

  # The test environment is used exclusively to run your application's
  # test suite. You never need to work with it otherwise. Remember that
  # your test database is "scratch space" for the test suite and is wiped
  # and recreated between test runs. Don't rely on the data there!
  config.cache_classes = true

  # Do not eager load code on boot. This avoids loading your whole application
  # just for the purpose of running a single test. If you are using a tool that
  # preloads Rails for running tests, you may have to set it to true.
  config.eager_load = false

  # Configure static file server for tests with Cache-Control for performance.
  config.serve_static_files   = true
  config.static_cache_control = 'public, max-age=3600'

  # Show full error reports and disable caching.
  config.consider_all_requests_local       = true
  config.action_controller.perform_caching = false

  # Raise exceptions instead of rendering exception templates.
  config.action_dispatch.show_exceptions = false

  # Disable request forgery protection in test environment.
  config.action_controller.allow_forgery_protection = false

  # Tell Action Mailer not to deliver emails to the real world.
  # The :test delivery method accumulates sent emails in the
  # ActionMailer::Base.deliveries array.
  config.action_mailer.delivery_method = :test

  # Randomize the order test cases are executed.
  config.active_support.test_order = :random

  # Print deprecation notices to the stderr.
  config.active_support.deprecation = :stderr

  # Raises error for missing translations
  # config.action_view.raise_on_missing_translations = true
  
  #admin domain username and password
  config.admin_username = 'admin'
  config.admin_password = '1d989cc0ce4221aaa7081a43a2877294'
  
  # Root of APGCMS
  config.APGCMS_root = "/home/centos/APGCMS"
  # csv lib files for maintenance in admin domain.
  # mz_intensity DBs
  config.APGCMS_mz_intensity_dir = "#{config.APGCMS_root}/DB/mz_intensity"
  config.alkane_lib_file = "#{config.APGCMS_mz_intensity_dir}/lib_alkane.csv"
  config.saliva_lib_file =  "#{config.APGCMS_mz_intensity_dir}/lib_saliva.csv"
  config.serum_lib_file = "#{config.APGCMS_mz_intensity_dir}/lib_serum.csv"
  config.urine_lib_file = "#{config.APGCMS_mz_intensity_dir}/lib_urine.csv"
  # calibations DBs
  config.APGCMS_calibration_dir = "#{config.APGCMS_root}/DB/calibrations"
  config.saliva_calibration_lib_file = "#{config.APGCMS_calibration_dir}/lib_saliva_CalibrationCurve.csv"
  config.serum_calibration_lib_file = "#{config.APGCMS_calibration_dir}/lib_serum_CalibrationCurve.csv"
  config.urinecholesterol_lib_file = "#{config.APGCMS_calibration_dir}/lib_urine_CalibrationCurve_Cholesterol.csv"
  config.urinesuccinicacidd4_lib_file =  "#{config.APGCMS_calibration_dir}/lib_urine_CalibrationCurve_SuccinicAcidD4.csv"
  config.urinetropicacid_lib_file =  "#{config.APGCMS_calibration_dir}/lib_urine_CalibrationCurve_TropicAcid.csv"

  # user cases input and output dir 
  config.APGCMS_input_dir = "#{config.APGCMS_root}/JOBS"
  config.APGCMS_output_dir = "#{config.APGCMS_root}/JOBS"

end
