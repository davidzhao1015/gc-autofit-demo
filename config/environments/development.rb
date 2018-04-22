Rails.application.configure do
  # Settings specified here will take precedence over those in config/application.rb.

  # In the development environment your application's code is reloaded on
  # every request. This slows down response time but is perfect for development
  # since you don't have to restart the web server when you make code changes.
  config.cache_classes = false

  # Do not eager load code on boot.
  config.eager_load = false

  # Show full error reports and disable caching.
  config.consider_all_requests_local       = true
  config.action_controller.perform_caching = false

  # Don't care if the mailer can't send.
  config.action_mailer.raise_delivery_errors = false

  # Print deprecation notices to the Rails logger.
  config.active_support.deprecation = :log

  # Raise an error on page load if there are pending migrations.
  config.active_record.migration_error = :page_load

  # Debug mode disables concatenation and preprocessing of assets.
  # This option may cause significant delays in view rendering with a large
  # number of complex assets.
  config.assets.debug = true

  # Asset digests allow you to set far-future HTTP expiration dates on all assets,
  # yet still be able to expire them through the digest params.
  config.assets.digest = true

  # Adds additional error checking when serving assets at runtime.
  # Checks for improperly declared sprockets dependencies.
  # Raises helpful error messages.
  config.assets.raise_runtime_errors = true
  
  #config.assets.precompile  += %w( print.css )
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

  # METABOLITES_DIR
  config.APGCMS_METABOLITES_DIR = "#{config.APGCMS_root}/DB/metabolites"

  # user cases working dir 
  config.APGCMS_job_dir = "#{config.APGCMS_root}/JOBS"

  # APGCMSs' example dir
  config.APGCMS_example_dir = "#{config.APGCMS_root}/example"

  # number of copies of dated csv files of each db
  config.APGCMS_copy_number = 20

end
