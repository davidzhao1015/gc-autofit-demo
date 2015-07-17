class Submission < ActiveRecord::Base
  DATABASES = {
    #db      #display
    'serum' => 'Serum',
    'urine' => 'Urine',
    'custom' => 'Custom',
    'upload' => 'Upload Your Library'

  }
  INTERNAL_STANDARDS = %w[ Ribitol Cholesterol ]

  serialize :custom_database, Array

end
