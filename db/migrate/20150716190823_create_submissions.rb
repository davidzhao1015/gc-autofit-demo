class CreateSubmissions < ActiveRecord::Migration
  def change
    create_table :submissions do |t|
      t.string :database
      t.string :job_id
      t.string :secret_id
      t.text :error
      t.string :status
      t.boolean :profile, default: false
      t.string :internal_standard
      t.attachment :input_zip
      t.attachment :profile_library
      t.attachment :calibration
      t.text :custom_database

      t.timestamps null: false
    end
  end
end
