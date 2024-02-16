class CreateSubmissions < ActiveRecord::Migration[5.2]
  def change
    create_table :submissions do |t|
      t.string :database
      t.string :job_id
      t.string :secret_id
      t.text :error
      t.string :status
      t.integer :runtime
      t.boolean :profile, default: false
      t.string :internal_standard
      t.attachment :input_zip
      t.attachment :profile_library
      t.attachment :calibration
      t.text :database_subset

      t.timestamps null: false
    end
  end
end
