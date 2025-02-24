class CreateSpectra < ActiveRecord::Migration[5.2]
  def change
    create_table :spectra do |t|
      t.string  :category
      t.string  :status
      t.text  :error
      t.string :job_id
      t.integer :runtime
      t.integer :submission_id

      t.attachment :spectrum_data
      t.attachment :json_results
      t.attachment :plot

      t.timestamps null: false
    end
  end
end
