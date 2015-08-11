class CreateSpectra < ActiveRecord::Migration
  def change
    create_table :spectra do |t|
      t.string  :category
      t.integer :runtime
      t.integer :submission_id

      t.attachment :spectrum_data
      t.attachment :json_results

      t.timestamps null: false
    end
  end
end
