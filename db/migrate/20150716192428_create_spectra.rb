class CreateSpectra < ActiveRecord::Migration
  def change
    create_table :spectra do |t|
      t.integer :runtime

      t.timestamps null: false
    end
  end
end
