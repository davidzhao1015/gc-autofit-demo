# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 2023_08_04_055858) do

  create_table "sessions", options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb3", force: :cascade do |t|
    t.string "session_id", null: false
    t.text "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["session_id"], name: "index_sessions_on_session_id", unique: true
    t.index ["updated_at"], name: "index_sessions_on_updated_at"
  end

  create_table "spectra", options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb3", force: :cascade do |t|
    t.string "category"
    t.string "status"
    t.text "error"
    t.string "job_id"
    t.integer "runtime"
    t.integer "submission_id"
    t.string "spectrum_data_file_name"
    t.string "spectrum_data_content_type"
    t.bigint "spectrum_data_file_size"
    t.datetime "spectrum_data_updated_at"
    t.string "json_results_file_name"
    t.string "json_results_content_type"
    t.bigint "json_results_file_size"
    t.datetime "json_results_updated_at"
    t.string "plot_file_name"
    t.string "plot_content_type"
    t.bigint "plot_file_size"
    t.datetime "plot_updated_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "submissions", options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb3", force: :cascade do |t|
    t.string "database"
    t.string "job_id"
    t.string "secret_id"
    t.text "error"
    t.string "status"
    t.integer "runtime"
    t.boolean "profile", default: false
    t.string "internal_standard"
    t.string "input_zip_file_name"
    t.string "input_zip_content_type"
    t.bigint "input_zip_file_size"
    t.datetime "input_zip_updated_at"
    t.string "profile_library_file_name"
    t.string "profile_library_content_type"
    t.bigint "profile_library_file_size"
    t.datetime "profile_library_updated_at"
    t.string "calibration_file_name"
    t.string "calibration_content_type"
    t.bigint "calibration_file_size"
    t.datetime "calibration_updated_at"
    t.text "database_subset"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "mf_score_threshold"
    t.boolean "update_library"
  end

end
