# encoding: UTF-8
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

ActiveRecord::Schema.define(version: 20180813181427) do

  create_table "sessions", force: :cascade do |t|
    t.string   "session_id", limit: 255,   null: false
    t.text     "data",       limit: 65535
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "sessions", ["session_id"], name: "index_sessions_on_session_id", unique: true, using: :btree
  add_index "sessions", ["updated_at"], name: "index_sessions_on_updated_at", using: :btree

  create_table "spectra", force: :cascade do |t|
    t.string   "category",                   limit: 255
    t.string   "status",                     limit: 255
    t.text     "error",                      limit: 65535
    t.string   "job_id",                     limit: 255
    t.integer  "runtime",                    limit: 4
    t.integer  "submission_id",              limit: 4
    t.string   "spectrum_data_file_name",    limit: 255
    t.string   "spectrum_data_content_type", limit: 255
    t.integer  "spectrum_data_file_size",    limit: 4
    t.datetime "spectrum_data_updated_at"
    t.string   "json_results_file_name",     limit: 255
    t.string   "json_results_content_type",  limit: 255
    t.integer  "json_results_file_size",     limit: 4
    t.datetime "json_results_updated_at"
    t.string   "plot_file_name",             limit: 255
    t.string   "plot_content_type",          limit: 255
    t.integer  "plot_file_size",             limit: 4
    t.datetime "plot_updated_at"
    t.datetime "created_at",                               null: false
    t.datetime "updated_at",                               null: false
  end

  create_table "submissions", force: :cascade do |t|
    t.string   "database",                     limit: 255
    t.string   "job_id",                       limit: 255
    t.string   "secret_id",                    limit: 255
    t.text     "error",                        limit: 65535
    t.string   "status",                       limit: 255
    t.integer  "runtime",                      limit: 4
    t.boolean  "profile",                                    default: false
    t.string   "internal_standard",            limit: 255
    t.string   "input_zip_file_name",          limit: 255
    t.string   "input_zip_content_type",       limit: 255
    t.integer  "input_zip_file_size",          limit: 4
    t.datetime "input_zip_updated_at"
    t.string   "profile_library_file_name",    limit: 255
    t.string   "profile_library_content_type", limit: 255
    t.integer  "profile_library_file_size",    limit: 4
    t.datetime "profile_library_updated_at"
    t.string   "calibration_file_name",        limit: 255
    t.string   "calibration_content_type",     limit: 255
    t.integer  "calibration_file_size",        limit: 4
    t.datetime "calibration_updated_at"
    t.text     "database_subset",              limit: 65535
    t.datetime "created_at",                                                 null: false
    t.datetime "updated_at",                                                 null: false
    t.integer  "mf_score_threshold",           limit: 4
  end

end
