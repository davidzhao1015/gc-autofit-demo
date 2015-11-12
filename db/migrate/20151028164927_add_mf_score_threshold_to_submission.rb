class AddMfScoreThresholdToSubmission < ActiveRecord::Migration
  def change
    add_column :submissions, :mf_score_threshold, :integer
  end
end
