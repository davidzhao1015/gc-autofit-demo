class AddMfScoreThresholdToSubmission < ActiveRecord::Migration[5.2]
  def change
    add_column :submissions, :mf_score_threshold, :integer
  end
end
