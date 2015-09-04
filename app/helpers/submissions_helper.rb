module SubmissionsHelper

  def custom_db_table(submission)
    ids = Metabolites.available_ids
    html = "<div class='db-list-wrapper'><table class='db-list table table-striped table-condensed'>"
    html << "<thead><tr><th></th><th>HMDB ID</th><th>Compound Name</th></tr></thead><tbody>"
    ids.each do |id|
      checked = submission.database_subset.include?(id)
      html << "<tr><td>"
      html << check_box_tag('submission[database_subset][]', id, checked, 'data-checked-state' => checked)
      html << "</td><td>#{bio_link_out(:hmdb, id)}</td>"
      html << "<td>#{Metabolites.name_for(id)}</td>"
    end
    html << "</tbody></table></div>"
    html.html_safe
  end

end
