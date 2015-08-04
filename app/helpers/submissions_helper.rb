module SubmissionsHelper

  def custom_db_table(submission)
    ids = Metabolites.available_ids
    html = "<div class='db-list-wrapper'><table class='db-list table table-striped table-condensed'>"
    html << "<thead><tr><th></th><th>HMDB ID</th><th>Compound Name</th><th>Status</th></tr></thead><tbody>"
    ids.each do |id|
      status = Metabolites.status_for(id)
      checked = submission.custom_database.include?(id)
      html << "<tr class='status-#{status.downcase}'><td>"
      html << check_box_tag('submission[db_list][]', id, checked, 'data-checked-state' => checked)
      html << "</td><td>#{bio_link_out(:hmdb, id)}</td>"
      html << "<td>#{Metabolites.name_for(id)}</td>"
      html << "<td>#{status}</td></tr>"
    end
    html << "</tbody></table></div>"
    html.html_safe
  end

end
