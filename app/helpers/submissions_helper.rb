module SubmissionsHelper

  def custom_db_table(submission, database)
    html = "<div class='db-list-wrapper'><table class='db-list table table-striped table-condensed'>"
    html << "<thead><tr><th></th><th>HMDB ID</th><th>Compound Name</th></tr></thead><tbody>"
    Metabolites.available_for(database).each do |id, name|
      checked = submission.database_subset.include?(id)
      html << "<tr><td>"
      html << check_box_tag('submission[database_subset][]', id, checked, 'data-checked-state' => checked)
      html << "</td><td><a target=\"_blank\" href=\"http://www.hmdb.ca/metabolites/#{id}\">#{id}<span class=\"glyphicon glyphicon-new-window\"> </span></a></td>"
      html << "<td>#{name}</td>"
    end
    html << "</tbody></table></div>"
    html.html_safe
  end

end
