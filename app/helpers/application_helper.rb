module ApplicationHelper

  def render_form_errors(entry)
    render :partial => '/shared/form_errors', :locals => { :entry => entry }
  end

  # model must have
  #  - display_status method that returns the text to show
  #  - running? method indicating that a spinner should be shown
  def display_status(model, large=false)
    html = ''
    if model.running?
      if large
        html << '<span class="spinner-lg"></span> '
      else
        html << '<span class="spinner"></span> '
      end
    end
    html << model.display_status
    html.html_safe
  end

end
