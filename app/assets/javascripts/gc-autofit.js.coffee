$ ->
  $(".nav-tabs a[data-toggle=tab]").on "click", (e) ->
    if ($(this).parent().hasClass("disabled"))
      e.preventDefault()
      return false

  # Javascript to enable link to tab
  hash = document.location.hash
  prefix = "tab_"
  if (hash)
    $('.nav-tabs a[href='+hash.replace(prefix,"")+']').tab('show')

