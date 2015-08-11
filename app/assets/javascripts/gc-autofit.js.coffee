$ ->
  $(".nav-tabs a[data-toggle=tab]").on "click", (e) ->
    if ($(this).parent().hasClass("disabled"))
      e.preventDefault()
      return false
