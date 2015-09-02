$ ->
  if $('#submission-status').length > 0
    setTimeout(updateSubmissionStatus, 5000)
  if $('#samples-status').length > 0
    setTimeout(updateSamplesStatus, 5000)

  $('#spectra-list tbody').on('click', 'tr.spectrum-active', () ->
  #   window.location.href = $(this).data('spectrum-link')
  #   TODO: set results tab to this spectrum
  )


updateSubmissionStatus = () ->
  if !$('#submission-status').data('finalized')
    secret_id = $('#submission-status').data('secret-id')
    $.getScript('/submissions/' + secret_id + '.js')
    setTimeout(updateSubmissionStatus, 5000)

updateSamplesStatus = () ->
  if !$('#samples-status').data('finalized')
    secret_id = $('#submission-status').data('secret-id')
    $.getScript('/submissions/' + secret_id + '.js')
    setTimeout(updateSamplesStatus, 5000)

