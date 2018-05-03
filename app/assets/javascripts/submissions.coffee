$ ->
  if $('#submission-status').length > 0
    setTimeout(updateSubmissionStatus, 5000)
  if $('#samples-status').length > 0
    setTimeout(updateSamplesStatus, 5000)

  $('#profiling').on('click', 'tr.spectrum-active', () ->
    window.location.href = $(this).data('spectrum-link')
  )

  # Handle 'User uploaded database'
  if $('#database-selection :radio:checked').val() != 'upload'
      $('#custom-database-upload').hide()
  $('#database-selection :radio').on 'change', () ->
    if $(this).val() == 'upload'
      $('#custom-database-upload').slideDown('fast')
    else
      $('#custom-database-upload').slideUp('fast')

  # Handle 'Other' Internal standard slection
  if $('#internal-standard-selection :radio').val() != 'Other'
      $('#custom_internal_standard').hide()
  $('#internal-standard-selection :radio').on 'click', () ->
    if $(this).val() == 'Other'
      $('#custom_internal_standard').fadeIn('fast')
    else
      $('#custom_internal_standard').fadeOut('fast')

  #Upload Spectra format
  $('.toggle-upload').on 'click', () ->
    toggle = $(this).attr('id').replace('toggle-upload-','')
    $('#upload_spectra_format').val(toggle)




updateSubmissionStatus = () ->
  if !$('#submission-status').data('finalized')
    secret_id = $('#submission-status').data('secret-id')
    $.getScript '/submissions/' + secret_id + '.js', () ->
      window.load_alkane_viewer()
    setTimeout(updateSubmissionStatus, 5000)
  
updateSamplesStatus = () ->
  if !$('#samples-status').data('finalized')
    secret_id = $('#submission-status').data('secret-id')
    $.getScript('/submissions/' + secret_id + '.js')
    setTimeout(updateSamplesStatus, 5000)


