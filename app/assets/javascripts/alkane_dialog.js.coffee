$(window).load ->
  # Alkane Standards Dialog

  $('#preprocessing').on 'shown.bs.modal', '.alkane-dialog', () ->
      $('#alkane-standard-input').focus()

  $('#preprocessing').on 'click', '#alkane-save', () ->
    # Validate
    if /^\s*C\d+\s*$/.test( $('#alkane-standard-input').val() )
      # Save locally
      label_id = $('.alkane-dialog').data('label-id')
      label = window.alkane_sv.annotation.labels.get(label_id)
      label.text = $('.alkane-dialog #alkane-standard-input').val().replace(/^\s+|\s+$/g, '')
      window.alkane_sv.draw()
      # Save remotely
      save_alkane_standards()

  $('#preprocessing').on 'keyup', '#alkane-standard-input', () ->
    if /^\s*C\d+\s*$/.test( $(this).val() )
      $('#alkane-save').removeClass('disabled')
      $('~ span', this).addClass('glyphicon-ok').removeClass('glyphicon-remove')
      $('.alkane-group').addClass('has-success').removeClass('has-error')
      $('#alkane-message').html('')
    else
      $('#alkane-save').addClass('disabled')
      $('~ span', this).addClass('glyphicon-remove').removeClass('glyphicon-ok')
      $('.alkane-group').addClass('has-error').removeClass('has-success')
      $('#alkane-message').html('Should be C+number (e.g. C12)')



save_alkane_standards = () ->
  data = window.alkane_sv.annotation.labels.map (label) ->
    { x: label.x, y: label.y, text: label.text }
  submission_id = $('#alkane-viewer').data('submission-id')
  $.ajax {
    url: '/submissions/' + submission_id + '/save_alkane_standards.json',
    type: 'POST',
    data:
      alkane_standards: JSON.stringify(data),
    dataType: 'text',
    success: () ->
      $('.alkane-dialog').modal('hide')
    error: (jqXHR, status, error) ->
      alert 'There was a problem saving the adjustments: ' + error
  }

