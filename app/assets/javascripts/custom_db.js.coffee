$ -> 
  # ------
  # Custom Defined Mixture Code
  
  db_table = $('.db-list').DataTable({
    # dataTables 1.10 settings
    "scrollCollapse": true,
    "scrollY": "300px",
    "paging": false
    "destroy": true
    "columnDefs": [ {
       "targets": 0,
       "orderable": false
     } ]
  })

  # Fix the width of the header columns after show the dialog
  # Also activate tool-tips
  $('#custom-db-list').on 'shown.bs.modal', ->
    db_table.columns.adjust().draw()

  # Tool tips settings
  $('.tool-tip').tooltip()

  $('.btn-disabled').on 'click', ->
    false;


  # Radio button only shows the dialog if no metabolites have been selected
  $('#radio-database-custom').on 'click', ->
    if custom_db_count() == 0
      $('#custom-db-list').modal('show')

  # Cancel
  $('.custom-db-cancel').on 'click', ->
    $('#custom-db-list').modal('hide')


  # Save
  $('#custom-db-save').on 'click', ->
    $('#custom-db-list input:checkbox').each ->
      $(this).data('checked-state', this.checked)
    $('#custom-db-list').modal('hide')

  # Dialog Closed
  # Set check boxes to checked-state
  $('#custom-db-list').on 'hide.bs.modal', ->
    $('#custom-db-list input:checkbox').each ->
      $(this).prop('checked', $(this).data('checked-state'))
    check_custom_list_count()

  # Edit
  $('#custom-db-edit').on 'click', ->
    $('#custom-db-list').modal('show')
    return false

  # Select All
  $('#db-button-all').on 'click', ->
    $('#custom-db-list input:checkbox').prop('checked', true).change()
    return false

  # Select None
  $('#db-button-none').on 'click', ->
    $('#custom-db-list input:checkbox').prop('checked', false).change()
    return false

  # Select Other (eg. csf, serum ...)
  $('.db-button-select').on 'click', ->
    ids = $(this).data('hmdb-ids').split(',')
    $('#custom-db-list input:checkbox').prop('checked', false).change()
    $.each ids, (index, value) ->
      $('#custom-db-list input:checkbox[value="' + value + '"]').prop('checked', true).change()
    return false

  # Update list count as checkboxes are selected
  $('#custom-db-list input:checkbox').on 'change', ->
    update_custom_list_count()

  # Update and Show/Hide "Edit list"
  # If no items are selected, the first radio button will be selected
  check_custom_list_count = ->
    update_custom_list_count()
    $('#custom-db-edit').toggle(custom_db_count() > 0)
    if custom_db_count() == 0
      $($('input:radio[name="nmr_request[biofluid_type]"]').first()).prop('checked', true)

  # Update number of metabolites selected
  update_custom_list_count = ->
    $('.custom-db-count').html(custom_db_count())


  # number of metabolites selected
  custom_db_count = ->
    $('#custom-db-list input:checked').length


