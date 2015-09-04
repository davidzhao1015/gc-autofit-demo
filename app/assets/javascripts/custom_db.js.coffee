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
  $('.custom-db-list').on 'shown.bs.modal', ->
    db_table.columns.adjust().draw()

  # Tool tips settings
  $('.tool-tip').tooltip()

  $('.btn-disabled').on 'click', ->
    false

  dialog_for = (element) ->
    $(element).parents('.custom-db-list')

  # Edit
  $('.custom-db-edit').on 'click', ->
    # Determine which dialog
    db = $(this).attr('href')
    dialog = $('.custom-db-list[data-db=' + db + ']')
    # Check all metabolites of count is 0
    if custom_db_count(dialog) == 0
      $('.db-button-all', dialog).trigger 'click'
    dialog.modal('show')
    return false

  # Cancel
  $('.custom-db-cancel').on 'click', ->
    $(dialog_for(this)).modal('hide')

  # Save
  $('.custom-db-save').on 'click', ->
    return if $(this).hasClass('disabled')
    dialog = dialog_for(this)
    # Only want to save if it is a subset.
    if $('input:checkbox:checked', dialog).length == $('input:checkbox', dialog).length
      $('.db-button-none', dialog).trigger 'click'
    # Save check-state
    $('input:checkbox', dialog).each ->
      $(this).data('checked-state', this.checked)
    $(dialog).modal('hide')

  # Dialog Closed
  # Set check boxes to saved checked-state
  $('.custom-db-list').on 'hide.bs.modal', ->
    $('.custom-db-list input:checkbox').each ->
      $(this).prop('checked', $(this).data('checked-state'))
    check_custom_list_count()

  # Select All
  $('.db-button-all').on 'click', ->
    $('input:checkbox', dialog_for(this)).prop('checked', true).change()
    return false

  # Select None
  $('.db-button-none').on 'click', ->
    $('input:checkbox', dialog_for(this)).prop('checked', false).change()
    return false

  # Update list count as checkboxes are selected
  $('.custom-db-list input:checkbox').on 'change', ->
    update_custom_list_count(dialog_for(this))

  # Update and Show/Hide "Edit list"
  # If no items are selected, the first radio button will be selected
  check_custom_list_count = ->
    # update_custom_list_count()
    # TODO: change color of edit button
    # $('#custom-db-edit').toggle(custom_db_count() > 0)

  # Update number of metabolites selected
  update_custom_list_count = (dialog)->
    checked_count = custom_db_count(dialog)
    $('.custom-db-count', dialog).html(checked_count)
    $('.custom-db-save', dialog).toggleClass('disabled', checked_count == 0)

  # number of metabolites selected
  custom_db_count = (dialog) ->
    $('input:checked', dialog).length


