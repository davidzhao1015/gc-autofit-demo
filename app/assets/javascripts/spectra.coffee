# Since the SpectraViewer can take a while to load,
# use $(window).load rather than $(document).ready, so images are loaded first
$(window).load ->

  $('#spectra-viewer').each ->
    # Initialize Spectra Viewer
    sv = new JSV.SpectraViewer('#spectra-viewer', {
      width: $('main').width(),
      height: 400,
      # debug: true,
      drag_drop_load: false,
      zoom_max: 500,
      axis_y_show: true,
      axis_y_lock: 0.04,
      axis_x_reverse: false,
      axis_x_title: 'Seconds',
      axis_y_title: 'Intensity'
    })
    window.sv = sv
    sv.flash('Loading...')

    # Load viewer data
    load_spectrum = (path) ->
      if sv.spectra().empty() then sv.clear()
      sv.flash('Loading...')
      $.ajax
        dataType: 'json',
        url: path,
        success: (data) ->
          saved_domains = [sv.scale.x.domain(), sv.scale.y.domain()]
          viewer_was_not_empty = sv.spectra().length > 0
          sv.remove_all_spectra()
          sv.add_spectrum({xy_line: data.spectrum_xy, tolerance: 0.001})
          if (viewer_was_not_empty)
            sv.set_domain('x', saved_domains[0])
            sv.set_domain('y', saved_domains[1])
          sv.draw()
          load_results_table()
        error: () ->
          sv.clear()
          sv.remove_all_spectra()
          sv.flash('Spectrum is not available')
          # clear_quantities_table()

    # Load viewer data
    load_spectrum($(this).data('spectra-path'))

    # $.getJSON $(this).data('spectra-path'), (data) ->
    #   # sv.add_bayesil_data(data)
    #   # load_quantities_table()
    #   sv.add_spectrum({xy_line: data.spectrum_xy, tolerance: 0.001})
    #   window.data = data
    #   sv.draw()


    # Viewer locking

    # Make Spectra Viewer Stick to window top when scrolling
    $window = $(window)
    sv_container = $('#spectra-container')
    sv_anchor = $('#spectra-anchor')  # Viewer offset replacement with viewer becomes fixed
    top_margin = $('#main-nav').height()
    viewer_top = sv_container.offset().top
    min_window_height = sv_container.height() + top_margin + 30
    $('#spectra-header').width($('main').width())
    $('#spectra-controls').width($('main').width())
    $('#phase-controls').width($('main').width())

    if $window.height() <= min_window_height
      $('#lock-viewer').addClass('disabled')

    $window.scroll () ->
      if $window.height() > min_window_height && $('#lock-viewer').hasClass('btn-on')
        sv_container.addClass('sticky')
        sv_anchor.height(sv_container.height())
        $('#spectra-container hr').addClass('drop-shadow')
        if $window.scrollTop() + top_margin < viewer_top
          $window.scrollTop(viewer_top - top_margin)
      else
        sv_container.removeClass('sticky')
        sv_anchor.height(0)
        $('#spectra-container hr').removeClass('drop-shadow')


    $('#lock-viewer').on 'click', () ->
      $(this).toggleClass('btn-on')
      if $(this).html() == $(this).data('on-name')
        $(this).html( $(this).data('off-name'))
      else
        $(this).html( $(this).data('on-name'))
      $window.scroll() # trigger scroll event to update stickiness
      return false
    # Window Resize
    $window.resize () ->
      sv.resize($('main').width(), null, true)
      $('#spectra-header').width($('main').width())
      $('#spectra-controls').width($('main').width())
      $('#phase-controls').width($('main').width())
      if $window.height() <= min_window_height
        $('#lock-viewer').addClass('disabled')
        $('#lock-viewer').html( $('#lock-viewer').data('off-name'))
        $('#lock-viewer').removeClass('btn-on')
        $window.scroll() # trigger scroll event to update stickiness
      else
        $('#lock-viewer').removeClass('disabled')

    # Resizable bar below viewer
    # http://events.manzwebdesigns.com/2011/09/26/jquery-move-able-div/
    $('#spectra-resize-bar').on 'mousedown', (event) ->
      event.preventDefault()
      y1 = event.clientY
      $(document).on 'mousemove', (event) ->
        y2 = event.clientY
        dy = y2 - y1
        sv.resize(null, sv.height + dy, true)
        y1 = y2
      $(document).one 'mouseup', () ->
        $(this).off('mousemove')
        sv.draw()

    # Batch Controls
    $('#batch-select').on 'change', () ->
      spectrum_id = $('#spectra-container').data('spectrum-id')
      submission_id = $('#spectra-container').data('submission-id')
      path_base = '/submissions/' + submission_id + '/spectra/' + $(this).val()
      $.getScript path_base + '.js',  () ->
        load_spectrum(path_base + '.json')

    $('#spectrum-prev,#spectrum-next').on 'click', () ->
      path_base = $(this).attr('href')
      $.getScript path_base + '.js',  () ->
        load_spectrum(path_base + '.json')
      return false

    standard_columns = ['Cn', 'ALKRT', 'Intensity']

    fake_standard = () ->
      labels = [ ]
      [10, 11, 12, 13, 14].forEach (cn, i) ->
        labels.push {
          x: 400+100*i,
          y: 3000000*i,
          text: 'label' + i,
          meta: {
            table: {
              'Cn': cn,
              'ALKRT': 400+100*i
              'Intensity': 3000000*i
            }
          }
        }
      labels


    load_results_table = () ->
      table = $('.results-table').DataTable()
      # Clear previous data
      table.clear()
      # Set up fake data
      fake_standard().forEach (label) ->
        sv.spectra(1).labels.add(label)

      # Load new data
      sv.spectra(1).labels.get().each () ->
        # Generate row
        data = this.meta.table
        row_data = [ ]
        column_names = standard_columns
        column_names.forEach (column) ->
          row_data.push(data[column])
        row_data.push('testbbo')
        row_node = table.row.add(row_data).node()
        # Add table row id
        $(row_node).attr('id', this.id)
      # # Format Concentration column
      # table.column(2).nodes().to$().addClass('number')
      table.draw()
      sv.draw()



