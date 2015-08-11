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
    $.getJSON $(this).data('spectra-path'), (data) ->
      # sv.add_bayesil_data(data)
      # load_quantities_table()
      sv.add_spectrum({xy_line: data.spectrum_xy, tolerance: 0.001})
      window.data = data
      sv.draw()


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

