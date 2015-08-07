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
