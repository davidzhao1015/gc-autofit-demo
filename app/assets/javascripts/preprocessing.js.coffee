$(window).load ->

  # LOAD ALKANE VIEWER

  $('#alkane-viewer').each ->
    alkane_sv = new JSV.SpectraViewer('#alkane-viewer', {
      width: 800,
      height: 400,
      # debug: true,
      drag_drop_load: false,
      zoom_max: 500,
      axis_y_show: true,
      axis_y_lock: 0.04,
      axis_x_reverse: false,
      axis_x_title: 'Seconds',
      axis_y_title: 'Intensity',
      axis_y_tick_format: '.1e',
      axis_y_gutter: 90,
      legend_show: false
    })
    alkane_sv.flash('Loading...')

    # Load alkane data
    $.getJSON $(this).data('spectra-path'), (data) ->
      if data
        alkane_sv.add_spectrum({xy_line: data.xy_data, labels: data.labels, tolerance: 0.001})
        alkane_sv.draw()





