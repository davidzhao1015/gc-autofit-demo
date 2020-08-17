$(window).load ->

  # LOAD ALKANE VIEWER

  load_alkane_viewer = () ->
    $('#alkane-viewer').each ->
      alkane_sv = new JSV.SpectraViewer('#alkane-viewer', {
        width: 950,
        height: 400,
        # debug: true,
        drag_drop_load: false,
        zoom_max: 500,
        axis_y_show: true,
        axis_y_lock: 0.04,
        axis_x_reverse: false,
        axis_x_title: 'Minutes',
        axis_y_title: 'Intensity',
        axis_y_tick_format: '.1e',
        axis_y_gutter: 90,
        legend_show: false
      })
      alkane_sv.flash('Loading...')
      window.alkane_sv = alkane_sv

      # Load alkane data
      $.ajax
        dataType: 'json'
        url: $(this).data('spectra-path')
        success: (data) ->
          if data && data.xy_data
            # increase y max so top of highest peak and name is visible
            y_value_max = d3.max(data.xy_data.y)
            adjusted_y_value_max = y_value_max * 1.2
            alkane_sv.boundary.update({x: data.xy_data.x, y:data.xy_data.y.concat(adjusted_y_value_max)})
            alkane_sv.scale.update({x: data.xy_data.x, y:data.xy_data.y.concat(adjusted_y_value_max)})
            alkane_sv.add_spectrum({xy_line: data.xy_data, labels: data.labels, tolerance: 0.001})
            alkane_sv.draw()
        # Error handling not working for some reason
        # error: (jqXHR, textStatus, errorThrown) ->
        #   console.log('error')
      # $.getJSON $(this).data('spectra-path'), (data) ->
      #   if data
      #     alkane_sv.add_spectrum({xy_line: data.xy_data, labels: data.labels, tolerance: 0.001})
      #     alkane_sv.draw()

      alkane_sv.on 'label-click', (label) ->
        $('#alkane-save').removeClass('disabled')
        $('#alkane-message').html('')
        $('.alkane-group').removeClass('has-success').removeClass('has-error')
        $('#alkane-standard-input ~ span').removeClass('glyphicon-remove').removeClass('glyphicon-ok')
        $('.alkane-dialog').modal('show')
        $('.alkane-dialog').data('label-id', label.label_id())
        $('.alkane-dialog #alkane-standard-input').val(label.text)

  load_alkane_viewer()
  window.load_alkane_viewer = load_alkane_viewer

  if $('#profiling-tab:not(.disabled)').length > 0
    window.alkane_sv.annotation.hover = false
    window.alkane_sv.annotation.label_color = 'black'




