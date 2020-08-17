//////////////////////////////////////////////////////////////////////////////
// JSpectraViewer Setup
//////////////////////////////////////////////////////////////////////////////
var JSpectraViewer = {};
 
JSpectraViewer.version = '0.1'

if (window.JSV === undefined) window.JSV = JSpectraViewer;

(function(JSV) {

  JSV.inherits = function(child, parent) {
    child.prototype = Object.create(parent.prototype);
  }

  JSV.initialize = function(){
    JSV._viewers = new JSV.SVSet();

    JSV.viewers = function(term) {
      return JSV._viewers.get(term);
    }

  }


})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * The SpectraViewer object controls how the viewer looks and behaves and
   * is the main object in JSV. Spectra are added to the SpectraViewer
   * using the [add_spectrum](#add_spectrum) method. A large number of options
   * can be set when creating a new viewer:
   *
   *  Option              | Default     | Description
   *  --------------------|-------------------------------------------------
   *  title               | _undefined_ | Title will appear above viewer if present
   *  id                  | jsv-1       | ID to select viewer
   *  width               | 800         | Width of viewer in pixels
   *  height              | 400         | Height of viewer in pixels
   *  axis_x_title        | 'ppm'       | Label of x-axis
   *  axis_x_show         | true        | Show x-axis
   *  axis_x_grid         | true        | Show grid along x-axis
   *  axis_y_title        | 'Intensity' | Label of y-axis
   *  axis_y_show         | false       | Show y-axis
   *  axis_y_grid         | false       | Show grid along y-axis
   *  axis_y_lock         | false       | If true, locks axis to minimum y value. If a number locks the y-axis a percentage below 0 (e.g. 0.1 sets the y-axis minimum to 10% of the viewable y-axis scale below 0)
   *  min_boundaries      | _undefined_ | The minimum/maximum values for the x/y-axis. e.g. {x: [-1, 10], y: [0, 1]}. _Note_: _min_boundaries_ must be set, if only providing peak data.
   *
   * @param {String} container_id The id of the element to contain the viewer.
   *   The contents of this element will be replaced with the viewer.
   * @param {Object} options Options to set up the viewer. Described below.
   * @return {SpectraViewer}
   */
  var SpectraViewer = function(container, options) {
    var sv = this;
    this.container = d3.select(container);
    // Get options
    options = options || {};
    this.title          = options.title;
    this.width          = JSV.default_for(options.width, 800);
    this.height         = JSV.default_for(options.height, 400);
    this.tick_count     = JSV.default_for(options.tick_count, 10);
    this.tick_length    = JSV.default_for(options.tick_length, 10);
    this.tick_precision = JSV.default_for(options.tick_precision, 3);
    this.axis_x_tick_format = d3.format(JSV.default_for(options.axis_x_tick_format, '.g'));
    this.axis_y_tick_format = d3.format(JSV.default_for(options.axis_y_tick_format, '.g'));
    this.axis_x_title   = JSV.default_for(options.axis_x_title, 'ppm');
    this.axis_y_title   = JSV.default_for(options.axis_y_title, 'Intensity');
    this.axis_y_show    = JSV.default_for(options.axis_y_show, false);
    this.axis_x_show    = JSV.default_for(options.axis_x_show, true);
    this.axis_y_grid    = JSV.default_for(options.axis_y_grid, false);
    this.axis_x_grid    = JSV.default_for(options.axis_x_grid, false);
    // this.axis_y_reverse = JSV.default_for(options.axis_y_reverse, false);
    this.axis_x_reverse = JSV.default_for(options.axis_x_reverse, true);
    // If logical true, locks y axis to minimum y value
    // If a number locks the y axis to a percentage below 0 (e.g. 0.1 sets the y-axis min to 10% of the 
    // viewable y axis scale below 0).
    this.axis_y_lock    = JSV.default_for(options.axis_y_lock, false);
    // min_boundaries must be set, if only providing peak data
    this.min_boundaries = options.min_boundaries; // Array example: {x: [-1, 10], y: [0, 1]}]
    this.zoom_max       = JSV.default_for(options.zoom_max, 100);
    this.zoombox_show   = JSV.default_for(options.zoombox_show, true);
    this.zoombox_size   = JSV.default_for(options.zoombox_size, 15); // percentage of viewer
    this.legend_show    = JSV.default_for(options.legend_show, true);
    // Controls the number of pixels saved for plots while zooming/draging
    // Increasing this number will descrease the number of pixels and thus speed up animations.
    // If no tolerance is provided, it will be calculated automatically from the spectra noise.
    this.simplify_tolerance = options.simplify_tolerance;
    // An array of debug keys to display or true to show all.
    this.debug = JSV.default_for(options.debug, false);
    this.debug_data = { time: {}, data: {}, drag: {}, zoom: {}, zbox: {} };
    // Set viewer ID
    var current_ids = JSV.viewers().map(function(viewer) { return viewer.id; } );
    this.id = JSV.default_for(options.id, JSV.unique_id('jsv-', 1, current_ids));

    // Space required for axes
    this.axis_y_gutter = this.axis_y_show ? JSV.default_for(options.axis_y_gutter, 60) : 0;
    this.axis_x_gutter = this.axis_x_show ? JSV.default_for(options.axis_x_gutter, 50) : 0;

    // Delete contents of container and add title
    var header = this.title ? '<h3>' + this.title + '</h3>' : ''
    this.container.html(header)
      // .style('width', this.width + 2 + 'px');

    this.sv_wrapper = this.container.append('div')
      .attr('class', 'sv-wrapper')
      .style('position', 'relative');

    // Add div to store the current key press
    this.container.append('div')
      .attr('class', 'sv-key-down')
      .style('display', 'none');

    // Create the viewer canvas
    // NOTE: anything drawn to the canvas must take the pixel ratio into account
    //       and should use the pixel() method.
    this.canvas = this.sv_wrapper.append("canvas")
      .attr("id", this.container.attr('id'))
      .attr("class", 'spectra-viewer ' + this.class)
      .style({'border': '1px solid #DDD'})
      .attr("width", this.width)
      .attr("height", this.height).node();

    // Check for canvas support
    if (!this.canvas.getContext) {
      this.container.html('<h3>Spectra Viewer requires Canvas, which is not supported by this browser.</h3>');
      throw('Canvas not supported');
    }

    // Get pixel ratio and upscale canvas depending on screen resolution
    // http://www.html5rocks.com/en/tutorials/canvas/hidpi/
    JSV.pixel_ratio = JSV.get_pixel_ratio(this.canvas);
    JSV.scale_resolution(this.canvas, JSV.pixel_ratio);

    // This would need to be adjusted in width setter
    this.font = this.adjust_font();
    this.axis_title_font = this.adjust_font(1, undefined, 'bold'); 
    // Set viewer context
    this.context = this.canvas.getContext('2d');

    // Add a placeholder function for browsers that don't have setLineDash()
    if (!this.context.setLineDash) {
      this.context.setLineDash = function () {}
    }

    // Set up scales for plot area
    this.scaled_height = JSV.pixel(this.height - this.axis_x_gutter);
    this.scaled_width  = JSV.pixel(this.width - this.axis_y_gutter);

    // Scale hold the current x/y scales
    // NOTE: to reverse axis, reverse the ranges
    this.scale = new JSV.SVScale();
    this.scale.y.range([this.scaled_height, 0]);
    // this.axis_x_reverse = false
    this.scale.x.range(this.x_range());

    // boundary hold the domain extents
    this.boundary = new JSV.SVScale();
    this.boundary.x.range(this.x_range());
    this.boundary.y.range([this.scaled_height, 0]);
    this.boundary.clamp(true);

    // Set minimun boundaries
    if (this.min_boundaries) {
      this.boundary.update(this.min_boundaries);
      this.scale.update(this.min_boundaries);
    }

    // Initialize containers
    this._spectra = new JSV.SVSet();


    // Create SVG overlay
    this.svg = this.sv_wrapper.append('svg')
      .attr('width', this.width)
      .attr('height', this.height)
      .attr('class', 'svg-viewer')
      .style('position', 'absolute')
      .style('top', 0)
      .style('left', 0);

    // Set cursor for svg
    this.svg.style('cursor', 'all-scroll');

    //TODO: change drag drop options to: none, replace, add
    // This option must be set after the svg is set up
    this.drag_drop_load = JSV.default_for(options.drag_drop_load, true);

    this.initialize_zooming();
    this.initialize_dragging();
    this.initialize_brushing();
    sv.legend = new JSV.SVLegend(sv);

    // Add keyboard callback
    // d3.select(window)
    //   .on("keydown", function() { if (sv.contains_mouse) sv.keydown() })
    //   .on("keyup",   function() { sv.keyup() });
    d3.select(window)
      .on("keydown", function() { sv.keydown() })
      .on("keyup",   function() { sv.keyup() });

    this.svg.on('mousemove', function() {
      var pos = d3.mouse(sv.canvas);
      sv.mx = sv.scale.x.invert(JSV.pixel(pos[0]))
      sv.my = sv.scale.y.invert(JSV.pixel(pos[1]))
      // console.log([sv.mx, sv.my]);
      sv.highlight.hover();
      sv.annotation.check_hover();
    });

    this.svg.on('mouseover', function() {
      sv.contains_mouse = true;
      // Remove focus from other elements (e.g. NMRLib editor), so that
      // pressing keys like 'a' or 's' do not continue typing in other elements.
      document.activeElement.blur();
    });

    this.svg.on('mouseout', function() {
      sv.contains_mouse = false;
    });

    // Setup SVEvents
    this.handlers = new JSV.SVEvents();
    this.on = this.handlers.on
    this.off = this.handlers.off
    this.trigger = this.handlers.trigger

    if (this.drag_drop_load) { this.initialize_drag_drop_load(); }

    this.selection = new JSV.SVSelection(this, JSV.default_for(options.select, {}));

    // Initialize Labels
    // this.labels = new JSV.SVLabels(sv);
    this.annotation = new JSV.SVAnnotation(sv);

    // Initialize Zoom Box
    this.zoombox = new JSV.ZoomBox(sv);
    this.zoombox.visible = this.zoombox_show;

    // This option must be set after sv_wrapper and zoombox
    this.highlight = new JSV.SVHighlighter(this, JSV.default_for(options.highlight, {}));

    // Initialize Menu
    this.menu = new JSV.SVMenu(sv);

    // Initialize Help
    this.help = new JSV.SVHelp(sv);


    this.initial_settings();

    this.cluster_navigation_id = options.cluster_navigation_id;

    JSV._viewers.push(this);

    // Draw viewer
    this.draw();
  }

  SpectraViewer.prototype.toString = function() { return 'SpectraViewer' }

  /////////////////////////////////////////////////////////////////////////////
  // SpectraViewer Properties (setters/getters)
  /////////////////////////////////////////////////////////////////////////////
  Object.defineProperties(SpectraViewer.prototype, {
    'drag_drop_load': {
      get: function() { return this._drag_drop_load; },
      set: function(val) {
        this._drag_drop_load = val;
        if (this._drag_drop_load) {
          this.initialize_drag_drop_load();
        } else {
          this.svg.on('.dragndrop', null);
        }
      }
    },
    'cluster_navigation_id': {
      get: function() { return this._cluster_navigation_id; },
      set: function(val) {
        this._cluster_navigation_id = val;
        if (this._cluster_navigation_id) {
          this._cluster_navigation = new JSV.SVClusterNavigator(this, val);
        } else {
          if (this._cluster_navigation) {
            this._cluster_navigation.detach();
            this._cluster_navigation = undefined;
          }
        }
      }
    }
    // 'zoombox_show': {
    //   get: function() { return this.zoombox.visible; },
    //   set: function(val) {
    //     this.zoombox.visible = val
    //   }
    // }
  });

  SpectraViewer.prototype.x_range = function(width) {
    var width = width || this.width;
    var scaled_width  = JSV.pixel(width - this.axis_y_gutter);
    return this.axis_x_reverse ?
      [scaled_width, 0] :
      [JSV.pixel(this.axis_y_gutter), JSV.pixel(width)];
  }

  SpectraViewer.prototype.resize = function(width, height, fast) {
    this.width = width || this.width;
    this.height = height || this.height;

    this.container
      .style('width', this.width + 'px');
    d3.select(this.canvas)
      .attr('width', this.width)
      .attr('height', this.height);
    this.font = this.adjust_font();
    this.axis_title_font = this.adjust_font(1, undefined, 'bold');
    this.scaled_height = JSV.pixel(this.height - this.axis_x_gutter);
    this.scaled_width  = JSV.pixel(this.width - this.axis_y_gutter);
    this.scale.y.range([this.scaled_height, 0]);
    this.boundary.y.range([this.scaled_height, 0]);
    this.scale.x.range(this.x_range());
    this.boundary.x.range(this.x_range());

    this.svg.attr('width', this.width).attr('height', this.height);
    JSV.scale_resolution(this.canvas, JSV.pixel_ratio);
    this.zoombox.update();
    this.legend.update();
    this.draw(fast);
  }

  SpectraViewer.prototype.keyval = function() {
    return d3.select('.sv-key-down').html();
  }
  

  d3.selection.prototype.moveToFront = function() { 
    return this.each(function() { 
      this.parentNode.appendChild(this); 
    }); 
  }; 

  SpectraViewer.prototype.keydown = function() {
    // This has to be done globally on all viewers
    if (d3.event[this.zoom_brush_key]) {
      d3.selectAll('.sv-wrapper .svg-viewer').style('cursor', 'crosshair');
      // d3.selectAll('.zoom-brush').moveToFront();
    }
    d3.selectAll('.sv-key-down').html(d3.event.keyCode);
    // console.log(d3.event);
  }

  SpectraViewer.prototype.keyup = function() {
    // 16 is shift
    if (d3.event.keyCode == 16) {
      d3.selectAll('.sv-wrapper .svg-viewer').style('cursor', 'all-scroll');
      // d3.selectAll('.select-brush').moveToFront();
    }
    // Test external custom cursor
    // d3.selectAll('.sv-wrapper .svg-viewer').style('cursor', "url('http://www.javascriptkit.com/dhtmltutors/cursor-hand.gif'), 'pointer'");
    d3.selectAll('.sv-key-down').html('');
  }


  SpectraViewer.prototype.pixels_to_units_of_axis = function(axis, number_of_pixels) {
    var sv = this;
    return sv.scale[axis].invert(JSV.pixel(number_of_pixels)) - sv.scale[axis].invert(0);
  }


  // Change the zoom axis and zoom scale to the current zoom level if the axis is changing
  SpectraViewer.prototype.set_zoom_axis = function(zoom_y_key_down) {
    if (zoom_y_key_down) {
      if (this.zoom_axis != 'y') {
        this.zoom_axis = 'y';
        this.zoom_behavior.scale(this.zoom_y);
        this.set_zoom_cursor(zoom_y_key_down);
      }
    } else {
      if (this.zoom_axis != 'x') {
        this.zoom_axis = 'x';
        this.zoom_behavior.scale(this.zoom_x);
        this.set_zoom_cursor(zoom_y_key_down);
      }
    }
  }

  SpectraViewer.prototype.set_zoom_cursor = function(zoom_y_key_down) {
    if (zoom_y_key_down) {
      this.svg.style('cursor', 'ns-resize');
    } else {
      this.svg.style('cursor', 'ew-resize');
    }
  }

  // Zoom the spectra for the supplied axis based on the current zoom level.
  // - axis: either 'x' or 'y'
  // - zoom: zoom level for the axis
  SpectraViewer.prototype.scale_axis = function(axis, zoom_level) {
    // Calculate the difference between min and max values on axis after zooming
    var axis_diff = Math.abs(this.boundary[axis].domain()[0] - this.boundary[axis].domain()[1]) / zoom_level
    // Value of axis at the mouse position
    var mouse_index = axis === 'x' ? 0 : 1;
    var value = this.scale[axis].invert(this.mouse(this.canvas)[mouse_index]);
    // Calculate the ratio the mouse position is along the axis
    var axis_ratio = (value - this.scale[axis].domain()[0]) / Math.abs(this.scale[axis].domain()[0] - this.scale[axis].domain()[1]);

    // Constrain y zooming so that y domain minimum stays constant
    if (axis == 'y') {
      axis_ratio = 0;
      value = this.scale.y.domain()[0];
    }

    // Initially set the zoomed domain using the axis ratio
    var domain = [value - (axis_diff * axis_ratio), value + (axis_diff * (1-axis_ratio) )];
    this.scale[axis].domain(domain);

    // Update the domain making sure they are within the boundaries
    this.set_domain(axis, domain);

    // Save zoom level
    // this['zoom_' + axis] = zoom_level

    // DEBUG INFO
    if (this.debug) {
      axis = axis.toUpperCase();
      this.debug_data.zoom['d' + axis]  = JSV.round(axis_diff);
      this.debug_data.zoom['v' + axis]  = JSV.round(value);
      this.debug_data.zoom['r' + axis]  = JSV.round(axis_ratio);
    }
  }

  // Get mouse position in the 'container' taking into account the pixel ratio
  SpectraViewer.prototype.mouse = function(container) {
    return d3.mouse(container).map(function(p) { return JSV.pixel(p); });
  }

  // Translates the spectra for the supplied axis.
  // - axis: either 'x' or 'y'
  // - translation: number of pixels to move the specta  on the axis
  SpectraViewer.prototype.translate_axis = function(axis, translation) {
    // Calculate new domain based on translation
    var domain = this.scale[axis].range().map(function(r) { return r - JSV.pixel(translation); }).map(this.scale[axis].invert);
    this.set_domain(axis, domain);
  }

  // http://bl.ocks.org/mbostock/5731979
  // Move from the current x,y domain to the supplied x/y domain using a transition.
  // - domains: 2D array containing the min/max for the X/Y domains:
  //   [ [Xmin, Xmax], [Ymin, Ymax] ]
  // - duration: time of move transition in milliseconds
  SpectraViewer.prototype.move_to = function(domains, duration) {
    var sv = this;
    var duration = duration || 500;
    // Change Ymin to boundaries Ymin if the y axis is locked
    if (this.axis_y_lock !== false) {
        domains[1][0] = this.axis_y_lock_value(domains[1]);
    }
    // Flatten 2D domain arrays so they have the format: [Xmin, Xmax, Ymin, Ymax]
    var end_domains = [];
    var start_domains = []
    end_domains = end_domains.concat.apply(end_domains, domains);
    start_domains = start_domains.concat
      .apply(start_domains, [sv.scale.x.domain(), sv.scale.y.domain()]);

    d3.select(this.canvas).transition()
      .duration(duration)
      .tween('move', function() {
        var interm_domains = d3.interpolateArray(start_domains, end_domains)
        return function(t) {
          sv.set_domain('x', [interm_domains(t)[0], interm_domains(t)[1]]);
          sv.set_domain('y', [interm_domains(t)[2], interm_domains(t)[3]]);
          sv.fast_draw();
        }
      }).each('end', function() { sv.full_draw(); });
    // Reset zoom axis
    sv.zoom_axis = '';
  }

  // Sets the domain (mininum and maximum value) for the supplied axis
  // after adjusting the domain to make sure it is within the boundaries of
  // the plot. Also adjusts the domain based on max_zoom.
  // - axis: either 'x' or 'y'
  // - domain: array with 2 elements, the min and max value for the domain
  SpectraViewer.prototype.set_domain = function(axis, domain) {
    var scale = this.scale;
    var boundary = this.boundary;


    // Determine boundary domain (difference between min and max values on the axis)
    var boundary_diff = Math.abs(boundary[axis].domain()[1] - boundary[axis].domain()[0]);
    // Determine visible domain (difference between min and max values on the axis)
    var domain_diff = Math.abs(domain[1] - domain[0]);

    // Optionally lock minimum domain value to minimum boundary value
    if ( (axis == 'y') && (this['axis_y_lock'] !== false) ) {
      domain[0] = this.axis_y_lock_value(domain);
      domain[1] = domain[0] + domain_diff;
    }

    // Changing the domain may change the zoom level
    // Check that the zoom is not above the max
    var new_zoom = boundary_diff / domain_diff;
    if (new_zoom > this.zoom_max) {
      new_zoom = this.zoom_max;
      var old_domain_diff = domain_diff;
      domain_diff = boundary_diff / new_zoom;
      // Center the domain
      var center = (domain[0] + domain[1]) / 2
      domain[0] = center - (domain_diff / 2);
      domain[1] = domain[0] + domain_diff;
      if (this.debug) {
        this.debug_data.zoom['center-' + axis] = JSV.round(center);
        this.debug_data.zoom['domain-diff-' + axis] = JSV.round(domain_diff);
      }
    }
    // Check that the zoom is not below 1
    if (new_zoom < 1) {
      new_zoom = 1;
      domain_diff = boundary_diff / new_zoom;
      domain[0] = boundary[axis].min();
      domain[1] = boundary[axis].max();
    }
    if (this['zoom_' + axis] != new_zoom) {
      this['zoom_' + axis] = new_zoom;
      this.trigger('zoom');
    }

    // Check that domain is within the plot boundaries
    if (domain[0] < boundary[axis].domain()[0]) {
      domain[0] = boundary[axis].domain()[0];
      domain[1] = domain[0] + domain_diff;
    } else  if (domain[1] > boundary[axis].domain()[1]) {
      domain[1] = boundary[axis].domain()[1];
      domain[0] = domain[1] - domain_diff;
    }

    // Set domain
    this.scale[axis].domain(domain);
    this.trigger('domain-change');
  }

  SpectraViewer.prototype.axis_y_lock_value = function(domain) {
    if (this.axis_y_lock === true) {
      return this.boundary.y.domain()[0];
    } else {
      var domain_diff = Math.abs(domain[1] - domain[0]);
      return 0 - domain_diff * this.axis_y_lock;
    }
  }

  SpectraViewer.prototype.zoom_out_completely = function() {
    this.move_to([this.boundary.x.domain(), this.boundary.y.domain()])
    this.scale.x.domain(this.boundary.x.domain());
    this.scale.y.domain(this.boundary.y.domain());
    this.zoom_x = 1;
    this.zoom_y = 1;
  }


  /**
   * Adds a new spectrum to the viewer using the data provided. The _data_
   * can be a previously generated [Spectrum](Spectrum.js.html) or the data that will be
   * passed on to create a new [Spectrum](Spectrum.js.html).
   * Example Data:
   * ```javascript
   * data = {
   *   // ID to identify the spectrum. Defaults to spectrum_1, spectrum2, etc.
   *   id: 'fit',
   *   // Name displayed in legend. Defaults to the id.
   *   name: 'My Spectrum',
   *   // One of the following must be provided to create a Spectrum.
   *   // If more than one is provided, only one will be used and they are prioritized in
   *   // order shown. Details on each data type can be found below.
   *   compounds: compound_data,
   *   peak_list: peak_data,
   *   xy_line: xy_data,
   *   // Optional display options
   *   display: {color: 'blue', lineWidth: 3},
   *   // Optional meta data. Accessible through the spectrum's meta property.
   *   meta: {extra_info: 'This spectrum is really good!'},
   *   // Optional annotation data. Accessible through the spectrum's labels property
   *   // See below for details
   *   labels: labels
   * }
   *
   * // Describes one or more compounds
   * compound_data = [
   *   {
   *     id: 'HMDB01659',
   *     name: 'Acetone',
   *     concentration: '50',
   *     clusters: [
   *       {
   *         peaks: [
   *           { center: 2.22, amplitude: 0.6, width: 0.005 }
   *       }
   *     ]
   *   }
   * ]
   *
   * // A simple peak list
   * peak_data = [
   *   { center: 3.2, amplitude: 1.1, width: 0.05 },
   *   { center: 5.1, amplitude: 1.3, width: 0.05 },
   *   { center: 4.5, amplitude: 0.6, width: 0.04 }
   * ]
   *
   * // xy_data can be one of two formats:
   * // An object with x and y arrays [Better]
   * xy_data = {
   *   x: [1, 2, 3],
   *   y: [0.5, 0.4, 0.3]
   * }
   *
   * // To reduce JSON download size, previously loaded spectrum data can be used for the x array.
   * // In this case a spectrum with the id 'my_spectrum' should already be loaded in the viewer.
   * xy_data = {
   *   x: 'my_spectrum',
   *   y: [0.5, 0.4, 0.3]
   * }
   *
   *
   * // An array of x and y points [Deprecated]
   * xy_data = [
   *   {x: 1, y: 0.5}, {x: 2, y: 0.4}, {x: 3, y: 0.3}
   * ]
   *
   * // Annotation labels: array of label objects
   * labels = [
   *   { x: 1,
   *     y: 1,
   *     text: 'my_label',
   *     display: { vertical: true, font_size: 12 }, // optional
   *     meta: { something_useful: 'value' } // optional
   *   }
   * ]
   * ```
   * @param {Object} data Data required to create a new [Spectrum](Spectrum.js.html).
   *   Data can also be a Spectrum object, in which case, _display_ and _meta_ are ignored.
   * @param {Object} display Display options as described in [SVPath](SVPath.js.html)
   * @param {Object} meta User-defined properties
   */
  SpectraViewer.prototype.add_spectrum = function(data, display, meta) {
    var start_time = new Date().getTime();

    //FIXME: Should be done elsewhere and in a better way
    // Determine max number of points from current spectra
    // This is used to draw spectra from peak data
    var max_points = d3.max(this.spectra().map(function(spectrum) { return spectrum.xy_data.length()}));
    if (!max_points) max_points = 40000;
    this.points_per_ppm = max_points / (this.boundary.x.domain()[1] - this.boundary.x.domain()[0]);

    data.tolerance = JSV.default_for(data.tolerance, this.simplify_tolerance);
    data.number_of_points = JSV.default_for(data.number_of_points, max_points);
    data.min_x = JSV.default_for(data.min_x, this.boundary.x.domain()[0]);
    data.max_x = JSV.default_for(data.max_x, this.boundary.x.domain()[1]);
    var current_ids = this.all_spectra().map(function(spectrum) { return spectrum.name; } );
    data.id = JSV.default_for(data.id, JSV.unique_id('Spectrum_', 1, current_ids));

    // If a spectrum ID is provided for x data, use that spectrums x data
    if ( data.xy_line && (typeof data.xy_line.x == 'string') ) {
      var x_id = data.xy_line.x;
      if ( this.spectra(x_id) ) {
        data.xy_line.x = this.spectra(x_id).xy_data.x;
      } else {
        throw new Error("There is no spectrum with the id provided by '" + data.id + "' xy_line.x:" + x_id);
      }
    }

    var spectrum = (data.toString() == 'Spectrum') ? data : new JSV.Spectrum(data, display, meta);
    spectrum._sv = this;

    this._spectra.push(spectrum);
    this.boundary.update(spectrum.xy_data);
    this.scale.update(spectrum.xy_data);
    this.zoombox.update();
    this.legend.update();
    if (this.debug) {
      this.debug_data.time[data.id + '~add'] = JSV.elapsed_time(start_time);
      this.debug_data.data[data.id + '~noise'] = spectrum.noise;
      this.debug_data.data[data.id + '~points-all'] = spectrum.xy_data.length();
      this.debug_data.data[data.id + '~points-simple'] = spectrum.simple_xy_data.length();
    }
    this.draw();
  }

  /**
   * Add the processed spectra and the compound fit generated by Bayesil
   * @param {Object} data Bayesil results.json data
   */
  SpectraViewer.prototype.add_bayesil_data = function(data) {
    this.add_spectrum( { id: 'Spectrum', xy_line: data.spectrum_xy } );
    this.add_spectrum( JSV.convert_bayesil_data(data) );
  }

  SpectraViewer.prototype.replace_bayesil_data = function(data, keep_domains) {
    var sv = this;
    var saved_domains = [sv.scale.x.domain(), sv.scale.y.domain()];
    var viewer_was_not_empty = sv.spectra().length > 0;
    sv.remove_all_spectra();
    sv.add_bayesil_data(data);
    // if (json_parsed.fit_xy) {
    //   sv.add_spectrum({ id: 'FIT', xy_line: json_parsed.fit_xy}, {color: 'orange'});
    // }
    if (viewer_was_not_empty && keep_domains)  {
      sv.set_domain('x', saved_domains[0]);
      sv.set_domain('y', saved_domains[1]);
    }
    sv.draw();
  }

  SpectraViewer.prototype.remove_spectra = function(id) {
    this._spectra = new JSV.SVSet( this._spectra.filter(function(spectrum) { return spectrum.id != id; }) );
    this.zoombox.remove_spectra(id);
    this.legend.update();
  }

  SpectraViewer.prototype.remove_all_spectra = function() {
    var sv = this;
    sv.selection.clear();
    this.all_spectra().forEach(function(spectrum) { sv.remove_spectra(spectrum.id) });
    sv.reset_boundaries();
  }

  SpectraViewer.prototype.reset_boundaries = function() {
    var sv = this;
    sv.selection.clear();
    sv.boundary.initialized = false
    sv.scale.initialized = false
    if (sv.min_boundaries) {
      sv.boundary.update(sv.min_boundaries);
      sv.scale.update(sv.min_boundaries);
    } else {
      sv.boundary.update({ x: [0, 1], y: [0, 1] })
      sv.scale.update({ x: [0, 1], y: [0, 1] })
    }
  }

  // If no term is given, only active spectra are returned
  // Otherwise all spectra will be search with the term
  SpectraViewer.prototype.spectra = function(term) {
    if (term) return this._spectra.get(term);
    var active_spectra = new JSV.SVSet(this._spectra.filter(function(s) { return s.active; }));
    return active_spectra.get(term);
  }

  SpectraViewer.prototype.all_spectra = function(term) {
    return this._spectra.get(term);
  }

  SpectraViewer.prototype.compounds = function(term) {
    var compounds = new JSV.SVSet();
    for (var i=0, len=this._spectra.length; i < len; i++) {
      compounds.merge(this._spectra[i].compounds());
    }
    return compounds.get(term);
  }

  SpectraViewer.prototype.clusters = function(term) {
    var clusters = new JSV.SVSet();
    for (var i=0, len=this._spectra.length; i < len; i++) {
      clusters.merge(this._spectra[i].clusters());
    }
    return clusters.get(term);
  }

  SpectraViewer.prototype.peaks = function(term) {
    var peaks = new JSV.SVSet();
    for (var i=0, len=this._spectra.length; i < len; i++) {
      peaks.merge(this._spectra[i].peaks());
    }
    return peaks.get(term);
  }

  // Clear the viewer canvas
  SpectraViewer.prototype.clear = function() {
    this.context.clearRect(0, 0, JSV.pixel(this.width), JSV.pixel(this.height));
  }

  SpectraViewer.prototype.draw = function(fast, calculated, pixel_skip) {
    var start_time = new Date().getTime();
    var sv = this;
    var fast = fast || false
    var context = this.context;
    var scale = this.scale;
    this.clear();
    // Draw Grid lines
    if (this.axis_y_grid) this.draw_grid_lines('y');
    if (this.axis_x_grid) this.draw_grid_lines('x');
    this.spectra().forEach(function(spectrum) {
      // Draw compounds
      spectrum.compounds().draw(context, scale, fast, calculated, pixel_skip);
      // Draw clusters
      spectrum.clusters().draw(context, scale, fast, calculated, pixel_skip);
      // Draw peaks
      spectrum.peaks().draw(context, scale, fast, true, pixel_skip);
      // Draw spectra
      spectrum.draw(context, scale, fast, calculated, pixel_skip);
    });
    // Draw selection
    this.selection.draw(context, scale, fast, calculated, pixel_skip);
    this.zoombox.set_zoom_area(this.scale);
    // Draw labels
    this.annotation.draw();
    if (this.legend_show) this.legend.draw();
    this.draw_axes();
    this.selection.draw_bounds(context, scale);
    if (this.debug) {
      this.debug_data.time['draw'] = JSV.elapsed_time(start_time);
      this.draw_debug(this.legend.bottom());
    }
  }

  // Draw using the full data (not simplified). 
  // If zoomed in enough so that the number of points in the spectra
  // is less than the number of pixels in the viewer then then lines
  // will be calculated if possible.
  SpectraViewer.prototype.full_draw = function() {
    var ppm_range = this.scale.x.domain()[1] - this.scale.x.domain()[0];
    var point_range = this.points_per_ppm * ppm_range;
    var pixel_range = Math.abs(this.scale.x.range()[1] - this.scale.x.range()[0]);
    ( (pixel_range / point_range) > 3 ) ? this.draw(false, true, 1) : this.draw(false, false);
  }

  // Draw by calculating the line shapes, using every visible pixel
  SpectraViewer.prototype.calc_draw = function() {
    this.draw(false, true, 1);
  }

  // Draw by calculating the line shapes, skipping several pixels
  SpectraViewer.prototype.fast_calc_draw = function() {
    // this.draw(true, true, 2);
    this.draw(false, true, 2);
  }

  // Draw using simplified data if available. When zoomed in enough
  // the full data will be used.
  SpectraViewer.prototype.fast_draw = function() {
    // Turn off fast draw when zoomed in 10x or more
    // TODO: determine at what level fast draw should be turned off based on the spectra length
    //       - this would be best if done at the spectrum level
    this.zoom_x < 10 ? this.draw(true) : this.draw();
    // this.draw(true);
  }

  // Flash a message on the center of the viewer
  SpectraViewer.prototype.flash = function(msg) {
    this.context.font = this.adjust_font(1.5);
    this.context.textAlign = 'center';
    this.context.textBaseline = 'center';
    var x = JSV.pixel(this.width) / 2
    var y = JSV.pixel(this.height) / 2
    this.context.fillText(msg, x, y);
  }

  // Draws any information in 'data' onto the left side of the viewer
  SpectraViewer.prototype.draw_debug = function(y) {
    if (!this.debug) return;
    var context = this.context;
    var data = this.debug_data;

    context.font = this.adjust_font(1, 'monospace');
    context.fillStyle = 'black';
    var line_height = JSV.pixel(18);
    var y =  y || 0;
    if (this.axis_x_reverse) {
      var x = JSV.pixel(10);
      context.textAlign = 'left';
    } else {
      var x = JSV.pixel(this.width - 10);
      context.textAlign = 'right';
    }
    var section_keys = this.debug === true ? Object.keys(data) : this.debug;
    var i = 0;
    section_keys.forEach(function(section_key) {
      var data_keys = Object.keys(data[section_key]);
      data_keys.forEach(function(data_key) {
        context.fillText((section_key + '|' + data_key + ': ' + data[section_key][data_key]), x, y + (line_height * i));
        i += 1;
      });
    })
  }

  // Returns a canvas font string adjusted to the size of the canvas
  SpectraViewer.prototype.adjust_font = function(font_factor, font_family, font_style) {
    font_factor = font_factor || 1;
    font_family = font_family || 'Sans-Serif';
    font_style  = font_style  || '';
    // ratio of default font size over default canvas width
    var ratio = 9 / 1400 * font_factor;
    var fontsize = ratio * JSV.pixel(this.width) + JSV.pixel(5);
    return font_style + ' ' + fontsize + 'pt ' + font_family;
  }

  SpectraViewer.prototype.draw_axes = function() {
    var scale = this.scale;
    this.context.strokeStyle = 'black'; // axes color
    this.context.lineWidth = 1; // axes color
    this.context.setLineDash([1,0]);
    // Clear plot graphics from the X axis area
    var y_gutter = JSV.pixel(this.axis_y_gutter);
    var x_gutter = JSV.pixel(this.axis_x_gutter);
    // Clear plot graphics from the X axis area
    this.context.clearRect(scale.x.range_min(), scale.y.range_max(), scale.x.range_max() + y_gutter, x_gutter);
    // Clear plot graphics from the Y axis area
    var x = this.axis_x_reverse ? scale.x.range_max() : 0;
    this.context.clearRect(x, scale.y.range_min(), y_gutter, scale.y.range_max() + x_gutter);
    // Draw
    this.context.fillStyle = 'black';
    if (this.axis_y_show) this.draw_y_axis();
    if (this.axis_x_show) this.draw_x_axis();
  }

  SpectraViewer.prototype.draw_x_axis = function() {
    // Create a variable so we can access the chart in the tick drawing function
    var self = this;
    var tick_length = JSV.pixel(this.tick_length);
    var context = this.context;
    var scale = this.scale;

    // Draw axis line
    context.beginPath();
    context.moveTo(scale.x.range()[1], scale.y.range()[0]);
    context.lineTo(scale.x.range()[0], scale.y.range()[0]);
    context.stroke();
    // Set up text
    context.textAlign = 'center';
    context.textBaseline = 'top';
    context.font = this.font;
    // Draw ticks and text
    scale.x.ticks(this.tick_count).forEach(function(tick_x) {
      context.beginPath();
      context.moveTo(scale.x(tick_x), scale.y.range()[0]);
      context.lineTo(scale.x(tick_x), scale.y.range()[0] + tick_length);
      context.stroke();
      // context.fillText(d3.round(tick_x, self.tick_precision), scale.x(tick_x), scale.y.range()[0] + tick_length);
      rounded_tick_x = d3.round(tick_x, self.tick_precision);
      context.fillText(self.axis_x_tick_format(rounded_tick_x), scale.x(tick_x), scale.y.range()[0] + tick_length);
    });
    // Draw x label
    if (this.axis_x_title) {
      context.font = this.axis_title_font;
      var label_x = scale.x.range_diff() / 2;
      var label_y = scale.y.range()[0] + (3 * tick_length)
      context.fillText(this.axis_x_title, label_x, label_y)
    }
  }

  SpectraViewer.prototype.draw_y_axis = function() {
    var sv = this;
    var context = this.context;
    var scale = this.scale;
    var padding = 5;
    var direction = this.axis_x_reverse ? 1 : -1
    var tick_length = direction * JSV.pixel(this.tick_length);
    var text_x = scale.x.range()[0] +  ( direction * JSV.pixel(this.tick_length + padding) );
    // Draw axis line
    context.beginPath();
    context.moveTo(scale.x.range()[0], scale.y.range()[1]);
    context.lineTo(scale.x.range()[0], scale.y.range()[0]);
    context.stroke();
    // Set up text
    context.textAlign = this.axis_x_reverse ? 'left' : 'right';
    context.textBaseline = 'middle';
    context.font = this.font;
    var max_label_width = 0;
    // Determine number of decimal places
    // var decimal_places = d3.max( scale.y.ticks(this.tick_count).map(function(tick_y) { return JSV.decimalPlaces(tick_y); }) );

    // Draw ticks and text
    scale.y.ticks(this.tick_count).forEach(function(tick_y) {
      context.beginPath();
      context.moveTo(scale.x.range()[0], scale.y(tick_y));
      context.lineTo(scale.x.range()[0] + tick_length, scale.y(tick_y));
      context.stroke();
      // context.fillText(tick_y.toFixed(decimal_places), text_x, scale.y(tick_y));
      context.fillText(sv.axis_y_tick_format(tick_y), text_x, scale.y(tick_y));
      if (sv.axis_y_title) {
        var label_width = Number(context.measureText(tick_y).width);
        if (label_width > max_label_width) max_label_width = label_width;
      }
    });
    // Draw y label
    if (this.axis_y_title) {
      var margin = JSV.pixel(4);
      var gutter = JSV.pixel(this.axis_y_gutter);
      var font_height = /(\d+\.?\d*)pt/.exec(this.axis_title_font)[1];
      // Width of text and ticks filling up the gutter
      var draw_width = max_label_width + Math.abs(tick_length) + Number(font_height) + margin + padding;
      if ( draw_width < gutter) {
        context.save();
        context.textAlign = 'center';
        context.textBaseline = 'middle';
        context.font = this.axis_title_font;
        // Determine center point of label
        var label_x = scale.x.range()[0] + ( ( gutter - margin - Number(font_height)/2 ) * direction );
        var label_y = scale.y.range()[0]/2;
        // Move to center
        context.translate(label_x, label_y);
        // Rotate Label
        context.rotate(direction * Math.PI / 2);
        // Move back to origin
        context.translate(-label_x, -label_y);
        // Draw label
        context.fillText(this.axis_y_title, label_x, label_y);
        context.restore();
      }
    }
  }

  SpectraViewer.prototype.draw_grid_lines = function(axis) {
    var sv = this;
    var context = this.context;
    var orig_context = context;
    var scale = this.scale;
    scale[axis].ticks(this.tick_count).forEach(function(tick) {
      context.beginPath();
      if (axis == 'y') {
        context.moveTo(scale.x.range()[0], scale.y(tick));
        context.lineTo(scale.x.range()[1], scale.y(tick));
      } else if (axis == 'x') {
        context.moveTo(scale.x(tick), scale.y.range()[0]);
        context.lineTo(scale.x(tick), scale.y.range()[1]);
      }
      context.strokeStyle = 'rgb(210,210,210)'; // axes color
      context.lineWidth = JSV.pixel(0.5);
      // context.setLineDash([2,2]);
      context.stroke();
    });
    this.context = orig_context;
  }

  SpectraViewer.prototype.draw_y_axis2 = function() {
    var sv = this;
    var context = this.context;
    var scale = this.scale;
    var padding = 5;

    var min_x = d3.min(scale.x.range());
    var max_x = d3.max(scale.x.range());
    var min_y = d3.min(scale.y.range());
    var max_y = d3.max(scale.y.range());
    var line_x, text_x, tick_length;
    if (this.axis_y_left) {
      line_x = min_x;
      text_x = line_x - JSV.pixel(this.tick_length + padding);
      context.textAlign = 'right';
      tick_length = -JSV.pixel(this.tick_length);
    } else {
      line_x = max_x;
      text_x = line_x + JSV.pixel(this.tick_length + padding);
      context.textAlign = 'left';
      tick_length = JSV.pixel(this.tick_length);
    }

    // Draw axis line
    context.beginPath();
    context.moveTo(line_x, min_y);
    context.lineTo(line_x, max_y);
    context.stroke();
    // Set up text
    context.textBaseline = 'middle';
    context.font = this.font;
    var max_label_width = 0;
    // Draw ticks and text
    scale.y.ticks(this.tick_count).forEach(function(tick_y) {
      context.beginPath();
      context.moveTo(line_x, scale.y(tick_y));
      context.lineTo(line_x + tick_length, scale.y(tick_y));
      context.stroke();
      context.fillText(tick_y, text_x, scale.y(tick_y));
      if (sv.axis_y_title) {
        var label_width = Number(context.measureText(tick_y).width);
        if (label_width > max_label_width) max_label_width = label_width;
      }
    });
    // Draw y label
    if (this.axis_y_title) {
      var margin = JSV.pixel(4);
      var gutter = JSV.pixel(this.axis_y_gutter);
      var font_height = /(\d+)pt/.exec(this.axis_title_font)[1];
      // Width of text and ticks filling up the gutter
      var draw_width = max_label_width + tick_length + Number(font_height) + margin;
      if ( draw_width < gutter) {
        context.save();
        context.textAlign = 'center';
        context.textBaseline = 'hanging';
        context.font = this.axis_title_font;
        context.rotate(Math.PI / 2);
        var label_y = -scale.x.range()[0] - gutter + margin;
        var label_x = scale.y.range()[0]/2;
        context.fillText(this.axis_y_title, label_x, label_y);
        context.restore();
      }
    }
  }

  // Returns a 2D array of the x and y domains [[Xmin, Xmax], [Ymin, Ymax]]
  // peaks: an array of peak objects
  // exact: -true: the exact boundaries of the peaks
  //               good for drawing a selection line
  //        -false: add a little to dimensions.
  //                Best for domains to zoom in on.
  SpectraViewer.prototype.get_peak_domains = function(peaks, exact) {
    var margin_percentage = 0.1;
    var min_peak = peaks[0];
    var max_peak = peaks[0];
    var min, max;
    for (var i = 1; i < peaks.length; i++) {
      var peak = peaks[i];
      if (peak.center < min_peak.center) min_peak = peak;
      if (peak.center > max_peak.center) max_peak = peak;
    }
    // Calculate sum of peaks to determine top
    var xy_for_peaks = JSV.xy_from_peaks(peaks, 1000, min_peak.center, max_peak.center);
    // var top = d3.max(xy_for_peaks.map(function(xy) { return xy.y; }));
    var top = d3.max(xy_for_peaks.y);
    if (exact) {
      min = min_peak.center - (min_peak.width / 2);
      max = max_peak.center + (max_peak.width / 2);
    } else {
      min = min_peak.center - (min_peak.width);
      max = max_peak.center + (max_peak.width);
      var margin = (max - min) * margin_percentage;
      min -= margin;
      max += margin;
      top += (top * margin_percentage);
    }
    return [[min, max], [0, top]];
  }

  SpectraViewer.prototype.move_to_compound = function(spectrum_id, compound_id, duration) {
    var spectrum = this.spectra(spectrum_id);
    var compound = spectrum && spectrum.compounds(compound_id);
    this.move_to_peaks(compound.peaks(), duration);
  }

  SpectraViewer.prototype.move_to_peaks = function(peaks, duration, scale) {
    scale = scale || 1;
    var domains = this.get_peak_domains(peaks);
    domains[1][1] = domains[1][1] * scale;
    var domain_diff = Math.abs(domains[0][1] - domains[0][0]);
    domains[0][0] = domains[0][0] - (domain_diff - domain_diff / scale);
    domains[0][1] = domains[0][1] + (domain_diff - domain_diff / scale);
    this.move_to(domains, duration);
  }

  // Extent is a 2D array [[x0, y0], [x1, y1]]
  // - x0 and y0 are the lower bounds of the extent
  // - x1 and y1 are the upper bounds of the extent
  SpectraViewer.prototype.set_domain_to_brush_extent = function(extent, use_pixel_ratio) {
    var domains = this.domains_from_brush_extent(extent, use_pixel_ratio);
    this.set_domain('x', domains[0]);
    this.set_domain('y', domains[1]);
  }

  SpectraViewer.prototype.domains_from_brush_extent = function(extent, use_pixel_ratio) {
    var x_domain = [extent[0][0], extent[1][0]];
    var y_domain = [extent[0][1], extent[1][1]];
    if (use_pixel_ratio) {
      scale = this.scale;
      x_domain = x_domain.map(function(d) { return JSV.pixel(scale.x(d)); }).map(scale.x.invert);
      y_domain = y_domain.map(function(d) { return JSV.pixel(scale.y(d)); }).map(scale.y.invert);
    }
    return [x_domain, y_domain];
  }

  SpectraViewer.prototype.elements = function(element_type, parent, possible_elements, visible_only) {
    var sv = this;
    var elements = possible_elements;
    if (!possible_elements) {
      parent = JSV.default_for(parent, sv);
      if (typeof parent == 'string') {
        parent = sv.spectra(parent);
      }
      if (!parent) {
        elements = new JSV.SVSet();
      } else if (element_type == 'peak') {
        elements = parent.peaks();
      } else if (element_type == 'cluster') {
        elements = parent.clusters();
      } else if (element_type == 'compound') {
        elements = parent.compounds();
      } else if (element_type == 'spectrum') {
        elements = parent.spectra();
      } else {
        elements = new JSV.SVSet();
      }
    }
    if (visible_only) {
      elements = new JSV.SVSet(elements.filter(function(e) { return e.visible; }));
    }
    return elements;
  }

  SpectraViewer.prototype.find_element_mouse_over = function(element_type, spectrum, possible_elements, visible_only) {
    var sv = this;
    var found_element;
    elements = sv.elements(element_type, spectrum, possible_elements, visible_only);
    // Calculate y value for each element at the mouse x position
    elements.forEach(function(element) {
      element.y_for_mouse_x = JSV.sum_of_peaks(sv.mx, element.peaks());
    });
    // Sort elements by y value
    elements.order_by('y_for_mouse_x');
    // Find first element
    for (var i = 0; i < elements.length; i++) {
      if (sv.my <= elements[i].y_for_mouse_x && sv.my >= 0) {
        found_element = elements[i];
        break;
      }
    }
    return found_element;
  }

  SpectraViewer.prototype.max_y_in_range = function(x_min, x_max) {


  }


  SpectraViewer.prototype.image = function(width, height) {
    width = width || this.width;
    height = height || this.height;

    // Save current settings
    var orig_context = this.context;
    var zoombox_visible = this.zoombox.visible;

    // Generate new context and scales
    var temp_canvas = d3.select('body').append('canvas')
      .attr({ width: width, height: height }).node()

    JSV.scale_resolution(temp_canvas, JSV.pixel_ratio);
    this.context = temp_canvas.getContext('2d');

    var scaled_height = JSV.pixel(height - this.axis_x_gutter);
    this.scale.x.range(this.x_range(width));
    this.scale.y.range([scaled_height, 0]);

    this.zoombox.visible = false;

    // Generate image
    this.full_draw();
    image = temp_canvas.toDataURL();

    // Restore original settings
    this.scale.x.range(this.x_range());
    this.scale.y.range([this.scaled_height, 0]);
    this.context = orig_context;
    this.zoombox.visible = zoombox_visible;

    // Delete temp canvas
    d3.select(temp_canvas).remove();

    return image;
  }

  SpectraViewer.prototype.initial_settings = function() {
    var self = this;
    this.settings = new JSV.SVSettings(this, {
      title: 'JSV Settings',
      width: '250',
      height: '210',
      settings: [ {
          label: 'Grid lines [y-axis]',
          property: 'axis_y_grid',
          type: 'boolean'
        }, {
          label: 'Grid lines [x-axis]',
          property: 'axis_x_grid',
          type: 'boolean'
        }, {
          label: 'Show Zoombox',
          property: function(value) {
            if (arguments.length == 0) return self.zoombox.visible;
            self.zoombox.visible = value;
          },
          type: 'boolean'
        }, {
          label: 'Show Legend',
          property: 'legend_show',
          type: 'boolean'
        }
      ]
    });
  }


  JSV.SpectraViewer = SpectraViewer;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Initializing Zooming, Dragging, Brushing, Drag-n-Drop, AdjustFit
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * Initialize Spectra Viewer Dragging.
   */
  JSV.SpectraViewer.prototype.initialize_dragging = function() {
    var sv = this;
    // var current_selected_elements;
    sv.drag_event = false;
    sv.drag_behavior = d3.behavior.drag()
      .on('dragstart', dragstart)
      .on('drag',      dragging)
      .on('dragend',   dragend);
    sv.svg.call(sv.drag_behavior);

    function dragstart() {
      sv.drag_event = true;
      d3.event.sourceEvent.preventDefault(); // Prevent text cursor
      sv.svg.style('cursor', 'all-scroll');
      // Store current selection to restore it if dragging does occur
      // current_selected_elements = sv.selection.elements();
      sv.trigger('drag-start');
    }

    function dragging() {
      var start_time = new Date().getTime();
      // Restore selected peaks
      // if (sv.selection.empty()) sv.selection._elements = current_selected_elements;
      sv.translate_axis('x', d3.event.dx);
      sv.translate_axis('y', d3.event.dy);
      sv.trigger('drag');
      sv.fast_draw();

      // DEBUG INFO
      if (sv.debug) {
        console.log('dragging');
        sv.debug_data.time['drag'] = JSV.elapsed_time(start_time);
        sv.debug_data.drag['dX'] = JSV.round(d3.event.dx);
        sv.debug_data.drag['dY'] = JSV.round(d3.event.dy);
        sv.debug_data.drag['zX'] = JSV.round(sv.zoom_x);
        sv.debug_data.drag['zY'] = JSV.round(sv.zoom_y);
      }
    }

    function dragend() {
      sv.drag_event = false;
      sv.trigger('drag-end');
      sv.full_draw();
    }
  }

  /**
   * Initialize Spectra Viewer Zooming.
   */
  JSV.SpectraViewer.prototype.initialize_zooming = function() {
    var sv = this;
    sv.zoom_behavior = d3.behavior.zoom()
      .x(d3.scale.linear())
      .scaleExtent([1, sv.zoom_max])
      .on('zoomstart', zoomstart)
      .on('zoom',      zooming)
      .on('zoomend',   zoomend);
    sv.svg.call(sv.zoom_behavior)
      .on('dblclick.zoom', null); // Remove double click zoom behaviour
    // Set starting zoom levels and axis
    sv.zoom_x = 1;
    sv.zoom_y = 1;
    sv.zoom_axis = 'x';
    // Possigle keys: 'altKey', 'shiftKey', 'ctrlKey', 'metaKey'
    sv.zoom_y_key = 'shiftKey';

    function zoomstart() {
      if (sv.drag_event) return; // Spectra dragging takes precedence over zooming
      // d3.event.sourceEvent.preventDefault(); // Try to prevent browser defaults
      var zoom_y_key_down = d3.event.sourceEvent[sv.zoom_y_key];
      sv.set_zoom_axis(zoom_y_key_down);
      sv.set_zoom_cursor(zoom_y_key_down);
      sv.trigger('zoom-start');
    }

    function zooming() {
      if (sv.drag_event) return; // Spectra dragging takes precedence over zooming
      var start_time = new Date().getTime();

      // Set up which axis to zoom
      sv.set_zoom_axis(d3.event.sourceEvent[sv.zoom_y_key]);

      // Scale axes based on current level
      sv.scale_axis(sv.zoom_axis, sv.zoom_behavior.scale())

      // sv.trigger('zoom');
      sv.fast_draw();

      // DEBUG INFO
      if (sv.debug) {
        console.log('zooming');
        sv.debug_data.time['zoom'] = JSV.elapsed_time(start_time);
        sv.debug_data.zoom['zX'] = JSV.round(sv.zoom_x);
        sv.debug_data.zoom['zY'] = JSV.round(sv.zoom_y);
      }
    }

    function zoomend() {
      if (sv.drag_event) return; // Spectra dragging takes precedence over zooming
      sv.svg.style('cursor', 'all-scroll');
      sv.trigger('zoom-end');
      sv.full_draw();
    }
  }

  /**
   * Initialize Spectra Viewer Brushing.
   * Note: that while zoom brushing the Alt and space key have additional function.
   */
  JSV.SpectraViewer.prototype.initialize_brushing = function() {
    var sv = this;
    sv.brush = d3.svg.brush()
      .x(sv.scale.x)
      .y(sv.scale.y)
      .on('brushstart', brushstart)
      .on('brush',      brushing)
      .on('brushend',   brushend);

    sv.brush_g = sv.svg.append("g")
      .attr("class", "zoom-brush active-brush")
      .call(sv.brush);

    // Remove default brush cursor styles
    sv.svg.selectAll('.zoom-brush rect.background').style('cursor', '');
    sv.svg.selectAll('.resize').style('cursor', '');
    // Set key to use to activate the brush zoom
    sv.zoom_brush_key = 'shiftKey';

    function brushstart() {
      if (d3.event.sourceEvent[sv.zoom_brush_key]) {
        d3.event.sourceEvent.stopPropagation(); // silence other listeners
        d3.event.sourceEvent.preventDefault(); // Prevent text cursor
        sv.svg.selectAll('.zoom-brush').attr("class", "zoom-brush active-brush");
        sv.brush_event = true;
        sv.svg.style('cursor', 'crosshair');
      } else {
        sv.svg.selectAll('.zoom-brush').attr("class", "zoom-brush empty-brush");
        sv.brush_event = false;
      }
      sv.svg.selectAll('.zoom-brush .background').attr('height', '400')
    }

    function brushing() {

      var ex = sv.brush.extent();
      var x1 = ex[0][0];
      var x2 = ex[1][0];
      var y_min = sv.scale.y.invert(sv.scale.y(sv.scale.y.domain()[0]) / JSV.pixel_ratio);
      var x_min = sv.scale.x.invert(sv.scale.x(sv.scale.x.domain()[0]) / JSV.pixel_ratio);

      if (sv.axis_y_lock !== false) {
        // Set y1 to the min y domain, but take into account the size of brush background due to the pixel ratio
        var y1 = y_min
        var mouse_y = sv.scale.y.invert(d3.mouse(sv.svg.node())[1]);
        var max_y = sv.scale.y.domain()[1];
        // var y2 = Math.min(mouse_y, max_y);
        // var y2 = Math.max(y1, y2);
        var y2 = mouse_y > max_y ? max_y : mouse_y;
        var y2 = y2 < y1 ? y1 : y2;
      } else {
        // Check that brush is not below y-axis
        var y1 = ex[0][1];
        y1 = y1 < y_min ? y_min : y1;
        var y2 = ex[1][1];
        y2 = y2 < y_min ? y_min : y2;
      }

      // Check that brush is not below x-axis
      x1 = x1 < x_min ? x_min : x1;
      x2 = x2 < x_min ? x_min : x2;

      // Set brush and redraw
      sv.brush.extent([[x1, y1], [x2, y2]]);
      sv.svg.selectAll('.zoom-brush').call(sv.brush);

      // DEBUG INFO
      if (sv.debug) {
        console.log('brushing-' + sv.brush_event);
      }
    }

    function brushend() {
      if (sv.brush_event) {
        if (sv.brush.empty() && d3.event.sourceEvent.shiftKey) {
          // Zoom out if a shift click event
          sv.zoom_out_completely();
        } else {
          // Zoom in on brush area
          var domains = sv.domains_from_brush_extent(sv.brush.extent(), true);
          sv.move_to(domains)
        }
        sv.svg.style('cursor', 'all-scroll');
        sv.brush_event = false;
      }
      // Clear the brush
      sv.brush.clear();
      sv.svg.selectAll('.zoom-brush').call(sv.brush);
    }
  }


  /**
   * Initialize Spectra Viewer Drag-n-Drop.
   */
  JSV.SpectraViewer.prototype.initialize_drag_drop_load = function() {
    var sv = this;
    this.svg.on('dragleave.dragndrop', function(e) {
      d3.event.preventDefault();
      d3.event.stopPropagation();
      sv.draw();
    });

    this.svg.on('dragover.dragndrop', function(e) {
      d3.event.preventDefault();
      d3.event.stopPropagation();
      sv.draw();
      sv.flash('Drop Bayesil JSON File...');
    });

    this.svg.on('drop.dragndrop', function() {
      d3.event.preventDefault();
      d3.event.stopPropagation();
      sv.draw();
      var file = d3.event.dataTransfer.files[0];
      // console.log(file.type)
      sv.flash('Loading "' + file.name + '"...');
      var reader = new FileReader();
      sv.json_file = file;
      reader.onload = function() {
        var json_obj = reader.result;
        try {
          var json_parsed = JSON.parse(json_obj);
          // TODO: add file and error checking
          sv.replace_bayesil_data(json_parsed, true);
          sv.trigger('drop');
        } catch (e) {
          sv.draw();
          sv.flash('Could not read file: ' + e.message);
        }
      }
      reader.readAsText(file);
    });
  }

  /** @ignore */

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Scale
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  var LinearScale = function() {

    var d3_scale = d3.scale.linear();

    d3_scale.min = function(x) {
      if (!arguments.length) return d3_scale.domain()[0];
      if (x > d3_scale.max()) {
        throw new Error('x must be less than domain max: ' + d3_scale.max());
      }
      // d3_scale.domain([ x, d3_scale.domain()[1] ]);
      d3_scale.domain([ x, d3_scale.max() ]);
      return d3_scale;
    }

    d3_scale.max = function(x) {
      if (!arguments.length) return d3_scale.domain()[1];
      if (x < d3_scale.min()) {
        throw new Error('x must be greater than domain min: ' + d3_scale.min());
      }
      // d3_scale.domain([ d3_scale.domain()[0], x ]);
      d3_scale.domain([ d3_scale.min(), x ]);
      return d3_scale;
    }

    d3_scale.diff = function() {
      return Math.abs(d3_scale.max() - d3_scale.min());
    }

    d3_scale.range_diff = function() {
      return Math.abs(d3_scale.range()[1] - d3_scale.range()[0]);
    }

    d3_scale.range_min = function() {
      return d3.min(d3_scale.range());
    }

    d3_scale.range_max = function() {
      return d3.max(d3_scale.range());
    }

    return d3_scale;
  }

  var SVScale = function() {
    this.x = JSV.LinearScale();
    this.y = JSV.LinearScale();
    this.initialized = false;
  }

  SVScale.prototype.clamp = function(x) {
    this.x.clamp(x);
    this.y.clamp(x);
  }

  /**
   * Update the scale/boundary based on the added data and the current scale/boundary.
   * @param {Object} data Data must be an object with an x and y array of min/max values.
   *   For example:
   *   ```js
   *   boundaries = { x: [-1, 10], y: [0, 1] };
   *   ```
   */
  SVScale.prototype.update = function(data) {
    // Only assign current x/y min/max if boundaries have been set before
    var current_x_max = this.initialized ? this.x.max() : null
    var current_x_min = this.initialized ? this.x.min() : null
    var current_y_max = this.initialized ? this.y.max() : null
    var current_y_min = this.initialized ? this.y.min() : null

    // Determine new boundaries (min/max x/y)
    var x_max = d3.max(data.x.concat(current_x_max));
    var x_min = d3.min(data.x.concat(current_x_min));
    var y_max = d3.max(data.y.concat(current_y_max));
    var y_min = d3.min(data.y.concat(current_y_min));

    // Update scale boundaries
    this.x.domain([x_min, x_max]);
    this.y.domain([y_min, y_max]);
    this.initialized = true;
  }

  JSV.LinearScale = LinearScale;
  JSV.SVScale = SVScale;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Events
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * SVEvents is a system to plug in callbacks to specific events in JSV.
   * Use [on](#on) to add a callback and [off](#off) to remove it.
   * Here are a list of events supported in JSV:
   *
   *  Event               | Description
   *  --------------------|-------------
   *  drag-start          | Called once before viewer starts drag animation
   *  drag                | Called every frame of the drag animation
   *  drag-end            | Called after dragging is complete
   *  zoom-start          | Called once before viewer starts zoom animation
   *  zoom                | Called every frame of the zoom animation
   *  zoom-end            | Called after zooming is complete
   *  domain-change       | Called after the viewer domains have changed
   *  adjust-start        | Called once on mouse down on a selection
   *  adjust              | Called while moving the selection
   *  adjust-end          | Called once after adjustment is complete, including deletions and peak creation
   *  selection-add       | Called when an element is added to the selection
   *  selection-remove    | Called after an element is removed from the selection
   *  selection-clear     | Called before the selection is cleared
   *  selection-empty     | Called after the selection becomes empty
   *  highlight-start     | Called when an element is highlighted
   *  highlight-end       | Called when an element is unhighlighted
   *  label-click         | Called when a annotation label is clicked
   */
  var SVEvents = function() {
    var handlers = {}

    /**
     * Attach a callback function to a specific JSV event.
     * ```js
     * sv = new JSV.SpectraViewer('#my-spectra');
     * sv.on('drag-start', function() { console.log('Dragging has begun!') };
     *
     * // The event can be namespaced for easier removal later
     * sv.on('drag-start.my_plugin', function() { console.log('Dragging has begun!') };
     * ```
     * @param {String} event Name of event. Events can be namespaced.
     * @param {Function} callback Function to call when event is triggered
     */
    this.on = function(event, callback) {
      check_type(event);
      var type = parse_event(event)
      if ( !handlers[type] ) handlers[type] = [];
      handlers[type].push( new Handler(event, callback) );
    }

    /**
     * Remove a callback function from a specific JSV event. If no __callback__ is provided,
     * then all callbacks for the event will be removed. Namespaced events can and should be used 
     * to avoid unintentionally removing callbacks attached by other plugins.
     * ```js
     * // Remove all callbacks attached to the 'drag-start' event.
     * // This includes any namespaced events.
     * sv.off('drag-start');
     *
     * // Remove all callbacks attached to the 'drag-start' event namespaced to 'my_plugin'
     * sv.off('drag-start.my_plugin');
     *
     * // Remove all callbacks attached to any events namespaced to 'my_plugin'
     * sv.off('.my_plugin');
     * ```
     * @param {String} event Name of event. Events can be namespaced.
     * @param {Function} callback Specfic function to remove
     */
    this.off = function(event, callback) {
      check_type(event);
      var type = parse_event(event);
      var namespace = parse_namespace(event);
      // If no callback is supplied remove all of them
      if (arguments.length == 1) {
        if (namespace) {
          if (type) {
            handlers[type] = handlers[type].filter(function(h) { return h.namespace != namespace; });
          } else {
            Object.keys(handlers).forEach(function(key) {
              handlers[key] = handlers[key].filter(function(h) { return h.namespace != namespace; });
            });
          }
        } else {
          handlers[type] = undefined;
        }
      } else {
        // Remove specific callback
        handlers[type] = handlers[type].filter(function(h) { return h.callback != callback; });
      }
    }

    /**
     * Trigger a callback function for a specific event.
     * ```js
     * // Triggers all callback functions associated with drag-start
     * sv.trigger('drag-start');
     *
     * // Triggers can also be namespaced
     * sv.trigger('drag-start.my_plugin');
     * ```
     * @param {String} event Name of event. Events can be namespaced.
     * @param {Object} object Object to be passed back 'on'.
     */
    this.trigger = function(event, object) {
      check_type(event);
      var type = parse_event(event);
      var namespace = parse_namespace(event);
      if (Array.isArray(handlers[type])) {
        handlers[type].forEach(function(handler) {
          if (namespace) {
            if (handler.namespace == namespace) handler.callback.call(null, object);
          } else {
            handler.callback.call(null, object);
          }
        });
      }
    }

    /** @ignore */

    var check_type = function(type) {
      if (typeof type != 'string') {
        throw new Error('Type must be a string');
      }
    }
  }

  var Handler = function(event, callback) {
    this.callback = callback;
    this.event_type = parse_event(event);
    this.namespace = parse_namespace(event);
  }

  var parse_event = function(event) {
    return event.replace(/\..*/, '');
  }

  var parse_namespace = function(event) {
    result = event.match(/\.(.*)/);
    return result ? result[1] : undefined
  }


  JSV.SVEvents = SVEvents;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SVSet
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * SVSet is essentially an array for holding JSV Objects. Any method
   * that works directly on an Array (Mutator methods) will work on a SVSet
   * (e.g. pop, push, reverse)
   *
   * If a single array is provided it will be converted to an SVSet.
   * If mulitple elements are provided, they will be added to the new SVSet.
   */
  var SVSet = function() {
    if ( (arguments.length == 1) && (Array.isArray(arguments[0])) ) {
      this.push.apply(this, arguments[0])
    } else if (arguments.length > 0) {
      this.push.apply(this, arguments)
    }
  }
  SVSet.prototype = Object.create(Array.prototype);

  /**
   * Return the string 'SVSet'
   * @return {String}
   */
  SVSet.prototype.toString = function() { return 'SVSet' }

  /**
   * Push the elements of the supplied SVSet/Array on to the SVSet.
   * @param {SVSet|Array} svset SVSet or Array to add
   * @return {SVSet}
   */
  SVSet.prototype.merge = function(svset) {
    this.push.apply(this, svset);
    return this;
  };

  // Change one or more attributes of SVSet:
  // .attr(key, value)
  // .attr({key1: value1, key2, value2})
  /**
   * Change one or more properties of each element of the SVSet.
   * ```javascript
   * .attr(property, value)
   * .attr( {property1: value1, property2: value2} )
   * ```
   *
   * @param {Key, Value} attributes
   * @param {Object}     attributes
   * @return {SVSet}
   */
  SVSet.prototype.attr = function(attributes) {
    if ( (arguments.length == 1) && (typeof attributes == 'object') ) {
      var keys = Object.keys(attributes);
      var key_len = keys.length;
      for (var set_i=0, set_len=this.length; set_i < set_len; set_i++) {
        for (var key_i=0; key_i < key_len; key_i++) {
          this[set_i][keys[key_i]] = attributes[keys[key_i]];
        }
      }
    } else if (arguments.length == 2) {
      for (var i=0, len=this.length; i < len; i++) {
        this[i][arguments[0]] = arguments[1];
      }
    } else if (attributes != undefined) {
      throw new Error('attr(): must be 2 arguments or a single object');
    }
    return this;
  }

  /**
   * Call the draw method for each element in the SVSet.
   * See [SVPath.draw](SVPath.js.html#draw) for details
   * @param {} context
   * @param {} scale
   * @param {} fast
   * @param {} calculated
   * @param {} pixel_skip
   * @retrun {SVSet}
   */
  SVSet.prototype.draw = function(context, scale, fast, calculated, pixel_skip) {
    for (var i=0, len=this.length; i < len; i++) {
      this[i].draw(context, scale, fast, calculated, pixel_skip);
    }
    return this;
  }

  /**
   * Iterates through each element of the SVSet and run the callback.
   * In the callback _this_ will refer to the element.
   * ```javascript
   * .each(function(index, element))
   * ```
   *
   * Note: This is slower then a _forEach_ or a _for loop_ directly on the set.
   * @param {Function} callback Callback run on each element of SVSet.
   *   The callback will be called with 2 parameters: the index of the element
   *   and the element itself.
   * @return {SVSet}
   */
  SVSet.prototype.each = function(callback) {
    for (var i = 0, len = this.length; i < len; i++) {
      callback.call(this[i], i, this);
    }
    return this;
  }

  /**
   * Returns true if the SVSet contains the element.
   * @param {Object} element Element to check for
   * @return {Boolean}
   */
  SVSet.prototype.contains = function(element) {
    return (this.indexOf(element) >= 0)
  }

  // SVSet.prototype.remove = function(element) {
  //   var self = this;
  //   self = new SVSet( self.filter(function(i) { return i != element }) );
  //   return self;
  // }

  /**
   * Return true if the SVSet is empty.
   * @return {Boolean}
   */
  SVSet.prototype.empty = function() {
    return this.length == 0;
  }

  /**
   * Returns true if the SVSet is not empty.
   * @return {Boolean}
   */
  SVSet.prototype.present = function() {
    return this.length > 0;
  }

  // Array sort should also work
  // @property: property to order each element set by [default: 'center']
  // @descending: order in descending order (default: false)
  SVSet.prototype.order_by = function(property, descending) {
    // Sort by function call
    if (this.length > 0) {

      if (typeof this[0][property] === 'function'){
        this.sort(function(a,b) {
          if (a[property]() > b[property]()) {
            return 1;
          } else if (a[property]() < b[property]()) {
            return -1;
          } else {
            return 0;
          }
        })
      } else {
      // Sort by property
        this.sort(function(a,b) {
          if (a[property] > b[property]) {
            return 1;
          } else if (a[property] < b[property]) {
            return -1;
          } else {
            return 0;
          }
        })
      }
    }
    if (descending) this.reverse();
    return this;
  }

  SVSet.prototype.lineWidth = function(width) {
    for (var i=0, len=this.length; i < len; i++) {
      this[i].lineWidth = width;
    }
    return this;
  }

  /**
   * Retrieve subset of SVSet or an individual element from SVSet depending on term provided.
   * @param {Undefined} term Return full SVSet
   * @param {Integer}   term Return element at that index (base-1)
   * @param {String}    term Return first element with id same as string. If the id starts
   *   with 'path-id-', the first element with that path-id will be returned.
   * @param {Array}     term Return SVSet with elements with matching ids
   * @return {SVSet|or|Element}
   */
  SVSet.prototype.get = function(term) {
    // if (arguments.length == 0) {
    if (term == undefined) {
      return this;
    } else if (Number.isInteger(term)) {
      return this[term-1];
    } else if (typeof term == 'string') {
      if ( term.match(/^path-id-/) ) {
        return this.filter(function(element) { return element.path_id() == term; })[0];
      } else if ( term.match(/^label-id-/) ) {
        return this.filter(function(element) { return element.label_id() == term; })[0];
      } else {
        return this.filter(function(element) { return element.id == term; })[0];
      }
    } else if (Array.isArray(term)) {
      var filtered = this.filter(function(element) { return term.some(function(id) { return element.id == id; }); });
      var svset = new SVSet();
      svset.push.apply(svset, filtered);
      return svset;
    } else {
      return new SVSet();
    }
  }

  /**
   * Return new SVSet with no duplicated values.
   * @return {SVSet}
   */
  SVSet.prototype.unique = function() {
    return new SVSet(this.filter( onlyUnique ));
  }

  function onlyUnique(value, index, self) { 
    return self.indexOf(value) === index;
  }

  /** @ignore */

  JSV.SVSet = SVSet;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SVData
// Object to store X/Y data
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  var SVData = function(data) {
    if (data === undefined) { data = {x: [], y:[]} };
    // If data is an array of objects convert them.
    if (data instanceof Array) { data = array_to_object(data); }
    // Confirm x and y are the same length
    if (data.x.length != data.y.length) { throw new Error('x and y must be the same length!'); }
    if (data.yi && (data.yi.length != data.x.length)) { throw new Error('If present yi must be the same length as x and y!'); }
    // Confirm arrays start at lowest x values first. If not reverse.
    var reverse_data = (data.x[0] > data.x[data.x.length - 1]);

    this.x = reverse_data ? data.x.reverse() : data.x;
    this.y = reverse_data ? data.y.reverse() : data.y;
    this.yi = (data.yi && reverse_data) ? data.yi.reverse() : data.yi;
  }

  SVData.prototype.length = function() {
    return this.x.length;
  }

  SVData.prototype.asArray = function() {
    var data = [];
    var point;
    for (var i=0, len=this.length(); i < len; i++) {
      point = { x: this.x[i], y: this.y[i] }
      if (this.yi) { point.yi = this.yi[i]; }
      data.push(point);
    }
    return data;
  }

  SVData.prototype.index_of = function(search_value, upper) {
    return JSV.index_of_value(this.x, search_value, upper);
  }

  SVData.prototype.simplify = function(tolerance, highQuality) {
    // return new SVData( simplify(this.asArray(), tolerance, highQuality) );
    return new SVData( JSV.simplify(this, tolerance, highQuality) );
  }

  var array_to_object = function(data) {
    var x = [ ], y = [ ], yi = [ ];
    for (var i=0, len=data.length; i < len; i++) {
      x.push(data[i].x);
      y.push(data[i].y);
      if (data[0].yi) { yi.push(data[i].yi); }
    }
    var object = {x:x, y:y};
    if (data[0] && data[0].yi) { object.yi = yi; }
    return object;
  }



  JSV.SVData = SVData;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SVPath
// Objects that will be drawn as paths should inherit from SVPath
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  path_id = 0;

  /**
   * Objects that will be drawn as paths should inherit form SVPath
   * (e.g. [Spectrum](Spectrum.js.html), [Peak](Peak.js.html), etc)
   * @param {Object} options Properties for the canvas context to use when drawing the path
   *
   *   - _color_ [default: 'black']
   *   - _fill_ [default: '']
   *   - _lineWidth_ [default: 1]
   *   - _visible_ [default: true]
   *   - _dash_ [default: []]
   *
   * @param {Object} meta User-defined _key_:_value_ pairs to add to the SVPath.
   */
  var SVPath = function(options, meta) {
    options = options || {};
    this.meta = meta || {};
    this.lineWidth = JSV.default_for(options.lineWidth, 1);
    this.color     = JSV.default_for(options.color, 'black');
    this.visible   = JSV.default_for(options.visible, true);
    this.dash      = JSV.default_for(options.dash, []);
    this.fill      = options.fill;
    this.path_id();
  }

  SVPath.prototype.peaks = function() {
    return new JSV.SVSet();
  }

  SVPath.prototype.path_id = function() {
    var new_id = generate_path_id();
    this.path_id = function() { return new_id; }
    return new_id;
  }

  var generate_path_id = function() {
    return 'path-id-' + path_id++;
  }

  SVPath.prototype.attr = function(attributes) {
    var keys = Object.keys(attributes);
    for (var i=0, len=keys.length; i < len; i++) {
      this[keys[i]] = attributes[keys[i]];
    }
    return this;
  }

  SVPath.prototype.display_settings = function(options) {
    if (options) {
      return this.attr(options);
    } else {
      return {
        color: this.color,
        fill: this.fill,
        lineWidth: this.lineWidth,
        visible: this.visible,
        dash: this.dash
      }
    }
  }

  // Determine best data to use for drawing based on 'fast' and 'calculate' and the 
  // available data in the SVPath.
  SVPath.prototype.data_for_draw = function(scale, fast, calculate, pixel_skip) {
    var data;
    var has_data = this.xy_data != undefined;

    if (this.phasing && has_data && this.xy_data.yi != undefined) {
      var ph;
      var y = [ ];
      var len = this.xy_data.length();
      for (var i=0; i < len; i++) {
        ph = Math.PI * (1*this.ph0 + 1*(len-i-1)*this.ph1/len) / 180.0;
        y.push( ( Math.cos(ph) * this.xy_data.y[i] ) + ( Math.sin(ph) * this.xy_data.yi[i] ) );
      }
      data = {x:this.xy_data.x, y:y};
    } else if ( (calculate && this.peaks().length > 0) || (!has_data && this.peaks().length > 0) ) {
      if (!pixel_skip) pixel_skip = 1;
      var number_of_points = scale.x.range_diff() / JSV.pixel_ratio / pixel_skip;
      var fast_calc = pixel_skip > 1;
      data = JSV.xy_from_peaks(this.peaks(), number_of_points, scale.x.domain()[0], scale.x.domain()[1], fast_calc);
    } else if (has_data) {
      data = fast ? this.simple_xy_data : this.xy_data;
    } else {
      data = {x: [0], y: [0]};
      console.log('No data or Peaks for SVPath:');
      console.log(this);
    }
    return data;
  }

  SVPath.prototype.draw = function(context, scale, fast, calculate, pixel_skip) {
    if (this.visible) {
      var data = this.data_for_draw(scale, fast, calculate, pixel_skip);
      var min_index = JSV.index_of_value(data.x, scale.x.domain()[0], false);
      var max_index = JSV.index_of_value(data.x, scale.x.domain()[1], true);
      context.translate(0.5, 0.5);
      context.beginPath();
      var y_start = this.fill ? scale.y(0) : scale.y(data.y[min_index]);
      context.moveTo( scale.x(data.x[min_index]), y_start );
      for (var i = min_index; i <= max_index; i++) {
        context.lineTo( scale.x(data.x[i]), scale.y(data.y[i]) );
      }
      if (this.fill) {
        context.lineTo( scale.x(data.x[max_index]), scale.y(0) );
        context.fillStyle = this.fill;
        context.fill();
      }
      // context.lineWidth = JSV.pixel(0.5);
      context.lineWidth = JSV.default_for(this.lineWidth, 1);
      context.strokeStyle = this.color;
      context.setLineDash(this.dash);
      context.stroke();
      context.translate(-0.5, -0.5);
    }
  }

  JSV.SVPath = SVPath;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Legend
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function Rect(x, y, width, height) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  Rect.prototype.contains_pt = function(x, y) {
    return ( x >= this.x && x <= (this.x + this.width) && y >= this.y && y <= (this.y + this.height) )
  }


  function SVLegend(sv) {
    var self = this;
    this.sv = sv;
    this.margin_top = JSV.pixel(5);
    this.margin_side = JSV.pixel(10);
    this.key_label_space = JSV.pixel(5);
    this.key_width = JSV.pixel(10);
    this.line_width = JSV.pixel(3);
    this.text_height = JSV.pixel(18);
    this.items = [];

    sv.svg.on('mousedown.legend', function() {
      var pt = sv.mouse(sv.canvas);
      for (var i=0, len=self.items.length; i < len; i++) {
        item = self.items[i];
        if (item.rect.contains_pt(pt[0], pt[1])) {
          item.path.visible = !item.path.visible;
          sv.full_draw();
        }
      }
    })

  }

  SVLegend.prototype.bottom = function() {
    var text_y = (this.sv.zoombox.visible ? JSV.pixel(this.sv.zoombox.height) : 0) + this.margin_top;
    return this.sv.legend_show ? this.offset_y : text_y;
  }

  SVLegend.prototype.update = function() {
    var sv = this.sv;
    var context = sv.context;
    this.offset_y = (sv.zoombox.visible ? JSV.pixel(sv.zoombox.height) : 0) + this.margin_top;
    var offset_x = this.margin_side;
    context.font = sv.adjust_font(1, 'monospace');
    this.items = [];
    for (var i = 0; i < sv.spectra().length; i++) {
      var spectrum = sv.spectra()[i];
      // if (! spectrum.active) continue;
      var item = new JSV.SVLegendItem(this);
      var rect = new Rect();
      rect.y = this.offset_y;
      rect.height = this.text_height;
      rect.width = this.key_width + this.key_label_space + context.measureText(spectrum.name).width;
      rect.x = sv.axis_x_reverse ?
        offset_x :
        JSV.pixel(sv.width) - offset_x - rect.width;
      item.rect = rect;
      item.path = spectrum;
      this.offset_y += this.text_height;
      this.items.push(item);
    }
  }

  SVLegend.prototype.draw = function() {
    var sv = this.sv;
    var context = sv.context;
    context.save();
    context.textAlign = 'left';
    context.textBaseline = 'middle';
    context.font = sv.adjust_font(1, 'monospace');
    for (var i = 0; i < this.items.length; i++) {
      this.items[i].draw();
    }
    context.restore();
  }

  function SVLegendItem(legend, path, rect) {
    this.legend = legend;
    this.sv = legend.sv;
    this.path = path; // e.g. spectra
    this.rect = rect; // Rect defining space in legend
  }

  SVLegendItem.prototype.key_x1 = function() {
    return this.sv.axis_x_reverse ?
      this.rect.x :
      this.rect.x + this.rect.width - this.legend.key_width;
  }

  SVLegendItem.prototype.key_x2 = function() {
    return this.key_x1() + this.legend.key_width;
  }

  SVLegendItem.prototype.key_y = function() {
    return this.rect.y + (this.rect.height / 2);
  }

  SVLegendItem.prototype.label_x = function() {
    return this.sv.axis_x_reverse ?
      this.rect.x + this.legend.key_width + this.legend.key_label_space :
      this.rect.x;
  }


  SVLegendItem.prototype.draw = function() {
    var rect = this.rect;
    this.sv.context.clearRect(rect.x, rect.y, rect.width, rect.height);
    if (this.path.visible) this.draw_key();
    this.draw_label();
  }

  SVLegendItem.prototype.draw_key = function() {
    var context = this.sv.context;
    context.beginPath();
    context.moveTo(this.key_x1(), this.key_y());
    context.lineTo(this.key_x2(), this.key_y());
    context.lineWidth = this.legend.line_width;
    context.strokeStyle = this.path.color;
    context.setLineDash(this.path.dash);
    context.stroke();
  }

  SVLegendItem.prototype.draw_label = function() {
    this.sv.context.fillStyle = this.path.visible ? 'black' : '#999';
    this.sv.context.fillText(this.path.name, this.label_x(), this.key_y());
  }

  JSV.SVLegend = SVLegend;
  JSV.SVLegendItem = SVLegendItem;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Labels
//////////////////////////////////////////////////////////////////////////////
// TODO:
// - Only check for collisions with spectrum if number of labels is < 100
// - When checking for overlap with spectrum only use 4 points in increments of (width / 3)
// - consider having labels per spectrum, them group visible ones together for drawing
(function(JSV) {

  function SVAnnotation(sv, options) {
    options = options || {};
    var self = this;
    this.sv = sv;
    this.highlighted_label;
    this.point_gap = JSV.default_for(options.point_gap, JSV.pixel(12));
    this.label_color = JSV.default_for(options.label_color, '#5555DD');
    this.hover = JSV.default_for(options.hover, true);
    this.labels = new JSV.SVSet();
    this.visible_labels = new JSV.SVSet();
    // Test label-click
    // sv.on('label-click', function(label) {
    //   console.log(label.text)
    // })
    sv.svg.on('mousedown.label', function() {
      if (self.highlighted_label && self.hover) {
        sv.trigger('label-click', self.highlighted_label);
      }
    });
  }

  SVAnnotation.prototype.get = function(term) {
    return this.labels.get(term);
  }

  SVAnnotation.prototype.update = function() {
    var sv = this.sv;
    var labels = sv.labels ? sv.labels.get() : new JSV.SVSet();
    sv.spectra().forEach(function(spectrum) {
      if (spectrum.visible && spectrum.active && spectrum.labels) {
        labels.push.apply(labels, spectrum.labels.get())
      }
    });
    this.labels = labels;

    var visible_labels = this.reduce_labels_by_view();
    visible_labels = this.reduce_labels_by_height(visible_labels);
    visible_labels = this.adjust_labels(visible_labels);
    this.visible_labels = visible_labels;
  }

  SVAnnotation.prototype.rect_for_label = function(label) {
    var sv = this.sv;
    var rect, x, y;
    var point_x = sv.scale.x(label.x);
    var point_y = sv.scale.y(label.y);
    var text_width = sv.context.measureText(label.text).width;
    var text_height = JSV.pixel(label.font_size);
    if (label.vertical) {
      x = point_x - (text_height/2);
      y = point_y - text_width - this.point_gap;
      rect = new Rect(x, y, text_height, text_width);
    } else {
      x = point_x - (text_width/2);
      y = point_y - text_height - this.point_gap;
      rect = new Rect(x, y, text_height, text_width);
    }
    return rect;
  }

  // Return labels within view
  SVAnnotation.prototype.reduce_labels_by_view = function(labels) {
    if (!labels) labels = this.labels;
    var scale = this.sv.scale;
    return labels.filter(function(label) { return (label.x > scale.x.min() && label.x < scale.x.max() && label.y < scale.y.max())})
  }

  // Return only highest labels of overlapping sets
  SVAnnotation.prototype.reduce_labels_by_height = function(labels) {
    if (!labels) labels = this.labels;
    var scale = this.sv.scale;
    var font_min = 6;
    var reduced = new JSV.SVSet();

    for (var i=0, len=labels.length; i < len; i++) {
      label = labels[i];
      label.font_size = font_min;
      label.rect = this.rect_for_label(label);
      label.adjust_rect(reduced);
      if (label.stack_level < 3) {
        reduced.push(label);
      }
    }

    return reduced;
  }

  SVAnnotation.prototype.adjust_labels = function(labels) {
    var sv = this.sv;
    var peak, label, label_rects, spectra_overlap;
    // Number of stacked labels to accept before trying to reduce the font size
    var max_stack = 1;
    // space between label line and peak/label
    var line_spacer = JSV.pixel(2);
    // minimum gap between peak and label
    var peak_gap = JSV.pixel(12);
    var font_max = 13;
    var font_min = 8;
    var font_range = font_max - font_min;
    var font_current = font_max;
    var font_current_min = font_min;

    var scale = sv.scale;
    var context = sv.context;
    context.save();
    var bad_stack = true;
    while(bad_stack) {
      bad_stack = false;
      adjusted_labels = new JSV.SVSet();
      context.font = JSV.pixel(font_current) + "px Arial";
      // Adjust min font based on zoom level
      font_current_min = font_min + d3.round(font_range * sv.zoom_x / sv.zoom_max);
      for (var i=0, len=labels.length; i < len; i++) {
        label = labels[i];
        label.font_size = font_current;
        label.rect = this.rect_for_label(label);
        // spectra_overlap = this.label_over_spectra(label, this.sv.peaks());
        label.adjust_rect(adjusted_labels);
        if ( (label.stack_level > max_stack) && font_current > font_current_min) {
          bad_stack = true;
          font_current -= 1;
          break;
        } else {
          adjusted_labels.push(label);
        }
      }
    }
    return labels;
  }

  // Check if the bottom line of rect overlaps with sumline created from peaks
  SVAnnotation.prototype.label_over_spectra = function(label, peaks, tolerance) {
    var sv = this.sv;
    var x_vals = [];
    for (var i=0, len=label.rect.width; i < len; i++) {
      x_vals.push(sv.scale.x.invert(label.rect.x + i));
    }
    var y = sv.scale.y.invert(label.rect.bottom());
    var overlap = false;
    for (var i=0, len=x_vals.length; i < len; i++) {
      if (y < JSV.sum_of_peaks(x_vals[i], peaks)) {
        overlap = true;
        break;
      }
    }
    return overlap;
  }

  SVAnnotation.prototype.draw = function() {
    var sv = this.sv;
    var context = sv.context;
    var scale = sv.scale;
    var label;
    var line_spacer = JSV.pixel(2);
    context.save();

    this.update();
    var visible_labels = this.visible_labels;
    // Draw the label lines, joining the peak to the label
    for (var i=0, len=visible_labels.length; i < len; i++) {
      label = visible_labels[i];
      var line_x = scale.x(label.x);
      var line_y1 = scale.y(label.y) - line_spacer;
      var line_y2 = label.rect.bottom() + line_spacer;
      context.beginPath();
      context.moveTo(line_x, line_y1);
      context.lineTo(line_x, line_y2);
      context.strokeStyle = '#999999';
      context.lineWidth = JSV.pixel(0.5);
      context.stroke();
    }

    // Draw the labels
    context.fillStyle = this.label_color;
    for (var i=0, len=visible_labels.length; i < len; i++) {
      label = visible_labels[i];
      if (label === this.highlighted_label) {
        context.font = "bold " + context.font;
      } else {
        context.font = context.font.replace('bold ', '');
      }
      context.textBaseline = 'top';
      if (label.vertical) {
        context.textAlign = 'right';
        context.save();
        context.translate(label.rect.x, label.rect.y);
        context.rotate(-Math.PI/2);
        context.fillText(label.text, 0, 0);
        context.restore();
      } else {
        context.textAlign = 'left';
        context.fillText(label.text, label.rect.x, label.rect.y);
      }
    }
    context.restore();
  }

  SVAnnotation.prototype.check_hover = function() {
    var sv = this.sv;
    var label;
    if (this.hover) {
      var x = sv.scale.x(sv.mx);
      var y = sv.scale.y(sv.my);
      var old_label = this.highlighted_label;
      var current_label;

      for (var i=0, len=this.visible_labels.length; i < len; i++) {
        label = this.visible_labels[i];
        if (label.rect.contains_pt(x, y)) {
          current_label = label;
          break;
        }
      }
      if (old_label != current_label) {
        this.highlighted_label = current_label;
        this.sv.trigger('click-start');
        sv.full_draw();
      }
      if (this.highlighted_label) {
        sv.svg.style('cursor', 'pointer');
      } else {
        sv.svg.style('cursor', 'move');
      }
    }
  }


  function SVLabelSet(options) {
    options = options || {};
    this.spectrum = options.spectrum
    this.labels = new JSV.SVSet();
  }

  SVLabelSet.prototype.get = function(term) {
    return this.labels.get(term);
  }

  SVLabelSet.prototype.add = function(data, display, meta) {
    this.labels.push( new SVLabel(this, data, display, meta) );
  }

  // If the SVLabelSet is associated with a spectrum with peaks those labels will be added
  SVLabelSet.prototype.add_peaks = function() {
    if (this.spectrum) {
      var peaks = this.spectrum.peaks();
      for (var i=0, len=peaks.length; i < len; i++) {
        var peak = peaks[i];
        var y = JSV.sum_of_peaks(peak.center, peak.spectrum().peaks())
        this.add({ x: peak.center, y: y, text: peak.label() })
      }
    }
  }


  // If the SVLabelSet is associated with a spectrum with peaks those labels will be regenerated
  SVLabelSet.prototype.update_peaks = function() {
    if (this.spectrum) {
      // TODO: only remove peak associated labels
      this.labels = new JSV.SVSet();
      this.add_peaks();
    }
  }


  label_id = 0;

  function SVLabel(label_set, data, display, meta) {
    data = data || {};
    display_defaults = { vertical: true, font_size: 12 };
    display = JSV.merge(display_defaults, data.display, display);
    this.meta = JSV.merge(data.meta, meta);

    this.label_set = label_set;
    this.vertical = display.vertical;
    this.font_size = display.font_size;
    // this.id = data.id;
    // this.name = data.name || this.id;
    this.text = data.text;
    this.x = data.x;
    this.y = data.y;
    this.stack_level = 0;
    this.label_id();
  }

  SVLabel.prototype.label_id = function() {
    var new_id = generate_label_id();
    this.label_id = function() { return new_id; }
    return new_id;
  }

  var generate_label_id = function() {
    return 'label-id-' + label_id++;
  }

  // Adjust the rect so it does not overlap any rects in rect_array
  // Returns an object with the adjusted rect and how many levels the
  // rect had to be stacked to not overlap any more.
  SVLabel.prototype.adjust_rect = function(labels) {
    var overlap = true;
    var count = 0;
    while( this.overlap(labels) ) {
      this.rect.y -= JSV.pixel(this.font_size) * 2;
      count += 1;
    }
    this.stack_level = count
  }

  SVLabel.prototype.overlap =  function(labels) {
    return this.rect.overlap( labels.map( function(label) { return label.rect; } ) );
  }


  function Rect(x, y, width, height) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  Rect.prototype.bottom = function() {
    return this.y + this.height;
  }

  Rect.prototype.top = function() {
    return this.y;
  }

  Rect.prototype.left = function() {
    return this.x;
  }

  Rect.prototype.right = function() {
    return this.x + this.width;
  }

  // Check if rect overlaps with any rects in rect_array
  Rect.prototype.overlap = function(rect_array) {
    // Gap between labels
    var width_gap = JSV.pixel(4);
    var r1 = this;
    var overlap = false;
    for (var i=0, len=rect_array.length; i < len; i++){
      var r2 = rect_array[i];
      if (r1.x <= r2.right() && r2.x <= (r1.right() + width_gap) && r1.y <= r2.bottom() && r2.y <= r1.bottom()) {
        overlap = true;
        break;
      }else{
        overlap = false;
      }
    }
    return overlap;
  }

  Rect.prototype.contains_pt = function(x, y) {
    return ( x >= this.x && x <= (this.x + this.width) && y >= this.y && y <= (this.y + this.height) )
  }

  JSV.SVAnnotation = SVAnnotation;
  JSV.SVLabelSet = SVLabelSet;
  JSV.SVLabel = SVLabel;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Spectrum
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * Specrum object blah [SVPath](SVPath.js.html)
   * ```javascript
   * spectrum = new Spectrum('fit');
   * ```
   * @param {Object} data 
   * @param {Object} display Display options as described in [SVPath](SVPath.js.html)
   * @param {Object} meta User-defined properties
   */
  function Spectrum(data, display, meta) {
    var self = this;
    data = data || { };
    display_defaults = { };
    display = JSV.merge(display_defaults, data.display, display);
    meta = JSV.merge(data.meta, meta);
    JSV.SVPath.call(this, display, meta);

    this.id = data.id;
    this.name = data.name || this.id;
    this.tolerance = data.tolerance;
    this.noise;
    this.active = true;
    this.xy_data = new JSV.SVData();
    this.simple_xy_data = new JSV.SVData();
    this.labels = new JSV.SVLabelSet({spectrum: this});
    this._compounds = new JSV.SVSet();

    if (data.compounds) {
      data.compounds.forEach(function(compound_data) {
        self.add_compound(new JSV.Compound(compound_data));
      })
    } else if (data.peak_list) {
      var compound_data = { clusters: [ { peaks: data.peak_list } ] };
      self.add_compound(new JSV.Compound(compound_data));
    }

    if (self.compounds().present()) {
      var number_of_points = JSV.default_for(data.number_of_points, 10000);
      var min_x = JSV.default_for(data.min_x, -1);
      var max_x = JSV.default_for(data.max_x, 10 );
      var xy_line = JSV.xy_from_peaks(self.peaks(), number_of_points, min_x, max_x);
      self.add_xy_line(xy_line);
    } else if (data.xy_line) {
      self.add_xy_line(data.xy_line);
    }

    if (data.labels) {
      data.labels.forEach(function(label) {
        self.labels.add(label);
      });
    }

  }
  JSV.inherits(Spectrum, JSV.SVPath);

  Spectrum.prototype.toString = function() { return 'Spectrum'; }

  /**
   * Add a [Compound](Compound.js.html) to the spectrum
   * @param {Compound} compound [Compound](Compound.js.html) to add
   */
  Spectrum.prototype.add_compound = function(compound) {
    this._compounds.push(compound);
    compound._spectrum = this;
    // TODO: update xy_data??
  }

  /**
   * Update xy_line based on peak data. Also update the zoombox.
   */
  Spectrum.prototype.update = function() {
    var sv = this._sv;
    var xy_data = JSV.xy_from_peaks(this.peaks(), this.xy_data.length(), sv.boundary.x.min(), sv.boundary.x.max());
    this.add_xy_line(xy_data);
    sv.zoombox.update();
  }

  /**
   * Returns a specific compound or all the compounds for the spectrum as an [SVSet](SVSet.js.html).
   * See [SVSet.get()](SVSet.js.html#get) for details.
   *
   * @param {} term See [SVSet.get()](SVSet.js.html#get) for details
   * @return {SVSet|or|Compound}
   */
  Spectrum.prototype.compounds = function(term) {
    return this._compounds.get(term)
  }

  Spectrum.prototype.add_xy_line = function(xy_data) {
    this.xy_data = new JSV.SVData(xy_data);
    this.noise = this.calculate_noise();
    var tolerance = this.tolerance || this.noise
    this.simple_xy_data = this.xy_data.simplify(tolerance, true);
  }

  // Noise is calculated as the difference between the maximum
  // and minimum peak betwen the range.
  // TODO: if region does not exist, use first 0.01 in data
  Spectrum.prototype.calculate_noise = function() {
    // Region to calculate noise
    var region = [-0.05, -0.04];
    var xy_min = this.xy_data.index_of(region[0]);
    var xy_max = this.xy_data.index_of(region[1]);
    var y_noise = this.xy_data.y.slice(xy_min, xy_max);
    return (d3.max(y_noise) - d3.min(y_noise)).toPrecision(2);
  }

  Spectrum.prototype.clusters = function(term) {
    var clusters = new JSV.SVSet();
    for (var i=0, len=this._compounds.length; i < len; i++) {
      clusters.merge(this._compounds[i].clusters());
    }
    return clusters.get(term);
  }

  Spectrum.prototype.peaks = function(term) {
    var peaks = new JSV.SVSet();
    this.compounds().forEach(function(compound) {
      compound.clusters().forEach(function(cluster) {
        peaks.merge(cluster.peaks());
      })
    })
    return peaks.get(term);
  }

  /////////////////////////////////////////////////////////////////////////////
  // Spectrum Properties (setters/getters)
  /////////////////////////////////////////////////////////////////////////////
  Object.defineProperties(Spectrum.prototype, {
    'active': {
      get: function() { return this._active },
      set: function(val) {
        this._active = val;
        var sv = this._sv;
        if (sv) {
          sv.reset_boundaries();
          sv.all_spectra().each(function() {
            if (this.active) {
              sv.boundary.update(this.xy_data);
              sv.scale.update(this.xy_data);
            }
          });
          sv.zoombox.update();
          sv.legend.update();
        }
      }
    }
  });

  JSV.Spectrum = Spectrum;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Compound
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function Compound(data, display, meta) {
    var self = this;
    data = data || { };
    display_defaults = { visible: false };
    display = JSV.merge(display_defaults, data.display, display);
    meta = JSV.merge(data.meta, meta);
    JSV.SVPath.call(this, display, meta);

    this._clusters = new JSV.SVSet();
    if (data) {
      this.id = data.id;
      this.name = data.name;
      this.concentration = data.concentration
      if (data.clusters) {
        data.clusters.forEach(function(cluster_data) {
          self.add_cluster(new JSV.Cluster(cluster_data))
        });
      }
    }
    // this.update_xy_data();
    this.update();
  }
  JSV.inherits(Compound, JSV.SVPath);

  Compound.prototype.toString = function() { return 'Compound'; }

  /**
   * Add a [Cluster](Cluster.js.html) to the Compound
   * @param {Peak} peak [Cluster](Cluster.js.html) to add
   */
  Compound.prototype.add_cluster = function(cluster) {
    this._clusters.push(cluster);
    cluster._compound = this;
    // cluster.update_center();
    // TODO: update xy_data??
  }

  // Compound.prototype.update_xy_data = function() {
  Compound.prototype.update = function() {
    this.x_min = JSV.peak_min(this.peaks());
    this.x_max = JSV.peak_max(this.peaks());
    this.xy_data = new JSV.SVData(JSV.xy_from_peaks(this.peaks(), 5000, this.x_min, this.x_max));
    this.simple_xy_data = this.xy_data.simplify(0.0005, true);
    this._clusters.order_by('center');
  }

  Compound.prototype.clusters = function(term) {
    return this._clusters.get(term);
  }

  Compound.prototype.peaks = function(term) {
    var peaks = new JSV.SVSet();
    this._clusters.forEach(function(cluster) {
      peaks.merge(cluster.peaks());
    })
    return peaks.get(term);
  }

  Compound.prototype.spectrum = function() {
    return this._spectrum;
  }

  Compound.prototype.intersect = function(x, y, margin) {
    var cy = JSV.sum_of_peaks(x, this.peaks());
    // return y <= (cy + margin) && y >= (cy - margin);
    return y <= (cy) && y >= 0;
  }

  Compound.prototype.display_concentration = function() {
    return JSV.round(this.concentration, 1) + ' \u03BCM';
  }


  JSV.Compound = Compound;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Cluster
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function Cluster(data, display, meta) {
    var self = this;
    data = data || { };
    display_defaults = { visible: false };
    display = JSV.merge(display_defaults, data.display, display);
    meta = JSV.merge(data.meta, meta);
    JSV.SVPath.call(this, display, meta);

    this._peaks = new JSV.SVSet();

    this.lower_bound = JSV.number(data.lower_bound);
    this.upper_bound = JSV.number(data.upper_bound);
    this.weight = JSV.number(data.weight);
    this.protons = JSV.number(data.protons);

    if (data.peaks) {
      data.peaks.forEach(function(peak_data) {
        self.add_peak(new JSV.Peak(peak_data))
      });
    }

  }
  JSV.inherits(Cluster, JSV.SVPath);

  Cluster.prototype.toString = function() { return 'Cluster'; }

  /**
   * Add a [Peak](Peak.js.html) to the cluster
   * @param {Peak} peak [Peak](Peak.js.html) to add
   */
  Cluster.prototype.add_peak = function(peak) {
    this._peaks.push(peak);
    peak._cluster = this;
  }


  Cluster.prototype.update = function() {
    this._peaks.order_by('center');
  }

  Cluster.prototype.peaks = function(term) {
    return this._peaks.get(term);
  }

  Cluster.prototype.compound = function() {
    return this._compound;
  }

  Cluster.prototype.spectrum = function() {
    return this.compound().spectrum();
  }

  /**
   * Return the center of the cluster: an average of center value for all the
   * peaks in the cluster.
   * @return {Number}
   */
  Cluster.prototype.center = function() {
    var center;
    if (this.peaks().present()) {
      center = d3.mean(this.peaks(), function(p) { return p.center });
    }
    return center;
  }


  JSV.Cluster = Cluster;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Peak
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * Peak inherits methods and properties from [SVPath](SVPath.js.html).
   *
   * @param {Object} data Data used to create the peak:
   *
   *   - _center_: center of the peak
   *   - _width_: width of peak at half height
   *   - _amplitude_: height of peak
   *
   * @options {Object} display Display options passed to [SVPath](SVPath.js.html)
   * @param {Object} meta User-defined _key_:_value_ pairs to add to the Peak.
   */
  function Peak(data, display, meta) {
    var self = this;
    data = data || { };
    display_defaults = { visible: false };
    display = JSV.merge(display_defaults, data.display, display);
    meta = JSV.merge(data.meta, meta);
    JSV.SVPath.call(this, display, meta);

    this.center = JSV.number(data.center);
    this.amplitude = JSV.number(data.amplitude);
    this.width = JSV.number(data.width);
    // this.update_xy_data();
  }
  JSV.inherits(Peak, JSV.SVPath);

  Peak.prototype.toString = function() { return 'Peak'; }

  Peak.prototype.set_amplitude = function(new_value) {
    if (new_value <= 0) new_value = 0.00001;
    this.amplitude = new_value;
  }

  // Set the center to the 'new_value'
  // Also updates cluster center and bounds
  Peak.prototype.set_center = function(new_value) {
    this.center = new_value;
  }

  Peak.prototype.update = function() {
  }

  // Remove the peak from the parent cluster
  // FIXME: Not very elegant. Should have parent/children generic methods
  //        and store parent/children in consisently named variables
  Peak.prototype.delete = function() {
    var self = this;
    var cluster = this.cluster();
    var compound = this.compound();
    var spectrum = this.spectrum();
    var sv = spectrum._sv;
    var updated_peaks = cluster._peaks.filter( function(p) { return p != self } );
    cluster._peaks = new JSV.SVSet(updated_peaks);
    if (cluster.peaks().empty()) {
      var updated_clusters = compound._clusters.filter( function(c) { return c != cluster; } );
      compound._clusters = new JSV.SVSet(updated_clusters);
    }
    if (compound.peaks().empty()) {
      var updated_compounds = spectrum._compounds.filter( function(c) { return c != compound; } );
      spectrum._compounds = new JSV.SVSet(updated_compounds);
    }
    if (spectrum.peaks().empty()) {
      var updated_spectra = sv._spectra.filter( function(c) { return c != spectrum; } );
      sv._spectra = new JSV.SVSet(updated_spectra);
    }
  }

  Peak.prototype.peaks = function() {
    return new JSV.SVSet(this);
  }

  Peak.prototype.cluster = function() {
    return this._cluster;
  }

  Peak.prototype.compound = function() {
    return this.cluster().compound();
  }

  Peak.prototype.spectrum = function() {
    return this.compound().spectrum();
  }

  Peak.prototype.label = function() {
    return this.center.toFixed(3);
  }

  JSV.Peak = Peak;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SVSelection
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  // OPTIONS?:
  // - element_type, undefined means no selection
  // - show_text
  // - display: settings to draw the selection
  // - multi_select: use select tool and add addtional elements to selection
  // - adjust/move:
  // - delete: allow removing items from viewer
  // - add: allow adding new elements

  function SVSelection(sv, options) {
    JSV.SVPath.call(this);
    var self = this;
    this.sv = sv;
    this.element_type = options.element_type;
    this.allow_multiselect = JSV.default_for(options.allow_multiselect, true);
    this.allow_adjust = JSV.default_for(options.allow_adjust, true);
    this.allow_width_adjust = JSV.default_for(options.allow_width_adjust, true);
    this.allow_peak_creation = JSV.default_for(options.allow_peak_creation, false);
    this.show_bounds = JSV.default_for(options.show_bounds, true);
    this.display_info = JSV.default_for(options.display_info, true);
    this.constrain_adjust = options.constrain_adjust
    this.restriction_spectrum_id = options.restriction_spectrum_id;
    this.possible_elements = options.possible_elements;
    display_defaults = { fill: 'rgba(150, 150, 150, 0.2)', visible: true };
    this.display_settings(JSV.merge(display_defaults, options.display));
    this.handle_spacer = JSV.pixel(10);
    this.handle_size = JSV.pixel(15);
    this._elements = new JSV.SVSet();
    this.initialize_selection_events();
    // this.popup_box = this.sv.sv_wrapper.append('div').attr('class', 'jsv-select-popup-box').style('display', 'none');
    this.popup_box = this.sv.sv_wrapper.append('div').attr('class', 'jsv-select-popup-box').style('visibility', 'hidden');
    this.text_container = this.popup_box.append('div').attr('class', 'jsv-select-text-container');
    this.draw = function() {
      if (self.present()) {
        // Call SVPath draw method
        SVSelection.prototype.draw.apply(this, arguments);
        // First argument is the context; Second argument is the scale
        self.draw_adjust_handles(arguments[0], arguments[1]);
      }
      // Draw box with selection information
      (self.present() && self.display_info) ? self.show_popup_box() : self.hide_popup_box();
    }
  }
  JSV.inherits(SVSelection, JSV.SVPath);

  SVSelection.prototype.peaks = function(term) {
    var peaks = new JSV.SVSet();
    for (var i=0, len=this._elements.length; i < len; i++) {
      peaks.merge(this._elements[i].peaks());
    }
    return peaks.get(term);
  }

  SVSelection.prototype.elements = function(term) {
    return this._elements.get(term);
  }

  SVSelection.prototype.clear = function() {
    this.sv.trigger('selection-clear');
    this._elements = new JSV.SVSet();
    this.sv.trigger('selection-empty');
    return this
  }

  SVSelection.prototype.add_element = function(element) {
    this._elements.push(element);
    // Remove any previous highlighting
    this.sv.highlight.remove();
    this.sv.trigger('selection-add');
    return this
  }

  SVSelection.prototype.remove_element = function(element) {
    this._elements = new JSV.SVSet( this._elements.filter(function(e) { return e != element }) );
    this.sv.trigger('selection-remove');
    if (this.empty()) this.sv.trigger('selection-empty');
    return this
  }

  SVSelection.prototype.size = function() {
    return this._elements.length;
  }

  SVSelection.prototype.present = function() {
    return this._elements.length > 0;
  }

  SVSelection.prototype.empty = function() {
    return this._elements.length == 0;
  }

  SVSelection.prototype.contains = function(element) {
    return this._elements.contains(element);
  }

  SVSelection.prototype.delete = function() {
    this.peaks().each(function() { this.delete(); });
    this.update();
    this.sv.trigger('adjust-end');
    this.clear();
    this.sv.full_draw();
  }

  SVSelection.prototype.hide_popup_box = function() {
    // this.popup_box.style('display', 'none');
    this.popup_box.style('visibility', 'hidden');
  }

  SVSelection.prototype.show_popup_box = function() {
    var element = this.highlighted_element;
    // Increase popup width before adding text, so text_container is not compressed
    this.popup_box.style('width', '100%');
    var position = this.sv.axis_x_reverse ? 'right' : 'left'
    this.popup_box.style(position, this.sv.axis_y_gutter + JSV.pixel(2) + 'px');
    this.text_container.html(this.info_text());
    // this.popup_box.style('display', 'block').style('width', parseInt(this.text_container.style('width')) + 20);
    var box_width = this.text_container.node().offsetWidth + 20;
    this.popup_box.style('visibility', 'visible').style('width', box_width + 'px');
  }

  SVSelection.prototype.info_text = function() {
    var compound_text = '', cluster_text = '', peak_text = '';
    var compounds = this.compounds();
    var clusters = this.clusters();
    var peaks = this.peaks();
    if (compounds.length == 1) {
      var compound = compounds[0];
      compound_text = compound.name + ' [' + compound.display_concentration() + ']';
    } else {
      compound_text = compounds.length + ' compounds';
    }
    if (clusters.length == 1) {
      var cluster = clusters[0];
      cluster_text = 'cluster: ' + cluster.center().toFixed(3) + ' ppm';
    } else {
      cluster_text = clusters.length + ' clusters';
    }
    if (peaks.length == 1) {
      var peak = peaks[0];
      peak_text = 'peak: ' + peak.center.toFixed(3) + ' ppm';
    } else {
      peak_text = peaks.length + ' peaks';
    }
    text = '<p>' + compound_text + '</p><p>' + cluster_text + '</p><p>' + peak_text + '</p>';
    return text;
  }

  SVSelection.prototype.compounds = function(term) {
    var compounds = new JSV.SVSet();
    this.peaks().forEach(function(peak) {
      compounds.push(peak.compound());
    });
    return compounds.unique().get(term);
  }

  SVSelection.prototype.clusters = function(term) {
    var clusters = new JSV.SVSet();
    this.peaks().forEach(function(peak) {
      clusters.push(peak.cluster());
    });
    return clusters.unique().get(term);
  }

  SVSelection.prototype.adjust_concentration = function() {
    return this.element_type == 'compound' || this.constrain_adjust == 'compound';
  }

  SVSelection.prototype.draw_adjust_handles = function(context, scale) {
    var self = this;
    var sv = this.sv;
    var spacer_direction;
    if (self.allow_adjust && self.allow_width_adjust && self.present()) {
      var spacer = this.handle_spacer;
      var handle_size = this.handle_size;
      var domains = sv.get_peak_domains(self.peaks(), true);
      var width_min = scale.x(d3.min(domains[0]));
      var width_max = scale.x(d3.max(domains[0]));
      if (sv.axis_x_reverse) {
        this.handle_min_x = width_min + spacer;
        this.handle_max_x = width_max - spacer - handle_size;
        spacer_direction = -1;
      } else {
        this.handle_min_x = width_min - spacer - handle_size;
        this.handle_max_x = width_max + spacer;
        spacer_direction = 1;
      }
      var half_height = (scale.y(d3.max(domains[1]) / 2));
      this.handle_y = half_height - (handle_size / 2);

      context.translate(0.5, 0.5);
      // Handles
      context.beginPath();
      context.strokeStyle = 'rgba(50, 50, 50, 0.9)';
      context.rect(this.handle_min_x, this.handle_y, handle_size, handle_size);
      context.rect(this.handle_max_x, this.handle_y, handle_size, handle_size);
      context.stroke();

      // Line to Handles
      context.beginPath();
      context.strokeStyle = 'rgba(150, 150, 150, 0.5)';
      context.moveTo(width_min, half_height);
      context.lineTo(width_min - (spacer * spacer_direction), half_height);
      context.moveTo(width_max, half_height);
      context.lineTo(width_max + (spacer * spacer_direction), half_height);

      // context.moveTo(width_x1 - spacer, half_height);
      // context.lineTo(width_x1, half_height);
      // context.moveTo(width_x2, half_height);
      // context.lineTo(width_x2 + spacer, half_height);
      context.stroke();

      context.translate(-0.5, -0.5);
    }
  }

  // Draw bounds of single selected element, if the element
  // has lower_bound and upper_bound properties and a center function.
  SVSelection.prototype.draw_bounds = function(context, scale) {
    var self = this;
    var size = JSV.pixel(10);
    var sv = this.sv;
    if ( should_draw_bounds() ) {
      var element = this.elements(1);
      var domains = sv.get_peak_domains(self.peaks(), true);
      var y = scale.y.range()[0];
      var lower_x = scale.x(element.lower_bound);
      var upper_x = scale.x(element.upper_bound);
      var center = scale.x(element.center());

      context.save();
      context.translate(0.5, 0.5);
      context.beginPath();
      context.lineWidth = 2;
      // Set color
      if ( (element.center() < element.lower_bound) || (element.center() > element.upper_bound) ) {
        context.strokeStyle = 'red';
        context.fillStyle = 'red';
      } else {
        context.strokeStyle = 'blue';
        context.fillStyle = 'white';
      }
      // upper bound
      context.moveTo(lower_x, y);
      context.lineTo(lower_x + size, y - size);
      context.lineTo(lower_x + size, y + size);
      context.lineTo(lower_x, y);
      // lower bound
      context.moveTo(upper_x, y);
      context.lineTo(upper_x - size, y - size);
      context.lineTo(upper_x - size, y + size);
      context.lineTo(upper_x, y);
      // center line
      context.moveTo(center, y - size);
      context.lineTo(center, y + size);

      context.fill();
      context.stroke();

      context.translate(-0.5, -0.5);
      context.restore();
    }

    function should_draw_bounds() {
      var element = self.elements(1);
      return self.show_bounds &&
        self.size() == 1 &&
        typeof element.center == 'function' &&
        JSV.isNumeric( element.center() ) &&
        JSV.isNumeric( element.lower_bound ) &&
        JSV.isNumeric( element.upper_bound );
    }
  }

  SVSelection.prototype.mouse_in_handle = function() {
    var self = this;
    var sv = this.sv;
    var handle;
    if (self.allow_adjust && self.allow_width_adjust && self.present()) {
      var x = sv.scale.x(sv.mx);
      var y = sv.scale.y(sv.my);
      if ( (y > this.handle_y) && (y < this.handle_y + this.handle_size) ) {
        if ( (x > this.handle_max_x) && (x < this.handle_max_x + this.handle_size) ) handle = 'left';
        if ( (x > this.handle_min_x) && (x < this.handle_min_x + this.handle_size) ) handle = 'right';
      }
    }
    return handle;
  }

  SVSelection.prototype.mouse_in_selection = function() {
    var self = this;
    var sv = this.sv;
    if (self.present()) {
      var x = sv.scale.x(sv.mx);
      var y = sv.scale.y(sv.my);
      var y_for_mouse_x = JSV.sum_of_peaks(sv.mx, self.peaks());
      return (sv.my <= y_for_mouse_x && sv.my >= 0);
    }
  }

  SVSelection.prototype.update = function() {
    var paths = new JSV.SVSet();
    this.constrained_peaks().forEach(function(peak) {
      paths.push(peak, peak.cluster(), peak.compound(), peak.spectrum());
    });
    paths.unique().forEach(function(p) { p.update(); });
  }

  SVSelection.prototype.constrained_peaks = function() {
    var self = this;
    var peaks;
    if (self.constrain_adjust) {
      peaks = new JSV.SVSet();
      self.peaks().forEach(function(peak) {
        if (peak[self.constrain_adjust]) {
          peaks.merge(peak[self.constrain_adjust]().peaks());
        }
      });
      peaks = peaks.unique();
    } else {
      peaks = self.peaks();
    }
    return peaks;
  }

  /**
   * Initialize Selectiona and  Adjust Fit.
   */
  SVSelection.prototype.initialize_selection_events = function() {
    var sv = this.sv;
    var self = this;
    var x_mouse, y_mouse, dx_mouse, dy_mouse, dx_axis, dy_axis, selected_handle;
    var select_rect, added_elements, select_box_elements, x_start, y_start;
    var max_amplitude, new_amplitude;
    var do_not_clear;
    // var initial_selected_elements;

    sv.svg.on('mousedown.selection', function() {
      // initial_selected_elements = self.elements();
      do_not_clear = false;
      var down_x = d3.event.x;
      var down_y = d3.event.y;
      if (active_selection() && self.allow_adjust) {
        adjuststart();
        sv.svg.on('mousemove.fit', function() { adjusting() });
        sv.svg.on('mouseup.fit', function() {
          adjustend();
          sv.svg.on('mousemove.fit', null);
          sv.svg.on('mouseup.fit', null);
        })
      } else if (select_box_mode() && self.allow_multiselect) {
        selectstart();
        sv.svg.on('mousemove.fit', function() { selecting() });
        sv.svg.on('mouseup.fit', function() {
          selectend();
          sv.svg.on('mousemove.fit', null);
          sv.svg.on('mouseup.fit', null);
        })
      } else {
        sv.svg.on('mouseup.selection', function() {
          if (down_x == d3.event.x && down_y == d3.event.y) {
            if (create_peak_mode()) {
              create_peak();
            } else if (!do_not_clear) {
              self.clear();
            }
          }
          sv.svg.on('mouseup.selection', null);
        });
      }
      // if (create_peak_mode()) {
      //   sv.svg.on('mouseup.create_peak', function() {
      //     if (down_x == d3.event.x && down_y == d3.event.y) {
      //       create_peak();
      //     }
      //     sv.svg.on('mouseup.create_peak', null);
      //   });
      // }
    })

    // // Removing a peak must be done from cluster (a peak knows its cluster)
    // sv.svg.on('mousedown.create_peak', function() {
    //   var down_x = d3.event.x;
    //   var down_y = d3.event.y;
    //   if (create_peak_mode()) {
    //     sv.svg.on('mouseup.create_peak', function() {
    //       if (down_x == d3.event.x && down_y == d3.event.y) {
    //         create_peak();
    //       }
    //       sv.svg.on('mouseup.create_peak', null);
    //     });
    //   }
    // });

    function create_peak_mode() {
      // 65: 'a'
      return ( self.allow_peak_creation && ( sv.create_peak_mode || (sv.keyval() == '65') || (d3.event.altKey && d3.event.metaKey) ) );
    }

    function multi_select_mode() {
      // 83: 's'
      return ( self.allow_multiselect && ((sv.keyval() == '83') || (d3.event.altKey)) );
    }

    function select_box_mode() {
      return ( multi_select_mode() && !self.mouse_in_handle() && !find_element_mouse_over() );
    }

    function find_element_mouse_over() {
     return sv.find_element_mouse_over(self.element_type, self.restriction_spectrum_id, self.possible_elements, self.visible_only);
    }

    function active_selection() {
      if (self.mouse_in_handle()) return true;
      if (create_peak_mode()) return false;
      var active;

      var element = find_element_mouse_over();
      var mouse_in_selection = self.mouse_in_selection();

      if (element || mouse_in_selection) {
        // Multiselect Mode: remove or add elements
        if (multi_select_mode()) {
          if (self.contains(element)) {
            self.remove_element(element);
            active = false;
            do_not_clear = true;
          } else if (!mouse_in_selection) {
            self.add_element(element);
            active = true;
          }
        // Selecting new element
        } else if (!mouse_in_selection) {
            self.clear();
            self.add_element(element);
            // sv.trigger('element_selected');
            active = true;
        // Mouse is under sum line of selection
        } else {
          active = true;
        }
        // sv.full_draw();
        sv.fast_calc_draw();
      } else {
        // No element selected
        // self.clear();
        active = false
      }
      return active;
    }

    function adjuststart() {
      x_mouse = d3.event.x;
      y_mouse = d3.event.y;
      selected_handle = self.mouse_in_handle();
      sv.svg.style('cursor', 'grabbing');
    }

    function adjusting() {
      d3.event.stopPropagation(); // silence other listeners
      do_not_clear = true;
      dx_mouse = d3.event.x - x_mouse;
      dy_mouse = d3.event.y - y_mouse;
      x_mouse = d3.event.x
      y_mouse = d3.event.y
      dx_axis = sv.pixels_to_units_of_axis('x', dx_mouse);;
      dy_axis = sv.pixels_to_units_of_axis('y', dy_mouse);;
      if (selected_handle) {
        // Set width
        var direction = selected_handle == 'right' ? 1 : -1
        self.peaks().forEach(function(peak) {
          // TODO: Scale the width change to the maximum width
          peak.width = peak.width - (direction * dx_axis * 2);
          if (peak.width < 0.00001) peak.width = 0.00001;
        });
      } else {
        // Save original amplitudes for concentration adjustments
        if (self.adjust_concentration()) {
          self.compounds().forEach(function(c) {
            c.orig_amplitude = d3.max(c.peaks(), function(p) { return p.amplitude; });
          });
        }
        // Set amplitude
        // Scale the amplitude change to the maximum peak
        var peaks = self.constrained_peaks();
        max_amplitude = d3.max(peaks, function(p) { return p.amplitude; });
        if (max_amplitude <= 0) max_amplitude = 0.00001;
        peaks.forEach(function(peak) {
          new_amplitude = peak.amplitude + ( dy_axis / max_amplitude * peak.amplitude );
          peak.set_amplitude(new_amplitude);
        });
        // Set center
        self.peaks().forEach(function(peak) {
          peak.set_center(peak.center + dx_axis);
        });
        // Adjust concentrations
        if (self.adjust_concentration()) {
          self.compounds().forEach(function(c) {
            var highest_amplitude = d3.max(c.peaks(), function(p) { return p.amplitude; });
            c.concentration = c.concentration * highest_amplitude / c.orig_amplitude;
          });
        }
      }
      // TODO: scroll viewer when dragging selection outside of view
      // sv.calc_draw();
      sv.fast_calc_draw();
    }


    function adjustend() {
      self.update();
      sv.svg.style('cursor', 'grab');
      sv.trigger('adjust-end');
      sv.full_draw();
    }

    function selectstart() {
      // if (self.empty()) self._elements = initial_selected_elements;
      added_elements = new JSV.SVSet();
      var mouse_pos = d3.mouse(sv.canvas);
      x_start = mouse_pos[0];
      y_start = mouse_pos[1];
      select_rect = sv.svg.append('rect').
        attr('class', 'select-box').
        attr('x', x_start).attr('y', y_start).attr('width', 0).attr('height', 0);
    }

    function selecting() {
      d3.event.stopPropagation(); // silence other listeners
      do_not_clear = true;
      var mouse_pos = d3.mouse(sv.canvas);
      x_new = mouse_pos[0];
      y_new = mouse_pos[1];
      var select_x = d3.min([x_start, x_new]);
      var select_y = d3.min([y_start, y_new]);
      var width = Math.abs(x_new - x_start);
      var height = Math.abs(y_new - y_start);
      select_rect.attr('x', select_x).attr('y', select_y).
        attr('width', width).attr('height', height);
      var select_box_elements = elements_in_select_box(select_x, select_y, width, height);
      // Add new elements to selection
      select_box_elements.forEach(function(element) {
        if (!self.contains(element)) {
          self.add_element(element);
          added_elements.push(element);
        }
      });
      // Remove elements that are no longer selected
      added_elements = new JSV.SVSet(added_elements.filter(function(element) {
        if (!select_box_elements.contains(element)) {
          self.remove_element(element);
          return false;
        }
        return true;
      }));
      sv.fast_draw();
    }

    function selectend() {
      select_rect.remove();
      sv.full_draw();
    }

    function elements_in_select_box(x, y, width, height) {
      var selected_elements = new JSV.SVSet();
      var x_plot;
      var y_plot = sv.scale.y.invert( JSV.pixel(y + height) );
      var elements = sv.elements(self.element_type, self.restriction_spectrum_id, self.possible_elements, self.visible_only);
      for (var e = 0, len = elements.length; e < len; e++) {
        var element = elements[e];
        for (var i = 0; i < width; i++) {
          x_plot = sv.scale.x.invert( JSV.pixel(x + i) );
          if (y_plot < JSV.sum_of_peaks(x_plot, element.peaks())) {
            selected_elements.push(element);
            break;
          }
        }
      }
      return selected_elements;
    }

    function create_peak() {
      var cluster, width;
      if (self.present()) {
        // Add peak to cluster of first peak in selection
        // TODO: change to cluster of closest peak to mouse click
        cluster = self.peaks(1).cluster();
        width = d3.mean(self.peaks(), function(p) { return p.width});
      } else {
        // Add peak to new cluster of first compound of first spectra (or to restriction_spectrum)
        var spectrum;
        if (self.restriction_spectrum_id) {
          spectrum = sv.spectra(self.restriction_spectrum_id);
        } else {
          spectrum = sv.spectra().filter(function(s) { return s.compounds().present(); })[0];
        }
        cluster = new JSV.Cluster();
        spectrum.compounds(1).add_cluster(cluster);
        if (spectrum.peaks().present()) {
          width = d3.mean(spectrum.peaks(), function(p) { return p.width; });
        } else {
          width = 0.003;
        }
      }
      var peak = new JSV.Peak({ width: width, center: 0});
      cluster.add_peak(peak);
      var mouse_xy = sv.mouse(sv.canvas);
      peak.set_center(sv.scale.x.invert(mouse_xy[0]));
      peak.set_amplitude(sv.scale.y.invert(mouse_xy[1]));
      if (self.element_type == 'peak') {
        self.add_element(peak);
      } else if (self.empty() && self.element_type == 'cluster') {
        self.add_element(cluster);
      } else if (self.empty() && self.element_type == 'compound') {
        self.add_element(cluster.compound());
      }
      adjustend();
    }
  }
  /** @ignore */

  JSV.SVSelection = SVSelection;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Highlighter
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function SVHighlighter(sv, options) {
    this.sv = sv;
    this.element_type = options.element_type;
    this.restriction_spectrum_id = options.restriction_spectrum_id;
    this.possible_elements = options.possible_elements;
    this.visible_only = JSV.default_for(options.visible_only, true);
    this.text_display = JSV.default_for(options.text_display, true);
    display_defaults = { lineWidth: 3, visible: true };
    this.display = JSV.merge(display_defaults, options.display);
    // this.popup_box = this.sv.sv_wrapper.append('div').attr('class', 'jsv-highlight-popup-box').style('display', 'none');
    this.popup_box = this.sv.sv_wrapper.append('div').attr('class', 'jsv-highlight-popup-box').style('visibility', 'hidden');
    this.text_container = this.popup_box.append('div').attr('class', 'jsv-highlight-text-container');
  }

  SVHighlighter.prototype.hover = function() {
    var sv = this.sv;
    if (this.element_type) {
      var old_element = this.highlighted_element;
      var element = sv.find_element_mouse_over(this.element_type, this.restriction_spectrum_id, this.possible_elements, this.visible_only);
      if ( sv.selection.mouse_in_selection() || sv.selection.mouse_in_handle() ) {
        element = undefined;
      }
      if (old_element != element) {
        this.highlighted_element = element;
        // Remove previous highlighting
        if (old_element) {
          old_element.display_settings(this.saved_display_settings);
          this.hide_popup_box();
        }
        // Highlight new element
        if (element) {
          this.saved_display_settings = element.display_settings();
          element.display_settings(this.display);
          this.show_popup_box();
        }
        // sv.calc_draw();
        sv.full_draw();
      }
    }
  }

  SVHighlighter.prototype.remove = function() {
    if (this.highlighted_element) {
      this.highlighted_element.display_settings(this.saved_display_settings);
      this.highlighted_element = undefined;
      this.hide_popup_box();
    }
  }

  SVHighlighter.prototype.hide_popup_box = function() {
    this.sv.trigger('highlight-end');
    // this.popup_box.style('display', 'none');
    this.popup_box.style('visibility', 'hidden');
  }

  SVHighlighter.prototype.show_popup_box = function() {
    var element = this.highlighted_element;
    var text = '';
    if (this.text_display === true) {
      text = this.default_text();
    } else if (typeof this.text_display == 'string') {
      text = this.parsed_text();
    }
    this.sv.trigger('highlight-start');
    if (this.text_display) {
      // Increase popup width before adding text, so text_container is not compressed
      this.popup_box.style('width', '100%');
      this.text_container.html(text);
      // this.popup_box.style('display', 'block').style('width', parseInt(this.text_container.style('width')) + 20);
      var box_width = this.text_container.node().offsetWidth + 20;
      // Alter position if menu is showing
      var top = this.sv.menu.visible() ? this.sv.menu.height() + 5 : 15 ;
      // Show
      this.popup_box.style('visibility', 'visible')
        .style('top', top + 'px')
        .style('width', box_width + 'px');
    }
  }

  SVHighlighter.prototype.default_text = function() {
    var text = '';
    if (this.element_type == 'peak') {
      text = 'Peak: ' + d3.round(this.highlighted_element.center, 3) + ' ppm';
    } else if (this.element_type == 'cluster') {
      text = 'Cluster: ' + d3.round(this.highlighted_element.center(), 3) + ' ppm';
    } else if (this.element_type == 'compound') {
      text = this.highlighted_element.name;
    } else if (this.element_type == 'spectrum') {
      text = this.highlighted_element.name;
    }
    return text;
  }


  SVHighlighter.prototype.parsed_text = function() {
    var element = this.highlighted_element;
    var parser = function(match, p1, p2) {
      var text;
      if (p1 == 'p') {
        text = element[p2];
      } else if (p1 == 'f') {
        text = element[p2]();
      } else if (p1 == 'm') {
        text = element.meta[p2];
      }
      return text;
    }
  // 'bob#{a:1}test#{b:2}'.replace(/\#\{(.):(.*?)\}/g, function(match, p1, p2) {return ' - ' + p2 + ' - '})
    return this.text_display.replace(/#\{(.):(.*?)\}/g, parser);
  }

  JSV.SVHighlighter = SVHighlighter;

})(JSpectraViewer);



//////////////////////////////////////////////////////////////////////////////
// SpectraViewer ClusterNavigator
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function SVClusterNavigator(sv, id) {
    var self = this;
    this.sv = sv;
    this.id = id;
    this.speed = 400;
    this.active_ids = [];
    this.container = d3.select(id);
    this.table = this.container.append('table').style('display', 'none')
      .attr('class', 'jsv-cluster-navigator');
    this.thead = this.table.append('thead');
    // this.thead.html('<tr><th>Compound</th><th>Concentration</th><th>Clusters</th></tr>')
    this.tbody = this.table.append('tbody');

    sv.on('selection-add.cluster-navigator', function() {
      self.table.style('display', 'block');
      self.update_table();
    });
    if (sv.selection.present()) sv.trigger('selection-add.cluster-navigator');

    sv.on('selection-remove.cluster-navigator', function() {
      self.update_table();
    });

    sv.on('adjust-end.cluster-navigator', function() {
      self.update_table();
    });

    sv.on('selection-empty.cluster-navigator', function() {
      self.table.style('display', 'none');
    });


    d3.select('.jsv-cluster-navigator')
      .on('click', function() {
        var target = d3.select(d3.event.target);
        window.getSelection().removeAllRanges()
        if (target.classed('cluster-unit')) {
          self.move_to_cluster(target.node());
        } else if (target.classed('jsv-cluster-nav')) {
          self.cycle_clusters(target);
        }
      });
  }

  SVClusterNavigator.prototype.update_table = function() {
    this.tbody.html( update_rows(this.sv.selection.compounds()) );
    if (this.active_ids.length > 0) {
      d3.selectAll(this.active_ids.join(',')).classed('active', true);
    }
  }

  var update_rows = function(compounds) {
    return compounds.map(function(c) { return compound_row(c); }).join('');
  }

  var compound_row = function(compound) {
    return '<tr><td>'+ compound.name + '</td><td>' + compound.display_concentration() + '</td><td>' + cluster_cell(compound.clusters()) + '</td></tr>';
  }

  var cluster_cell = function(clusters) {
    clusters.order_by('center', true);
    var nav_arrows = "<span class='jsv-cluster-nav jsv-cluster-nav-left'>&#10094;</span>&nbsp;<span class='jsv-cluster-nav jsv-cluster-nav-right'>&#10095;</span>";
    return nav_arrows + clusters.map(function(c) { return cluster_span(c); }).join('');
  }
  var cluster_span = function(cluster) {
    return '<span class="cluster-unit" id="' + cluster.path_id() + '">' + cluster.center().toFixed(2) + '</span>';
  }

  SVClusterNavigator.prototype.cycle_clusters = function(target) {
    var forward = target.classed('jsv-cluster-nav-right');
    var parent = d3.select(target.node().parentNode);
    var active = parent.select('.active');
    var cluster_ids = parent.selectAll('.cluster-unit')[0].map(function(c) { return c.id; });
    var index = active.node() ? cluster_ids.indexOf(active.node().id) : -1;

    if (index < 0) {
      index = 0;
    } else if (forward) {
      index = (index + 1 < cluster_ids.length) ? index + 1 : 0;
    } else {
      index = (index - 1 < 0) ? cluster_ids.length - 1 : index -1;
    }
    var new_cluster = parent.select('#' + cluster_ids[index])
    this.move_to_cluster(new_cluster.node());
  }

  SVClusterNavigator.prototype.move_to_cluster = function(cluster_node) {
    var cluster = sv.clusters(cluster_node.id);
    sv.move_to_peaks(cluster.peaks(), this.speed, 2);
    d3.select(cluster_node.parentNode).selectAll('.cluster-unit').classed('active', false);
    d3.select(cluster_node).classed('active', true);
    this.active_ids = d3.selectAll('.cluster-unit.active')[0].map(function(c) { return '#' + c.id; });
    sv.selection.clear();
    sv.selection.add_element(cluster);
  }

  SVClusterNavigator.prototype.detach = function() {
    this.sv.off('.cluster-navigator');
    this.table.remove();
  }

  JSV.SVClusterNavigator = SVClusterNavigator;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Menu
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  // NOTE: need to explicitly state menu and handle sizes here and not just in CSS
  // in order to work with hidden elements like tabs
  function SVMenu(sv) {
    var self = this;
    this.sv = sv;
    this.slide_time = 500;
    this._visible = true;
    this.menu = sv.sv_wrapper.append('div')
      .style('visibility', 'hidden')
      .attr('class', 'jsv-menu')
      .on('click', function() { window.getSelection().removeAllRanges() });

    this.menu_svg = this.menu.append('svg')
      .attr('width', this.width())
      .attr('height', this.height());

    this.handle = sv.sv_wrapper.append('div')
      .attr('class', 'jsv-menu-handle')
      .on('click', function() {
        if (self.opened()) {
          self.close();
        } else {
          self.open();
        }
      })
      .on('mouseover', function() { self.handle_mouseover(); })
      .on('mouseout', function() { self.handle_mouseout(); });

    // var handle_width = this.handle.node().offsetWidth;
    // var handle_height = this.handle.node().offsetHeight;
    var handle_width = 40;
    var handle_height = 12;

    this.handle_svg = this.handle.append('svg')
      .attr('width', handle_width)
      .attr('height', handle_height);

    this.stroke_width = 4
    this.handle_data_closed = [ {x: 0, y: 0}, {x: handle_width/2, y: handle_height - this.stroke_width}, {x: handle_width, y: 0} ];
    this.handle_data_opened = [ {x: 0, y: handle_height}, {x: handle_width/2, y: this.stroke_width}, {x: handle_width, y: handle_height} ];

    this.draw();
    sv.trigger('domain-change.menu');
    this.close(0);
  }

  SVMenu.prototype.visible = function(value) {
    if (arguments.length == 0) return this._visible;
    if (value) {
      this._visible = true;
      this.handle.style('visibility', 'visible');
      this.menu.style('visibility', 'visible');
    } else {
      this._visible = false;
      this.handle.style('visibility', 'hidden');
      this.menu.style('visibility', 'hidden');
    }
  }

  SVMenu.prototype.opened = function() {
    return (this.menu.style('visibility') == 'visible');
  }

  SVMenu.prototype.width = function() {
    // return this.menu.node().offsetWidth;
    // return this.menu.node().getBoundingClientRect().width;
    return 300;
  }

  SVMenu.prototype.height = function() {
    // return this.menu.node().offsetHeight;
    // return this.menu.node().getBoundingClientRect().height;
    return  41;
  }

  SVMenu.prototype.open = function(duration) {
    duration = JSV.default_for(duration, this.slide_time)
    this.menu.style('visibility', 'visible');
    this.menu.transition().duration(duration)
      .style('top', '0px')
      .style('opacity', 1);

    this.handle_path.transition().duration(duration).attr('d', line_function(this.handle_data_opened))
  }

  SVMenu.prototype.close = function(duration) {
    duration = JSV.default_for(duration, this.slide_time)
    this.menu.transition().duration(duration)
      .style('top', '-50px')
      .style('opacity', 0)
      .each('end', function() {
        d3.select(this).style('visibility', 'hidden');
      });

    this.handle_path.transition().duration(duration).attr('d', line_function(this.handle_data_closed))
  }

  SVMenu.prototype.handle_mouseover = function() {
    this.handle_path.transition().duration(200)
      .attr('stroke', 'black');
  }

  SVMenu.prototype.handle_mouseout = function() {
    this.handle_path.transition().duration(200)
      .attr('stroke', 'grey');
  }

  SVMenu.prototype.draw = function() {
    var sv = this.sv;
    var self = this;
    var timeout;
    var translate_px = 5;
    var mousedown_delay = 4;

    // Handle
    this.handle_path = this.handle_svg.append("path")
      .attr("d", line_function(this.handle_data_closed))
      .attr("stroke", "grey")
      .attr("stroke-width", this.stroke_width)
      .attr("fill", "none");

    // Scroll/Move Buttons
    var left_arrow_data = [ {x: 11, y: 4}, {x: 4, y: 15}, {x: 11, y: 26} ];
    var right_arrow_data = [ {x: 4, y: 4}, {x: 11, y: 15}, {x: 4, y: 26} ];

    var left_arrow = path(this.menu_svg, left_arrow_data);
    var right_arrow = path(this.menu_svg, right_arrow_data);

    this.nav_group = this.menu_svg.append('g');
    this.scroll_left_button = button(this.nav_group, 0, 0, 15, 30, left_arrow);
    this.scroll_right_button = button(this.nav_group, 17, 0, 15, 30, right_arrow);
    this.nav_group.attr('transform', 'translate(' + 7 + ',' + 4 + ')');

    this.scroll_left_button.on('mousedown', function() {
      if (d3.select(this).classed('disabled')) return;
      timeout = scroll_interval(sv, 'x', translate_px, mousedown_delay);
      return false;
    })

    this.scroll_right_button.on('mousedown', function() {
      if (d3.select(this).classed('disabled')) return;
      timeout = scroll_interval(sv, 'x', -translate_px, mousedown_delay);
      return false;
    })

    $(document).mouseup(function(){
      if (timeout) {
        clearInterval(timeout);
        sv.full_draw();
      }
    });

    // Zoom Buttons
    this.zoom_group = this.menu_svg.append('g');
    this.zoom_y_minus_button = button(this.zoom_group, 6, 18, 16, 16, minus_path(this.menu_svg));
    this.zoom_y_plus_button = button(this.zoom_group, 6, 0, 16, 16, plus_path(this.menu_svg));
    this.zoom_x_minus_button = button(this.zoom_group, 25, 9, 16, 16, minus_path(this.menu_svg));
    this.zoom_x_plus_button = button(this.zoom_group, 43, 9, 16, 16, plus_path(this.menu_svg));
    scale_path(this.zoom_group, 0, 0.5, 5, 34, 0);
    scale_path(this.zoom_group, 25.5, 32, 5, 34, -90);
    this.zoom_group.attr('transform', 'translate(' + 55 + ',' + 2 + ')');

    this.zoom_x_minus_button.on('click', function() {
      if (d3.select(this).classed('disabled')) return;
      var zoom_diff = sv.scale.x.diff() / 2;
      var new_domains =  [ [sv.scale.x.min() - zoom_diff, sv.scale.x.max() + zoom_diff], sv.scale.y.domain() ];
      sv.move_to(new_domains)
    })

    this.zoom_x_plus_button.on('click', function() {
      if (d3.select(this).classed('disabled')) return;
      var zoom_diff = sv.scale.x.diff() / 4;
      var new_domains =  [ [sv.scale.x.min() + zoom_diff, sv.scale.x.max() - zoom_diff], sv.scale.y.domain() ];
      sv.move_to(new_domains)
    })

    this.zoom_y_minus_button.on('click', function() {
      if (d3.select(this).classed('disabled')) return;
      var zoom_diff = sv.scale.y.diff() / 2;
      var new_domains =  [ sv.scale.x.domain(), [sv.scale.y.min() - zoom_diff, sv.scale.y.max() + zoom_diff] ];
      sv.move_to(new_domains)
    })

    this.zoom_y_plus_button.on('click', function() {
      if (d3.select(this).classed('disabled')) return;
      var zoom_diff = sv.scale.y.diff() / 4;
      var new_domains =  [ sv.scale.x.domain(), [sv.scale.y.min() + zoom_diff, sv.scale.y.max() - zoom_diff] ];
      sv.move_to(new_domains)
    })

    // Set button disabled status
    sv.on('domain-change.menu', function() {
      if (sv.zoom_x == 1) {
        self.zoom_x_minus_button.classed('disabled', true);
      } else if (sv.zoom_x >= sv.zoom_max) {
        self.zoom_x_plus_button.classed('disabled', true);
      } else {
        self.zoom_x_minus_button.classed('disabled', false);
        self.zoom_x_plus_button.classed('disabled', false);
      }
      if (sv.zoom_y == 1) {
        self.zoom_y_minus_button.classed('disabled', true);
      } else if (sv.zoom_y >= sv.zoom_max) {
        self.zoom_y_plus_button.classed('disabled', true);
      } else {
        self.zoom_y_minus_button.classed('disabled', false);
        self.zoom_y_plus_button.classed('disabled', false);
      }
      if (sv.scale.x.min() == sv.boundary.x.min()) {
        self.scroll_right_button.classed('disabled', true);
      } else {
        self.scroll_right_button.classed('disabled', false);
      }
      if (sv.scale.x.max() == sv.boundary.x.max()) {
        self.scroll_left_button.classed('disabled', true);
      } else {
        self.scroll_left_button.classed('disabled', false);
      }
    });


    // Help Button
    help_icon = this.menu_svg.append('text')
      .attr({
        x: 15,
        y: 24,
        'font-family': 'sans-serif',
        'font-size': '26px',
        'stroke-width': 1,
        'fill': 'black',
        'class': 'jsv-button-text'
      })
      .style('text-anchor', 'middle' )
      .text('?');
    this.help_button = button(this.menu_svg, 260, 4, 30, 30, help_icon);

    this.help_button.on('click', function() {
      sv.help.dialog.open();
    })

    // Save/Download Button
    download_group = download_path(this.menu_svg)
      .attr('transform', 'translate(5,7)');

    this.download_button = button(this.menu_svg, 220, 4, 30, 30, download_group);
    this.download_dialog = new JSV.SVDialog(sv, {
      header_text: 'Save Image',
      content_text: download_html(sv),
      buttons: {
        'Cancel': function() { this.close(); },
        'Generate': function() { download_image(sv, this); }
      }, width: 400,
      height: 250
    });

    this.download_button.on('click', function() {
      self.download_dialog.open();
    })

    // Settings Button
    settings_group = settings_path(this.menu_svg)
      .attr('transform', 'translate(5,5)');

    this.settings_button = button(this.menu_svg, 180, 4, 30, 30, settings_group);

    this.settings_button.on('click', function() {
      sv.settings.open();
    })

    // JSV Button
    // TODO: add link to JSV website when available
    jsv_icon = this.menu_svg.append('text')
      .attr({
        x: 149,
        y: 32,
        'font-family': 'sans-serif',
        'font-size': '16px',
        'font-weight': 'bold',
        'stroke-width': 1,
        'fill': 'grey',
        'class': 'jsv-button-text'
      })
      .style('text-anchor', 'middle' )
      .text('JSV');

  }

  var scroll_interval = function(sv, axis, translate_px, delay) {
    return setInterval(function() {
      sv.translate_axis(axis, translate_px);
      sv.fast_draw();
    }, delay)
  }

  var path = function(svg, path_data) {
    return svg.append('path')
      .attr('d', line_function(path_data))
      .attr('stroke', 'black')
      // .attr('stroke-linecap', 'round')
      .attr("stroke-width", 3)
      .attr("fill", "none");
  }

  var plus_path = function(svg) {
    var group = svg.append('g');
    group.append('line')
      .attr('x1', 3)
      .attr('y1', 8)
      .attr('x2', 13)
      .attr('y2', 8)
      .attr('stroke-width', 3)
      .attr('stroke', 'black');

    group.append('line')
      .attr('x1', 8)
      .attr('y1', 3)
      .attr('x2', 8)
      .attr('y2', 13)
      .attr('stroke-width', 3)
      .attr('stroke', 'black');

    return group;
  }

  var minus_path = function(svg) {
    return svg.append('line')
      .attr('x1', 3)
      .attr('y1', 8)
      .attr('x2', 13)
      .attr('y2', 8)
      .attr('stroke-width', 3)
      .attr('stroke', 'black');
  }

  var scale_path = function(svg, x, y, width, height, angle) {
    var group = svg.append('g');
    var stroke_width = 1;
    var gap = 2;
    var y1_with_gap = y + gap;
    var y2_with_gap = y + height - gap;
    var head_len = 2;
    var center = x + (width / 2);
    group.append('line').attr({
      x1: x, y1: y,
      x2: x + width, y2: y,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: x, y1: y + height,
      x2: x + width, y2: y + height,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: center, y1: y1_with_gap,
      x2: center, y2: y2_with_gap,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: center, y1: y1_with_gap,
      x2: center - head_len, y2: y1_with_gap + head_len,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: center, y1: y1_with_gap,
      x2: center + head_len, y2: y1_with_gap + head_len,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: center, y1: y2_with_gap,
      x2: center - head_len, y2: y2_with_gap - head_len,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: center, y1: y2_with_gap,
      x2: center + head_len, y2: y2_with_gap - head_len,
      'stroke-width': stroke_width
    });

    group
      .attr('stroke', 'rgb(150, 150, 150)')
      .attr('transform', 'rotate(' + angle + ',' + x + ',' + y + ')');
    return group;
  }

  var settings_path = function(svg) {
    var group = svg.append('g');
    var stroke_width = 4;
    group.append('circle').attr({
      cx: 10, cy:10, r: 7,
    }).style('fill', 'rgb(75, 75, 75');
    group.append('line').attr({
      x1: 10, y1: 1,
      x2: 10, y2: 19,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: 1, y1: 10,
      x2: 19, y2: 10,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: 3.5, y1: 3.5,
      x2: 16.5, y2: 16.5,
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: 16.5, y1: 3.5,
      x2: 3.5, y2: 16.5,
      'stroke-width': stroke_width
    });
    group.append('circle').attr({
      cx: 10, cy:10, r: 3,
    }).style('fill', 'white');

    // group.append('line').attr({
    //   x1: 10, y1: 3,
    //   x2: 10, y2: 17,
    //   'stroke-width': stroke_width
    // });
    // group.append('line').attr({
    //   x1: 3, y1: 10,
    //   x2: 17, y2: 10,
    //   'stroke-width': stroke_width
    // });
    // group.append('line').attr({
    //   x1: 5, y1: 5,
    //   x2: 15, y2: 15,
    //   'stroke-width': stroke_width
    // });
    // group.append('line').attr({
    //   x1: 15, y1: 5,
    //   x2: 5, y2: 15,
    //   'stroke-width': stroke_width
    // });
    // group.append('circle').attr({
    //   cx: 10, cy:10, r: 5,
    // }).style('fill', 'rgb(75, 75, 75');
    // group.append('circle').attr({
    //   cx: 10, cy:10, r: 3,
    // }).style('fill', 'white');

    return group;
  }

  var download_path = function(svg) {
    var group = svg.append('g');
    var stroke_width = 3;
    group.append('line').attr({
      x1: 10, y1: 0,
      x2: 10, y2: 12,
      'stroke-linecap': 'round',
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: 6, y1: 7,
      x2: 10, y2: 12,
      'stroke-linecap': 'round',
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: 14, y1: 7,
      x2: 10, y2: 12,
      'stroke-linecap': 'round',
      'stroke-width': stroke_width
    });
    group.append('line').attr({
      x1: 2, y1: 16,
      x2: 18, y2: 16,
      'stroke-linecap': 'round',
      'stroke-width': stroke_width
    });

    return group;
  }

  var button = function(svg, x, y, width, height, path_group) {
    var button_group = svg.append('g').attr('class', 'jsv-menu-button');
    button_group.append('rect')
      .attr({
        x: 0,
        y: 0,
        width: width,
        height: height,
        rx: 2,
        ry: 2
      })
      .style({
        'stroke-width': 1
      });
    
    var path = path_group.remove();
    button_group.append('g').
      attr('class', 'jsv-button-image').
      append(function() { return path.node(); });

    button_group.attr('transform', 'translate(' + x + '.5,' + y + '.5)')

    return button_group;
  }

  var line_function = d3.svg.line()
    .x(function(d) { return d.x; })
    .y(function(d) { return d.y; })
    .interpolate("linear");

  var download_html = function(sv) {
    return   '' +
    '<div class="jsv-alert">Display the viewer image in a new window to download or print. Note that you must allow pop-ups!</div>' +
    '<div><label class="jsv-label">Width</label><div class="jsv-input-group">' + 
    '<input class="jsv-input" id="jsv-save-width" type="text" value="' + sv.width + '" /><div class="jsv-input-addon">px</div></div></div>' +
    '<div><label class="jsv-label">Height</label><div class="jsv-input-group">' + 
    '<input class="jsv-input" id="jsv-save-height" type="text" value="' + sv.height + '" /><div class="jsv-input-addon">px</div></div></div>';
  }

  var download_image = function(sv, dialog) {
    var height = sv.sv_wrapper.select('#jsv-save-height').property('value');
    var width = sv.sv_wrapper.select('#jsv-save-width').property('value');
    var image = sv.image(width, height);
    var window_name = 'JSV-Image-' + width + 'x' + height;
    var win = window.open(image, window_name);
    dialog.close();
    setTimeout(function() { win.document.title = window_name }, 100);
  }



  JSV.SVMenu = SVMenu;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Help
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function SVHelp(sv) {
    var self = this;
    this.sv = sv;

    this.dialog = new JSV.SVDialog(sv, {
      header_text: 'JSpectraViewer (JSV) Help',
      content_text: help_text,
      width: 700,
      height: 350
    });

  }

  var help_text = '' +
    'The view of the spectra can be scrolled around or scaled using the controls in the menu or by using the various mouse and keyboard shortcuts:' + 
    '<h3>Viewer Controls</h3>' +
    '<table class="jsv-table">' +
    '<thead><tr><th>Action</th><th>Command</th></tr></thead><tbody>' +
    '<tr><td>Zoom In/Out X Axis</td><td>Scroll wheel</td></tr>' +
    '<tr><td>Zoom In/Out Y Axis</td><td>Shift Key + Scroll wheel</td></tr>' +
    '<tr><td>Zoom In on Area</td><td>Shift Key + Click and Drag around area</td></tr>' +
    '<tr><td>Zoom Out Completely</td><td>Shift Key + Click once anywhere on viewer </td></tr>' +
    '<tr><td>Move Around</td><td>Click and Drag</td></tr></tbody></table>' +
    '<h3>Zoombox Controls (box in upper left corner)</h3>' +
    '<table class="jsv-table">' +
    '<thead><tr><th>Action</th><th>Command</th></tr></thead><tbody>' +
    '<tr><td>Move Around</td><td>Click and Drag grey selection box</td></tr>' +
    '<tr><td>Zoom In on Area</td><td>Click on unselected region and drag around new selection</td></tr>' +
    '<tr><td>Zoom Out Completely</td><td>Click once anywhere in unselected region</td></tr>' +
    '<tr><td>Alter Zoomed Area</td><td>Click and Drag on sides of grey selection box</td></tr></tbody></table>' +
    '<h3>Troubleshooting</h3>' +
    '<p>If the viewer is not showing any spectra or is slow, try updating to the latest version of your ' +
    'browser. We have found that <a href="https://www.google.com/chrome" target="_blank">Google Chrome</a> is the fastest.</p></div></div>';

  JSV.SVHelp = SVHelp;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Dialog
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function SVDialog(sv, options) {
    options = options || {};
    var self = this;
    this.wrapper = sv.sv_wrapper.node();

    this.fade_time = JSV.default_for(options.fade_time, 500);
    this.header_text = JSV.default_for(options.header_text, '');
    this.content_text = JSV.default_for(options.content_text, '');
    this.height = JSV.default_for(options.height, 300);
    this.width = JSV.default_for(options.width, 300);
    this.buttons = options.buttons

    this.box = d3.select(this.wrapper).append('div')
      .style('display', 'none')
      .attr('class', 'jsv-dialog');

    this.header = this.box.append('div')
      .attr('class', 'jsv-dialog-header')
      .html(this.header_text);

    this.dismiss = this.box.append('div')
      .attr('class', 'jsv-dialog-dismiss')
      .html('X')
      .on('click', function() { self.close(); });

    this.contents = this.box.append('div')
      .attr('class', 'jsv-dialog-contents jsv-scroll');

    if (this.buttons) {
      this.footer = this.box.append('div')
        .attr('class', 'jsv-dialog-footer');
      this.generate_buttons();
    }

    this.contents.html(this.content_text);

    this.adjust_size();

    return self;
  }

  SVDialog.prototype.visible = function() {
    return (this.box.style('display') != 'none');
  }


  SVDialog.prototype.open = function() {
    this.adjust_size();
    this.box.style('display', 'block');
    this.box.transition().duration(this.fade_time)
      .style('opacity', 1);
    return this;
  }

  SVDialog.prototype.close = function() {
    this.box.transition().duration(this.fade_time)
      .style('opacity', 0)
      .each('end', function() {
        d3.select(this).style('display', 'none');
      });
    return this;
  }

  SVDialog.prototype.generate_buttons = function() {
    var self = this;
    var labels = Object.keys(this.buttons);
    labels.forEach(function(label) {
      self.footer.append('button')
        .html(label)
        .attr('class', 'jsv-button')
        .on('click', function() { self.buttons[label].call(self) });
    });

  }

  SVDialog.prototype.adjust_size = function() {
    // Minimum buffer between dialog and edges of container (times 2)
    var buffer = 50;
    var wrapper_width = this.wrapper.offsetWidth;
    var wrapper_height = this.wrapper.offsetHeight;
    var width = this.width;
    var height = this.height;

    if (this.height > wrapper_height - buffer) height = wrapper_height - buffer;
    if (this.width > wrapper_width - buffer) width = wrapper_width - buffer;

    var header_height = 40;
    var footer_height = this.buttons ? 35 : 0;
    var content_height = height - header_height - footer_height;

    this.box
      .style('width', width + 'px')
      .style('height', height + 'px')

    this.contents
      .style('height', content_height + 'px');
  }

  JSV.SVDialog = SVDialog;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// SpectraViewer Settings
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  function SVSettings(sv, options) {
    options = options || {};
    var self = this;
    this.sv = sv;
    this.object = JSV.default_for(options.object, sv);
    this.title = JSV.default_for(options.title, 'Settings');
    this.height = JSV.default_for(options.height, 400);
    this.width = JSV.default_for(options.width, 400);
    options.settings = options.settings || [];
    this.settings = [];
    options.settings.forEach(function(setting_options) {
      self.settings.push(new Setting(self.object, setting_options));
    });

    this.dialog = new JSV.SVDialog(sv, {
      header_text: this.title,
      content_text: this.settings_html(),
      buttons: {
        'Done': function() { this.close(); }
      },
      width: this.width,
      height: this.height
    });

    return this;
  }

  // The html and listeners are regerated every time the dialog is opened
  // to catch any changes that may have been programatically
  SVSettings.prototype.open = function() {
    var self = this;
    this.dialog.contents.html(this.settings_html());
    d3.selectAll('.jsv-setting')
      .on('change', function() {
        var input = d3.select(this);
        var setting = self.find( input.attr('id') );
        var value = input.property('checked');
        setting.value(value);
        self.sv.full_draw();
      });

    this.dialog.open();
    return this;
  }

  SVSettings.prototype.close = function() {
    this.dialog.close();
    return this;
  }

  SVSettings.prototype.find = function(id) {
    return this.settings.filter(function(s) { return s.id() == id; })[0];
  }

  SVSettings.prototype.settings_html = function() {
    html = '<div class="jsv-settings">';
    this.settings.forEach(function(setting) {
      html += '<div><label class="jsv-label">' + setting.label + '</label>';
      html += '<div class="jsv-input-group">';
      if (setting.type == 'boolean') {
        html += checkbox(setting);
      }
      html += '</div></div>';
    });
    html += '</div>';

    return html;
  }

  var checkbox = function(setting) {
    html = '';
    html += '<input type="checkbox" class="jsv-input jsv-setting" id="' + setting.id() + '"';
    if (setting.value()) {
      html += ' checked="true" ';
    }
    html +=  ' />';
    return html;
  }

  setting_id = 0;
  function Setting(object, options) {
    options = options || {};
    this.object = object;
    this.label = options.label;
    this.property = options.property;
    this.type = JSV.default_for(options.type, 'boolean');
    this.id();
  }

  Setting.prototype.id = function() {
    var new_id = generate_setting_id();
    this.id = function() { return new_id; }
    return new_id;
  }

  var generate_setting_id = function() {
    return 'jsv-setting-id-' + setting_id++;
  }

  Setting.prototype.value = function(value) {
    if (arguments.length == 0) {
      if (typeof this.property === 'function') {
        value = this.property.call(this.object);
      } else {
        value = this.object[this.property];
      }
    } else {
      if (typeof this.property === 'function') {
        this.property.call(this.object, value);
      } else {
        this.object[this.property] = value;
      }
      self.sv.full_draw();
    }
    return value;
  }

  JSV.SVSettings = SVSettings;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Zoombox
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  // ZOOMBOX
  function ZoomBox(sv) {
    JSV.pixel(10);
    var self = this;
    this.sv = sv;
    // Set up zoom box size
    var percentage = sv.zoombox_size || 10
    this.width = sv.width * percentage / 100;
    this.height = sv.height * percentage / 100;

    // Create SVG
    this.svg = sv.sv_wrapper.append('svg')
      .attr('width', this.width + 1)
      .attr('height', this.height + 1)
      .style('position', 'absolute')
      .style('top', 0)
      .style('left', sv.width - this.width - 1);

    if (sv.axis_x_reverse) {
      this.svg.style('left', 0);
    }

    // Set cursor for zoom box
    // this.svg.style('cursor', 'crosshair');

    // Draw Border
    this.border = this.svg.append('rect')
      .attr('x', 0)
      .attr('y', 0)
      .attr('width', this.width)
      .attr('height', this.height)
      .attr('fill', 'white')
      .attr('stroke', '#BBB');

    // Set initial scales
    // TODO: move to SVScale
    // this.scale = {
    //   x:d3.scale.linear()
    //     .range( [this.width, 0] ),
    //   y:d3.scale.linear()
    //     .range( [this.height, 0] ) }
    this.scale = new JSV.SVScale();
    this.scale.y.range([this.height, 0]);
    var x_range = this.sv.axis_x_reverse ? [this.width, 0] : [0, this.width];
    this.scale.x.range(x_range);


    // Brush for selecting a zoom area
    this.initialize_brushing();

    this.spectra_function = d3.svg.line()
      .x(function(d) { return this.scale.x(d.x); })
      .y(function(d) { return this.scale.y(d.y); })
      .interpolate('linear');
  }

  Object.defineProperty(ZoomBox.prototype, 'visible', {
    get: function() { return this._visible; },
    set: function(val) {
      this._visible = val;
      if (this._visible) {
        this.svg.style('display', 'block');
      } else {
        this.svg.style('display', 'none');
      }
      this.sv.legend.update();
      this.sv.draw();
    }
  });

  // ZOOM BOX BRUSHING
  ZoomBox.prototype.initialize_brushing = function() {
    var zb = this;
    var sv = zb.sv;
    zb.brush = d3.svg.brush()
      .x(zb.scale.x)
      .y(zb.scale.y)
      .on('brushstart', brushstart)
      .on('brush',      brushing)
      .on('brushend',   brushend);

     zb.select_brush = zb.svg.append("g")
          .attr("class", "active-brush")
          .call(zb.brush);

    function brushstart() {
      d3.event.sourceEvent.preventDefault(); // Prevent text cursor
    }

    function brushing() {
      if (!zb.brush.empty()) {
        if (sv.axis_y_lock !== false) {
          var ex = zb.brush.extent();
          var x1 = ex[0][0];
          var x2 = ex[1][0];
          // Set y1 to the min y domain
          var y1 = zb.scale.y.domain()[0];
          var mouse_y = zb.scale.y.invert(d3.mouse(zb.svg.node())[1]);
          var max_y = zb.scale.y.domain()[1];
          var y2 = Math.min(mouse_y, max_y);
          var y2 = Math.max(y1, y2);
          zb.brush.extent([[x1, y1], [x2, y2]]);
          // Redraw brush
          zb.svg.selectAll('.zoom-brush').call(zb.brush);
        }
        sv.set_domain_to_brush_extent(zb.brush.extent(), false);
        sv.fast_draw();

        // DEBUG INFO
        if (sv.debug) {
          sv.debug_data.zbox['X1'] = JSV.round(zb.brush.extent()[0][0]);
          sv.debug_data.zbox['Y1'] = JSV.round(zb.brush.extent()[0][1]);
          sv.debug_data.zbox['X2'] = JSV.round(zb.brush.extent()[1][0]);
          sv.debug_data.zbox['Y2'] = JSV.round(zb.brush.extent()[1][1]);
        }
      }
    }

    function brushend() {
      // If brush extent is empty, zoom out completely
      if (zb.brush.empty()) {
        zb.set_zoom_area(sv.boundary);
        sv.zoom_out_completely();
      }
      // Force setting next zoom to update zoom level
      sv.zoom_axis = '';

      sv.draw();
    }
  }

  ZoomBox.prototype.add_spectrum = function(spectrum) {
    var boundary = this.sv.boundary;
    this.scale.x.domain(boundary.x.domain());
    this.scale.y.domain(boundary.y.domain());

    this.svg.append('path')
      .attr('id', spectrum.id)
      .attr('d', this.spectra_function(spectrum.simple_xy_data.asArray()))
      .attr('stroke', spectrum.color)
      .attr('stroke-width', 0.5)
      .attr('fill', 'none');

    // Redraw brush
    this.set_zoom_area(boundary);
  }

  ZoomBox.prototype.remove_spectra = function(id) {
    this.sv.container.select('path#' + id).remove();
  }

  ZoomBox.prototype.update = function() {
    var self = this;
    var sv = this.sv;
    if (sv.axis_x_reverse) {
      this.svg.style('left', 0);
    } else {
      this.svg.style('left', sv.width - this.width - 1);
    }
    this.sv.all_spectra().forEach(function(spectrum) {
      self.remove_spectra(spectrum.id);
      if (spectrum.active) {
        self.add_spectrum(spectrum);
      }
    });
  }

  ZoomBox.prototype.set_zoom_area = function(scale) {
    var x1 = scale.x.domain()[0];
    var x2 = scale.x.domain()[1];
    var y1 = scale.y.domain()[0];
    var y2 = scale.y.domain()[1];
    this.brush.extent([[x1, y1], [x2, y2]]);
    // Redraw brush
    this.svg.selectAll('.active-brush').call(this.brush);
  }

  JSV.ZoomBox = ZoomBox;

})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Utils
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  /**
   * Return the _default_value_ if _value_ is undefined
   * @param {Obejct} value         Returned if it is defined
   * @param {Object} default_value Returned if _value_ is undefined
   * @return {Object}
   */
  JSV.default_for = function(value, default_value) {
    return (value === undefined) ? default_value : value;
  }

  // Default pixel ratio
  JSV.pixel_ratio = 1;

  // Converts pixels based on pixel ratio
  JSV.pixel = function(px) {
    return px * JSV.pixel_ratio;
  }

  JSV.get_pixel_ratio = function(canvas) {
    var context = canvas.getContext('2d');
    //  query the various pixel ratios
    var devicePixelRatio = window.devicePixelRatio || 1;

    var backingStoreRatio = context.webkitBackingStorePixelRatio ||
      context.mozBackingStorePixelRatio ||
      context.msBackingStorePixelRatio ||
      context.oBackingStorePixelRatio ||
      context.backingStorePixelRatio || 1;

    return devicePixelRatio / backingStoreRatio;
  }

  JSV.scale_resolution = function(canvas, ratio){
    // get the canvas and context
    var context = canvas.getContext('2d');

    // upscale the canvas if the two ratios don't match
    if (ratio != 1) {

      var oldWidth  = canvas.width;
      var oldHeight = canvas.height;

      canvas.width  = oldWidth  * ratio;
      canvas.height = oldHeight * ratio;

      canvas.style.width  = oldWidth  + 'px';
      canvas.style.height = oldHeight + 'px';
    }
  }

  // Function to create subclasses
  Function.prototype.inherits = function(parent) {
    this.prototype = Object.create(parent.prototype);
  };

  /**
   * Merges top level properties of each supplied object.
   * ```javascript
   * JSV.merge({a:1, b:1}, {b:2, c:2}, {c:3, d:3});
   * //=> {a: 1, b: 2, c: 3, d: 3}
   * ```
   * If a non object is provided, it is ignored. This can be useful if
   * merging function arguments that may be undefined.
   * @param {Object} object_1,object_2,..,object_n Objects to merge
   * @return {Object}
   */
  JSV.merge = function() {
    var data = {};
    var object, keys, key;
    for (var arg_i=0, arg_len=arguments.length; arg_i < arg_len; arg_i++) {
      object = arguments[arg_i];
      if (typeof object === 'object') {
        keys = Object.keys(object);
        for (var key_i=0, key_len=keys.length; key_i < key_len; key_i++){
          key = keys[key_i];
          data[key] = object[key];
        }
      }
    }
    return data;
  }

  /**
   * Returns a string id using the _id_base_ and _start_ while
   * making sure the id is not in _current_ids_.
   * ```javascript
   * JSV.unique_id('spectra_', 1, ['spectra_1', 'spectra_2']);
   * //=> 'spectra_3'
   * ```
   * @param {String} id_base Base of ids
   * @param {Integer} start Integer to start trying to creat ids with
   * @param {Array} current_ids Array of current ids
   * @return {String}
   */
  JSV.unique_id = function(id_base, start, current_ids) {
    var id;
    do {
      id = id_base + start;
      start++;
    } while (current_ids.indexOf(id) > -1);
    return id;
  }

  JSV.round = function(value, places) {
    var places = places || 2;
    return d3.round(value, places);
  }

  JSV.elapsed_time = function(old_time) {
    var elapsed = (new Date().getTime()) - old_time;
    return elapsed + ' ms';
  }

  // Binary search to find the index of data where data[index] equals search_value
  // If no element equals value, the returned index will be the upper or lower [default]
  // index that surrounds the value.
  //  - data: array of numbers. Must be sorted from lowest to highest
  //  - search_value: the value to search for
  //  - upper: only used if no element equals the value
  //           true: return index to right of value
  //           false: return index to left of value [default]
  JSV.index_of_value = function(data, search_value, upper) {
    var min_index = 0;
    var max_index = data.length - 1;
    var current_index, current_value;
    if (data[min_index] >= search_value) return min_index;
    if (data[max_index] <= search_value) return max_index;

    while (max_index - min_index > 1) {
      current_index = (min_index + max_index) / 2 | 0;
      current_value = data[current_index];
      if (current_value < search_value) {
        min_index = current_index;
      } else if (current_value > search_value){
        max_index = current_index;
      } else {
        return current_index;
      }
    }
    return (upper ? max_index : min_index);
  }

  JSV.lorentzian = function(xpos, center, width, amplitude) {
    var w2 = width * width;
    var shift = xpos - center;
    return amplitude * w2 / (w2 + (4 * shift * shift));
  }

  // Calculates the cumulative Y value from the peaks at xpos
  // Each peak must have the following properties
  // - center
  // - width
  // - amplitude
  JSV.sum_of_peaks = function(xpos, peaks) {
    var sum = 0;
    var peak;
    for (var i = 0; i < peaks.length; i++) {
      peak = peaks[i];
      sum += JSV.lorentzian(xpos, peak.center, peak.width, peak.amplitude);
    }
    return sum;
  };

  // Generate an array of x/y points for the provided peaks, making sure to
  // include the points at the peak amplitude for the peak within the limits
  // @peaks: a SVSet of peaks
  JSV.xy_from_peaks = function(peaks, number_of_points, lowerlim, upperlim, fast) {
    if (fast) peaks = JSV.peaks_in_range(peaks, lowerlim, upperlim, 6);
    var peaks_in_view = JSV.peaks_in_range(peaks, lowerlim, upperlim);
    // var centers = peaks.order_by('center').map(function(p) { return p.center; });
    var centers = peaks_in_view.order_by('center').map(function(p) { return p.center; });
    var xpos = lowerlim;
    var xmax = upperlim;
    var x = [ ], y = [ ];
    var delta = ((upperlim - lowerlim) / number_of_points);
    while (xpos < xmax) {
      x.push(xpos);
      y.push(JSV.sum_of_peaks(xpos, peaks));
      if (centers[0] && (centers[0] <= xpos + delta) ) {
        xpos = centers.shift();
      } else {
        xpos += delta;
      }
    }
    // Add last point
    x.push(upperlim);
    y.push(JSV.sum_of_peaks(upperlim, peaks));
    return {x:x, y:y};
  }

  JSV.peaks_in_range = function(peaks, lowerlim, upperlim, range) {
    range = range || 0;
    var new_peaks = new JSV.SVSet();
    peaks.forEach(function(peak) {
      if ( (JSV.peak_min([peak], range) < upperlim) && (JSV.peak_max([peak], range) > lowerlim ) ) {
        new_peaks.push(peak);
      }
    });
    // console.log(peaks.length + ' -> ' + new_peaks.length)
    return new_peaks
  }


  // TODO: this should be merged or extracted from get_peak_domains()
  // Returns the minimum x value for the supplied set of peaks.
  // First finds the minimum peak by center, then subtracts the
  // number of widths from the center.
  JSV.peak_min = function(peaks, widths) {
    widths = widths || 3;
    var min, best_peak;
    if (peaks.length > 0) {
      best_peak = peaks[0];
    }
    peaks.forEach(function(peak) {
      if (peak.center < best_peak.center) {
        best_peak = peak;
      }
    })
    if (best_peak) {
      min = best_peak.center - (widths * best_peak.width);
    }
    return min;
  }
  //
  // Returns the maximum x value for the supplied set of peaks.
  // First finds the maximum peak by center, then adds the
  // number of widths from the center.
  JSV.peak_max = function(peaks, widths) {
    widths = widths || 3;
    var max, best_peak;
    if (peaks.length > 0) {
      best_peak = peaks[0];
    }
    peaks.forEach(function(peak) {
      if (peak.center > best_peak.center) {
        best_peak = peak;
      }
    })
    if (best_peak) {
      max = best_peak.center + (widths * best_peak.width);
    }
    return max;
  }

  // Return a number for n or undefined
  JSV.number = function(n) {
    if (n === undefined) return;
    return Number(n);
  }

  JSV.isNumeric = function (n) {
    return isFinite(n) && parseFloat(n) == n;
  }

  JSV.decimalPlaces = function(num) {
    var match = (''+num).match(/(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/);
    if (!match) { return 0; }
    return Math.max(
               0,
               // Number of digits right of decimal point.
               (match[1] ? match[1].length : 0)
               // Adjust for scientific notation.
               - (match[2] ? +match[2] : 0));
  }

  // COLORS
  // http://krazydad.com/tutorials/makecolors.php
  JSV.colors = function(len, center, width, alpha, freq1, freq2, freq3,
                                   phase1, phase2, phase3) {
    var colors = [];
    if (len == undefined)      len    = 50;
    if (center == undefined)   center = 200;
    if (width == undefined)    width  = 30;
    if (alpha == undefined)    alpha  = 1;
    if (freq1 == undefined)    freq1  = 2.4;
    if (freq2 == undefined)    freq2  = 2.4;
    if (freq3 == undefined)    freq3  = 2.4;
    if (phase1 == undefined)   phase1 = 0;
    if (phase2 == undefined)   phase2 = 2;
    if (phase3 == undefined)   phase3 = 4;

    for (var i = 0; i < len; ++i) {
      var red   = Math.round(Math.sin(freq1*i + phase1) * width + center);
      var green = Math.round(Math.sin(freq2*i + phase2) * width + center);
      var blue  = Math.round(Math.sin(freq3*i + phase3) * width + center);
      colors.push('rgba(' + red + ',' + green + ',' + blue + ',' + alpha + ')');
    }
    return colors;
  }

  JSV.test_colors = function(colors) {
    colors.forEach(function(color) {
      document.write( '<font style="color:' + color + '">&#9608;</font>')
    })
    document.write( '<br/>')
  }

  /**
   * Convert JSON results from Bayesil to the proper format for JSV
   * @param {Object} bayesil_data Bayesil results as JSON
   * @return {Object}
   */
  JSV.convert_bayesil_data = function(bayesil_data) {
    var spectrum = {
      id: JSV.default_for(bayesil_data.id, 'Fit'),
      name: JSV.default_for(bayesil_data.name, 'Fit'),
      compounds: [ ],
      display: JSV.merge( { color: 'blue' }, bayesil_data.display),
      meta: { dss_concentration: bayesil_data.dss_conc }
    }
    bayesil_data.metabolites.forEach(function(compound_data) {
      var compound = {
        concentration: compound_data.concentration,
        concentration_units: '\u03BCM',
        id: compound_data.id,
        name: compound_data.name,
        clusters: [ ],
        display: { visible: false },
        meta: {
          dss_ratio: JSV.default_for(JSV.number(compound_data.dss_ratio), 1),
          threshold: compound_data.threshold,
          score: compound_data.score,
        }
      };
      compound.meta.amplitude_coefficient = compound.concentration / compound.meta.dss_ratio / spectrum.meta.dss_concentration;
      compound_data.clusters.forEach(function(cluster_data) {
        var cluster = {
          peaks: [ ],
          meta: { shift: cluster_data.shift }
        };
        cluster_data.shift = JSV.default_for(cluster_data.shift, 0);
        // cluster.center = cluster_data.center;
        cluster.upper_bound = cluster_data.upper_bound;
        cluster.lower_bound = cluster_data.lower_bound;
        cluster.meta.lower_bound_diff = cluster_data.lower_bound_diff;
        cluster.meta.upper_bound_diff = cluster_data.upper_bound_diff;
        cluster_data.peaks.forEach(function(peak_data) {
          var peak = {
            center: Number(peak_data.center) + Number(cluster_data.shift),
            width: Number(peak_data.width),
            amplitude: Number(peak_data.amplitude) * Number(compound.meta.amplitude_coefficient),
            meta: { initial_width: peak_data.initial_width }
          }
          cluster.peaks.push(peak);
        });
        compound.clusters.push(cluster);
      });
      spectrum.compounds.push(compound);
    });
    return spectrum;
  }

  /**
   * Convert spectra data to Bayesil JSON format
   * @param {Object} xy_spectrum The fid spectrum to extract xy data from
   * @param {Object} fit_spectrum The fit  spectrum to extract compound data from
   * @return {Object}
   */
  JSV.create_bayesil_json = function(xy_spectrum, fit_spectrum) {
    var bayesil = {
      dss_conc: fit_spectrum.meta.dss_concentration,
      metabolites: []
    }
    xy_spectrum = xy_spectrum || { xy_data: { x: [], y: [] } };
    fit_spectrum.compounds().forEach(function(compound) {
      var compound_data = {
        name: compound.name,
        id: compound.id,
        concentration: compound.concentration,
        dss_ratio: compound.meta.dss_ratio,
        score: compound.meta.score,
        threshold: compound.meta.threshold,
        clusters: []
      }
      compound.clusters().forEach(function(cluster) {
        var cluster_data = {
          center: cluster.center(),
          initial_center: cluster.meta.initial_center,
          shift: cluster.center(), // TEMP
          lower_bound: cluster.lower_bound,
          upper_bound: cluster.upper_bound,
          peaks: []
        }
        cluster.peaks().forEach(function(peak) {
          var peak_data = {
            amplitude: peak.amplitude / compound.meta.amplitude_coefficient,
            center: peak.center - cluster.center(),
            width: peak.width,
            initial_width: peak.meta.initial_width
          }
          if (isNaN(peak_data.amplitude)) peak_data.amplitude = 0;
          cluster_data.peaks.push(peak_data);
        });
        compound_data.clusters.push(cluster_data);
      });
      bayesil.metabolites.push(compound_data);
    });
    bayesil.spectrum_xy = {x: xy_spectrum.xy_data.x, y: xy_spectrum.xy_data.y};

    return bayesil;
  }

  /////////////////////////////////////////////////////////////////////////////
  // POLYFILLS
  /////////////////////////////////////////////////////////////////////////////
  if (!Number.isInteger) {
    Number.isInteger = function isInteger (nVal) {
      return typeof nVal === "number" && isFinite(nVal) && nVal > -9007199254740992 && nVal < 9007199254740992 && Math.floor(nVal) === nVal;
    };
  }


})(JSpectraViewer);


//////////////////////////////////////////////////////////////////////////////
// Simplify code
// Adapted from Simplify.js by Vladimir Agafonkin (Details are at the bottom)
//////////////////////////////////////////////////////////////////////////////
(function(JSV) {

  var _points = [ ];

  // square distance between 2 points
  function getSqDist(p1, p2) {

      var dx = _points.x[p1] - _points.x[p2],
          dy = _points.y[p1] - _points.y[p2];

      return dx * dx + dy * dy;
  }

  // square distance from a point to a segment
  function getSqSegDist(p, p1, p2) {

      var x = _points.x[p1],
          y = _points.y[p1],
          dx = _points.x[p2] - x,
          dy = _points.y[p2] - y;

      if (dx !== 0 || dy !== 0) {

          var t = ((_points.x[p] - x) * dx + (_points.y[p] - y) * dy) / (dx * dx + dy * dy);

          if (t > 1) {
              x = _points.x[p2];
              y = _points.y[p2];

          } else if (t > 0) {
              x += dx * t;
              y += dy * t;
          }
      }

      dx = _points.x[p] - x;
      dy = _points.y[p] - y;

      return dx * dx + dy * dy;
  }

  // basic distance-based simplification
  function simplifyRadialDist(pts, sqTolerance) {

      var prevPoint = pts[0],
          newPoints = [prevPoint],
          pt;

      for (var i = 1, len = pts.length; i < len; i++) {
          pt = pts[i];

          if (getSqDist(pt, prevPoint) > sqTolerance) {
              newPoints.push(pt);
              prevPoint = pt;
          }
      }

      if (prevPoint !== pt) newPoints.push(pt);

      return newPoints;
  }

  // simplification using optimized Douglas-Peucker algorithm with recursion elimination
  function simplifyDouglasPeucker(pts, sqTolerance) {

      var len = pts.length,
          MarkerArray = typeof Uint8Array !== 'undefined' ? Uint8Array : Array,
          markers = new MarkerArray(len),
          first = 0,
          last = len - 1,
          stack = [],
          newPoints = [],
          i, maxSqDist, sqDist, index;

      markers[first] = markers[last] = 1;

      while (last) {

          maxSqDist = 0;

          for (i = first + 1; i < last; i++) {
              sqDist = getSqSegDist(pts[i], pts[first], pts[last]);

              if (sqDist > maxSqDist) {
                  index = i;
                  maxSqDist = sqDist;
              }
          }

          if (maxSqDist > sqTolerance) {
              markers[index] = 1;
              stack.push(first, index, index, last);
          }

          last = stack.pop();
          first = stack.pop();
      }

      for (i = 0; i < len; i++) {
          if (markers[i]) newPoints.push(pts[i]);
      }

      return newPoints;
  }

  /**
   * Simplify.js is a high-performance JS polyline simplification library by Vladimir Agafonkin.
   * [Orignal Project](http://mourner.github.io/simplify-js)
   *
   * It has been adapted here to work with a different point format.
   * The format used in JSV is an object of arrays:
   * ```js
   * points = { x: [1, 2, 3], y: [10, 20, 30] };
   * ```
   * There can also be an optional yi array, to hold the imaginary component of
   * the line. This is used when incorporating phasing into the viewer.
   * 
   * The orginal format was an array of points:
   * ```js
   * // Do not use this format
   * points = [ {x:1, y:10}, {x:2, y:20}, {x:3, y:30} ];
   * ```
   * ***
   * Copyright (c) 2012, Vladimir Agafonkin
   * All rights reserved.
   *
   * Redistribution and use in source and binary forms, with or without modification, are
   * permitted provided that the following conditions are met:
   *
   *   1. Redistributions of source code must retain the above copyright notice, this list of
   *      conditions and the following disclaimer.
   *
   *   2. Redistributions in binary form must reproduce the above copyright notice, this list
   *      of conditions and the following disclaimer in the documentation and/or other materials
   *      provided with the distribution.
   *
   * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
   * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
   * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
   * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)
   *
   * @param {Object} points An object containing an x and y array of points. There may also be an yi array.
   * @param {Number} tolerance Affects the amount of simplification. [Default: 1]
   * @param {Boolean} highestQuality Excludes distance-based preprocessing step which leads to highest quality simplification but runs ~10-20 times slower. [Default: false]
   */
  function simplify(points, tolerance, highestQuality) {

      var length = points.x.length;
      if (length <= 1) return points;

      _points = points;
      var pts = new Array(length);
      for (var i=0; i < length; i++) {
        pts[i] = i;
      }

      var sqTolerance = tolerance !== undefined ? tolerance * tolerance : 1;

      pts = highestQuality ? pts : simplifyRadialDist(pts, sqTolerance);
      pts = simplifyDouglasPeucker(pts, sqTolerance);

      var x = new Array(pts.length);
      var y = new Array(pts.length);
      for (var i=0, len=pts.length; i < len; i++) {
        x[i] = _points.x[pts[i]];
        y[i] = _points.y[pts[i]];
      }
      var yi;
      if (_points.yi) {
        yi = new Array(pts.length);
        for (var i=0, len=pts.length; i < len; i++) {
          yi[i] = _points.yi[pts[i]];
        }
      }

      points = {x:x, y:y};
      if (yi) { points.yi = yi; }
      return points
  }
  /** @ignore */

  JSV.simplify = simplify;

})(JSpectraViewer);

JSpectraViewer.initialize();
