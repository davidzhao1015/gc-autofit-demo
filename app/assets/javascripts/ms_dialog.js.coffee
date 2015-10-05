$(window).load ->
  # TODO: only if ms-dialog on page
  margin = {top: 5, right: 30, bottom: 30, left: 65}
  height = 200 - margin.top - margin.bottom
  width = 600 - margin.left - margin.right

  x = d3.scale.linear()
      .range([0, width])

  y = d3.scale.linear()
      .range([height, 0])

  xAxis = d3.svg.axis()
         .scale(x)
         .orient("bottom")

  yAxis = d3.svg.axis()
          .scale(y)
          .orient("left")

  line = d3.svg.line()
           .x( (d) ->  x(d.mz) )
           .y( (d) ->  y(d.intensity) )


  svg = d3.select(".ms-dialog .modal-body").append("svg")
          .attr("width", width + margin.left + margin.right)
          .attr("height", height + margin.top + margin.bottom)
          .append("g")
          .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

  # Mass Spectrum Dialog
  window.sv.on 'label-click', (label) ->
    return unless label.meta.ms_data
    $('.ms-dialog').modal('show')
    $('.ms-dialog .ms-name').html(label.text + ' (' + label.meta.table_data['HMDB ID'] + ')')
    data = []
    for i in [0..label.meta.ms_data['m/z'].length - 1]
      data[i] = { mz: label.meta.ms_data['m/z'][i], intensity: label.meta.ms_data.Intensity[i] }

    x.domain( d3.extent(data, (d) -> d.mz ) )
    # y.domain( d3.extent(data, (d) -> d.intensity ) )
    y.domain([0, d3.max(label.meta.ms_data.Intensity) ])

    # Clear old data
    svg.selectAll('*').remove()

    svg.append("g")
       .attr("class", "x axis")
       .attr("transform", "translate(0," + height + ")")
       .call(xAxis)
       .append("text")
       .attr("x", width/2)
       .attr("y", 30)
       .style("text-anchor", "middle")
       .text("m/z")

    svg.append("g")
       .attr("class", "y axis")
       .call(yAxis)
       .append("text")
       .attr("transform", "rotate(-90)")
       .attr("y", -margin.left+10)
       .attr("x", -height/2)
       .style("text-anchor", "middle")
       .text("Intensity")

    svg.selectAll(".line")
       .data(data)
       .enter().append('line')
       .attr('x1', (d) -> x(d.mz))
       .attr('x2', (d) -> x(d.mz))
       .attr('y1', y(0))
       .attr('y2', (d) -> y(d.intensity))
       .attr("class", "line")


