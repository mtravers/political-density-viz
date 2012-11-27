
var width = 960,
height = 500;
s_size = 500;

var quantize = d3.scale.quantize()
    .domain([0, 100])
    .range(d3.range(9).map(function(i) { return "q" + i; }));

var path = d3.geo.path();

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("class", "map");

var svg2 = d3.select("body").append("svg")
    .attr("width", s_size)
    .attr("height", s_size)
    .attr("class", "scatter");

queue()
    .defer(d3.json, "data/us-counties.json")
    .defer(d3.json, "data/us-states.json")
    .defer(d3.json, "data/election-data.json")
    .await(ready);

function ready(error, counties, states, election) {
    var rateById = {};
    election.forEach(function (c) { rateById[c.id] = +c['dem%'];});


    svg.append("g")
	.attr("class", "counties")
	.selectAll("path")
	.data(counties.features)
	.enter().append("path")
	.attr("class", function(d) { return quantize(rateById[d.id]); })
	.attr("d", path);

    svg.append("path")
	.datum(states)
	.attr("class", "states")
	.attr("d", path);


    // scatter

    var make_scale = function(prop,range_min,range_max) {
	return d3.scale.linear().domain([d3.min(election, function(c) { return c[prop]; }),
					 d3.max(election, function(c) { return c[prop]; })])
            .range([range_min,range_max]);
    }

    x_scale = make_scale('log_density', 10, s_size - 10);
    y_scale = make_scale('dem%', 10, s_size - 10);
    population_scale = make_scale('population', 4, 900); // square of radius


    svg2.selectAll("circle")
        .data(election)
        .enter().append("svg:circle")
        .attr("class", "datapoint")
        .attr("cx", function(d) { return x_scale(d['log_density']); })
        .attr("cy", function(d) { return y_scale(d['dem%']); })
        .attr("r", function(d) { return Math.sqrt(population_scale(d['population'])); })
        .append("svg:title").text(function(d) { return d['county'] + ", " + d['state']; });


    // Brush.
    var brush = d3.svg.brush()
	.on("brushstart", brushstart)
	.on("brush", brush)
	.on("brushend", brushend);

    svg2.append("g")
      .attr("class", "brush")
      .call(brush.x(x_scale).y(y_scale));

    function brushstart(p) {
	svg2.classed("selecting", true);
	if (brush.data !== p) {
	    svg2.call(brush.clear());
	    brush.x(p.x).y(p.y).data = p;
	}
    }

    function brushend(p) {
	svg2.classed("selecting", false);
	svg2.call(brush.clear());
    }

    function brush(p) {
	var e = brush.extent();
	svg2.selectAll("circle").attr("class", function(d) {
	    return e[0][0] <= d['log_density'] && d['log_density'] <= e[1][0]
		&& e[0][1] <= d['dem%'] && d['dem%'] <= e[1][1]
		? "selected" : null;
	});
    }
}






