
var width = 960,
height = 500;
s_size = 500;

var quantize = d3.scale.quantize()
    .domain([0, 100])
    .range(d3.range(9).map(function(i) { return "q" + i; }));

var path = d3.geo.path();

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height + s_size)

var map = svg.append("g")
    .attr("class", "map");

var scatter = svg.append("g")
//    .attr("width", s_size)
//    .attr("height", s_size)
    .attr("transform", "translate(0,500)")
    .attr("class", "scatter");

queue()
    .defer(d3.json, "data/us-counties.json")
    .defer(d3.json, "data/us-states.json")
    .defer(d3.json, "data/election-data.json")
    .await(ready);

function ready(error, counties, states, election) {
    var rateById = {};
    election.forEach(function (c) { rateById[c.id] = +c['dem%'];});
    map
	.selectAll("path")
	.data(counties.features)
	.enter().append("path")
	.attr("class", function(d) { return "datapoint " + quantize(rateById[d.id]); })
	.attr("d", path);


    map.append("path")
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


    scatter.selectAll("circle")
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

    scatter.append("g")
      .attr("class", "brush")
      .call(brush.x(x_scale).y(y_scale));

    function brushstart(p) {
	scatter.classed("selecting", true);
	if (brush.data !== p) {
	    scatter.call(brush.clear());
	    brush.x(p.x).y(p.y).data = p;
	}
    }

    function brushend(p) {
	scatter.classed("selecting", false);
	scatter.call(brush.clear());
    }

    function brush(p) {
	var e = brush.extent();
	var selected = {};
	scatter.selectAll(".datapoint").classed("selected", function(d) {
	    var sel =  e[0][0] <= d['log_density'] && d['log_density'] <= e[1][0] && e[0][1] <= d['dem%'] && d['dem%'] <= e[1][1];
	    selected[d['id']] = sel;
	    return sel;
	});
	map.selectAll(".datapoint").classed("selected", function(d) {
	    return selected[d['id']];
	});
    }
}






