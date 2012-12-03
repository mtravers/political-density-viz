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
    .attr("transform", "translate(0,500)")
    .attr("class", "scatter");

queue()
    .defer(d3.json, "data/us-counties-plus.json")
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

    var make_scale = function(values, prop, range_min, range_max) {
	return d3.scale.linear()
	    .domain([d3.min(values, function(c) { return c[prop]; }),
		     d3.max(values, function(c) { return c[prop]; })])
            .range([range_min,range_max]);
    }

    x_scale = make_scale(election, 'log_density', 10, s_size - 10);
    y_scale = make_scale(election, 'dem%', 10, s_size - 10);
    population_scale = make_scale(election, 'population', 4, 900); // square of radius

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

    // other brush

    var brush2 = d3.svg.brush()
	.on("brushstart", brushstart2)
	.on("brush", brush2)
	.on("brushend", brushend2);

    var mx_scale = d3.scale.linear().domain([0,1000]).range([0,1000]);
    var my_scale = d3.scale.linear().domain([0,1000]).range([0,1000]);

    map.append("g")
	.attr("class", "brush")
	.call(brush2.x(mx_scale).y(my_scale)); 
    
    function brushstart2(p) {
	map.classed("selecting", true); 
	if (brush2.data !== p) {
	    map.call(brush2.clear());
	    brush2.x(p.x).y(p.y).data = p; 
	}
    }

    function brushend2(p) {
	map.classed("selecting", false);
	map.call(brush2.clear());
    }

    function brush2(p) {
	var e = brush2.extent();
	var selected = {};
	map.selectAll(".datapoint").classed("selected", function(d) {
//	    var sel =  e[0][0] <= d['xCenter'] && d['xCenter'] <= e[1][0] && e[0][1] <= d['yCenter'] && d['yCenter'] <= e[1][1];
	    centroid = path.centroid(d);
	    var sel =  e[0][0] <= centroid[0] && centroid[0] <= e[1][0] && e[0][1] <= centroid[1] && centroid[1] <= e[1][1];
	    selected[d['id']] = sel;
	    return sel;
	});
	scatter.selectAll(".datapoint").classed("selected", function(d) {
	    return selected[d['id']];
	});
    }
}






