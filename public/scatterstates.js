var s_margin = {top: 20, right: 20, bottom: 30, left: 40},
    s_width = 600 - s_margin.left - s_margin.right,
    s_height = 550 - s_margin.top - s_margin.bottom;

var m_size = 1000;
var t_width = s_width + m_size;
var t_height = 550;

var quantize = d3.scale.quantize()
    .domain([0, 100])
    .range(d3.range(9).map(function(i) { return "q" + i; }));

var path = d3.geo.path();

var svg = d3.select("body").append("svg")
    .attr("width", t_width)
    .attr("height", t_height);

var scatter = svg.append("g")
    .attr("class", "scatter");

var map = svg.append("g")
    .attr("transform", "translate(600,0)")
    .attr("class", "map");

queue()
    .defer(d3.json, "data/us-new.json")
    .defer(d3.json, "data/election-data-more-fixed.json")
    .await(ready);

var clearSelection;

function ready(error, us, election) {
    var rateById = {};
    election.forEach(function (c) { rateById[c.id] = +c['dem%'];});
    map
	.selectAll("path")
	.data(topojson.object(us, us.objects.counties).geometries)
	.enter().append("path")
	.attr("class", function(d) { return "datapoint " + quantize(rateById[d.id]); })
	.attr("d", path);

    map.append("path")
	.datum(topojson.mesh(us, us.objects.states), function(a, b) { return a.id !== b.id; })
	.attr("class", "states")
	.attr("d", path);

    // scatter

    var make_scale = function(values, prop, range_min, range_max) {
	return d3.scale.linear()
	    .domain([d3.min(values, function(c) { return c[prop]; }),
		     d3.max(values, function(c) { return c[prop]; })])
            .range([range_min,range_max]);
    };

    var x_scale = make_scale(election, 'log_density', s_margin.left, s_margin.left + s_width);
    var y_scale = make_scale(election, 'dem%', s_margin.top + s_height, s_margin.top);
    var population_scale = make_scale(election, 'population', 4, 900); // square of radius

    scatter.selectAll("circle")
        .data(election)
        .enter().append("svg:circle")
// bands look crappy -- how about a side scale?
	.attr("class", function(d) { return "datapoint " + quantize(rateById[d.id]); })
//	.attr("class", "datapoint")
        .attr("r", function(d) { return Math.sqrt(population_scale(d['population'])); })
        .attr("cx", function(d) { return x_scale(d['log_density']); })
        .attr("cy", function(d) { return y_scale(d['dem%']); })
        .append("svg:title").text(function(d) { return d['county'] + ", " + d['state']; });

    var yAxis = d3.svg.axis()
	    .scale(y_scale)
	    .orient("left");

    var xAxis = d3.svg.axis()
	    .scale(x_scale)
	    .orient("bottom");    

    // y axis
    scatter.append("g")
      .attr("class", "y axis")
	.attr("transform", "translate(" + s_margin.left + ",0)")
	.call(yAxis)
	.append("text")
	.attr("class", "label")
	.attr("transform", "rotate(-90)")
	.attr("y", 6)
	.attr("dy", ".71em")
	.style("text-anchor", "end")
	.text("% Democratic");

    scatter.append("g")
	.attr("class", "x axis")
	.attr("transform", "translate(0," + (s_height + s_margin.top) + ")")
	.call(xAxis)
	.append("text")
	.attr("class", "label")
	.attr("x", s_width)
	.attr("y", -6)
	.style("text-anchor", "end")
	.text("log(density)");

    // Brush.
    var brush = d3.svg.brush()
	.on("brushstart", brushstart)
	.on("brush", brush)
	.on("brushend", brushend);

    scatter.append("g")
	.attr("class", "brush")
	.call(brush.x(x_scale).y(y_scale));

    // not quite
    clearSelection = function() {
	scatter.call(brush.clear());
	map.call(brush2.clear());
	scatter.classed("selecting", false);
	map.classed("selecting", false);
    };

    function brushstart(p) {
	scatter.classed("selecting", true);
	map.classed("selecting", true);
	if (brush.data !== p) {
	    scatter.call(brush.clear());
	    brush.x(p.x).y(p.y).data = p;
	}
    }

    function brushend(p) {
//	scatter.classed("selecting", false);
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
	scatter.classed("selecting", true); 
	if (brush2.data !== p) {
	    map.call(brush2.clear());
	    brush2.x(p.x).y(p.y).data = p; 
	}
    }

    function brushend2(p) {
//	map.classed("selecting", false);
	map.call(brush2.clear());
    }

    function brush2(p) {
	var e = brush2.extent();
	var selected = {};
	map.selectAll(".datapoint").classed("selected", function(d) {
	    var centroid = path.centroid(d);
	    var sel =  e[0][0] <= centroid[0] && centroid[0] <= e[1][0] && e[0][1] <= centroid[1] && centroid[1] <= e[1][1];
	    selected[d['id']] = sel;
	    return sel;
	});
	scatter.selectAll(".datapoint").classed("selected", function(d) {
	    return selected[d['id']];
	});
    }
}





