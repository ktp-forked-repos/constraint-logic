function js_graph(xlabel, arg) {
    nv.addGraph(function() {
	      var chart = nv.models.discreteBarChart();

	      chart.xAxis
		        .axisLabel(xlabel);

	      chart.yAxis
		        .axisLabel("moves")
		        .tickFormat(d3.format("d"))
		    ;

	      d3.select("svg#thegraph")
		        .datum(arg)
		        .transition().duration(1).call(chart);

	      nv.utils.windowResize(
			      function() {
				        chart.update();
			      }
			  );

	      return chart;
    });
}
