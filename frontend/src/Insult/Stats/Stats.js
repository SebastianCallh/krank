"use strict";

exports.plotHistogram = function (insults) {
    return function () {
	const jsonInsults = JSON.parse(insults);


	const buckets = jsonInsults.reduce(function (buckets, insult)  {
	    if (insult.from in buckets) {
		buckets[insult.from].sent += insult.amount;
	    } else {
		buckets[insult.from] = {sent: insult.amount, received: 0};
	    }

	    if (insult.to in buckets) {
		buckets[insult.to].received += insult.amount;
	    } else {
		buckets[insult.to] = {sent: 0, received: insult.amount};
	    }
	    return buckets;
	}, {});


	const colors = {
	    'Elin': '#b30059',
	    'Jimmie': '#1a53ff',
	    'Karl': '#2eb8b8',
	    'Maria': '#e65c00',
	    'Mike': '#b30000',
	    'Seba': '#00b300',
	    'Tintin': '#ffcc00'
	};
	
	const hexToRgb = function (hex) {
	    const bigint = parseInt(hex.substring(1), 16);
	    const r = (bigint >> 16) & 255;
	    const g = (bigint >> 8) & 255;
	    const b = bigint & 255;
	    const x = 'rgba(' + [r, g, b, '1'].join() + ')';
	    return x
	};
	
	const users = Object.keys(buckets);
	const trace1 = {
	    x: users,
	    y: users.map(function (user) { return buckets[user].sent }),
	    type: 'bar',
	    marker: {
		color: users.map(function (user) { return hexToRgb(colors[user]) })
	    }	
	};

	const trace2 = {
	    x: users,
	    y: users.map(function (user) { return -buckets[user].received }),
	    type: 'bar',
	    marker: {
		color: users.map(function (user) { return hexToRgb(colors[user]) })
	    }
	};

	const data = [trace1, trace2];

	const layout = {	
	    title: 'Skickat och mottaget kränk',
	    barmode: 'relative',
	    showlegend: false,
	    margin: {
		l: 50,
		r: 50,
		b: 100,
		t: 100,
		pad: 4
	    }};

	const div = document.getElementById('histogram');
	Plotly.newPlot(div, data, layout, {displayModeBar: false});
    }
}
