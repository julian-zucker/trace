$('#select-class').selectize({
    create: true,
    maxItems: 5,
    sortField: 'text',
    onItemAdd: function() {
        updateChart();
    },
    onItemRemove: function() {
        updateChart();
    }
})

var colors = [
    "rgba(100, 181, 246, .8)",
    "rgba(190, 45, 60, .8)",
    "rgba(7, 94, 0, .8)",
    "rgba(45, 45, 45, .8)",
    "rgba(187, 2, 255, .8)",
];

function processEval(name, colorIndex) {
    let classResults = evals[name];
    let average = (arr) => {
        arr = arr.filter(e => e != undefined && e != "undefined");
        if (arr.length === 0) {
            return 0;
        }
        return arr.reduce((a, b) => a + b, 0) / arr.length;
    };
    let num = (chr) => chr ? +(chr.replace(/,/, "")) : undefined

    return {
        label: name,
        data: [
            average(classResults.map(e => num(e.courseQuestions))) * 20,
            average(classResults.map(e => num(e.learningQuestions))) * 20,
            average(classResults.map(e => num(e.wouldRecommend))) * 20,
            average(classResults.map(e => num(e.spentMinimum))),
            average(classResults.map(e => num(e.spentLow))),
            average(classResults.map(e => num(e.spentMedium))),
            average(classResults.map(e => num(e.spentHigh))),
            average(classResults.map(e => num(e.spentMax)))
        ],
        borderWidth: 1,
        backgroundColor: colors[colorIndex]
    };
}


var ctx = document.getElementById("mainChart").getContext('2d');
var labels = ["Course", "Learning",
    "Would Recommend", "Spent 1-4 Hours", "Spent 5-8 Hours", 
    "Spent 9-12 Hours", "Spent 13-16 Hours", "Spent 17+ Hours"
];

var chart = new Chart(ctx, {
    type: 'bar',
    data: {
        labels: labels,
        datasets: [processEval("AACE6000 Arts and Culture Leadership", 0)]
    },
    options: {
        scales: {
            yAxes: [{
                ticks: {
                    min: 0,
                    max: 100,
                }
            }]
        }
    }
})


function updateChart() {
    chart.destroy();
    let selected = $('#select-class').selectize()[0].selectize.getValue()

    let data = [];
    let i = 0;
    selected.sort().forEach((n) => data.push(processEval(n, i++)));


    chart = new Chart(ctx, {
    type: 'bar',
    data: {
        labels: labels,
        datasets: data
    },
    options: {
        scales: {
            yAxes: [{
                ticks: {
                    min: 0,
                    max: 100,
                }
            }]
        }
    }
})
}



