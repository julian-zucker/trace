$('#select-class').selectize({
    create: true,
    sortField: 'text',
    onItemAdd: function(value) {
        updateChart(value);
    }
})

function processEval(name) {
    let classResults = evals[name];
    let average = (arr) => {
        arr = arr.filter(e => e != undefined && e != "undefined");
        if (arr.length === 0) {return 0;}
        return arr.reduce((a, b) => a + b, 0) / arr.length;
    };
    let num = (chr) => chr ? +(chr.replace(/,/, "")) : undefined

    return [
        average(classResults.map(e => num(e.courseQuestions))) * 20,
        average(classResults.map(e => num(e.learningQuestions))) * 20,
        average(classResults.map(e => num(e.instructorQuestions))) * 20,
        average(classResults.map(e => num(e.wouldRecommend))) * 20,
        average(classResults.map(e => num(e.spentMinimum))),
        average(classResults.map(e => num(e.spentLow))),
        average(classResults.map(e => num(e.spentMedium))),
        average(classResults.map(e => num(e.spentHigh))),
        average(classResults.map(e => num(e.spentMax)))
    ];
}


var ctx = document.getElementById("mainChart").getContext('2d');
var label = ["Course", "Learning", "Instructor",
    "Would Recommend", "Spent 1-4 Hours", "Spent 5-8 Hours", "Spent 9-12 Hours", "Spent 13-16 Hours", "Spent 17+ Hours"
];
var name = 'AACE6000 Arts and Culture Leadership';
var chart = new Chart(ctx, {
    type: 'bar',
    data: {
        labels: label,
        datasets: [{
            label: 'Percent',
            data: processEval(name),
            borderWidth: 1,
            backgroundColor: "rgba(100, 181, 246, .9)"
        }]
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

function updateData(data) {
    chart.data.datasets[0].data = data
    chart.update();
}

function updateChart(selected) {
    updateData(processEval(selected))
    chart.update();
}