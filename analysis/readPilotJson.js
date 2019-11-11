
const path = require('path');
const fs = require('fs');

/** List of data files */
fs.readdir('../data/pilot', function (err, files) {
    //handling error
    if (err) {
        return console.log('Unable to scan directory: ' + err);
    }
    // Get json files
    dataFiles = files.filter(f => (path.extname(f) === '.json'));
    // Read necessary info for analysis
    dataFiles.forEach((file, index) => {
        readData(file, index);
    });
});

/** Take only necessary info for analysis */
function readData (filename, index) {
    const pid = index + 1;
    const taskData = (require(`../data/pilot/${filename}`)).task

    let data = {};
    let trials = [];
    let selected = [];
    let ts = [];

    Object.keys(taskData).forEach(d => {
        let trailId = d.slice(5,);
        let selection = taskData[d].clientData.selection.stone;
        let timestamp = taskData[d].clientData.selection.timestamp;

        trials.push(parseInt(trailId));
        selected.push(readCell(selection));
        ts.push(timestamp.toString());

        data.pid = Array(12).fill(pid);
        data.trials = trials;
        data.selections = selected;
        data.ts = ts;
    });

    writeFile(filename, JSON.stringify(data));
}

/** Write to fs */
function writeFile (filename, file) {
    fs.writeFile(`../data/pilot-cleaned/c-${filename}`, file, function(err) {
        if(err) {
            return console.log(err);
        }
        console.log("The file was saved!");
    });
};

/** Translate to human readable selections */
function readCell (cell) {
    const shapeIndex = cell.slice(4,6);
    const colorIndex = cell.slice(6,8);
    const shape = (shapeIndex === '00')? 'c' :
        (shapeIndex === '01') ? 's' : 'd';
    const color = (colorIndex === '00')? 'r' :
        (colorIndex === '01') ? 'y' : 'b';
    return (color + shape)
}
