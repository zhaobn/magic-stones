
/** Sumit button: save data */
document.getElementById('submit-btn').onclick = () => saveData();

/** Auto-download data file for off-line use */
function download(content, fileName, contentType) {
    var a = document.createElement("a");
    var file = new Blob([content], {type: contentType});
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
  }

function saveData () {
    /** Get data */
    let dataFile = {};
    const feedbackData = document.getElementById('feedback').value;
    const taskData = sessionStorage.taskData;

    dataFile.task = taskData;
    dataFile.feedback = feedbackData;

    /** Save data */
    download(JSON.stringify(dataFile), 'data.txt', '"text/csv"');
}
