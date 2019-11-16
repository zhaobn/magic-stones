
const doneBtn = document.getElementById('done-btn');
const debriefForm = document.getElementById('postquiz');

debriefForm.onchange = () => {
  isFilled('postquiz')? doneBtn.disabled = false: null;
}
doneBtn.onclick = () => saveData();

/** Check if form is filled */
/** Return TRUE if form is fully filled */
function isFilled (formID) {
  let notFilled = false;
  const nulls = [ '', '', 'noresp', '--', '--' ];
  const form = document.getElementById(formID);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((s, i) => {
    let input = inputs[s].value;
    notFilled = (notFilled || (input === nulls[i]));
  });
  return (!notFilled)
}

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
