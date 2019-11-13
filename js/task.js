
const idx = getRandomIndex(6) + 1;
effectsHistory(trainings[`learn0${idx}`]);
createDataObj(trainings[`learn0${idx}`]);

let tIdx = getRandomIndex(15) + 1;
let trial = 'trial' + tIdx.toString().padStart(2, '0');
createGeneralizationTask(taskData[trial]);

document.getElementById('next-one-btn').onclick = () => updateTask(trial);
