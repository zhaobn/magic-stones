
effectsHistory(learningTask);
createDataObj(learningTask);

let trial = 'trial01';
createGeneralizationTask(taskData[trial]);

document.getElementById('next-one-btn').onclick = () => updateTask(trial);
