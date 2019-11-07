
const currentTask = 'task01';
const trial = taskData[currentTask].trialId
const currentTrial = trails[trial];

createLearningTask(currentTrial.learn);
createGeneralizationTask(currentTrial.gen, currentTask);
