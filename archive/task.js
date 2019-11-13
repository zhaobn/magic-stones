
let tasks = Object.keys(taskData);
let taskIndex = isRandom? getRandomIndex(12): 0;
let currentTask = tasks[taskIndex];

/** Create first task */
createGeneralizationTask(gen01, currentTask);

/** Create button functionality */
document.getElementById('task-btn').onclick = () => showTask('generalization-task');
document.getElementById('proceed-btn').onclick = () => updateTask(currentTask);

/** Create after-effect box */
let effectBefore = {};
let effectAfter = {};
const learningTask = trails[currentTask].learn;

effectBefore.taskId = learningTask.taskId + '-before';
effectBefore.magicStone = learningTask.magicStone;
effectBefore.normalStone = learningTask.normalStone;

effectAfter.taskId = learningTask.taskId + '-before';
effectAfter.magicStone = learningTask.magicStone;
effectAfter.normalStone = learningTask.normalStone;
learningTask.rules.forEach (r => {
    effectAfter.normalStone = readEffect(effectAfter.normalStone, r)
});

createStones(effectBefore, '#membox-before');
createStones(effectAfter, '#membox-after');
