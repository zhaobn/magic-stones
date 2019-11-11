
let tasks = Object.keys(taskData);
let taskIndex = isRandom? getRandomIndex(12): 0;
let currentTask = tasks[taskIndex];

/** Create first task */
createTask(currentTask);

/** Create button functionality */
document.getElementById('task-btn').onclick = () => showTask('generalization-task');
document.getElementById('proceed-btn').onclick = () => updateTask(currentTask);
