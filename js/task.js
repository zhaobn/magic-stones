
let tasks = Object.keys(taskData);
let currentTask = tasks[getRandomIndex(12)];

/** Create first task */
createTask(currentTask);

/** Create button functionality */
document.getElementById('task-btn').onclick = () => showTask('generalization-task');
document.getElementById('proceed-btn').onclick = () => updateTask(currentTask);
