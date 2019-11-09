
let tasks = Object.keys(taskData);
let currentTask = tasks[0];

/** Create first task */
createTask(currentTask);

/** Create button functionality */
document.getElementById('task-btn').onclick = () => showTask('generalization-task');
document.getElementById('proceed-btn').onclick = () => updateTask(currentTask);
