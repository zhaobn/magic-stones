
let tasks = Object.keys(taskData);
let currentTask = tasks[0];

/** Create first task */
createTask(currentTask);

/** Create proceed button functionality */
document.getElementById('proceed-btn').onclick = () => updateTask(currentTask);

console.log('played: ' + played);
