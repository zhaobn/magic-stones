
const learningTask = trainings.learn04;

/** Create learning task stones */
createStones(learningTask);

/** Set learning task button functions */
const playBtn = document.getElementById('play-btn');
const resetBtn = document.getElementById('reset-btn');
const nextBtn = document.getElementById('next-btn');

resetBtn.onclick = () => resetStones(learningTask);

playBtn.onclick = () => {
    playEffects(learningTask);
    setTimeout(() => {
        resetBtn.disabled = false;
        nextBtn.disabled = false
    }, 2000);
};

nextBtn.onclick = () => {
    document.getElementById('learning').style.display = 'none';
    document.getElementById('task').style.display = 'inline';
    document.getElementById('trial').scrollIntoView({
        behavior: 'smooth'
    });
    resetStones(learningTask);
    nextBtn.disabled = true;
    resetBtn.disabled = true;
};

/** Create history stones */
effectsHistory(learningTask);

/** Create generalization tasks */
createDataObj(learningTask);

let trial = 'trial01';
createGeneralizationTask(taskData[trial]);

const nextOneButton = document.getElementById('next-one-btn');

nextOneButton.onclick = () => {
    document.getElementById('learning').style.display = 'inline';
    document.getElementById('task').style.display = 'none';
    updateTask(trial);
    nextOneButton.disabled = true;
}


document.getElementById('next-one-btn').onclick = () => updateTask(trial);
