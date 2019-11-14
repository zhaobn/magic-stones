
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
createTrialCounter(trial);

const nextOneButton = document.getElementById('next-one-btn');

nextOneButton.onclick = () => {
    document.getElementById('learning').style.display = 'inline';
    document.getElementById('task').style.display = 'none';
    updateTask(trial);
    clearTrialCounter();
    createTrialCounter(trial);
    nextOneButton.disabled = true;
}

/** Set trial count indicator */
function createTrialCounter (trial) {
    const text = `[${trial.slice(5,)}/15] ` + ' This magic stone will turn this normal stone into ... ?'
    const h = document.createElement('h2');
    const t = document.createTextNode(text);
    h.append(t);
    setAttributes(h, { 'id': 'trial-h2' });

    const h2Node = document.getElementById('trial-p');
    const parentDiv = h2Node.parentNode;
    parentDiv.insertBefore(h, h2Node);
}

function clearTrialCounter () {
    let clear = document.getElementById('trial-h2');
    clear.parentNode.removeChild(clear);
}
