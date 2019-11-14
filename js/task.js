/** Custom styling definitions */
const myColors = {
    red: 'rgb(255, 102, 102)',
    blue: 'rgb(61, 60, 134)',
    yellow: 'rgb(245, 230, 99)'
  }
const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';

/** Trask setups */
const trainings = {
  learn01: { taskId: 'learn01', magicStone: 'rd', normalStone: 'yc', rules: [ '-2d' ] },
  learn02: { taskId: 'learn02', magicStone: 'yc', normalStone: 'rd', rules: [ '-2s' ] },
  learn03: { taskId: 'learn02', magicStone: 'bs', normalStone: 'rc', rules: [ '-2b' ] },
  learn04: { taskId: 'learn04', magicStone: 'rc', normalStone: 'bs', rules: [ '-2y' ] },
  learn05: { taskId: 'learn05', magicStone: 'yd', normalStone: 'bs', rules: [ '-2y', '-2d' ] },
  learn06: { taskId: 'learn06', magicStone: 'bs', normalStone: 'yd', rules: [ '-2r', '-2c' ] },
}
const learningTask = trainings.learn04;
let taskData = {};

/** Create learning task stones */
createStones(learningTask);
/** Create history stones */
effectsHistory(learningTask);
/** Create generalization tasks */
createDataObj(learningTask);

let trial = 'trial01';
createGeneralizationTask(taskData[trial]);
createTrialCounter(trial);

/** Set learning task button functions */
const playBtn = document.getElementById('play-btn');
const resetBtn = document.getElementById('reset-btn');
const nextBtn = document.getElementById('next-btn');
const nextOneButton = document.getElementById('next-one-btn');

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
nextOneButton.onclick = () => {
    document.getElementById('learning').style.display = 'inline';
    document.getElementById('task').style.display = 'none';
    updateTask(trial);
    clearTrialCounter();
    createTrialCounter(trial);
    nextOneButton.disabled = true;
}

/** Helper functions */

/** Color changing effects */
const changeColor = (id, color) => {
    document.getElementById(id).style.background = myColors[color];
}

/** Shape changing effects */
function changeShape (id, shape) {
    const el = document.getElementById(id);
    switch (shape) {
        case 'diamond':
            el.style.borderRadius = '10%';
            el.style.transform = 'rotate(45deg) scale(.8)';
            break;
        case 'circle':
            el.style.borderRadius = '50%';
            break;
        case 'square':
            el.style.borderRadius = '10%';
            break;
    }
}

/** Animation effect for changing the normal stone */
function changeStone (task) {
    const normalStoneId = `${task.taskId}-normal-stone`;
    const normalStone = document.getElementById(normalStoneId);
    normalStone.style.transitionDelay = '1s';
    task.rules.forEach(rule => setEffect(normalStoneId, rule));
}

/** In order to show a new task, clear current page */
function clearTaskElements (taskId) {
    const elementsToClear = [
        `${taskId}-magic-stone`,
        `${taskId}-normal-stone`,
        `${taskId}-panel`,
    ];
    elementsToClear.forEach (el => {
        let clear = document.getElementById(el);
        clear.parentNode.removeChild(clear);
    })
}

function clearTrialCounter () {
    let clear = document.getElementById('trial-h2');
    clear.parentNode.removeChild(clear);
}

/** Create the generaliztion task */
function createGeneralizationTask (task) {
    !!task.taskId ? null: task.taskId = task.trialId; // hacky fix for inconsistent task types
    createStones(task, '.box-task');
    const panel = createPanel(task);
    document.querySelector('.box-panel').append(panel);
}

/** Create the learning task */
function createLearningTask (task) {
    /** Create stones */
    createStones(task);
    /** Trigger animations */
    document.getElementById('play-btn').onclick = () => playEffects(task);
}

/** Create selection panel */
function createPanel(trial) {
    const taskId = trial.trialId;
    tbs = [];

    let tbl = document.createElement('table');
    setAttributes(tbl, { 'class': 'selection-panel', 'id': `${taskId}-panel` })

    const styleClicked = (id) => {
        const selectedTb = id.replace(/cell/g, 'tb');
        tbs.forEach(tbid => {
            hover (tbid, selectedTb);
            const tb = document.getElementById(tbid);
            tb.style.border = (tbid === selectedTb)? '2px solid white' : '0px';
        })
    }

    const recordClick = (e) => {
        const tbId = e.target.id;
        let clicked = {};

        clicked.stone = tbId;
        clicked.timestamp = Date.now();

        taskData[taskId].selection = clicked;
        taskData[taskId].clicks.push(clicked);
        sessionStorage.setItem('taskData', JSON.stringify(taskData));

        styleClicked(tbId);
        document.getElementById('next-one-btn').disabled = false;
    }

    for(let i = 0; i < 3; i++){
        let tr = tbl.insertRow();
        for(let j = 0; j < 3; j++){
            let tbId = `tb0${i}0${j}`;
            tbs.push(tbId);

            let td = tr.insertCell();
            setAttributes(td, {'id': tbId});

            let tc = document.createElement('div');
            setAttributes(tc, {
                'class': `panel-stone-${i}-${j}`,
                'id': tbId.replace(/tb/g, 'cell'),
              })
            tc.addEventListener('click', recordClick);
            td.appendChild(tc);
        }
    }
    return tbl;
}

/** Create a pair of magic and normal stones */
function createStones (task, box = '.box-lt') {
    let magicStone = document.createElement('div');
    setAttributes(magicStone, {
        'class': `stone-${task.magicStone}`,
        'id': `${task.taskId}-magic-stone`,
    });
    magicStone.style.border = magicStoneBorderStyle;
    let normalStone = document.createElement('div');
    setAttributes(normalStone, {
        'class': `stone-${task.normalStone}`,
        'id': `${task.taskId}-normal-stone`,
    });
    const taskBox = document.querySelector(box);
    taskBox.append(magicStone);
    taskBox.append(normalStone);
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

/** Create magic effect history display */
function effectsHistory (learningTask) {
    let effectBefore = {};
    let effectAfter = {};

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
}


/** Helper function that reads clientPos of an element */
function getCurrentLocation(id) {
    let rect = {top: 0, bottom: 0, left: 0, right: 0};
    const pos = document.getElementById(id).getBoundingClientRect();
    rect.top = pos.top;
    rect.bottom = pos.bottom;
    rect.left = pos.left;
    rect.right = pos.right;
    return rect;
}

/** Psedo hover effects */
function hover (tbid, selected) {
    const tb = document.getElementById(tbid);
    tb.onmouseover = function() {
        (tbid !== selected)? this.style.border = '4px solid #4c4c4c' : null;
    };
    tb.onmouseleave = function() {
        (tbid !== selected)? this.style.border = '0px' : null;
    };
}

/** Animation effect for moving magic stone to the normal stone */
function moveStone (task) {
    const magicStoneId = `${task.taskId}-magic-stone`;
    const normalStoneId = `${task.taskId}-normal-stone`;

    const magicStone = document.getElementById(magicStoneId);

    const startPos = getCurrentLocation(magicStoneId).right;
    const endPos = getCurrentLocation(normalStoneId).left;

    const delta = Math.round(endPos - startPos);
    (delta > 0) && (magicStone.style.left = `${delta}px`);
}

/** For the `play` button of the learning task */
function playEffects (task) {
    moveStone(task);
    changeStone(task);
}

/** Show history */
function displayHistory () {
    document.getElementById('watch').style.display = 'none';
    document.getElementById('history').style.display = 'inline';
}

/** Compose the changed stone */
function readEffect (stone, rule) {
    let changed = '';
    if(rule[2] === 'r' || rule[2] === 'b' || rule[2] === 'y') {
        changed = rule[2] + stone[1];
    } else {
        changed = stone[0] + rule[2];
    }
    return changed;
}

/** For the `reset` button of the learning task */
function resetStones (task) {
    const magicStoneId = `${task.taskId}-magic-stone`;
    const normalStoneId = `${task.taskId}-normal-stone`;
    const stones = [ magicStoneId, normalStoneId ];
    // Clear existing stones
    stones.forEach(stone => {
        let el = document.getElementById(stone);
        el.parentNode.removeChild(el);
    });
    // Create new stones
    createStones(task);
}

/** Useful shorthand */
function setAttributes(el, attrs) {
    for(var key in attrs) {
      el.setAttribute(key, attrs[key]);
    }
}

/** Where magic happends */
function setEffect (id, rule) {
    const keyword = rule[2];
    switch (keyword) {
        case 'r':
            changeColor(id, 'red');
            break;
        case 'y':
            changeColor(id, 'yellow');
            break;
        case 'b':
            changeColor(id, 'blue');
            break;
        case 'c':
            changeShape(id, 'circle');
            break;
        case 's':
            changeShape(id, 'square');
            break;
        case 'd':
            changeShape(id, 'diamond');
            break;
    }
}

function updateTask (current) {
    let tidx = parseInt(current.slice(5,));

    if (tidx < 15) {
        trial = 'trial' + (tidx + 1).toString().padStart(2, '0');
        clearTaskElements(current);
        createGeneralizationTask(taskData[trial]);
    } else {
        location.href='feedback.html';
    }
}

/** Create data object */
function createDataObj (learningTask) {
    taskData.userId = '';
    taskData.learningTaskId = learningTask.taskId;

    for (let i = 1; i < 16; i++) {
        trialId = 'trial' + i.toString().padStart(2, '0');
        taskData[trialId] = {};
        taskData[trialId]['trialId'] = trialId;
        taskData[trialId]['magicStone'] = '';
        taskData[trialId]['normalStone'] = '';
        taskData[trialId]['selection'] = {};
        taskData[trialId]['clicks'] = [];
        createTrials(learningTask, taskData[trialId]);
    }
}

/** Create trial stones */
function createTrials (learningTask, trial) {
    const colors = [ 'r', 'y', 'b' ];
    const shapes = [ 'c', 'd', 's' ];

    const agentColor = learningTask.magicStone[0];
    const agentShape = learningTask.magicStone[1];
    const recipientColor = learningTask.normalStone[0];
    const recipientShape = learningTask.normalStone[1];

    const diffColor = (colors.filter(c => (c !== agentColor && c !== recipientColor)))[0];
    const diffShape = (shapes.filter(c => (c !== agentShape && c !== recipientShape)))[0];

    const trialIndex = parseInt(trial.trialId.slice(5,));
    // Set recipient properties
    switch(trialIndex % 4) {
        case 1:
            trial.normalStone = diffColor + recipientShape;
            break;
        case 2:
            trial.normalStone = recipientColor + diffShape;
            break;
        case 3:
            trial.normalStone = diffColor + diffShape;
            break;
        default:
            trial.normalStone = learningTask.normalStone;
            break;
    }
    // Set agent properties
    if (trialIndex > 0 && trialIndex < 4) {
        trial.magicStone = learningTask.magicStone;
    } else if (trialIndex > 3 && trialIndex < 8) {
        trial.magicStone = diffColor + agentShape;
    } else if (trialIndex > 7 && trialIndex < 12) {
        trial.magicStone = agentColor + diffShape;
    } else {
        trial.magicStone = diffColor + diffShape;
    }
}
