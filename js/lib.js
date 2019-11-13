
let taskData = {};

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
    // document.getElementById('reset-btn').onclick = () => resetStones(task);
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

// /** Main trial creation function */
// function createTask (taskId) {
//     const trial = taskData[taskId].trialId
//     const currentTrial = trails[trial];

//     createLearningTask(currentTrial.learn);
//     createGeneralizationTask(currentTrial.gen, currentTask);
// }

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

function getRandomIndex (ceil) {
    return Math.floor(Math.random() * ceil);
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

// /** Toggle generaliztion task display */
// function showTask (taskId) {
//     if (played > 0) {
//         document.getElementById(taskId).style.display = "block";
//         document.getElementById(taskId).scrollIntoView({
//             behavior: 'smooth'
//         });
//         /** Replace to the proceed button */
//         switchBtn('show-task', 'proceed');
//     } else {
//         window.alert('Please play the effects first!');
//     }
// }

function switchBtn (from, to) {
    const currentBtn = document.getElementById(from);
    const newBtn = document.getElementById(to);
    currentBtn.style.display = 'none';
    newBtn.style.display = 'flex';
}

/** For the `proceed` button on task page */
function updateTask (current) {
    let rest = [];
    const trials = Object.keys(taskData).filter(t => t.indexOf('trial') > -1);
    trials.forEach (t => (Object.keys(taskData[t].selection) < 1) ? rest.push(t) : null);

    if (rest.length > 0) {
        const nextIdx = getRandomIndex(rest.length);
        trial = rest[nextIdx];

        /** Clear existing trials */
        clearTaskElements(current);
        createGeneralizationTask(taskData[trial]);
    } else {
        location.href='feedback.html';
    }

    // /** Check: need to complete current task in order to proceed */
    // const selection = taskData[current].clientData.selection;

    // if (!!selection) {
    //     /** Clear current task */
    //     clearTaskElements(current);
    //     /** Get next task */
    //     tasks = tasks.filter(t => t !== current);
    //     /** If no more tasks, go to the debriefing page */
    //     if (tasks.length < 1) {
    //         location.href='feedback.html'
    //     } else {
    //         /** Create new task */
    //         taskIndex = isRandom? getRandomIndex(tasks.length): 0;
    //         currentTask = tasks[taskIndex];
    //         createTask(currentTask);
    //         switchBtn('proceed', 'show-task');
    //         document.querySelector('.generalization').style.display = "none";
    //         // window.scrollTo({top: 0, behavior: 'smooth'});
    //     }
    // } else {
    //     window.alert('Please complete the task first!')
    // }
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
    if (trialIndex < 4) {
        trial.normalStone = learningTask.normalStone;
    } else {
        switch(trialIndex % 3) {
            case 0:
                trial.normalStone = diffColor + diffShape;
                break;
            case 1:
                trial.normalStone = diffColor + recipientShape;
                break;
            case 2:
                trial.normalStone = recipientColor + diffShape;
                break;
        }
    }
    // Set agent properties
    if (trialIndex === 1 || (trialIndex > 6 && trialIndex < 10)) {
        trial.magicStone = diffColor + agentShape;
    } else if (trialIndex === 2 || (trialIndex > 9 && trialIndex < 13)) {
        trial.magicStone = agentColor + diffShape;
    } else if (trialIndex === 3 || (trialIndex > 12)) {
        trial.magicStone = diffColor + diffShape;
    } else {
        trial.magicStone = learningTask.magicStone;
    }
}
