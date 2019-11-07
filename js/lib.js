
/** Color changing effects */
const changeColor = (id, color) =>
    document.getElementById(id).style.background = myColors[color];

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
    task.rules.forEach(rule => setEffect(task, normalStoneId, rule));
}

/** In order to show a new task, clear current page */
function clearTaskElements (taskId) {
    const trial = trails[taskData[taskId].trialId];
    const elementsToClear = [
        `${trial.learn.taskId}-magic-stone`,
        `${trial.learn.taskId}-normal-stone`,
        `${trial.gen.taskId}-magic-stone`,
        `${trial.gen.taskId}-normal-stone`,
        `${taskId}-panel`,
    ];
    elementsToClear.forEach (el => {
        let clear = document.getElementById(el);
        clear.parentNode.removeChild(clear);
    })
}

/** Create the generaliztion task */
function createGeneralizationTask (task, currentTask) {
    createStones(task, '.box-task');
    const panel = createPanel(task, currentTask);
    document.querySelector('.box-panel').append(panel);
}

/** Create the learning task */
function createLearningTask (task) {
    /** Create stones */
    createStones(task);
    /** Trigger animations */
    document.getElementById('play-btn').onclick = () => playEffects(task);
    document.getElementById('reset-btn').onclick = () => resetStones(task);
}

/** Create selection panel */
function createPanel(gtask, taskId) {
    const dataEntry = taskId;
    let data = gtask;
    data.selection = {};
    data.clicks = [];
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

        data.selection = clicked;
        data.clicks.push(clicked);

        taskData[dataEntry].clientData = data;
        styleClicked(tbId);
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

/** Main trial creation function */
function createTask (taskId) {
    const trial = taskData[taskId].trialId
    const currentTrial = trails[trial];

    createLearningTask(currentTrial.learn);
    createGeneralizationTask(currentTrial.gen, currentTask);
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
        (tbid !== selected)? this.style.border = '4px solid #1A1A1D' : null;
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
const playEffects = (task) => {
    moveStone(task);
    changeStone(task);
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
function setEffect (task, id, rule) {
    const keyword = rule[2];
    // Check if effects takes place
    if (task.magicStone[0] === rule[0]) {
        // Apply effects
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
}

/** Toggle generaliztion task display */
function showTask (taskId) {
    document.getElementById(taskId).style.display = "block";
    document.getElementById(taskId).scrollIntoView({
        behavior: 'smooth'
    });
}

/** For the `proceed` button on task page */
function updateTask (current) {
    /** Check: need to complete current task in order to proceed */
    const selection = taskData[current].clientData.selection;

    if (!!selection) {
        /** Clear current task */
        clearTaskElements(current);
        /** Get next task */
        tasks = tasks.filter(t => t !== current);
        /** If no more tasks, go to the debriefing page */
        if (tasks.length < 1) {
            location.href='debrief.html'
        } else {
            /** Create new task */
            currentTask = tasks[0];
            createTask(currentTask);
            document.querySelector('.generalization').style.display = "none";
            // window.scrollTo({top: 0, behavior: 'smooth'});
        }
    } else {
        window.alert('Please complete the task first!')
    }
}
