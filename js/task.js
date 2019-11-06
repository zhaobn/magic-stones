
// Data to send
let data = {
    'gt1-1': {
        taskId: 'gt1-1',
        magicStone: 'yc',
        normalStone: 'bd',
        selection : {},
        clicks: [],
    },
    'gt1-2': {
        taskId: 'gt1-2',
        magicStone: 'rs',
        normalStone: 'rd',
        selection : {},
        clicks: [],
    }
}

// Helper definitions
const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';
let tbs = {};
Object.keys(data).forEach(d => tbs[d] = []);

// Create tasks
function createTask (taskId) {
    const boxId = `box-${taskId}`;
    const taskBody = document.getElementById(boxId);

    // Prep variable names
    let taskDivId = taskId + '-task-div';
    let panelDivId = taskId + '-panel-div';
    let taskMagicStoneId = taskId + '-magic-stone';
    let taskNormalStoneId = taskId + '-normal-stone';
    let panelId = taskId + '-panel';

    // Layout div's
    let taskDiv = document.createElement('div');
    let panelDiv = document.createElement('div');
    setAttributes(taskDiv, {
        'class': 'box-task',
        'id': taskDivId,
    })
    setAttributes(panelDiv, {
        'class': 'box-panel',
        'id': panelDivId,
      })
    taskBody.append(taskDiv);
    taskBody.append(panelDiv);

    // Create task stones
    let magicStone = document.createElement('div');
    setAttributes(magicStone, {
        'class': `stone-${data[taskId].magicStone}`,
        'id': taskMagicStoneId,
      })
    let normalStone = document.createElement('div');
    setAttributes(normalStone, {
        'class': `stone-${data[taskId].normalStone}`,
        'id': taskNormalStoneId,
      })
    magicStone.style.border = magicStoneBorderStyle;
    taskDiv.append(magicStone);
    taskDiv.append(normalStone);

    // Create selection panel
    let panel = createPanel(panelId);
    panelDiv.append(panel);
}

// Create selection panel for task x
function createPanel(id){
    const taskId = id.split('-').slice(0,2).join('-');
    let tbl = document.createElement('table');
    setAttributes(tbl, {
        'class': 'selection-panel',
        'id': id,
      })
    for(let i = 0; i < 3; i++){
        let tr = tbl.insertRow();
        for(let j = 0; j < 3; j++){
            let tbId = `${id}-tb0${i}0${j}`;
            tbs[taskId].push(tbId);

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

// Record selection
function recordClick (e) {
    const tbId = e.target.id;
    const taskId = tbId.split('-').slice(0,2).join('-');

    let clicked = {};
    clicked.stone = tbId;
    clicked.timestamp = Date.now();

    data[taskId].selection = clicked;
    data[taskId].clicks.push(clicked);

    styleClicked(taskId, tbId);
}

// Style panel selection
function styleClicked (task, id) {
    const selectedTb = id.replace(/cell/g, 'tb');
    tbs[task].forEach(tbid => {
        hover (tbid, selectedTb);
        const tb = document.getElementById(tbid);
        tb.style.border = (tbid === selectedTb)? '2px solid white' : '0px';
    })
}

// Psedo hover style
function hover (tbid, selected) {
    const tb = document.getElementById(tbid);
    tb.onmouseover = function() {
        (tbid !== selected)? this.style.border = '1px dotted white' : null;
    };
    tb.onmouseleave = function() {
        (tbid !== selected)? this.style.border = '0px' : null;
    };
}

// Main functionality: create generalization tasks
Object.keys(data).forEach(d => {
    createTask(d);
});
