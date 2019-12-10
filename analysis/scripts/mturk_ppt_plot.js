
const myColors = {
  red: 'rgb(255, 102, 102)',
  blue: 'rgb(61, 60, 134)',
  yellow: 'rgb(245, 230, 99)'
}
const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';

/** Declare setups */
let trainings = {
  learn01: { magicStone: 'rs', normalStone: 'yc', rules: [ '-2s' ] },
  learn02: { magicStone: 'yd', normalStone: 'rs', rules: [ '-2c' ] },
  learn03: { magicStone: 'bs', normalStone: 'rd', rules: [ '-2b' ] },
  learn04: { magicStone: 'rc', normalStone: 'bs', rules: [ '-2y' ] },
  learn05: { magicStone: 'yd', normalStone: 'bs', rules: [ '-2y', '-2c' ] },
  learn06: { magicStone: 'bs', normalStone: 'yc', rules: [ '-2b', '-2s' ] },
}
Object.keys(trainings).forEach (t => trainings[t].taskId = t);

/** Subject data */
const subjectConditions = {
  learn01: [ 20, 44, 51, 52 ],
  learn02: [ 25, 30, 42, 45, 46 ],
  learn03: [ 21, 22, 28, 29, 32, 33 ],
  learn04: [ 14, 15, 31, 36, 38, 39 ],
  learn05: [ 17, 18, 24, 26, 35, 40, 43 ],
  learn06: [ 16, 19, 23, 27, 34, 41 ],
}

const subjectTrials = {
  "14": [ "rs","rd","bd","rs","ys","rd","yd","ys","bc","yd","bd","rs","ys","rd","yd" ],
  "15": [ "ys","ys","ys","ys","rs","ys","rs","ys","bs","ys","bd","ys","bs","yd","rd" ],
  "16": [ "bs","bs","bs","bs","bs","bs","bs","rs","rs","rs","rs","rd","rd","rd","rd" ],
  "17": [ "bs","bc","bs","yc","ys","bc","bs","rc","rs","rc","rs","rc","rs","rc","rs" ],
  "18": [ "bs","bc","ys","bc","ys","yc","yc","rc","rs","rc","ys","rs","yd","rc","rs" ],
  "19": [ "bs","bs","bs","bd","bd","bd","bd","rs","rs","rs","rs","rd","rd","rd","rd" ],
  "20": [ "bs","yc","bc","ys","bs","yc","bc","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "21": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","bd","yd","yc","yc" ],
  "22": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "23": [ "rs","bs","bs","rs","rs","rs","rs","yd","rd","bd","bs","rd","rd","rd","rd" ],
  "24": [ "yc","yc","yc","rc","rc","rc","rc","yc","yc","yc","yc","rc","rc","rc","rc" ],
  "25": [ "bc","rd","bd","rc","bc","rd","bd","rc","bc","rd","bs","rd","bd","rs","bs" ],
  "26": [ "rc","bc","yc","rc","rc","rc","rc","ys","ys","ys","yc","rs","rs","rc","rc" ],
  "27": [ "bs","bs","bs","yc","yd","rc","bs","rs","rs","rc","rc","yc","bs","rc","rd" ],
  "28": [ "bs","bc","bc","bd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "29": [ "bc","bs","bc","bs","bc","bd","bc","bs","ys","yd","yc","rd","yc","yd","yc" ],
  "30": [ "bc","rs","bd","rc","bc","rd","bd","rd","bd","rs","bs","rd","bd","rs","bs" ],
  "31": [ "bc","bc","rc","ys","rc","yd","yd","rd","rc","yd","bd","rs","rs","rd","bd" ],
  "32": [ "bd","bd","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "33": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "34": [ "bs","bs","rd","yc","rc","rs","rs","bd","bs","bd","bd","rd","rd","rd","rd" ],
  "35": [ "rc","yc","yc","yc","rc","rc","rc","yc","ys","ys","ys","yd","rc","rs","rc" ],
  "36": [ "yd","bs","bd","ys","ys","bs","bd","ys","yd","bs","bd","ys","yd","bs","bd" ],
  "38": [ "yd","rs","rd","ys","yd","rs","rd","rs","rd","bs","bd","rs","rd","bs","bd" ],
  "39": [ "bs","yd","bd","ys","bs","yd","bd","ys","bs","yd","bd","rc","rc","rd","bd" ],
  "40": [ "yc","ys","ys","rc","rc","rc","rs","yd","yc","ys","yd","rc","rd","rs","rs" ],
  "41": [ "bd","bs","bs","bd","bd","bd","bd","rs","rs","rs","rs","rd","rd","rd","rd" ],
  "42": [ "yc","rs","bs","rc","bc","rs","bs","rc","bd","yd","yd","rd","bd","rs","bd" ],
  "43": [ "yd","yc","yc","yd","yc","yd","yc","rd","rc","ys","ys","bc","rc","rs","rc" ],
  "44": [ "rs","bs","bs","yd","yd","bd","bd","ys","ys","bs","bs","yd","yd","bd","bd" ],
  "45": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs" ],
  "46": [ "bc","rs","bs","rc","bc","rs","bs","rd","bd","rd","bd","rd","bd","rd","bd" ],
  "51": [ "rs","bs","bs","yd","yd","bd","bd","bs","ys","bs","bs","yd","yd","bd","bd" ],
  "52": [ "ys","bs","bs","ys","ys","bs","bs","ys","ys","bs","bs","yd","bd","bd","bd" ],
}

Object.keys(trainings).forEach (t => createViz(t));
//createViz('learn03');

function createViz(taskId) {
  let div = createDivWithId(`div-taskId`);
  div.append(createTaskInfo(taskId));
  div.append(createEffectSummary(taskId));
  div.append(createPptSummary(taskId));
  document.body.append(div);

  effectsHistory(trainings[taskId]);
}

function createPptSummary (taskId) {
  let nPpt = 4;
  let div = document.createElement('div');
  div.append(createElementWithText('h2', 'Trials'));

  const trials = createTrialDataObj(trainings[taskId]);
  div.append(createTrialPanel(taskId, trials))

  return div;
}

function createEffectSummary (taskId) {
  function createBox (type) {
    let innerBox = createDivWithClass("box-mem");
    innerBox.append(createElementWithText('p', capitalize(type)));

    let effectBox = createDivWithId(`membox-${taskId}-${type}`);
    effectBox.setAttribute("class", `membox-${type}`);
    innerBox.append(effectBox);

    return innerBox;
  }

  let div = document.createElement('div');
  div.append(createElementWithText('h2', 'Effect'));

  let box = createDivWithClass("box-lt");
  box.append(createBox('before'));
  box.append(createBox('after'));
  div.append(box)

  return div;
}


function createTaskInfo (taskId) {
  let div = document.createElement('div');
  div.append(createElementWithText('h1', capitalize(taskId)));
  return div;
}

function capitalize (s) {
  if (typeof s !== 'string') return ''
  return s.charAt(0).toUpperCase() + s.slice(1)
}

function createElementWithText (el, text) {
  let element = document.createElement(el);
  let tx = document.createTextNode(text);
  element.appendChild(tx);
  return element;
}

function createDivWithClass (className) {
  let element = document.createElement('div');
  element.setAttribute("class", className);
  return element;
}

function createDivWithId (id) {
  let element = document.createElement('div');
  element.setAttribute("id", id);
  return element;
}

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

  createStones(effectBefore, `membox-${learningTask.taskId}-before`);
  createStones(effectAfter, `membox-${learningTask.taskId}-after`);
}


function readEffect (stone, rule) {
  let changed = '';
  if(rule[2] === 'r' || rule[2] === 'b' || rule[2] === 'y') {
    changed = rule[2] + stone[1];
  } else {
    changed = stone[0] + rule[2];
  }
  return changed;
}

function createStones (task, boxId) {
  let magicStone = createDivWithClass(`stone-${task.magicStone}`);
  let normalStone = createDivWithClass(`stone-${task.normalStone}`);
  magicStone.style.border = magicStoneBorderStyle;

  const taskBox = document.getElementById(boxId);
  taskBox.append(magicStone);
  taskBox.append(normalStone);
}

function createTrialDataObj (learningTask) {
  let trials = {};
  for (let i = 1; i < 16; i++) {
    trialId = 'trial' + i.toString().padStart(2, '0');
    trials[trialId] = {};
    trials[trialId]['taskId'] = trialId;
    trials[trialId]['magicStone'] = '';
    trials[trialId]['normalStone'] = '';
    createTrials(learningTask, trials[trialId]);
  }
  return trials;
}

function createTrials (learningTask, trial, idx=true) {
  const colors = [ 'r', 'y', 'b' ];
  const shapes = [ 'c', 'd', 's' ];

  const agentColor = learningTask.magicStone[0];
  const agentShape = learningTask.magicStone[1];
  const recipientColor = learningTask.normalStone[0];
  const recipientShape = learningTask.normalStone[1];

  const diffColor = (colors.filter(c => (c !== agentColor && c !== recipientColor)))[0];
  const diffShape = (shapes.filter(c => (c !== agentShape && c !== recipientShape)))[0];

  const trialIndex = parseInt(trial.taskId.slice(5,));
  switch(trialIndex % 4) {
    case 1:
      trial.normalStone = idx? diffColor + recipientShape : recipientColor + diffShape;
      break;
    case 2:
      trial.normalStone = idx? recipientColor + diffShape : diffColor + recipientShape;
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
    trial.magicStone = idx? diffColor + agentShape : agentColor + diffShape;
  } else if (trialIndex > 7 && trialIndex < 12) {
    trial.magicStone = idx? agentColor + diffShape : diffColor + agentShape;
  } else {
    trial.magicStone = diffColor + diffShape;
  }
}

function createTrialPanel(taskId, trials) {
  const readTrial = (idx) => idx.toString().padStart(2, '0');
  const n = subjectConditions[taskId].length;

  let tbl = document.createElement('table');
  tbl.className = 'panel';

  let header = tbl.insertRow();
  const pars = subjectConditions[taskId].map(ix => `Pt ${ix}`);
  let headerEls = [ "Trial", "Agent", "Recipient"].concat(pars);
  headerEls.forEach(el => {
    header.insertCell().appendChild(createElementWithText('p', el))
  })

  for(let i = 1; i < 16; i++){
    let tr = tbl.insertRow();
    for(let j = 0; j < (n + 3); j++){
      let tc = (j == 0)? createElementWithText('p', i.toString()) :
        (j == 1)? createDivWithClass('stone-' + trials[`trial${readTrial(i)}`].magicStone):
          (j == 2)? createDivWithClass('stone-' + trials[`trial${readTrial(i)}`].normalStone):
            createDivWithClass('stone-' + drawSelection(taskId, i, j));
      tr.insertCell().appendChild(tc);
    }
  }
  return tbl;
}

function drawSelection(taskId, trialIdx, parIdx) {
  if (parIdx > 2 && parIdx < subjectConditions[taskId].length + 3) {
    let par = subjectConditions[taskId][parIdx-3];
    let selection = subjectTrials[par.toString()][trialIdx-1];
    return selection;
  }
}
/** Export ordered trials for R-cleanups */
// let exportToR = {
//   'learningTaskId': [], 'trial': [],
//   'agent': [], 'recipient': [],
// };
// Object.keys(trainings).forEach (t => {
//   exportToR['learningTaskId'] = exportToR['learningTaskId'].concat(Array(15).fill(t));
//   let trialInfo = createTrialDataObj(trainings[t]);
//   Object.keys(trialInfo).forEach(o => {
//     exportToR.trial.push(parseInt(o.slice(5,)));
//     exportToR.agent.push(trialInfo[o].magicStone);
//     exportToR.recipient.push(trialInfo[o].normalStone);
//   })
// });
// console.log(exportToR);
