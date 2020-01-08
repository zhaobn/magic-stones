
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
  random:  { magicStone: 'bs', normalStone: 'rd', rules: [ '-2b' ] },
  learn07: { magicStone: 'rd', normalStone: 'ys', rules: [ '-2b', '-2c' ] },
}
Object.keys(trainings).forEach (t => trainings[t].taskId = t);

/** Subject data */
const subjectConditions = {
  learn01: [ 20, 44, 51, 52, 71, 80, 84, 92 ],
  learn02: [ 25, 30, 42, 45, 46, 79, 87, 88, 89 ],
  learn03: [ 21, 22, 28, 29, 32, 33, 69, 74, 76, 82, 85 ],
  learn04: [ 14, 15, 31, 36, 38, 39, 70, 72, 75, 86, 91, 93 ],
  learn05: [ 17, 18, 24, 26, 35, 40, 43, 73, 78, 81, 83 ],
  learn06: [ 16, 19, 23, 27, 34, 41, 68, 77, 90, 94 ],
  random:  [ 59, 60, 61, 62, 63, 64, 65, 66 ],
  learn07: [ 96, 97, 98, 99, 100, 101, 102, 103, 104 ],
}

const subjectTrials = {
  "14": [ "rs","rd","bd","rs","ys","rd","yd","ys","bc","yd","bd","rs","ys","rd","yd" ],
  "15": [ "ys","ys","ys","ys","rs","ys","rs","ys","bs","ys","bd","ys","bs","yd","rd" ],
  "16": [ "bs","bs","bs","rs","rs","rs","rs","bs","bs","bs","bs","rd","rd","rd","rd" ],
  "17": [ "bc","bs","bs","rc","rc","rs","rs","yc","bc","ys","bs","rc","rc","rs","rs" ],
  "18": [ "bc","bs","ys","rc","rc","rs","ys","bc","yc","ys","yc","rs","rc","yd","rs" ],
  "19": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "20": [ "bs","yc","bc","ys","bs","yc","bc","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "21": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","bd","yd","yc","yc" ],
  "22": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "23": [ "rs","bs","bs","rs","rs","rs","rs","yd","rd","bd","bs","rd","rd","rd","rd" ],
  "24": [ "yc","yc","yc","rc","rc","rc","rc","yc","yc","yc","yc","rc","rc","rc","rc" ],
  "25": [ "bc","rd","bd","rc","bc","rd","bd","rc","bc","rd","bs","rd","bd","rs","bs" ],
  "26": [ "rc","bc","yc","rc","rc","rc","rc","ys","ys","ys","yc","rs","rs","rc","rc" ],
  "27": [ "bs","bs","bs","rs","rc","rs","rc","yc","rc","yd","bs","yc","rc","bs","rd" ],
  "28": [ "bs","bc","bc","bd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "29": [ "bs","bc","bc","bs","yd","ys","yc","bs","bd","bc","bc","rd","yd","yc","yc" ],
  "30": [ "bc","rs","bd","rc","bc","rd","bd","rd","bd","rs","bs","rd","bd","rs","bs" ],
  "31": [ "bc","bc","rc","ys","rc","yd","yd","rd","rc","yd","bd","rs","rs","rd","bd" ],
  "32": [ "bd","bd","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "33": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "34": [ "bs","bs","rd","yc","rc","rs","rs","bd","bs","bd","bd","rd","rd","rd","rd" ],
  "35": [ "rc","yc","yc","yc","rc","rc","rc","yc","ys","ys","ys","yd","rc","rs","rc" ],
  "36": [ "bs","yd","bd","ys","bs","yd","bd","ys","bs","ys","bd","ys","bs","yd","bd" ],
  "38": [ "rs","yd","rd","rs","bs","rd","bd","ys","rs","yd","rd","rs","bs","rd","bd" ],
  "39": [ "bs","yd","bd","ys","bs","yd","bd","ys","bs","yd","bd","rc","rc","rd","bd" ],
  "40": [ "yc","ys","ys","rc","rc","rc","rs","yd","yc","ys","yd","rc","rd","rs","rs" ],
  "41": [ "bs","bd","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "42": [ "yc","rs","bs","rc","bc","rs","bs","rc","bd","yd","yd","rd","bd","rs","bd" ],
  "43": [ "yc","yd","yc","rd","ys","rc","ys","yd","yd","yc","yc","bc","rs","rc","rc" ],
  "44": [ "bs","rs","bs","ys","bs","ys","bs","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "45": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs" ],
  "46": [ "bc","rs","bs","rc","bc","rs","bs","rd","bd","rd","bd","rd","bd","rd","bd" ],
  "51": [ "bs","rs","bs","bs","bs","ys","bs","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "52": [ "bs","ys","bs","ys","bs","ys","bs","ys","bs","ys","bs","yd","bd","bd","bd" ],
  "59": [ "bd","bc","bc","yd","bd","yc","yc","bd","bc","bc","bc","yd","yd","yc","yc" ],
  "60": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "61": [ "yd","bs","bs","rs","bs","yd","yc","bs","rs","bd","rc","yd","rd","yc","bc" ],
  "62": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","rs","yc","yc","yc" ],
  "63": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "64": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "65": [ "bd","bd","bd","yd","yd","yd","yd","bs","bs","bs","bd","ys","ys","ys","ys" ],
  "66": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "68": [ "rs","bd","bd","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "69": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "70": [ "bs","rd","bd","rs","bs","rd","bd","rs","bs","rd","bd","rs","bs","rd","bd" ],
  "71": [ "bs","ys","bs","ys","bs","ys","bs","ys","bd","yd","bd","yd","bd","yd","bd" ],
  "72": [ "rs","ys","rd","rs","rs","rs","bs","rs","bs","rs","rs","ys","bs","rs","rs" ],
  "73": [ "yc","yc","yc","rc","rc","rc","rc","yc","ys","ys","ys","rs","rs","rs","rs" ],
  "74": [ "rd","yd","rc","bd","bd","bc","rc","yd","bc","bc","bc","yd","bd","yc","bc" ],
  "75": [ "ys","yd","yd","ys","ys","yd","yd","ys","ys","yd","yd","ys","ys","yd","yd" ],
  "76": [ "bd","rd","bd","bd","bd","bd","yc","bd","rd","rc","bd","bd","yd","yc","yc" ],
  "77": [ "rs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "78": [ "yc","yc","yc","yc","rc","ys","ys","yd","yc","ys","ys","rd","rd","rs","rs" ],
  "79": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs" ],
  "80": [ "bs","ys","bs","ys","bs","ys","bs","ys","bs","yd","bd","yd","bd","yd","bd" ],
  "81": [ "yc","bc","ys","rc","rc","bs","rs","yc","yd","bs","yd","rd","rd","rs","rs" ],
  "82": [ "bd","bc","bc","yd","yd","yc","yc","bd","bc","bc","bc","yd","yd","yc","yc" ],
  "83": [ "yc","yc","yc","rc","rc","rc","rc","yc","yc","yc","yc","rc","rc","rc","rc" ],
  "84": [ "bs","ys","bs","ys","bs","ys","bs","yd","bd","yd","bd","yd","bd","bd","bd" ],
  "85": [ "bd","bd","bd","bd","bd","bd","bc","bd","bd","bd","bc","bd","bd","bd","bc" ],
  "86": [ "bs","yd","bd","ys","bs","yd","bd","ys","bs","yd","bs","rs","bs","yd","bd" ],
  "87": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs","rc","bd","rs","bs" ],
  "88": [ "bc","rd","bd","rc","bc","rd","bd","rd","bd","rc","bc","rs","bs","rc","rs" ],
  "89": [ "bc","rc","bs","rc","bc","rs","bs","rd","bc","rd","bd","rd","bd","rd","bd" ],
  "90": [ "rs","bs","bs","rs","rs","rs","rs","bs","bs","bd","bd","rd","rd","rd","rd" ],
  "91": [ "bs","yd","bd","ys","bs","yd","rd","ys","bs","yd","bd","rs","rs","rd","rd" ],
  "92": [ "bs","yd","bd","ys","bs","ys","bs","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "93": [ "rs","rs","rd","bs","bs","rd","bd","rs","bs","yd","bd","rs","bs","rd","bd" ],
  "94": [ "bs","bd","bs","bs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
}

/** Create page */
//Object.keys(trainings).forEach (t => createViz(t));
createViz('learn06');


/** Helper functions */
function createViz(taskId) {
  let div = createDivWithId(`div-taskId`);
  //div.append(createTaskInfo(taskId));
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
  let title = "Group " + taskId.slice(6,);
  div.append(createElementWithText('h1', capitalize(title)));
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
  const pars = subjectConditions[taskId].map(ix => `P ${ix}`);
  let headerEls = [ "Trial", "A", "R", "" ].concat(pars);
  headerEls.forEach(el => {
    header.insertCell().appendChild(createElementWithText('p', el))
  })

  for(let i = 1; i < 16; i++){
    let tr = tbl.insertRow();
    for(let j = 0; j < (n + 4); j++){
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
  if (parIdx > 3 && parIdx < subjectConditions[taskId].length + 4) {
    let par = subjectConditions[taskId][parIdx-4];
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
