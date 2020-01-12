
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
  learn07: { magicStone: 'rd', normalStone: 'ys', rules: [ '-2b', '-2c' ] },
}
Object.keys(trainings).forEach (t => trainings[t].taskId = t);

/** Subject data */
const subjectConditions = {
  learn01: [ 130, 131, 137, 138, 148, 152, 154, 162, 175, 177 ],
  learn02: [ 111, 124, 125, 136, 146, 153, 161, 164, 165, 166, 170, 173 ],
  learn03: [ 107, 112, 119, 121, 142, 143, 151, 156, 163 ],
  learn04: [ 113, 141, 145, 147, 150, 155, 158 ],
  learn05: [ 108, 110, 115, 117, 129, 132, 135, 139, 157, 167 ],
  learn06: [ 109, 116, 118, 123, 126, 134, 140, 144, 168, 169, 172, 174 ],
  learn07: [ 114, 120, 122, 127, 128, 133, 149, 159, 160, 171, 176 ],
}

const subjectTrials = {
  "107": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "108": [ "yc","yc","yc","rc","rc","rc","rc","yc","yc","yc","rc","rc","rc","rc","rc" ],
  "109": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","bd" ],
  "110": [ "yc","ys","ys","rc","rc","rs","rs","yd","yd","ys","ys","rd","rc","ys","rc" ],
  "111": [ "bc","rs","bs","rc","bc","rs","bs","rd","bd","rs","ys","rd","bd","rs","bs" ],
  "112": [ "bd","bc","bs","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "113": [ "bs","yd","bd","rs","bs","rd","ys","ys","bc","yd","bd","rc","yc","rd","yd" ],
  "114": [ "rc","bd","rd","bc","rc","bd","rd","bc","rc","bd","rd","bc","yc","bd","rd" ],
  "115": [ "yc","ys","ys","rc","rc","rs","rs","yd","yd","yc","yc","rd","rd","rc","rc" ],
  "116": [ "rd","yc","bd","ys","bc","bs","bc","rd","rd","yc","rc","yd","yc","yc","yd" ],
  "117": [ "yc","yc","yc","rc","rc","rc","rc","yc","yc","yc","yc","rc","rc","rc","rs" ],
  "118": [ "bs","bs","bs","rs","rs","rs","rd","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "119": [ "bs","rs","bs","bs","bc","yd","yd","bs","rs","rs","bd","ys","ys","yc","rs" ],
  "120": [ "yc","bd","ys","bc","yc","bs","ys","yd","yc","bs","ys","bc","bc","bs","bc" ],
  "121": [ "ys","ys","bd","bd","ys","bs","rs","bs","yc","yc","bs","rs","bs","bd","yc" ],
  "122": [ "yc","bs","ys","rc","rc","rs","rs","bd","bc","bd","bs","rc","rd","rd","bc" ],
  "123": [ "rs","ys","rs","ys","rs","ys","rs","ys","rs","ys","rs","yd","rs","yd","rs" ],
  "124": [ "bc","rc","bc","rc","bc","rc","bc","rc","bc","rc","bc","rc","bc","rc","bc" ],
  "125": [ "rc","rc","rc","rc","yd","rc","rc","bc","rc","rc","rc","rc","rc","rc","rc" ],
  "126": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "127": [ "yc","bs","ys","bc","yc","bs","ys","bc","yc","bs","ys","bc","yc","bs","ys" ],
  "128": [ "yc","bd","ys","rc","yc","rd","yd","bc","yc","bd","yd","rc","rc","rs","yd" ],
  "129": [ "ys","yc","yd","rs","rd","rc","rs","yd","yc","yd","ys","rc","rd","ys","rc" ],
  "130": [ "bs","ys","bs","ys","bs","ys","bd","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "131": [ "rs","ys","bs","ys","bs","ys","bs","rd","bd","yd","bd","yd","bd","yd","bd" ],
  "132": [ "yc","yc","yc","rc","rc","rc","rs","yd","yc","yc","yc","rc","yc","rc","yc" ],
  "133": [ "ys","bc","yc","rd","rd","rd","yd","bs","rc","ys","ys","rc","yc","bs","bs" ],
  "134": [ "bd","bc","bc","rs","rs","rc","rc","ys","bs","bc","bc","rs","rs","rc","rd" ],
  "135": [ "rs","rc","rc","rs","rs","rc","rc","bs","rs","bc","rc","rc","rs","rc","rc" ],
  "136": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bs" ],
  "137": [ "bs","yc","bc","ys","bs","yc","bc","ys","bs","yc","bc","ys","bs","yc","bs" ],
  "138": [ "ys","bc","ys","ys","ys","ys","ys","yd","bs","ys","ys","bs","bd","yd","rs" ],
  "139": [ "yc","ys","ys","rc","rc","rs","rs","yd","yd","ys","yd","rd","yd","ys","bs" ],
  "140": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rs" ],
  "141": [ "bs","yd","bd","ys","bs","yd","bd","ys","bs","yd","bd","rs","bs","rd","rd" ],
  "142": [ "yd","rc","yc","rd","yd","rc","yc","rd","yd","rc","yc","rd","yd","rc","yc" ],
  "143": [ "bd","bc","bc","bd","bd","bd","bc","bd","bd","bc","bc","bd","bd","bc","bc" ],
  "144": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "145": [ "ys","ys","ys","ys","ys","ys","ys","ys","ys","ys","ys","ys","ys","ys","yd" ],
  "146": [ "bc","rd","bd","rc","bc","rd","bd","rc","bc","rd","bd","rc","bc","rd","bd" ],
  "147": [ "rs","yd","rd","rs","rs","rd","rd","ys","rd","yd","rd","rs","rd","rd","rd" ],
  "148": [ "bs","ys","bs","yd","bd","yc","bc","ys","bs","yc","bc","ys","bs","yc","bc" ],
  "149": [ "yc","bs","ys","rc","yc","rs","ys","bc","bc","bc","bc","yd","bd","bc","bd" ],
  "150": [ "ys","yd","yd","rs","rs","rd","rd","ys","ys","yd","bd","rd","bs","rd","rd" ],
  "151": [ "bd","bc","bc","yd","yd","yc","yd","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "152": [ "bs","ys","bs","ys","bs","yd","bs","yd","bd","yd","bd","yd","bd","bd","bd" ],
  "153": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rs","bc","rc","bc","rc","rs" ],
  "154": [ "bs","ys","bd","ys","bs","bs","bs","yd","bd","yd","bd","bd","bd","bd","bd" ],
  "155": [ "bs","yd","bd","ys","rs","rd","rd","ys","bs","yd","bd","rs","rs","rd","rd" ],
  "156": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yc","bc","yc","bc" ],
  "157": [ "ys","ys","ys","rc","rc","rs","rs","yc","yc","ys","ys","rc","rd","rd","rd" ],
  "158": [ "rs","yd","rd","ys","rs","rd","rd","ys","rs","yd","rd","rs","rs","yd","bd" ],
  "159": [ "bs","yc","bc","ys","bs","yc","bc","ys","bs","yc","bc","ys","bs","yc","bc" ],
  "160": [ "bc","bc","bc","rc","rc","rc","rc","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "161": [ "bd","rd","bd","rc","yc","rd","rd","rc","bc","rd","bd","yc","bc","rd","bd" ],
  "162": [ "bs","ys","bs","ys","bs","ys","bs","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "163": [ "bd","bc","bc","yd","yd","yc","yc","bd","bd","bc","bc","yd","yd","yc","yc" ],
  "164": [ "bc","rs","bs","rc","bc","rs","bs","rd","bd","rd","bd","rd","bc","yd","bd" ],
  "165": [ "bc","rs","bs","rc","bc","rs","bs","rd","bd","rs","bs","rd","bd","rs","bs" ],
  "166": [ "bc","rd","bd","rc","bc","rd","bs","rc","bc","rs","bs","rc","bc","rs","bd" ],
  "167": [ "yc","yc","yc","rc","rc","rc","yc","yc","yc","yc","yc","rc","rs","rc","rs" ],
  "168": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "169": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "170": [ "bc","rs","bs","rc","bc","rs","bs","rc","bc","rd","bd","rc","bc","rd","bd" ],
  "171": [ "yc","bc","ys","rd","rc","rd","ys","bc","rd","rd","rd","rd","rd","rd","rd" ],
  "172": [ "bs","bs","bs","rs","rs","rs","rs","bd","bd","bd","bd","rd","rd","rd","rd" ],
  "173": [ "bc","rc","bc","rc","bc","rc","bd","rc","bc","rd","bd","rc","bd","rd","bd" ],
  "174": [ "rs","bs","rs","bs","ys","rc","rd","rs","rs","ys","bc","rs","yc","bs","rs" ],
  "175": [ "bs","ys","bs","ys","bs","ys","bs","yd","bd","yd","bd","yd","bd","yd","bd" ],
  "176": [ "bc","bc","bc","bc","bc","bc","bc","bc","bc","bc","bc","bc","bc","bc","bc" ],
  "177": [ "bs","ys","bs","ys","bs","ys","bs","yd","bd","yd","bd","ys","bs","ys","bs" ],
}

/** Create page */
//Object.keys(trainings).forEach (t => createViz(t));
createViz('learn07');


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
