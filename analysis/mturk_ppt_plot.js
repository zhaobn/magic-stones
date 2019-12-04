
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

// Object.keys(trainings).forEach (t => createViz(t));

createViz('learn01');

function createViz(taskId) {
  let div = createDivWithId(`div-taskId`);
  div.append(createTaskInfo(taskId));
  div.append(createEffectSummary(taskId));
  div.append(createPptSummary());
  document.body.append(div);

  effectsHistory(trainings[taskId]);
}

function createPptSummary () {
  let div = document.createElement('div');
  div.append(createElementWithText('h2', 'Trials summary'));
  return div;
}

function createEffectSummary (taskId) {
  function createBox (type) {
    let innerBox = createDivWithClass("box-mem");
    let pt = createElementWithText('p', capitalize(type));
    innerBox.append(pt);
    let effectBox = createDivWithId(`membox-${taskId}-${type}`);
    effectBox.setAttribute("class", `membox-${type}`);
    innerBox.append(effectBox);

    return innerBox;
  }

  let div = document.createElement('div');
  div.append(createElementWithText('h2', 'Effect summary'));

  let box = createDivWithClass("box-lt");
  box.append(createBox('before'));
  box.append(createBox('after'));
  div.append(box)

  return div;
}


function createTaskInfo (taskId) {
  let div = document.createElement('div');
  div.append(createElementWithText('h1', capitalize(taskId)));
  div.append(createElementWithText('p', 'Number of participants: 4'));
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
  const taskBox = document.getElementById(boxId);
  taskBox.append(magicStone);
  taskBox.append(normalStone);
}

function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}
