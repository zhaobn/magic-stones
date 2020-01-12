
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

let exportToR = {
  'learningTaskId': [], 'trial': [],
  'agent': [], 'recipient': [],
};
Object.keys(trainings).forEach (t => {
  exportToR['learningTaskId'] = exportToR['learningTaskId'].concat(Array(15).fill(t));
  let trialInfo = createTrialDataObj(trainings[t]);
  Object.keys(trialInfo).forEach(o => {
    exportToR.trial.push(parseInt(o.slice(5,)));
    exportToR.agent.push(trialInfo[o].magicStone);
    exportToR.recipient.push(trialInfo[o].normalStone);
  })
});
console.log(JSON.stringify(exportToR));

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
