
/** Custom styling definitions */
const myColors = {
    red: 'rgb(255, 102, 102)',
    blue: 'rgb(61, 60, 134)',
    yellow: 'rgb(245, 230, 99)'
  }

const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';

/** Trask setups */
const learn01 = { taskId: 'learn01', magicStone: 'rd', normalStone: 'yc', rules: [ 'r2d' ] };
const learn02 = { taskId: 'learn02', magicStone: 'rc', normalStone: 'bs', rules: [ 'r2d' ] };

const gen01 = { taskId: 'gen01', magicStone: 'rd', normalStone: 'bc' };
const gen02 = { taskId: 'gen02', magicStone: 'rd', normalStone: 'yd' };
const gen03 = { taskId: 'gen03', magicStone: 'rd', normalStone: 'ys' };
const gen04 = { taskId: 'gen04', magicStone: 'bd', normalStone: 'bc' };
const gen05 = { taskId: 'gen05', magicStone: 'bd', normalStone: 'yd' };
const gen06 = { taskId: 'gen06', magicStone: 'yd', normalStone: 'ys' };
const gen07 = { taskId: 'gen07', magicStone: 'yd', normalStone: 'bs' };
const gen08 = { taskId: 'gen08', magicStone: 'rs', normalStone: 'bs' };
const gen09 = { taskId: 'gen09', magicStone: 'bc', normalStone: 'bs' };
const gen10 = { taskId: 'gen10', magicStone: 'rs', normalStone: 'ys' };
const gen11 = { taskId: 'gen11', magicStone: 'rc', normalStone: 'bc' };
const gen12 = { taskId: 'gen12', magicStone: 'rc', normalStone: 'yd' };

/** Trail setups */
const trails = {
    'trial01': {'learn': learn01, 'gen': gen01},
    'trial02': {'learn': learn01, 'gen': gen02},
    'trial03': {'learn': learn01, 'gen': gen03},
    'trial04': {'learn': learn01, 'gen': gen04},
    'trial05': {'learn': learn01, 'gen': gen05},
    'trial06': {'learn': learn01, 'gen': gen06},
    'trial07': {'learn': learn02, 'gen': gen07},
    'trial08': {'learn': learn02, 'gen': gen08},
    'trial09': {'learn': learn02, 'gen': gen09},
    'trial10': {'learn': learn02, 'gen': gen10},
    'trial11': {'learn': learn02, 'gen': gen11},
    'trial12': {'learn': learn02, 'gen': gen12},
}

/** Task setup */
let taskData = {
    task01: {
        taskId: 'task01',
        trialId: 'trial01',
        clientData: {},
    },
    task02: {
        taskId: 'task02',
        trialId: 'trial04',
        clientData: {},
    },
    task03: {
        taskId: 'task03',
        trialId: 'trial03',
        clientData: {},
    },
    task04: {
        taskId: 'task04',
        trialId: 'trial11',
        clientData: {},
    }
};
