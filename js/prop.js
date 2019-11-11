
/** Dev control */
const isRandom = false;

/** Custom styling definitions */
const myColors = {
    red: 'rgb(255, 102, 102)',
    blue: 'rgb(61, 60, 134)',
    yellow: 'rgb(245, 230, 99)'
  }

const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';

/** Trask setups */
const learn01 = { taskId: 'learn01', magicStone: 'rd', normalStone: 'yc', rules: [ 'r2b', 'd2d' ] };
const learn02 = { taskId: 'learn02', magicStone: 'rc', normalStone: 'yc', rules: [ 'r2b' ] };
const learn03 = { taskId: 'learn02', magicStone: 'bd', normalStone: 'yc', rules: [ 'd2d' ] };

const gen01 = { taskId: 'gen01', magicStone: 'rd', normalStone: 'ys' };
const gen02 = { taskId: 'gen02', magicStone: 'rd', normalStone: 'rc' };
const gen03 = { taskId: 'gen03', magicStone: 'rc', normalStone: 'ys' };
const gen04 = { taskId: 'gen04', magicStone: 'rc', normalStone: 'rc' };
const gen05 = { taskId: 'gen05', magicStone: 'bd', normalStone: 'ys' };
const gen06 = { taskId: 'gen06', magicStone: 'bd', normalStone: 'bc' };

/** Trail setups */
const trails = {
    'trial01': {'learn': learn01, 'gen': gen01},
    'trial02': {'learn': learn01, 'gen': gen02},
    'trial03': {'learn': learn01, 'gen': gen03},
    'trial04': {'learn': learn01, 'gen': gen05},
    'trial05': {'learn': learn02, 'gen': gen03},
    'trial06': {'learn': learn02, 'gen': gen04},
    'trial07': {'learn': learn02, 'gen': gen01},
    'trial08': {'learn': learn02, 'gen': gen02},
    'trial09': {'learn': learn03, 'gen': gen01},
    'trial10': {'learn': learn03, 'gen': gen02},
    'trial11': {'learn': learn03, 'gen': gen05},
    'trial12': {'learn': learn03, 'gen': gen06},
}
