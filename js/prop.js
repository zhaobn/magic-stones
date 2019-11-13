
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
const trainings = {
  learn01: { taskId: 'learn01', magicStone: 'rd', normalStone: 'yc', rules: [ '-2d' ] },
  learn02: { taskId: 'learn02', magicStone: 'yc', normalStone: 'rd', rules: [ '-2s' ] },
  learn03: { taskId: 'learn02', magicStone: 'bs', normalStone: 'rc', rules: [ '-2b' ] },
  learn04: { taskId: 'learn04', magicStone: 'rc', normalStone: 'bs', rules: [ '-2y' ] },
  learn05: { taskId: 'learn05', magicStone: 'yd', normalStone: 'bs', rules: [ '-2y', '-2d' ] },
  learn06: { taskId: 'learn06', magicStone: 'bs', normalStone: 'yd', rules: [ '-2r', '-2c' ] },
}
