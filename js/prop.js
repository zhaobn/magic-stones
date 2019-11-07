const myColors = {
    red: 'rgb(255, 102, 102)',
    blue: 'rgb(61, 60, 134)',
    yellow: 'rgb(245, 230, 99)'
  }

const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';

// Task config object
const learningTaskConfig = {
    taskId: 'learning1',
    magicStone: 'rc',
    normalStone: 'ys',
    rules: [ 'r2r', 'y2c' ],
};

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
