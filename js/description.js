
const demo = {
    taskId: 'demo', magicStone: 'rc', normalStone: 'yc', rules: [ 'r2r' ]
};

createStones(demo);
document.getElementById('play-demo').onclick = () => playEffects(demo);
document.getElementById('reset-demo').onclick = () => resetStones(demo);
