
// Task config object
const learningTaskConfig = {
    taskId: 'learning1',
    magicStone: 'rc',
    normalStone: 'ys',
    rules: ['r2d'],
};

// Create stones
function createStones () {
    let magicStone = document.createElement('div');
    setAttributes(magicStone, {
        'class': `stone-${learningTaskConfig.magicStone}`,
        'id': `${learningTaskConfig.taskId}-magic-stone`,
    });
    magicStone.style.border = magicStoneBorderStyle;
    let normalStone = document.createElement('div');
    setAttributes(normalStone, {
        'class': `stone-${learningTaskConfig.normalStone}`,
        'id': `${learningTaskConfig.taskId}-normal-stone`,
    });

    const taskBox = document.getElementById(learningTaskConfig.taskId);
    taskBox.append(magicStone);
    taskBox.append(normalStone);
}

createStones();

// Set up animation
function moveStone () {
    const magicStoneId = `${learningTaskConfig.taskId}-magic-stone`;
    const normalStoneId = `${learningTaskConfig.taskId}-normal-stone`;

    const magicStone = document.getElementById(magicStoneId);

    const startPos = getCurrentLocation(magicStoneId).right;
    const endPos = getCurrentLocation(normalStoneId).left;

    const delta = Math.round(endPos - startPos);
    (delta > 0) && (magicStone.style.left = `${delta}px`);
};

function changeStone () {
    const normalStoneId = `${learningTaskConfig.taskId}-normal-stone`;
    const normalStone = document.getElementById(normalStoneId);
    normalStone.style.transitionDelay = '1s';
    normalStone.style.background = 'white';
}

function resetStones () {
    const magicStoneId = `${learningTaskConfig.taskId}-magic-stone`;
    const normalStoneId = `${learningTaskConfig.taskId}-normal-stone`;
    const stones = [ magicStoneId, normalStoneId ];

    // Clear stones
    stones.forEach(stone => {
        let el = document.getElementById(stone);
        el.parentNode.removeChild(el);
    });
    // Create stones
    createStones();
}

// Add play button function
const playBtn = document.getElementById('play1');
playBtn.onclick = () => {
    moveStone();
    changeStone();
};

// Add reset button function
const resetBtn = document.getElementById('reset1');
resetBtn.onclick = resetStones;
