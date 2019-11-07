
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

function setEffects (id) {
    const rules = learningTaskConfig.rules;
    rules.forEach(rule => getEffect(id, rule));
}

function getEffect (id, rule) {
    const keyword = rule[2];

    // Check if effects takes place
    if (learningTaskConfig.magicStone[0] === rule[0]) {
        // Apply effects
        switch (keyword) {
            case 'r':
                changeColor(id, 'red');
                break;
            case 'y':
                changeColor(id, 'yellow');
                break;
            case 'b':
                changeColor(id, 'blue');
                break;
            case 'c':
                changeShape(id, 'circle');
                break;
            case 's':
                changeShape(id, 'square');
                break;
            case 'd':
                changeShape(id, 'diamond');
                break;
        }
    }

}

function changeStone () {
    const normalStoneId = `${learningTaskConfig.taskId}-normal-stone`;
    const normalStone = document.getElementById(normalStoneId);
    normalStone.style.transitionDelay = '1s';
    setEffects(normalStoneId);
}

function resetStones () {
    const magicStoneId = `${learningTaskConfig.taskId}-magic-stone`;
    const normalStoneId = `${learningTaskConfig.taskId}-normal-stone`;
    const stones = [ magicStoneId, normalStoneId ];
    // Clear existing stones
    stones.forEach(stone => {
        let el = document.getElementById(stone);
        el.parentNode.removeChild(el);
    });
    // Create new stones
    createStones();
}

// Add play button function
const playBtn = document.getElementById('play1');
playBtn.onclick = () => {
    window.scrollTo({top: 0, behavior: 'smooth'});
    moveStone();
    changeStone();
};

// Add reset button function
const resetBtn = document.getElementById('reset1');
resetBtn.onclick = () => {
    window.scrollTo({top: 0, behavior: 'smooth'});
    resetStones();
};
