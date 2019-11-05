
// See prep.js for human readable configs
const magics = [
    colorToColor,
    colorToSize,
    colorToShape,
    sizeToColor,
    shapeToColor,
];

function magicEffects (active_id) {
    // Play magic effect on the overlap inactive stone
    // Only if the active one is a magic stone
    function doMagic (active_id, inactive_id) {
        const activePos = stones[active_id].rect;
        const inactivePos = stones[inactive_id].rect;
        const activeType = stones[active_id].type;
        const overlap = !(activePos.right < inactivePos.left ||
                          activePos.left > inactivePos.right ||
                          activePos.bottom < inactivePos.top ||
                          activePos.top > inactivePos.bottom);
        const isMagic = (activeType === 'magic') && overlap;
        // Smooth transition visual effect
        document.getElementById(inactive_id).style.transition = isMagic? "all 0.8s" : '';
        magics.map(magic => (isMagic && magic(active_id, inactive_id)));
    }
    // Compare with each inactive stones
    const inactive_ids = (Object.keys(stones).filter(s => !stones[s].active));
    inactive_ids.map(inactive_id => doMagic(active_id, inactive_id));
}

// Magic functions
function colorToColor(sender, receiver) {
    const colorFrom = getComputedStyle(document.getElementById(sender))['background-color'];
    const colorTo = colorRules[colorFrom];
    !!colorTo && (document.getElementById(receiver).style.background = colorTo);
}

function colorToSize(sender, receiver) {
    const colorFrom = getComputedStyle(document.getElementById(sender))['background-color'];
    const sizeTo = colorSizeRules[colorFrom];
    if (!!sizeTo) {
        document.getElementById(receiver).style.width = (sizeTo + 'px');
        document.getElementById(receiver).style.height = (sizeTo + 'px');
    }
}

function sizeToColor(sender, receiver) {
    const sizeFrom = document.getElementById(sender).clientHeight;
    const colorTo = sizeColorRules[sizeFrom];
    !!colorTo && (document.getElementById(receiver).style.background = colorTo);
}

function colorToShape(sender, receiver) {
    const colorFrom = getComputedStyle(document.getElementById(sender))['background-color'];
    const shapeTo = colorShapeRules[colorFrom];
    if (!!shapeTo) {
        switch (shapeTo) {
            case 'diamond':
                document.getElementById(receiver).style.borderRadius = '10%';
                document.getElementById(receiver).style.transform = 'rotate(45deg) scale(.8)';
                break;
            case 'circle':
                document.getElementById(receiver).style.borderRadius = '50%';
                break;
            case 'square':
                document.getElementById(receiver).style.borderRadius = '10%';
                break;
            default:
                console.log('Nothing to change');
        }
    }
}

function shapeToColor(sender, receiver) {
    const shapeFrom = document.getElementById(sender).style.borderRadius;
    const colorTo = shapeColorRules[shapeFrom];
    !!colorTo && (document.getElementById(receiver).style.backgroundColor = colorTo);
}
