
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
    function doMagic (active_id, inactive_id) {
        const activePos = stones[active_id].rect;
        const inactivePos = stones[inactive_id].rect;
        const overlap = !(activePos.right < inactivePos.left || 
                          activePos.left > inactivePos.right || 
                          activePos.bottom < inactivePos.top || 
                          activePos.top > inactivePos.bottom);
        // Smooth transition visual effect
        document.getElementById(inactive_id).style.transition = overlap? "all 0.8s" : '';
        magics.map(magic => (overlap && magic(active_id, inactive_id)));
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
    !!shapeTo && (document.getElementById(receiver).style.borderRadius = (shapeTo));
}

function shapeToColor(sender, receiver) {
    const shapeFrom = document.getElementById(sender).style.borderRadius;
    const colorTo = shapeColorRules[shapeFrom];
    !!colorTo && (document.getElementById(receiver).style.backgroundColor = colorTo);
}