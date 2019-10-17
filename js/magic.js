
function simpleMagic (active_id) {
    // See prep.js for human readable configs
    const magics = [
        colorToColor, 
        colorToSize, 
        colorToShape, 
        sizeToColor,
        shapeToColor,
    ];

    const activePos = stones[active_id].rect;
    // Hardcoded for now because I know there are only two stones.
    // TODO: make a logical looping over items.
    const inactive_id = (Object.keys(stones).filter(s => !stones[s].active))[0];
    const inactivePos = stones[inactive_id].rect;

    const overlap = !(activePos.right < inactivePos.left || 
                      activePos.left > inactivePos.right || 
                      activePos.bottom < inactivePos.top || 
                      activePos.top > inactivePos.bottom);
    
    document.getElementById(inactive_id).style.transition = overlap? "all 0.8s" : '';

    magics.map(magic => (overlap && doMagic(magic, active_id, inactive_id)));
}

function doMagic (magic, sender, receiver) {
    switch(sender) {
        case 'stone1':
            magic('stone1', 'stone2');
            break;
        case 'stone2':
            magic('stone2', 'stone1');
            break;
        default:
            console.log('No active stones detected.');
    }
}

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