
function simpleMagic (active_id) {
    const activePos = stones[active_id].rect;
    // Hardcoded for now because I know there are only two stones.
    // TODO: make a logical looping over items.
    const inactive_id = (Object.keys(stones).filter(s => !stones[s].active))[0];
    const inactivePos = stones[inactive_id].rect;

    const overlap = !(activePos.right < inactivePos.left || 
                      activePos.left > inactivePos.right || 
                      activePos.bottom < inactivePos.top || 
                      activePos.top > inactivePos.bottom);
    
    // Hardcoded for demo purpose.
    // TODO: read magical property from stones JSON.
    switch(active_id) {
        case 'stone1':
            overlap? changeColor('stone1', 'stone2') : null;
            break;
        case 'stone2':
            overlap? changeColor('stone2', 'stone1') : null;
            break;
    }
    
}

function changeColor(sender, receiver) {
    const colorFrom = getComputedStyle(document.getElementById(sender))['background-color'];
    const colorTo = colorRules[colorFrom];
    document.getElementById(receiver).style.background = colorTo;
}