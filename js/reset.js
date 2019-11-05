
// Read elements
const resetButton = document.querySelector('.reset-button');
const boxArea = document.querySelector('.box');

// Reset stones: position, properties
function resetStones () {
    // Clear all existing stones
    const stonesToClear = document.querySelectorAll('[id^="stone"]');
    stonesToClear.forEach(s => {
        let el = document.getElementById(s.id);
        el.parentNode.removeChild(el);
    });
    // Create default stones
    Object.keys(stones).map(s => {
        const newStone = document.createElement('div');
        setAttributes(newStone, {
            'class': (stones[s].type === 'magic')? 'magic-stone': 'normal-stone',
            'id': stones[s].id,
            'background-color': stones[s].color
        })
        boxArea.appendChild(newStone);
        // Re-initialize stones object
        // Because dragging functions need this to track properly
        Object.assign(stones[s], {
            "currentX": 0,
            "active": false,
            "currentY": 0,
            "initialX": 0,
            "initialY": 0,
            "xOffset": 0,
            "yOffset": 0,
            "zIndex": 0
        })
    })

}

resetButton.addEventListener('click', resetStones);
