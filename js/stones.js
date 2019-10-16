const stones = {
    "stone1": {
      "id": 'stone1',
      "color": 'rgb(61, 60, 134)',
      "zIndex": 0,
      "currentX": 0,
      "active": false,
      "currentY": 0,
      "initialX": 0,
      "initialY": 0,
      "xOffset": 0,
      "yOffset": 0
    },
    "stone2": {
      "id": 'stone2',
      "color": 'rgb(245, 230, 99)',
      "zIndex": 0,
      "currentX": 0,
      "active": false,
      "currentY": 0,
      "initialX": 0,
      "initialY": 0,
      "xOffset": 0,
      "yOffset": 0
    }
}

// Helper functions
function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}

// Create default stones
Object.keys(stones).map(s => {
  let stone = document.createElement('div');
  setAttributes(stone, {
    'class': 'stone',
    'id': stones[s].id,
    'background-color': 'red',
    'z-index': stones[s].zIndex,
  })
  document.querySelector('.box').append(stone);
})
