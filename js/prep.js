
// Prepare magic rules for rendering
let colorRules = {}
Object.keys(humanReadableColorRules).forEach(key => colorRules[myColors[key]] = myColors[humanReadableColorRules[key]]);

let colorSizeRules = {}
Object.keys(humanReadableColorSizeRules).forEach(key => colorSizeRules[myColors[key]] = humanReadableColorSizeRules[key]);

let sizeColorRules = {}
Object.keys(humanReadableSizeColorRules).forEach(key => sizeColorRules[key] = myColors[humanReadableSizeColorRules[key]]);

let colorShapeRules = {}
Object.keys(humanReadableColorShapeRules).forEach(key => colorShapeRules[myColors[key]] = humanReadableColorShapeRules[key]);

let shapeColorRules = {}
Object.keys(humanReadableShapeColorRules).forEach(key => shapeColorRules[key] = myColors[humanReadableShapeColorRules[key]]);

// Helper functions
function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}

function getCurrentLocation(id) {
  let rect = {top: 0, bottom: 0, left: 0, right: 0};
  const pos = document.getElementById(id).getBoundingClientRect();
  rect.top = pos.top;
  rect.bottom = pos.bottom;
  rect.left = pos.left;
  rect.right = pos.right;
  return rect;
}

// Create default stones
Object.keys(stones).map(s => {
  let stone = document.createElement('div');
  setAttributes(stone, {
    'class': (stones[s].type === 'magic')? 'magic-stone': 'normal-stone',
    'id': stones[s].id,
    'background-color': stones[s].defaultColor,
  })
  document.querySelector('.box').append(stone);
})

// Populate initial locations
Object.keys(stones).forEach(s => {
  stones[s].rect = getCurrentLocation(s);
})
