
// Helper definitions
const myColors = {
  red: 'rgb(255, 102, 102)',
  blue: 'rgb(61, 60, 134)',
  yellow: 'rgb(245, 230, 99)'
}

// Default stone attributes
const stones = {
    "stone1": {
      "id": 'stone1',
      "defaultColor": myColors.blue,
      "active": false,
      "zIndex": 0,
      "currentX": 0,
      "currentY": 0,
      "initialX": 0,
      "initialY": 0,
      "xOffset": 0,
      "yOffset": 0,
      "currentRec": {
        "top": 0,
        "bottom" : 0,
        "left" : 0,
        "right": 0
      }
    },
    "stone2": {
      "id": 'stone2',
      "defaultColor": myColors.yellow,
      "zIndex": 0,
      "currentX": 0,
      "active": false,
      "currentY": 0,
      "initialX": 0,
      "initialY": 0,
      "xOffset": 0,
      "yOffset": 0,
      "currentRec": {
        "top": 0,
        "bottom" : 0,
        "left" : 0,
        "right": 0
      }
    }
}

// Define transform rules here
const humanReadableColorRules = {
  'blue': 'red',
  'red': 'yellow',
}
const humanReadableColorSizeRules = {
  'yellow': '60',
}

const humanReadableSizeColorRules = {
  '60': 'blue',
}

const humanReadableColorShapeRules = {
  'blue': '20%',
  'yellow': '50%',
}

const humanReadableShapeColorRules = {
  '20%': 'yellow',
}


// Prepare transform rules for rendering
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
    'class': 'stone',
    'id': stones[s].id,
    'background-color': stones[s].defaultColor,
    'z-index': stones[s].zIndex,
  })
  document.querySelector('.box').append(stone);
})

// Populate initial locations
Object.keys(stones).forEach(s => {
  stones[s].rect = getCurrentLocation(s);
})
