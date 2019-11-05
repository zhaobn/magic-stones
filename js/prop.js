
// Define custom colors
const myColors = {
    red: 'rgb(255, 102, 102)',
    blue: 'rgb(61, 60, 134)',
    yellow: 'rgb(245, 230, 99)'
  }

// Default stone attributes
const stones = {
    "stone1": {
      "id": 'stone1',
      "type": 'magic',
      "defaultColor": myColors.blue,
    },
    "stone2": {
      "id": 'stone2',
      "type": 'normal',
      "defaultColor": myColors.yellow,
    }
}

// Initialize magic rules
let humanReadableColorRules = {};
let humanReadableColorShapeRules = {};
let humanReadableColorSizeRules = {};
let humanReadableShapeColorRules = {};
let humanReadableSizeColorRules = {}

// Define transform rules here
humanReadableColorRules = {
    'blue': 'red',
    'yellow': 'red',
}

// humanReadableColorSizeRules = {
//   'yellow': '80',
// }

// humanReadableSizeColorRules = {
//   '80': 'blue',
// }

humanReadableColorShapeRules = {
  'blue': 'diamond',
}

// humanReadableShapeColorRules = {
//   '20%': 'yellow',
// }
