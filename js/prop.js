
// Define custom colors
const myColors = {
    red: 'rgb(255, 102, 102)',
    blue: 'rgb(61, 60, 134)',
    yellow: 'rgb(245, 230, 99)'
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
    'red': 'yellow',
}

humanReadableColorSizeRules = {
  'yellow': '80',
}

humanReadableSizeColorRules = {
  '80': 'blue',
}

humanReadableColorShapeRules = {
  'blue': '20%',
  'yellow': '50%',
}

humanReadableShapeColorRules = {
  '20%': 'yellow',
}

