
const myColors = {
  red: 'rgb(255, 102, 102)',
  blue: 'rgb(61, 60, 134)',
  yellow: 'rgb(245, 230, 99)'
}
const magicStoneBorderStyle = '10px solid rgba(136, 136, 136, .5)';

const div = document.getElementById('draw');
const stones = [
  'bc', 'bd', 'bs',
  'rc', 'rd', 'rs',
  'yc', 'yd', 'ys',
]

stones.forEach(s => {
  div.append(createDivWithClass(`stone-${s}`))
})

function createDivWithClass (className) {
  let element = document.createElement('div');
  element.setAttribute("class", className);
  return element;
}
