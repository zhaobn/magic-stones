
// Set up stone attributes
let dragItem;
let stonePos = stones; // JSON object in js format
let dragPos = stonePos['stone1']; // default value


// Set up event listeners
const container = document.querySelector(".box");

container.addEventListener("touchstart", dragStart, false);
container.addEventListener("touchend", dragEnd, false);
container.addEventListener("touchmove", drag, false);

container.addEventListener("mousedown", dragStart, false);
container.addEventListener("mouseup", dragEnd, false);
container.addEventListener("mousemove", drag, false);

// Dragging functions
// See https://www.kirupa.com/html5/drag.htm
function dragStart(e) {
  if (e.target.id.indexOf('stone') > -1) {
    dragItem = e.target;
    dragPos = stonePos[e.target.id];
    dragPos.active = true;
  }
  if (e.type === "touchstart") {
    dragPos.initialX = e.touches[0].clientX - dragPos.xOffset;
    dragPos.initialY = e.touches[0].clientY - dragPos.yOffset;
  } else {
    dragPos.initialX = e.clientX - dragPos.xOffset;
    dragPos.initialY = e.clientY - dragPos.yOffset;
  }
}

function dragEnd(e) {
  dragPos.initialX = dragPos.currentX;
  dragPos.initialY = dragPos.currentY;
  dragPos.active = false;
}

function drag(e) {
  if (dragPos.active) {
    e.preventDefault();
  
    if (e.type === "touchmove") {
      dragPos.currentX = e.touches[0].clientX - dragPos.initialX;
      dragPos.currentY = e.touches[0].clientY - dragPos.initialY;
    } else {
      dragPos.currentX = e.clientX - dragPos.initialX;
      dragPos.currentY = e.clientY - dragPos.initialY;
    }
    
    dragPos.xOffset = dragPos.currentX;
    dragPos.yOffset = dragPos.currentY;
    setTranslate(dragPos.currentX, dragPos.currentY, dragItem);
  }
}

function setTranslate(xPos, yPos, el) {
  el.style.transform = "translate3d(" + xPos + "px, " + yPos + "px, 0)";
}