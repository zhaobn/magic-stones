
// Tuning configs
const boxAreaMargin = 30;
const dragElementKeyword = 'stone';
let itemPos = stones; // JSON in js format, load as script

// Initialize stone attributes
let dragItem;
let dragItemPos = itemPos['stone1']; // default value

// Set up event listeners
const container = document.querySelector(".box");

container.addEventListener("touchstart", dragStart, false);
container.addEventListener("touchend", dragEnd, false);
container.addEventListener("touchmove", drag, false);

container.addEventListener("mousedown", dragStart, false);
container.addEventListener("mouseup", dragEnd, false);
container.addEventListener("mousemove", drag, false);

// Restrict draggable area
const containerPos = container.getBoundingClientRect();

const topLimit = Math.round(containerPos.top) + boxAreaMargin;
const bottomLimit = Math.round(containerPos.bottom) - boxAreaMargin;
const rightLimit = Math.round(containerPos.right) - boxAreaMargin;
const leftLimit = Math.round(containerPos.left) + boxAreaMargin;


// Dragging functions
// See https://www.kirupa.com/html5/drag.htm
function dragStart(e) {
  if (e.target.id.indexOf(dragElementKeyword) > -1) {
    dragItem = e.target;
    dragItemPos = itemPos[dragItem.id];
    dragItemPos.active = true;
  }

  let clientPos = (e.type === "touchstart") ? e.touches[0] : e ;
  dragItemPos.initialX = clientPos.clientX - dragItemPos.xOffset;
  dragItemPos.initialY = clientPos.clientY - dragItemPos.yOffset;
  
}

function drag(e) {
  if (dragItemPos.active) {
    e.preventDefault();

    let clientPos = (e.type === "touchmove") ? e.touches[0] : e ;
    let isOutside = (clientPos.clientY < topLimit) || (clientPos.clientY > bottomLimit) ||
                    (clientPos.clientX < leftLimit) || (clientPos.clientX > rightLimit)
    
    if (!isOutside) {
      dragItemPos.currentX = clientPos.clientX - dragItemPos.initialX;
      dragItemPos.currentY = clientPos.clientY - dragItemPos.initialY;
    } else {
      dragEnd(e);
    }
    
    dragItemPos.xOffset = dragItemPos.currentX;
    dragItemPos.yOffset = dragItemPos.currentY;
    setTranslate(dragItemPos.currentX, dragItemPos.currentY, dragItem);
  }
}

function dragEnd(e) {
  dragItemPos.initialX = dragItemPos.currentX;
  dragItemPos.initialY = dragItemPos.currentY;
  dragItemPos.active = false;
}

function setTranslate(xPos, yPos, el) {
  el.style.transform = "translate3d(" + xPos + "px, " + yPos + "px, 0)";
}