
// Tuning configs
const boxAreaMargin = 30;
const dragElementKeyword = 'stone';
const itemPos = stones; // JSON in js format, load as script

// Initialize stone attributes
let dragItem;
let dragItemPos = itemPos['stone1']; // Pick a default value to avoid console warnings

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
const leftLimit = Math.round(containerPos.left) + boxAreaMargin;
const rightLimit = Math.round(containerPos.right) - boxAreaMargin;

// Move an element to top
function moveUp (elementId) {
  const el = document.getElementById(elementId);
  const currentTop = Math.max(...Object.keys(stones).map(s => stones[s].zIndex));
  
  stones[elementId].zIndex = currentTop + 1;
  el.style.zIndex = stones[elementId].zIndex;
}



// Dragging functions
// Adapted from https://www.kirupa.com/html5/drag.htm
function dragStart(e) {
  if (e.target.id.indexOf(dragElementKeyword) > -1) {
    dragItem = e.target;
    dragItemPos = itemPos[dragItem.id];
    dragItemPos.active = true;
    moveUp(dragItem.id)
    // Pick touch-screen friendly action
    const clientPos = (e.type === "touchstart") ? e.touches[0] : e ;
    dragItemPos.initialX = clientPos.clientX - dragItemPos.xOffset;
    dragItemPos.initialY = clientPos.clientY - dragItemPos.yOffset;
  
    // Update tracking location
    dragItemPos.rect = getCurrentLocation(dragItem.id);
  }

}

function drag(e) {
  if (dragItemPos.active) {
    e.preventDefault();
    document.getElementById(dragItem.id).style.cursor = 'pointer';

    const clientPos = (e.type === "touchmove") ? e.touches[0] : e ;
    
    // Restrict movements in the draggable area
    const isOutside = (clientPos.clientY < topLimit) || (clientPos.clientY > bottomLimit) ||
                      (clientPos.clientX < leftLimit) || (clientPos.clientX > rightLimit)
    if (!isOutside) {
      dragItemPos.currentX = clientPos.clientX - dragItemPos.initialX;
      dragItemPos.currentY = clientPos.clientY - dragItemPos.initialY;
    }
    
    dragItemPos.xOffset = dragItemPos.currentX;
    dragItemPos.yOffset = dragItemPos.currentY;
    setTranslate(dragItemPos.currentX, dragItemPos.currentY, dragItem);

    // Trigger effects
    dragItemPos.rect = getCurrentLocation(dragItem.id);
    magicEffects(dragItem.id);
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