
function changeColor(id, color) {
    document.getElementById(id).style.background = myColors[color];
}

function changeShape(id, shape) {
    const el = document.getElementById(id);
    switch (shape) {
        case 'diamond':
                el.style.borderRadius = '10%';
                el.style.transform = 'rotate(45deg) scale(.8)';
                break;
        case 'circle':
                el.style.borderRadius = '50%';
                break;
        case 'square':
                el.style.borderRadius = '10%';
                break;
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

function setAttributes(el, attrs) {
    for(var key in attrs) {
      el.setAttribute(key, attrs[key]);
    }
}

function showTask (taskId) {
    document.getElementById(taskId).style.display = "block";
    document.getElementById(taskId).scrollIntoView({
        behavior: 'smooth'
    });
}
