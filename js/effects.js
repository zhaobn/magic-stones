
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
