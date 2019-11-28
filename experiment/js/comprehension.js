
const checkBtn = document.getElementById('check-btn');
const checks = [ 'check1', 'check2', 'check3', 'check4' ];
const answers = [ true, false, true, false ];

const passBtn = document.getElementById('pass-btn');
const retryBtn = document.getElementById('retry-btn');


checkBtn.onclick = () => checkComprehension();
passBtn.onclick = () => location.href='task.html';
retryBtn.onclick = () => location.href='instruction.html';

document.getElementById('prequiz').onchange = () => isFilled() ? checkBtn.disabled = false : null;

function checkComprehension() {
    let inputs = [];
    checks.map(check => {
        const vals = document.getElementsByName(check);
        inputs.push(vals[0].checked);
    });
    const pass = (inputs.join('') === answers.join(''));
    showPostCheckPage(pass);
}

function showPostCheckPage (isPass) {
    const pageDiv = isPass? 'pass' : 'retry';
    document.getElementById('check-btn').style.display = 'none';
    document.getElementById(pageDiv).style.display = 'block';
}

function isFilled () {
    let radios = document.getElementsByTagName('input');
    let checked = 0;
    for (let i = 0; i < radios.length; i++) {
        checked += radios[i].checked;
    }
    return (checked > 3)
}
