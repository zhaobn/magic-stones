
const consentBtn = document.getElementById('consent-btn');
const reminderBtn = document.getElementById('reminder-btn');
const descriptionBtn = document.getElementById('desc-btn');

const checkBtn = document.getElementById('check-btn');
const checks = [ 'check1', 'check2', 'check3', 'check4' ];
const answers = [ true, false, true, true ];

const passBtn = document.getElementById('pass-btn');
const retryBtn = document.getElementById('retry-btn');

consentBtn.onclick = () => {
    document.getElementById('landing').style.display = 'none';
    document.getElementById('reminder').style.display = 'block';
}

reminderBtn.onclick = () => {
    document.getElementById('reminder').style.display = 'none';
    document.getElementById('description').style.display = 'block';
    document.body.className = 'dark-page';
}

descriptionBtn.onclick = () => {
    document.getElementById('description').style.display = 'none';
    document.getElementById('comprehension').style.display = 'block';
    document.getElementById('check-btn').style.display = 'block';
    document.body.className = 'light-page';
}

checkBtn.onclick = () => checkComprehension();
passBtn.onclick = () => location.href='task.html';
retryBtn.onclick = () => {
    document.getElementById('comprehension').style.display = 'none';
    document.getElementById('retry').style.display = 'none';
    document.getElementById('description').style.display = 'block';
    document.body.className = 'dark-page';
};

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
