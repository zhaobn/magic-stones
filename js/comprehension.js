
function checkComprehension() {
    let inputs = [];
    const answers = [ true, false, true ];
    const checks = [ 'check1', 'check2', 'check3' ];

    checks.map(check => {
        const vals = document.getElementsByName(check);
        inputs.push(vals[0].checked);
    })

    const pass = (inputs.join('') === answers.join(''));
    showPostCheckPage(pass);
}

function showPostCheckPage (isPass) {
    const pageDiv = isPass? 'pass' : 'retry';
    document.getElementById('check-btn').style.display = 'none';
    document.getElementById(pageDiv).style.display = 'block';
}

document.getElementById('check-btn').onclick = () => checkComprehension();
document.getElementById('pass-btn').onclick = () => location.href='task.html';
document.getElementById('retry-btn').onclick = () => location.href='description.html';
