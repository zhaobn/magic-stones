
function showTask (taskId) {
    document.getElementById(taskId).style.display = "block";
    document.getElementById(taskId).scrollIntoView({
        behavior: 'smooth'
    });
}

function check () {
    window.scrollTo({top: 0, behavior: 'smooth'});
    resetStones ();
}
