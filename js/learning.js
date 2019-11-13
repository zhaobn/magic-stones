

/** Create stones */
const learningTask = learn04;
createStones(learningTask);

/** Set button functions */
const playBtn = document.getElementById('play-btn');
const resetBtn = document.getElementById('reset-btn');
const nextBtn = document.getElementById('next-btn');

resetBtn.onclick = () => resetStones(learningTask);
nextBtn.onclick = () => location.href='task.html';

/** Control availability */
playBtn.onclick = () => {
    playEffects(learningTask);
    setTimeout(() => {
        resetBtn.disabled = false;
        nextBtn.disabled = false
    }, 2000);
};
