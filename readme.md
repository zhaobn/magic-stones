# Magic stones

A few-shot generalization task that inspects people's prior knowledge on abstract features' causal relations.

Proudly powered by [<img src="http://vanilla-js.com/assets/button.png">](http://vanilla-js.com)

- Stones are created as `<div />`s. Magic effects take place by manipulating CSS.
- Experiment configs - including task conditions and stone properties - can be found in the begining of `js/task.js`.
- HTML files live in `routes/` 
  - `landing.html` contains landing page, instructions, and comprehension quiz;
  - `task.html` contains the learning task and generalization tasks. At this time, each participant faces one learning task, and goes through 15 generalization tasks. 
  - Generaliztion trials are created by the `createTrials()` function in `js/task.js`.
  - `debrief.html` collects demographic data and participant feedbacks. In the [production version](https://github.com/zhaobn/flask-magic-stones), `debrief.html` is integrated into `task.html`.
- Javascript files are named after their HTML pages.

**Live experiment** is served via Flask App: https://github.com/zhaobn/flask-magic-stones
