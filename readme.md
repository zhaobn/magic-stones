

# Magic stones


A few-shot generalization task that inspects people's prior knowledge on abstract features' causal relations.


Served via [Flask Tasks](https://github.com/bramleyccslab/flask-tasks) at http://www.bramleylab.ppls.ed.ac.uk/experiments/magic_stones (live now :star2:! Please feel free to give it a try :relaxed:. If you experience any problem or want to share your feedback, please contact me at <b.zhao@ed.ac.uk>.)


Proudly powered by [<img src="http://vanilla-js.com/assets/button.png">](http://vanilla-js.com) and the Internet.


## Resources


- Web-experiment code is in `routes/`, `js/` and `css/` folders. See the [Development](##Development) section for more details.

- Pilot data and preliminary analysis scripts are in the `data/` and `analysis` folders, all prefixed with dates that data was collected.

- `notes/` folder has some initial modeling ideas.

- `asset/` folder stores (an old) demo video clip, participant consent documents, and all raw data.


## Development


For debugging purpose, each "view" of the web experiment has its own HTML file and JS file. The experiment follows the order: consent -> reminder -> instruction -> comprehension -> task -> debrief.

- `routes/` folder holds all the HTML files. 

- `js/` folder holds JS files for each HTML page - the only exception is that `reminder.html` has its js functions in the html file therefore it has no stand-alone js file.

Note that in this design, stones are created as `<div />`s, and magic effects take place by manipulating CSS.

Experiment setups - including task conditions and stone properties - can be found in the beginning of `js/task.js`.

To see how the experiment is served in production, check out its integration with the Flask App [here](https://github.com/zhaobn/flask-magic-stones).


