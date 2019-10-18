# Magic stones

Experiment environment for feature-based causal learning and generalization.

Proudly powered by [<img src="http://vanilla-js.com/assets/button.png">](http://vanilla-js.com)

## Notes

1. "Stones" are created as `<div />`s. Changing their color, size or shaped are controled by manipulating relavant CSS style properties of these `<div />`s.

2. `prep.js` contains properties of stones, magic rules, and create default stones when loading the page.

    You can **customize stone properties or magic rules in `prep.js`**.

3. `drag.js` codes the dragging behavior of stones, `magic.js` controls how transformation happens, and `reset.js` works for the "reset" button.