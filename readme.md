# Magic stones

Experiment environment for feature-based causal learning and generalization.

Proudly powered by [<img src="http://vanilla-js.com/assets/button.png">](http://vanilla-js.com)

## Notes

1. "Stones" are created as `<div />`s. Changing their color, size or shaped are controled by manipulating relavant CSS style properties of these `<div />`s.

    You can **customize stone properties or magic rules in `prop.js`**.

2. `prep.js` reads configs from `prop.js` to

    - Create default stones when loading the page;

    - Compile magic rules from the human-readable format to js-friendly objects.

3. `drag.js` codes the dragging behavior of stones, `magic.js` define functions that make the magic happen, and `reset.js` works for the "reset" button.
