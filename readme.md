# Magic stones

Experiment environment for feature-based causal learning and generalization.

Proudly powered by [<img src="http://vanilla-js.com/assets/button.png">](http://vanilla-js.com)

## Notes

1. "Stones" are created as `<div />`s. Changing their color, size or shape are controled by manipulating their relavant CSS styles.

    You can **customize stone properties or magic rules in `js/prop.js`**.

2. `js/prep.js` reads configs from `js/prop.js` to

    - create default stones when loading the page;

    - compile magic rules from a human-readable format to js-friendly objects.

3. `js/drag.js` codes the dragging behavior of stones, `js/magic.js` define functions that make the magic happen, and `js/reset.js` works for the "reset" button.
