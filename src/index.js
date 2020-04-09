require("material-components-web/dist/material-components-web.js");
require("material-components-web/dist/material-components-web.css");

import { Elm } from './Main.elm'

Elm.Main.init({
    node: document.getElementById("elm"),
})
