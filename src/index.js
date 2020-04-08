import { Elm } from './Main.elm'
require("material-components-web/dist/material-components-web.js");
require("material-components-web/dist/material-components-web.css");

const app = Elm.Main.init({
    node: document.getElementById("elm"),
})
