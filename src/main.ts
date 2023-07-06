import './style.css'
import { Elm } from "./Main.elm";

const root = document.querySelector("#app div");
Elm.Main.init({
    node: root,
    flags: {
        initialSeed: 1
    }
});
