//---------------------------------------------------------
// STYLE, FONTS and CSS
//---------------------------------------------------------
//noinspection ES6UnusedImports
import Bootstrap from "bootstrap/dist/css/bootstrap.css";
//noinspection ES6UnusedImports
import SourceCodePro from "../styles/fonts/sourcecodepro/stylesheet.css";
//noinspection ES6UnusedImports
import MyUnderwood from "../styles/fonts/myunderwood/stylesheet.css";
//noinspection ES6UnusedImports
import Main from "../styles/css/main.css";
//---------------------------------------------------------
// BEHAVIOR
//---------------------------------------------------------
import React from "react";
import ReactDOM from "react-dom";
import AppBar from "./appbar.jsx";
import WorldMap from "./worldmap.jsx";
import Terminal from "./terminal.jsx";
import Cities from "./core/cities.js";
import Bash from "./bash/bash.js";

const cities = new Cities();
const bash = new Bash();

const Root = () => (
    <div>
        <AppBar/>
        <div className="container">
            <div className="row">
                <div className="col-lg-8">
                    <WorldMap cities={cities}/>
                </div>
                <div className="col-lg-4">
                    <Terminal bash={bash}/>
                </div>
            </div>
        </div>
    </div>
);

ReactDOM.render(<Root />, document.getElementById('root'));