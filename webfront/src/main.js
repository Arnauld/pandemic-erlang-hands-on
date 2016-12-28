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
// STYLE, FONTS and CSS
//---------------------------------------------------------
import React from "react";
import ReactDOM from "react-dom";
import AppBar from "./appbar.jsx";
import WorldMap from "./worldmap.jsx";
import Cities from "./core/cities.js";

const cities = new Cities();

const Root = () => (
    <div>
        <AppBar/>
        <div className="container">
            <WorldMap cities={cities}/>
        </div>
    </div>
);

ReactDOM.render(<Root />, document.getElementById('root'));