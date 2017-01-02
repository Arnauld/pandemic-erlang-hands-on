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
import AppBar from "./AppBar.jsx";
import AppBarControl from "./AppBarControl";
import WorldMap from "./game/map/worldmap.jsx";
import Terminal from "./bash/terminal.jsx";
import Cities from "./game/cities.js";
import Game from "./game/game.js";
import Bash from "./bash/bash.js";
import InfectCommand from "./game/command/InfectCommand.js";
import EventsCommand from "./game/command/EventsCommand.js";
import ClearCommand from "./bash/command/ClearCommand.js";
import WorldMapCommand from "./game/map/WorldMapCommand.js";

const cities = new Cities();
const game = new Game(cities);
const worldMapCommand = new WorldMapCommand();
const commands = [
    new InfectCommand(game, cities),
    new EventsCommand(),
    new ClearCommand(),
    worldMapCommand
];
const appBarControl = new AppBarControl(cities);
const bash = new Bash(commands, ["Welcome! try 'help'"]);

//---------------------------------------------------------
// DOM
//---------------------------------------------------------
const Root = () => (
    <div>
        <AppBar controls={[appBarControl]}/>
        <div className="container">
            <div className="row">
                <div className="col-lg-8">
                    <WorldMap cities={cities} game={game} controls={[worldMapCommand, appBarControl]}/>
                </div>
                <div className="col-lg-4">
                    <Terminal bash={bash}/>
                </div>
            </div>
        </div>
    </div>
);

ReactDOM.render(<Root />, document.getElementById('root'));