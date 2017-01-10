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
import WorldMap from "./game/map/Worldmap.jsx";
import Terminal from "./bash/terminal.jsx";
import Cities from "./game/cities.js";
import Game, {GameListener} from "./game/game.js";
import Bash from "./bash/bash.js";
import InfectCommand from "./game/command/InfectCommand.js";
import SelectCommand from "./game/command/SelectCommand.js";
import EventsCommand from "./game/command/EventsCommand.js";
import ToggleCityCommand from "./game/command/ToggleCityCommand.js";
import StateOfCityCommand from "./game/command/StateOfCityCommand.js";
import ClearCommand from "./bash/command/ClearCommand.js";
import DisplayCommand from "./game/map/DisplayCommand";
//-
import {createStore} from "redux";
import pandemicApp from "./reducer";
//-
import {infectionReceived} from "./actions";
let store = createStore(pandemicApp);


class StoreGameListener extends GameListener {
    constructor(store) {
        super();
        this.store = store;
    }

    onInfection(infection) {
        store.dispatch(infectionReceived(infection));
    }
}

//-
const cities = new Cities(store);
const game = new Game(cities);
game.subscribe(new StoreGameListener(store));

const worldMapCommand = new DisplayCommand(store);
const commands = [
    new InfectCommand(game, cities),
    new EventsCommand(),
    new ClearCommand(),
    new SelectCommand(store, cities),
    new ToggleCityCommand(store, cities),
    worldMapCommand,
    new StateOfCityCommand(store, cities)
];
const bash = new Bash(commands, ["Welcome! try 'help'"]);

//---------------------------------------------------------
// DOM
//---------------------------------------------------------
const Root = () => (
    <div>
        <AppBar store={store} cities={cities}/>
        <div className="container">
            <div className="row">
                <div className="col-lg-8">
                    <WorldMap store={store} cities={cities} game={game}/>
                </div>
                <div className="col-lg-4">
                    <Terminal bash={bash}/>
                </div>
            </div>
        </div>
    </div>
);

ReactDOM.render(<Root />, document.getElementById('root'));