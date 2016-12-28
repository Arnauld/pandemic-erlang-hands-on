import React, {Component} from "react";
import config from "./config.json";

class Greeter extends Component {
    render() {
        return (
            <h1>{config.greetText}</h1>
        );
    }
}

export default Greeter
