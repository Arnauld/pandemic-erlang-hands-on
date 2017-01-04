import data from "./cities.json";
import {initializeCityStates, updateCityState} from "../actions";

class Cities {

    constructor(store) {
        this.store = store;
        this.initializeRandomStates();
    }

    initializeRandomStates() {
        const states = data.nodes.reduce((acc, n) => {
            acc[n.name] = {
                name: n.name,
                infectionLevel: 0,
                disabled: Math.random() > 0.3
            };
            return acc;
        }, {});
        this.store.dispatch(initializeCityStates(states));
    }

    get nodes() {
        return data.nodes;
    }

    get links() {
        return data.links;
    }

    nodeOf(city) {
        return data.nodes.filter(n => n.name == city)[0];
    }

    get all() {
        return data.nodes.map(n => n.name);
    }

    stateOf(city) {
        return this.store.getState().cityStates[city];
    }

    updateState(city, newState) {
        this.store.dispatch(updateCityState(city, newState));
    }

    linksOf(city) {
        return data
            .links
            .filter(([c1, c2]) => c1 === city || c2 === city)
            .map(([c1, c2]) => c1 === city ? c2 : c1);
    }

}


export default Cities