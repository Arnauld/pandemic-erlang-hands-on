import data from "./cities.json";

class Cities {

    constructor() {
        const states = {};
        data.nodes.forEach(n => {
            states[n.name] = {
                infectionLevel: 0,
                disabled: Math.random() > 0.3
            };
        });

        this.states = states;
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
        return this.states[city];
    }

    updateState(city, newState) {
        Object.assign(this.states[city], newState);
    }

    linksOf(city) {
        return data
            .links
            .filter(([c1, c2]) => c1 === city || c2 === city)
            .map(([c1, c2]) => c1 === city ? c2 : c1);
    }

}


export default Cities