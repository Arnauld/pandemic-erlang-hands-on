import data from "./cities.json";

class Cities {

    constructor() {
        const states = {};
        data.nodes.forEach(n => {
            states[n.name] = {infectionLevel: 0};
        });

        this.states = states;
    }

    get nodes() {
        return data.nodes;
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

    linksOf(city) {
        if (city === "paris")
            return ["algiers", "madrid", "london", "essen", "milan"];
        if (city == "london")
            return ["new_york", "madrid", "paris", "essen"];
        return [];
    }

}


export default Cities