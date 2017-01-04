class GameListener {
    onInfection(infection) {
    }
}

class Game {

    constructor(cities) {
        this.cities = cities;
        this.listeners = [];
    }

    recursiveInfect(infection) {
        const {toInfect, outbreak, states, changes} = infection;

        if (toInfect.length === 0)
            return infection;

        const [city, generation, source] = toInfect.shift();

        // keep a local copy to adjust state before updating cities
        if (!states[city]) {
            states[city] = Object.assign({}, this.cities.stateOf(city));
        }
        const cityState = states[city];

        if (cityState.infectionLevel < 3) {
            cityState.infectionLevel = cityState.infectionLevel + 1;
            changes.push({type: "infected", city: city, generation: generation, source: source});
        }
        else if (!outbreak.includes(city)) {
            outbreak.push(city);
            changes.push({type: "outbreak", city: city, generation: generation});
            const links = this.cities.linksOf(city);
            links.forEach(l => toInfect.push([l, generation + 1, city]));
        }
        return this.recursiveInfect(infection);
    }

    infect(citiesToInfect) {
        //simulate an async ajax call & response
        setTimeout(() => {
            const citiesWithGeneration = citiesToInfect.map(city => [city, 0, null]);
            const infection = this.recursiveInfect({
                toInfect: citiesWithGeneration,
                outbreak: [],
                states: {},
                changes: []
            });

            Object.keys(infection.states).forEach(k => this.cities.updateState(k, infection.states[k]));

            this.listeners.forEach(l => l.onInfection(infection));
        }, 1000);
    }

    subscribe(listener) {
        if (listener instanceof GameListener)
            this.listeners.push(listener);
        else
            throw "Invalid listener type: not a GameListener";
    }

}

export default Game
export {GameListener}