class AppBarControl {

    constructor(cities) {
        this.cities = cities;
    }

    worldmap(worldMap) {
        this.worldMap = worldMap;
    }

    citySelected(city) {
        const state = Object.assign({name: city}, this.cities.stateOf(city));
        this.appBar.setCity(state)
    }

    appBar(appBar) {
        this.appBar = appBar;
    }
}

export default AppBarControl