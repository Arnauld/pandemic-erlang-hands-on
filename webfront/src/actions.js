/*
 * action types
 */
export const CITY_SELECTED = "city-selected";
export const CITIES_SELECTED = "cities-selected";
export const LAYER_TOGGLED = "display-layer";
export const INFECTION_RECEIVED = "infection-received";
export const CITY_STATES_INITIALIZED = "city-states-initialized";
export const CITY_STATE_UPDATED = "city-state-updated";

/*
 * action creators
 */
export function selectCity(city) {
    return {type: CITY_SELECTED, city}
}

export function selectCities(cities) {
    return {type: CITIES_SELECTED, cities}
}

export function toggleLayer(layer) {
    return {type: LAYER_TOGGLED, layer}
}

export function infectionReceived(infection) {
    return {type: INFECTION_RECEIVED, infection}
}

export function initializeCityStates(cityStates) {
    return {type: CITY_STATES_INITIALIZED, cityStates}
}

export function updateCityState(city, state) {
    return {type: CITY_STATE_UPDATED, city, state}
}