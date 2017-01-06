import {
    CITY_SELECTED,
    CITIES_SELECTED,
    LAYER_TOGGLED,
    INFECTION_RECEIVED,
    CITY_STATES_INITIALIZED,
    CITY_STATE_UPDATED
} from "./actions";
import {INITIAL_LAYERS} from "./game/map/Worldmap.jsx";

const initialState = {
    city_selection: [],
    cityStates: {},
    layers: INITIAL_LAYERS,
    eventSeq: 0,
    events: []
};

function pandemicApp(state = initialState, action) {
    switch (action.type) {
        case CITY_SELECTED:
            return Object.assign({}, state, {city_selection: [action.city]});

        case CITIES_SELECTED:
            return Object.assign({}, state, {city_selection: action.cities.slice()});

        case LAYER_TOGGLED: {
            const newLayers = Object.assign({}, state.layers);
            newLayers[action.layer] = !newLayers[action.layer];
            return Object.assign({}, state, {layers: newLayers});
        }

        case INFECTION_RECEIVED: {
            const newEvents = state.events.slice();
            const newSeq = state.eventSeq + 1;
            const event = Object.assign({sequence: newSeq}, action);
            newEvents.push(event);
            return Object.assign({}, state, {events: newEvents, eventSeq: newSeq});
        }

        case CITY_STATES_INITIALIZED: {
            return Object.assign({}, state, {cityStates: action.cityStates});
        }

        case CITY_STATE_UPDATED: {
            const key = action.city;
            const newState = Object.assign({}, state.cityStates[action.city], action.state);
            const newCity = {};
            newCity[key] = newState;

            const newCities = Object.assign({}, state.cityStates, newCity);
            return Object.assign({}, state, {cityStates: newCities});
        }

        case "@@redux/INIT":
            // internal redux notification...
            return state;

        default:
            console.error("Action type not handled: ", action.type, action);
            // For now, don't handle any actions
            // and just return the state given to us.
            return state
    }
}

export default pandemicApp
