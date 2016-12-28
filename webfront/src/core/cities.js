import data from "./cities.json";

class Cities {
    get all() {
        return data.nodes;
    }
}


export default Cities