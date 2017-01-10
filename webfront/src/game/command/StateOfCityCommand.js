import Command from "../../bash/Command";
import Fun from "../../util/fun";
import Strings from "../../util/strings";
import {stateOfCity} from "../../actions";

class StateOfCityCommand extends Command {

    constructor(store, cities) {
        super("stateOf", "display city state");
        this.store = store;
        this.cities = cities;
    }

    autocomplete(input, command, args, outs) {
        const allCities = this.cities.all;
        if (args.length > 0) {
            const last = Fun.last(args);
            const others = Fun.removeLast(args);
            const cities = allCities.filter(n => {
                return n.startsWith(last)
            });
            const alreadySelectedCities = (others.length > 0 ? others.join(" ") + " " : "");

            if (cities.length === 1) {
                return command + " " + alreadySelectedCities + cities[0];
            }
            if (cities.length > 1) {
                outs.writeCommand(input);
                outs.writeMessage(cities);

                const prefix = Strings.longestCommonPrefix(cities);
                if(prefix) {
                    return command + " " + alreadySelectedCities + prefix;
                }
            }
        }
        else {
            outs.writeCommand(input);
            outs.writeMessage(allCities);
        }
        return super.autocomplete(input, command, args);
    }


    execute(command, args, outs) {
        args.forEach(city => {
            let state = this.cities.stateOf(city);
            outs.writeMessage(JSON.stringify(state));
        });
    }
}

export default StateOfCityCommand
