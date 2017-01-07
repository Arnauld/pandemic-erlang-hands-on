import Command from "../../bash/Command";
import Fun from "../../util/fun";
import {toggleCities} from "../../actions";

class ToggleCityCommand extends Command {

    constructor(store, cities) {
        super("toggleCity", "toggle city state between enabled/disabled");
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

            if (cities.length === 1) {
                return command + " " + (others.length > 0 ? others.join(" ") + " " : "") + cities[0];
            }
            if (cities.length > 1) {
                outs.writeCommand(input);
                outs.writeMessage(cities);
            }
        }
        else {
            outs.writeCommand(input);
            outs.writeMessage(allCities);
        }
        return super.autocomplete(input, command, args);
    }


    execute(command, args, outs) {
        this.store.dispatch(toggleCities(args));
    }
}

export default ToggleCityCommand
