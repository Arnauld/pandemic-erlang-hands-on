import Command from "../../bash/Command";
import Fun from "../../util/fun";
import {selectCities} from "../../actions";

class SelectCommand extends Command {

    constructor(store, cities) {
        super("select", "select the cities specified");
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
        this.store.dispatch(selectCities(args));
    }
}

export default SelectCommand
