import Command from "../../bash/Command";
import Fun from "../../util/fun";
import data from "../cities.json";

class InfectCommand extends Command {

    constructor() {
        super("infect", "infect the cities specified");
    }

    autocomplete(input, command, args, outs) {
        const allCities = data.nodes;
        if (args.length > 0) {
            const last = Fun.last(args);
            const others = Fun.removeLast(args);
            const cities = allCities.filter(n => {
                return n.name.startsWith(last)
            });

            if (cities.length === 1) {
                return command + " " + (others.length > 0 ? others.join(" ") + " " : "") + cities[0].name;
            }
            if (cities.length > 1) {
                outs.writeCommand(input);
                outs.writeMessage(cities.map(n => n.name));
            }
        }
        else {
            outs.writeCommand(input);
            outs.writeMessage(allCities.map(n => n.name));
        }
        return super.autocomplete(input, command, args);
    }
}

export default InfectCommand
