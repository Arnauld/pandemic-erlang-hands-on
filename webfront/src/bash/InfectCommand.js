import Command from "./command";
import Fun from "../util/fun";
import data from "../core/cities.json";

class InfectCommand extends Command {

    constructor() {
        super("infect");
    }

    autocomplete(input, command, args, outs) {
        if (args.length > 0) {
            const last = Fun.last(args);
            const others = Fun.removeLast(args);
            const cities = data.nodes.filter(n => {
                return n.name.startsWith(last)
            });

            if (cities.length === 1) {
                return command + " " + (others.length > 0 ? others.join(" ") + " " : "") + cities[0].name;
            }
            if (cities.length > 1) {
                outs.writeCommand(input);
                cities.forEach(n => {
                    outs.writeMessage(n.name);
                });
            }
        }
        return super.autocomplete(input, command, args);
    }
}

export default InfectCommand
