import Command from "../../bash/Command";
import Fun from "../../util/fun";
import data from "../cities.json";

class EventsCommand extends Command {

    constructor() {
        super("events", "display system's events");
        this.subCommands = ["watch", "list"];
    }

    autocomplete(input, command, args, outs) {
        if (args.length === 0) {
            outs.writeCommand(input);
            this.subCommands.forEach(n => outs.writeMessage(this.command + " " + n));
        }
        if (args.length > 0) {
            const last = Fun.last(args);
            const others = Fun.removeLast(args);
            const potentials = this.subCommands.filter(n => {
                return n.startsWith(last)
            });

            if (potentials.length === 1) {
                return command + " " + potentials[0];
            }
            if (potentials.length > 1) {
                outs.writeCommand(input);
                potentials.forEach(n => {
                    outs.writeMessage(n.name);
                });
            }
        }
        return super.autocomplete(input, command, args);
    }
}

export default EventsCommand
