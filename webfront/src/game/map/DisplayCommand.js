import Command from "../../bash/Command";
import Fun from "../../util/fun";
import Strings from "../../util/strings";
import {Layer} from "./Worldmap.jsx";
import {toggleLayer} from "../../actions";

class DisplayCommand extends Command {

    constructor(store) {
        super("toggleDisplay", "show/hide layer on map");
        this.store = store;
        this.subCommands = [Layer.LINKS, Layer.CITIES, Layer.CITY_HINTS, Layer.CITY_NAMES, Layer.BACKGROUND];
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
                    outs.writeMessage(n);
                });

                const prefix = Strings.longestCommonPrefix(potentials);
                if(prefix) {
                    return command + " " + prefix;
                }
            }
        }
        return super.autocomplete(input, command, args);
    }


    execute(command, args, outs) {
        this.store.dispatch(toggleLayer(args[0]));
    }
}

export default DisplayCommand
