import Command from "../../bash/Command";
import Fun from "../../util/fun";

class WorldMapCommand extends Command {

    constructor() {
        super("display", "adjust map's display");
        this.subCommands = ["links", "cities", "cityNames", "cityHints", "background"];
    }

    worldmap(worldmap) {
        this.worldmap = worldmap;
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
            }
        }
        return super.autocomplete(input, command, args);
    }


    execute(command, args, outs) {
        this.worldmap.toggleDisplay(args[0]);
    }
}

export default WorldMapCommand
