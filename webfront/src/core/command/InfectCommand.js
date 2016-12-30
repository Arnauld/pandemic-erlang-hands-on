import Command from "../../bash/Command";
import Fun from "../../util/fun";

class InfectCommand extends Command {

    constructor(game, cities) {
        super("infect", "infect the cities specified");
        this.game = game;
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
        this.game.infect(args);
    }
}

export default InfectCommand
