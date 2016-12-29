import InfectCommand from "./InfectCommand";
import ClearCommand from "./ClearCommand";
import Out from "./Out";

class Bash {

    constructor() {
        this.prevCommands = [];
        this.prevCommandsIndex = 0;
        this.outs = new Out();
        this.commands = [
            new InfectCommand(),
            new ClearCommand()
        ];
    }

    get history() {
        return this.outs.content;
    }

    get checksum() {
        return this.outs.checksum;
    }

    autocomplete(input) {
        const tokens = input.trim().split(/ +/);
        const command = tokens.shift();

        const potentials = this.commands.filter(c => c.command.startsWith(command));
        if (potentials.length === 1) {
            // any argument?
            if (tokens.length === 0)
                return potentials[0].command + " ";
            return potentials[0].autocomplete(input, command, tokens, this.outs);
        }
        return null;
    }

    execute(input, state) {
        this.prevCommands.push(input);
        this.prevCommandsIndex = this.prevCommands.length;

        // The shift() method removes the first element from an array and returns that element.
        // This method changes the length of the array.
        const tokens = input.trim().split(/ +/);
        const command = tokens.shift();
        const args = tokens;

        const potentials = this.commands.filter(c => c.command === command);
        if (potentials.length === 1) {
            this.outs.writeCommand(input);
            potentials[0].execute(command, args, this.outs);
        }
        else if (input.trim().length === 0) {
            this.outs.writeCommand(input);
        }
        else {
            this.outs.writeCommand(input);
            this.outs.writeError("Unknown command!");
        }


        const newState = {
            history: this.outs
        };
        return newState;
    }

    getPrevCommand() {
        return this.prevCommands[--this.prevCommandsIndex];
    }

    getNextCommand(input) {
        return this.prevCommands[++this.prevCommandsIndex];
    }

    hasPrevCommand() {
        return this.prevCommandsIndex !== 0;
    }

    hasNextCommand(input) {
        return this.prevCommandsIndex !== this.prevCommands.length - 1;
    }


}


export default Bash