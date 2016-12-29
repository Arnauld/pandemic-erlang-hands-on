import HelpCommand from "./command/HelpCommand";
import Out from "./Out";

class Bash {

    constructor(commands, history) {
        this.prevCommands = [];
        this.prevCommandsIndex = 0;
        this.outs = new Out();
        history.forEach(n => this.outs.writeMessage(n));

        const helpCommand = new HelpCommand();
        this.commands = commands.slice();
        this.commands.push(helpCommand);
        helpCommand.commands(this.commands);
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
            if (tokens.length === 0 && command !== potentials[0].command)
                return potentials[0].command + " ";
            return potentials[0].autocomplete(input, command, tokens, this.outs);
        }
        if (potentials.length > 1) {
            this.outs.writeCommand(input);
            potentials.forEach(n => this.outs.writeMessage(n.command));
            return input;
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
            this.outs.writeError("Unknown command! - try 'help'");
        }


        const newState = {
            history: this.outs
        };
        return newState;
    }

    ctrlC() {
        console.log("Ctrl+C");
        this.outs.writeError("^C");
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