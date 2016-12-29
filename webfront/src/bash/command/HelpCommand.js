import Command from "../Command";

class HelpCommand extends Command {

    constructor() {
        super("help", "display the list of available commands");
    }

    commands(commands) {
        this.commands = commands;
    }

    execute(command, args, outs) {
        outs.writeMessage("Available commands:");
        outs.writeMessage("-------------------");
        this.commands.forEach(n => {
            const text = n.command + " - " + n.description;
            outs.writeMessage(text);
        });
    }
}

export default HelpCommand
