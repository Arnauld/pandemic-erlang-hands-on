class Command {

    constructor(command) {
        this.command = command;
    }

    autocomplete(input, command, args, outs) {
        return input;
    }

    execute(command, args, outs) {
        outs.writeError("Not implemented!");
    }
}

export default Command
