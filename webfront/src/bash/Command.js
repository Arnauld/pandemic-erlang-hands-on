class Command {

    constructor(command, description) {
        this.command = command;
        this.description = description;
    }

    autocomplete(input, command, args, outs) {
        return input;
    }

    execute(command, args, outs) {
        outs.writeError("Not implemented!");
    }
}

export default Command
