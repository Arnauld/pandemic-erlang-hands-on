import Command from "./Command";

class ClearCommand extends Command {

    constructor() {
        super("clear", "clear the terminal");
    }


    execute(command, args, outs) {
        outs.clear();
    }
}

export default ClearCommand
