import Command from "./command";

class ClearCommand extends Command {

    constructor() {
        super("clear");
    }


    execute(command, args, outs) {
        outs.clear();
    }
}

export default ClearCommand
