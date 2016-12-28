class Bash {

    constructor() {
        this.lastInputs = [];
        this.outs = [];
    }

    get history() {
        return this.outs;
    }

    autocomplete(input) {
        if ("infect".startsWith(input))
            return "infect ";
        if ("clear".startsWith(input))
            return "clear";
        return null;
    }

    execute(input, state) {
        this.lastInputs.push(input);
        this.outs.push(input);
        this.outs.push("done!");

        const newState = {
            history: this.outs
        };
        return newState;
    }

    hasPrevCommand() {
        return this.lastInputs.length > 0;
    }

    getPrevCommand(input) {
        return null;
    }

    hasNextCommand() {
        return false;
    }

    getNextCommand(input) {
        return null;
    }


}


export default Bash