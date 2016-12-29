class Out {

    constructor() {
        this.data = [];
        this.comod = 1;
    }

    get checksum() {
        return this.comod;
    }

    get content() {
        return this.data;
    }

    writeCommand(input) {
        this.comod++;
        this.data.push({type: "command", text: input});
    }

    writeError(input) {
        this.comod++;
        this.data.push({type: "error", text: input});
    }

    writeMessage(input) {
        console.log("message: ", input);

        this.comod++;
        this.data.push({type: "message", text: input});
    }

    clear() {
        this.comod++;
        // http://stackoverflow.com/a/8134354
        this.data.splice(0, this.data.length);
    }
}

export default Out
