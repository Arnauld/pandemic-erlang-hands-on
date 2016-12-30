import React, {Component} from "react";
import Bash from "./bash/bash";

const CTRL_CHAR_CODE = 17;
const L_CHAR_CODE = 76;
const C_CHAR_CODE = 67;
const UP_CHAR_CODE = 38;
const DOWN_CHAR_CODE = 40;
const TAB_CHAR_CODE = 9;

class Terminal extends Component {

    constructor(props) {
        super(props);
        this.ctrlPressed = false;
        this.handleKeyDown = this.handleKeyDown.bind(this);
        this.handleKeyUp = this.handleKeyUp.bind(this);
    }

    /*
     * Keep input in view on change
     */
    componentDidUpdate() {
        this.refs.input.scrollIntoView();
    }

    /*
     * Forward the input along to the Bash autocompleter. If it works,
     * update the input.
     */
    attemptAutocomplete() {
        const input = this.refs.input.value;
        const suggestion = this.props.bash.autocomplete(input);

        const comod = this.props.bash.checksum;
        this.setState({comod: comod});

        if (suggestion) {
            this.refs.input.value = suggestion;
        }
    }

    /*
     * Handle keydown for special hot keys. The tab key
     * has to be handled on key down to prevent default.
     * @param {Event} evt - the DOM event
     */
    handleKeyDown(evt) {
        if (evt.which === CTRL_CHAR_CODE) {
            this.ctrlPressed = true;
        } else if (evt.which === TAB_CHAR_CODE) {
            // Tab must be on keydown to prevent default
            this.attemptAutocomplete();
            evt.preventDefault();
        }
    }

    /*
     * Handle keyup for special hot keys.
     * @param {Event} evt - the DOM event
     *
     * -- Supported hot keys --
     * ctrl + l : clear
     * ctrl + c : cancel current command
     * up - prev command from history
     * down - next command from history
     * tab - autocomplete
     */
    handleKeyUp(evt) {
        const input = this.refs.input.value;
        const bash = this.props.bash;

        if (evt.which === L_CHAR_CODE) {
            if (this.ctrlPressed) {
                this.setState(bash.execute('clear', this.state));
            }
        } else if (evt.which === C_CHAR_CODE) {
            if (this.ctrlPressed) {
                this.setState(bash.ctrlC());
                this.refs.input.value = '';
            }
        } else if (evt.which === UP_CHAR_CODE) {
            if (bash.hasPrevCommand()) {
                this.refs.input.value = bash.getPrevCommand(input);
            }
        } else if (evt.which === DOWN_CHAR_CODE) {
            if (bash.hasNextCommand()) {
                this.refs.input.value = bash.getNextCommand(input);
            } else {
                this.refs.input.value = '';
            }
        } else if (evt.which === CTRL_CHAR_CODE) {
            this.ctrlPressed = false;
        }
    }

    handleSubmit(evt) {
        evt.preventDefault();

        // Execute command
        const input = evt.target[0].value;
        const bash = this.props.bash;

        const newState = bash.execute(input, this.state);
        this.setState(newState);
        this.refs.input.value = '';
    }


    prefix() {
        return (<span className="prefix">{`$`}</span>);
    }

    renderHistoryItem(prefix) {
        return (item, key) => {
            if (item.type === "command") {
                return (<div key={key}>{prefix}<span className={item.type}>{item.text}</span></div>);
            }
            if (Array.isArray(item.text)) {
                let count = 0;
                return (
                    <div key={key} className="items">
                        {item.text.map(t => <div className={`${item.type} item"`} key={count++}>{t}</div>)}
                    </div>)
            }
            return (
                <div key={key}>
                    <span className={item.type}>{item.text}</span>
                </div>);
        }
    }

    render() {
        const history = this.props.bash.history;
        return (
            <div className="terminal" onClick={() => this.refs.input.focus()}>
                {history.map(this.renderHistoryItem(this.prefix()))}
                <form onSubmit={evt => this.handleSubmit(evt)}>
                    {this.prefix()}
                    <input
                        autoComplete="off"
                        onKeyDown={this.handleKeyDown}
                        onKeyUp={this.handleKeyUp}
                        ref="input"
                    />
                </form>
            </div>
        );
    }
}

Terminal.propTypes = {
    bash: React.PropTypes.instanceOf(Bash).isRequired
};

export default Terminal
