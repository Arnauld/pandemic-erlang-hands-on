import React, {Component} from "react";
import {
    Navbar,
    Nav,
    NavItem,
    NavDropdown,
    MenuItem,
    FormGroup,
    FormControl,
    ControlLabel,
    Button
} from "react-bootstrap";

class AppBar extends Component {
    constructor() {
        super();
        this.state = {value: ''};
        this.handleChange = this.handleChange.bind(this)
    }

    getValidationState() {
        const cmd = this.state.value;
        if (cmd.startsWith("infect")) {
            return 'warning';
        }
        const length = this.state.value.length;
        if (length > 10) return 'success';
        else if (length > 5) return 'warning';
        else if (length > 0) return 'error';
    }

    handleChange(e) {
        this.setState({value: e.target.value});
    }

    render() {
        return (
            <Navbar inverse collapseOnSelect>
                <Navbar.Header>
                    <Navbar.Brand>
                        <a href="#" className="pandemic">Pandemic</a>
                    </Navbar.Brand>
                    <Navbar.Toggle />
                </Navbar.Header>
                <Navbar.Collapse>
                    <Navbar.Form pullLeft>
                        <FormGroup
                            validationState={this.getValidationState()}>
                            <ControlLabel>&gt;</ControlLabel>
                            <FormControl type="text"
                                         placeholder="Search"
                                         className="cli"
                                         onChange={this.handleChange}/>
                            <FormControl.Feedback />
                        </FormGroup>
                        {' '}
                        <Button type="submit">Submit</Button>
                    </Navbar.Form>
                    {/*<Nav>*/}
                    {/*<NavItem eventKey={1} href="#">Link</NavItem>*/}
                    {/*<NavItem eventKey={2} href="#">Link</NavItem>*/}
                    {/*<NavDropdown eventKey={3} title="Dropdown" id="basic-nav-dropdown">*/}
                    {/*<MenuItem eventKey={3.1}>Action</MenuItem>*/}
                    {/*<MenuItem eventKey={3.2}>Another action</MenuItem>*/}
                    {/*<MenuItem eventKey={3.3}>Something else here</MenuItem>*/}
                    {/*<MenuItem divider/>*/}
                    {/*<MenuItem eventKey={3.3}>Separated link</MenuItem>*/}
                    {/*</NavDropdown>*/}
                    {/*</Nav>*/}
                    {/*<Nav pullRight>*/}
                    {/*<NavItem eventKey={1} href="#">Link Right</NavItem>*/}
                    {/*<NavItem eventKey={2} href="#">Link Right</NavItem>*/}
                    {/*</Nav>*/}
                </Navbar.Collapse>
            </Navbar>);
    }
}

export default AppBar