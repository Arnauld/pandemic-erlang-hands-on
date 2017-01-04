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
    }

    componentDidMount() {
        const store = this.props.store;
        store.subscribe(() => {
            let citySelection = store.getState().city_selection;
            this.setState({cities: citySelection})
        });
    }

    renderCity() {
        if (!this.state.cities || this.state.cities.length === 0)
            return [];

        return (<div className="pandemic appbar-cities">
            {
                this.state.cities.map((city, key) => {
                    const info = this.props.cities.stateOf(city);
                    return (<div key={key} className="appbar-city">
                        <div className="name">{info.name}</div>
                        <div className="infectionLevel">{info.infectionLevel}</div>
                    </div>);
                })
            }
        </div>);
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
                    {this.renderCity()}
                    {/*<Navbar.Form pullLeft>*/}
                    {/*<FormGroup*/}
                    {/*validationState={this.getValidationState()}>*/}
                    {/*<ControlLabel>&gt;</ControlLabel>*/}
                    {/*<FormControl type="text"*/}
                    {/*placeholder="Search"*/}
                    {/*className="cli"*/}
                    {/*onChange={this.handleChange}/>*/}
                    {/*<FormControl.Feedback />*/}
                    {/*</FormGroup>*/}
                    {/*{' '}*/}
                    {/*<Button type="submit">Submit</Button>*/}
                    {/*</Navbar.Form>*/}
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