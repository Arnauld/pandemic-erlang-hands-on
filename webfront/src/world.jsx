import React, {Component} from 'react'
import config from './config.json';
import Snap from 'snapsvg-cjs';

class Board extends Component {
	constructor(props) {
    	super(props);
  	}
  	componentDidMount() {
        this.updateCanvas();
    }
    componentDidUpdate() {
        this.updateCanvas();
    }
    updateCanvas() {
        const svg = this.refs.svg;
        const s = Snap(svg);

        s.circle(300, 150, 10);
    }
    render() {
		return (
			<svg ref="svg" width={this.props.width} height={this.props.height}>
			</svg>
		);
	}
}
Board.defaultProps = {
	width: "900px",
	height: "636px"
};

class Greeter extends Component{
  render() {
    return (
      <div>
        {config.greetText}
        <Board/>
      </div>
    );
  }
}

export default Greeter
