import React, {Component} from "react";
import Snap from "snapsvg-cjs";
import Cities from "./core/cities";
import BackgroundImg from "../styles/images/background.jpg";
import BackgroundSvg from "../styles/images/worldmap.svg";

const Styles = {
    city: {
        active: {
            fill: "#9E5223",
            stroke: "#E67733",
            strokeWidth: 5
        },
        disabled: {
            fill: "#383838",
            stroke: "#6b6b6b",
            strokeWidth: 5
        }
    },
    text: {
        active : {
            "fill": "#9E5223",
            "font-size": "12px"
        },
        disabled: {
            "fill": "#8f8f8f",
            "font-size": "12px"
        }
    }
};

class WorldMap extends Component {
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
        const background = s.g().attr({
            id: "g-background",
            transform: "s.75,.75,0,0"
        });
        const links = s.g().attr({id: "g-links"});
        const cities = s.g().attr({id: "g-cities"});
        const names = s.g().attr({id: "g-names"});
        if(this.props.showBackgroundImage) {
            const image = background.image(BackgroundImg, 0, 0, 1200, 849);
        }

        Snap.load(BackgroundSvg, function(f) {
            const g1 = f.select("g[id='layer1']");
            g1.attr({"transform":"matrix(1.105,0,0,1.105,-120,-90)"});
            g1.selectAll("path").forEach(n => n.attr({stroke: "#444"}));
            background.add(g1);

            const g2 = f.select("g[id='layer3']");
            g2.attr({"transform":"matrix(1.105,0,0,1.105,-120,-90)"});
            g2.selectAll("path").forEach(n => n.attr({stroke: "#eee"}));
            background.add(g2);
        });


        this.props.cities.all.forEach(node => {
            const disabled = Math.random() > 0.3;
            const newCity = cities.circle(node.cx, node.cy, 10)
                .attr(disabled ? Styles.city.disabled : Styles.city.active)
                .attr({
                    id: node.id,
                    name: node.name,
                    tx: node.tx,
                    ty: node.ty
                })
                .drag();

            const tx = node.tx || (node.cx - 20);
            const ty = node.ty || (node.cy - 15);
            const name = names.text(tx, ty, [node.name])
                .attr(disabled ? Styles.text.disabled : Styles.text.active)
                .attr({
                    id: node.id + "-text",
                    name: node.name
                })
                .drag();
        });
    }

    render() {
        return (
            <svg ref="svg" width={this.props.width} height={this.props.height}>
            </svg>
        );
    }
}

WorldMap.propTypes = {
    cities: React.PropTypes.objectOf(Cities).isRequired
};

WorldMap.defaultProps = {
    width: "900px",
    height: "636px"
};


export default WorldMap