import React, {Component} from "react";
import Snap from "snapsvg-cjs";
import Fun from "./util/fun";
import Cities from "./core/cities";
import Game, {GameListener} from "./core/game";
import BackgroundImg from "../styles/images/background.jpg";
import BackgroundSvg from "../styles/images/worldmap.svg";

const Styles = {
    outbreak: {
        fill: "none",
        stroke: "#9f1b33",
        strokeWidth: 10
    },
    infection: {
        fill: "none",
        stroke: "#E89F5A",
        strokeWidth: 5
    },
    city: {
        active: {
            fill: "#8D584C",
            stroke: "#E89F5A",
            strokeWidth: 5
        },
        disabled: {
            fill: "#383838",
            stroke: "#6b6b6b",
            strokeWidth: 5
        }
    },
    text: {
        active: {
            "fill": "#E89F5A",
            "font-size": "12px"
        },
        disabled: {
            "fill": "#6b6b6b",
            "font-size": "12px"
        }
    }
};

class WorldMapGameListener extends GameListener {

    constructor(worldMap) {
        super();
        this.worldMap = worldMap;
    }

    onInfection(infection) {
        const byGeneration = Fun.groupBy(infection.changes, c => c.generation);
        const generations = Object.keys(byGeneration).sort((a, b) => a - b);
        generations.forEach(g => {
            setTimeout(() => this.applyChanges(byGeneration[g]), g * 2000);
        });
    }

    applyChanges(changes) {
        changes.forEach(c => {
            if (c.type === "infected") {
                this.worldMap.cityInfected(c.city);
            }
            else if (c.type === "outbreak") {
                this.worldMap.cityOutbreak(c.city);
            }
        });
    }

}

class WorldMap extends Component {
    constructor(props) {
        super(props);
    }

    componentDidMount() {
        this.updateCanvas();
        this.props.game.subscribe(new WorldMapGameListener(this));
    }

    componentDidUpdate() {
        this.updateCanvas();
    }

    cityInfected(city) {
        const node = this.props.cities.nodeOf(city);
        const svg = this.refs.svg;
        const s = Snap(svg);
        const anims = s.select("g[id='g-anims']");
        const circle = anims
            .circle(node.cx, node.cy, 10)
            .attr({"class": "infection"})
            .attr(Styles.infection);
        circle.animate({r: 60, opacity: 0.2}, 1000, () => {
            circle.remove();
        });

        // Snap.animate(10, 60, val => {
        //     circle.attr({r: val});
        // }, 1000, () => {
        //     circle.remove();
        //     console.log("animation done!");
        // });
    }

    cityOutbreak(city) {
        const node = this.props.cities.nodeOf(city);
        const svg = this.refs.svg;
        const s = Snap(svg);
        const anims = s.select("g[id='g-anims']");

        const anim = (n) => {
            if (n == 0)
                return;

            const circle = anims
                .circle(node.cx, node.cy, 10)
                .attr({"class": "outbreak"})
                .attr(Styles.outbreak);
            circle.animate({r: 50, opacity: 0.2}, 500, () => {
                circle.remove();
                anim(n - 1);
            });
        };

        anim(3);
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
        const anims = s.g().attr({id: "g-anims"});
        if (this.props.showBackgroundImage) {
            const image = background.image(BackgroundImg, 0, 0, 1200, 849);
        }

        Snap.load(BackgroundSvg, function (f) {
            const g1 = f.select("g[id='layer1']");
            g1.attr({"transform": "matrix(1.105,0,0,1.105,-120,-90)"});
            g1.selectAll("path").forEach(n => n.attr({stroke: "#444"}));
            background.add(g1);

            const g2 = f.select("g[id='layer3']");
            g2.attr({"transform": "matrix(1.105,0,0,1.105,-120,-90)"});
            g2.selectAll("path").forEach(n => n.attr({stroke: "#eee"}));
            background.add(g2);
        });


        this.props.cities.nodes.forEach(node => {
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
            <svg ref="svg"
                 width={this.props.width}
                 height={this.props.height}
                 viewBox={"0 0 900 636"}>
            </svg>
        );
    }
}

WorldMap.propTypes = {
    cities: React.PropTypes.instanceOf(Cities).isRequired,
    game: React.PropTypes.instanceOf(Game).isRequired
};

WorldMap.defaultProps = {
    width: "900px",
    height: "636px"
};


export default WorldMap