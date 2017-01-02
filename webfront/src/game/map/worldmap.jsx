import React, {Component} from "react";
import Snap from "snapsvg-cjs";
import Fun from "../../util/fun";
import Cities from "../cities";
import Game, {GameListener} from "../game";
import BackgroundImg from "../../../styles/images/background.jpg";
import BackgroundSvg from "../../../styles/images/worldmap.svg";
import BioHazardSvg from "../../../styles/images/Radiation_warning_symbol2.svg";

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
        selected: {
            fill: "none",
            stroke: "#E89F5A",
            strokeDasharray: "5px",
            strokeDashoffset: "5px",
            strokeWidth: 3
        },
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
            setTimeout(() => this.applyChanges(byGeneration[g]), g * 6000);
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
        this.initCanvas();
        this.updateCanvas();
        this.props.game.subscribe(new WorldMapGameListener(this));
        this.props.controls.forEach(c => c.worldmap(this));
    }

    componentDidUpdate() {
        this.updateCanvas();
    }

    citySelected(city) {
        this.props.controls.forEach(c => {
            if (c.citySelected)
                c.citySelected(city);
        });
        this.drawCitySelection(city);
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
    }

    cityOutbreak(city) {
        const node = this.props.cities.nodeOf(city);
        const svg = this.refs.svg;
        const s = Snap(svg);
        const anims = s.select("g[id='g-anims']");

        // --- rotating radiation sign
        const group = anims.g();
        Snap.load(BioHazardSvg, f => {
            f.selectAll("g").forEach(g => {
                g.selectAll("circle").attr({"fill": Styles.outbreak.stroke});
                g.selectAll("path").attr({"fill": Styles.outbreak.stroke});
                g.attr({"transform": "scale(0.25, 0.25)"});
                group.add(g);
            });
            group.attr({"transform": Snap.matrix().translate(node.cx, node.cy)});
        });
        const n = 360 * 6;
        Snap.animate(0, n, val => {
            const s = Math.min(0.15, 0.15 * val / (360 * 4));
            group.selectAll("g").forEach(g => g.attr({"transform": `scale(${s}, ${s})`}));
            group.attr({"transform": `rotate(${val},${node.cx},${node.cy}) translate(${node.cx},${node.cy})`});
        }, 1000 * 6, () => group.remove());

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

        setTimeout(() => anim(4), 3000);
    }

    initCanvas() {
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
    }

    toggleDisplay(layer) {
        const svg = this.refs.svg;
        const s = Snap(svg);
        if (layer === "background") {
            const layer = s.select("g[id='g-background']");
            layer.toggleClass("hidden");
            return;
        }
        if (layer === "cities") {
            const layer = s.select("g[id='g-cities']");
            layer.toggleClass("hidden");
            return;
        }
        if (layer === "cityNames") {
            const layer = s.select("g[id='g-names']");
            layer.toggleClass("hidden");
            return;
        }
        if (layer === "cityHints") {
            const layer = s.select("g[id='g-background']").select("g[id='layer3']");
            layer.toggleClass("hidden");
            return;
        }
        if (layer === "links") {
            const layer = s.select("g[id='g-links']");
            layer.toggleClass("hidden");
            return;
        }
    }

    updateCanvas() {
        this.drawBackground();
        this.drawCities();
        this.drawCityNames();
        this.drawLinks();
    }

    drawBackground() {
        const svg = this.refs.svg;
        const s = Snap(svg);
        const background = s.select("g[id='g-background']");
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
    }

    drawCities() {
        const self = this;
        const svg = this.refs.svg;
        const s = Snap(svg);
        const cities = s.select("g[id='g-cities']");

        this.props.cities.nodes.forEach(node => {
            const disabled = this.props.cities.stateOf(node.name).disabled;
            const newCity = cities.circle(node.cx, node.cy, 10)
                .attr(disabled ? Styles.city.disabled : Styles.city.active)
                .attr({
                    id: node.id,
                    name: node.name,
                    tx: node.tx,
                    ty: node.ty
                })
                .drag();
            newCity.node.onclick = () => self.citySelected(node.name);
        });
    }

    drawCitySelection(city) {
        const self = this;
        const svg = this.refs.svg;
        const s = Snap(svg);
        const cities = s.select("g[id='g-cities']");
        const node = this.props.cities.nodeOf(city);

        cities.selectAll("circle[id='selected']").forEach(n => n.remove());
        cities.circle(node.cx, node.cy, 17)
            .attr(Styles.city.selected)
            .attr({
                id: "selected"
            });
    }

    drawCityNames() {
        const svg = this.refs.svg;
        const s = Snap(svg);
        const names = s.select("g[id='g-names']");

        this.props.cities.nodes.forEach(node => {
            const disabled = this.props.cities.stateOf(node.name).disabled;
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

    drawLinks() {
        const svg = this.refs.svg;
        const s = Snap(svg);
        const links = s.select("g[id='g-links']");

        this.props.cities.links.forEach(link => {
            const [city1, city2] = link;
            const node1 = this.props.cities.nodeOf(city1);
            const node2 = this.props.cities.nodeOf(city2);
            if (!node1)
                console.error("Unknown city ", city1);
            if (!node2)
                console.error("Unknown city ", city2);

            if ((link.includes("san_francisco") && link.includes("manila"))
                || (link.includes("san_francisco") && link.includes("tokyo"))
                || (link.includes("los_angeles") && link.includes("sydney"))) {

            }
            else {
                links.polyline(node1.cx, node1.cy, node2.cx, node2.cy)
                    .attr({
                        stroke: "#4d4d4d",
                        strokeWidth: 2
                    });
            }
        });
        const san_francisco = this.props.cities.nodeOf("san_francisco");
        const manila = this.props.cities.nodeOf("manila");
        const tokyo = this.props.cities.nodeOf("tokyo");
        const los_angeles = this.props.cities.nodeOf("los_angeles");
        const sydney = this.props.cities.nodeOf("sydney");
        const specials = [[san_francisco, manila],
            [san_francisco, tokyo],
            [los_angeles, sydney]];
        specials.forEach(([c1, c2]) => {
            links.polyline(c1.cx, c1.cy, c2.cx - 1000, c2.cy)
                .attr({
                    stroke: "#4d4d4d",
                    strokeWidth: 2
                });
            links.polyline(c1.cx + 1000, c1.cy, c2.cx, c2.cy)
                .attr({
                    stroke: "#4d4d4d",
                    strokeWidth: 2
                });
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