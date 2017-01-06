import React, {Component} from "react";
import Snap from "snapsvg-cjs";
import Fun from "../../util/fun";
import Cities from "../cities";
import Game from "../game";
import {selectCity, INFECTION_RECEIVED} from "../../actions";
import BackgroundImg from "../../../styles/images/background.jpg";
import BackgroundSvg from "../../../styles/images/worldmap.svg";
import BioHazardSvg from "../../../styles/images/Radiation_warning_symbol2.svg";

export const Layer = {
    LINKS: "links",
    CITIES: "cities",
    CITY_NAMES: "cityNames",
    CITY_HINTS: "cityHints",
    BACKGROUND: "background"
};
export const DEFAULT_LAYERS = [Layer.BACKGROUND, Layer.CITIES, Layer.CITY_NAMES, Layer.CITY_HINTS, Layer.LINKS];
export const INITIAL_LAYERS = DEFAULT_LAYERS.reduce((acc, l) => {
    acc[l] = true;
    return acc
}, {});

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

const SPECIAL_LINKS = [
    ["san_francisco", "manila"],
    ["san_francisco", "tokyo"],
    ["los_angeles", "sydney"]];


class WorldMap extends Component {
    constructor(props) {
        super(props);
        this.lastEventProcessed = -1;
    }

    componentDidMount() {
        this.initCanvas();
        this.updateCanvas();
        this.props.store.subscribe(() => this.updateFromStoreState());
    }

    updateFromStoreState() {
        const state = this.props.store.getState();
        // TODO find a more suitable way?
        // there is no shadow dom here, thus one need to make the diff by ourself
        this.drawCitySelection(state.city_selection);
        this.updateLayersVisibility(state.layers);
        this.processEvents(state.events);
    }

    componentDidUpdate() {
        this.updateCanvas();
    }

    processEvents(events) {
        let lastEvent = this.lastEventProcessed;
        console.log("Processing events from ", lastEvent);
        events.forEach(e => {
            if (e.sequence <= lastEvent)
                return;
            lastEvent = e.sequence;
            if (e.type === INFECTION_RECEIVED) {
                this.onInfection(e.infection);
            }
        });

        this.lastEventProcessed = lastEvent;
    }

    onInfection(infection) {
        const byGeneration = Fun.groupBy(infection.changes, c => c.generation);
        const generations = Object.keys(byGeneration).sort((a, b) => a - b);
        generations.forEach(g => {
            setTimeout(() => this.applyInfectionGenerationChanges(byGeneration[g]), g * 6000);
        });
    }

    applyInfectionGenerationChanges(changes) {
        console.log("Apply infection generation changes", changes);
        changes.forEach(c => {
            if (c.type === "infected") {
                this.onCityInfected(c.city);
            }
            else if (c.type === "outbreak") {
                this.onCityOutbreak(c.city);
            }
        });
    }

    citySelected(city) {
        this.props.store.dispatch(selectCity(city));
    }

    onCityInfected(city) {
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

    onCityOutbreak(city) {
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
        setTimeout(() => {
            const links = this.props.cities.linksOf(city);
            links.forEach(other => {
                const otherNode = this.props.cities.nodeOf(other);
                this.traverseLinks(node, otherNode, ([x1, y1], [x2, y2]) => {
                    const propagation = anims.circle(x1, y1, 10)
                        .attr({"class":"propagation"})
                        .attr({
                            fill: "#9f1b33",
                            stroke: "#9f1b33",
                            strokeWidth: 1
                        });
                    propagation.animate({cx:x2, cy:y2}, 2000, () => {
                        propagation.remove();
                    });
                    console.log("Draw anim from ", x1, y1, x2, y2);
                });
            });
        }, 4000);
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

    updateLayersVisibility(layers) {
        Object.keys(layers).forEach(k => {
            const layer = this.layerOf(k);
            if (layer)
                WorldMap.updateLayerVisibility(layer, layers[k]);
        });
    }

    static updateLayerVisibility(layer, visibility) {
        if (visibility) {
            if (layer.hasClass("hidden"))
                layer.removeClass("hidden");
        }
        else {
            if (!layer.hasClass("hidden"))
                layer.addClass("hidden");
        }
    }

    layerOf(layer) {
        const svg = this.refs.svg;
        const s = Snap(svg);
        if (layer === Layer.BACKGROUND) {
            return s.select("g[id='g-background']");
        }
        if (layer === Layer.CITIES) {
            return s.select("g[id='g-cities']");
        }
        if (layer === Layer.CITY_NAMES) {
            return s.select("g[id='g-names']");
        }
        if (layer === Layer.CITY_HINTS) {
            return s.select("g[id='g-background']").select("g[id='layer3']");
        }
        if (layer === Layer.LINKS) {
            return s.select("g[id='g-links']");
        }
    }

    updateCanvas() {
        this.drawBackground();
        this.drawCities();
        this.drawCityNames();
        this.drawLinks();
        this.updateFromStoreState();
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

    drawCitySelection(cities) {
        const svg = this.refs.svg;
        const s = Snap(svg);

        // unselect previous one
        const citiesLayer = s.select("g[id='g-cities']");
        citiesLayer.selectAll("circle[class='selected']").forEach(n => n.remove());

        // select if defined
        cities.forEach(city => {
            const node = this.props.cities.nodeOf(city);
            citiesLayer.circle(node.cx, node.cy, 17)
                .attr(Styles.city.selected)
                .attr({
                    class: "selected"
                });
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

    traverseLinks(node1, node2, callback) {
        if (SPECIAL_LINKS.find(([city1, city2]) =>
            (city1 === node1.name && city2 === node2.name) || (city2 === node1.name && city1 === node2.name))) {
            if (node1.cx < node2.cx) {
                // special cases
                callback([node1.cx, node1.cy], [node2.cx - 1000, node2.cy]);
                callback([node1.cx + 1000, node1.cy], [node2.cx, node2.cy]);
            }
            else {
                callback([node1.cx - 1000, node1.cy], [node2.cx, node2.cy]);
                callback([node1.cx, node1.cy], [node2.cx + 1000, node2.cy]);
            }
        }
        else {
            callback([node1.cx, node1.cy], [node2.cx, node2.cy]);
        }
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

            this.traverseLinks(node1, node2, ([x1, y1], [x2, y2]) => {
                links.polyline(x1, y1, x2, y2)
                    .attr({
                        stroke: "#4d4d4d",
                        strokeWidth: 2,
                        name: `${city1} - ${city2}`
                    });
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