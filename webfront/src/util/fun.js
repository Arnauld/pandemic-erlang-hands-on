class Fun {

    static last(xs) {
        return xs[xs.length - 1];
    };

    static removeLast(xs) {
        return xs.slice(0, xs.length - 1);
    };

    static groupBy(xs, key) {
        const map = {};
        xs.forEach(e => {
            const k = key(e);
            map[k] = map[k] || [];
            map[k].push(e);
        });
        return map;
    };

}

export default Fun
