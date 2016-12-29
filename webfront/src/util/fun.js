class Fun {

    static last(xs) {
        return xs[xs.length - 1];
    };

    static removeLast(xs) {
        return xs.slice(0, xs.length - 1);
    };


}

export default Fun
