class Strings {

    static longestCommonPrefix(xs) {
        if (xs.length === 0)
            return;
        if (xs.length === 1)
            return xs[0];

        const ref = xs[0];
        let len = ref.length;
        let i;
        xs.slice(1)
            .forEach(s => {
                for (i = 0; i < len; i++) {
                    if (s[i] !== ref[i]) {
                        len = i;
                        break;
                    }
                }
            });

        if (len > 0)
            return ref.substring(0, len);
        return;
    }

}

export default Strings