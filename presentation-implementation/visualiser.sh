#!/usr/bin/env sh

case ${1} in
    approxWeibull|weibull)
        data=$(echo "weibull" | stack runhaskell Examples.hs);
        python -c "import matplotlib.pyplot as plt; plt.hist(${data}, range=(-1, 8), bins=35); plt.show();"
    ;;
    abcgk|gk)
        data=$(echo "gk" | stack runhaskell Examples.hs);
        datas=( $data );
        g=${datas[0]};
        k=${datas[1]};
        python -c "import matplotlib.pyplot as plt; plt.hist(${g}, range=(-2, 2), bins=35, alpha=0.8); plt.hist(${k}, range=(-2, 2), bins=35, alpha=0.8); plt.show()";
    ;;
    *) echo "wrong arg" ;;
esac
