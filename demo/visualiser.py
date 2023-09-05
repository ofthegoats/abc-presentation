#!/usr/bin/env python3

import sys
import subprocess
import matplotlib.pyplot as plt

def main(model):
    if model == "weibull":
        result = subprocess.run(["stack", "runhaskell", "Examples.hs"], input="weibull\n".encode(), stdout=subprocess.PIPE).stdout.decode()
        result = [float(i) for i in result[1:-2].split(",")] # turn into list
        plt.hist(result, bins=35, range=(-1, 8));
        plt.show()
    elif model == "gk":
        result = subprocess.run(["stack", "runhaskell", "Examples.hs"], input="gk\n".encode(), stdout=subprocess.PIPE).stdout.decode()
        result = result[0:-1].split("\n")
        g = [float(i) for i in result[0][1:-1].split(",")]
        k = [float(i) for i in result[1][1:-1].split(",")]
        plt.hist(g, bins=35, range=(-3, 3), alpha=0.8)
        plt.hist(k, bins=35, range=(-3, 3), alpha=0.8)
        plt.show()
    else:
        print("bad argument")

if __name__ == '__main__':
    main(sys.argv[1])
