#!/usr/bin/env python3

import matplotlib.pyplot as plt
import subprocess

[rs, mh] = subprocess.run(['stack','runhaskell','./ABCExamples.hs'], stdout=subprocess.PIPE).stdout.decode('utf-8').splitlines()
rs = list(map(lambda x : float(x), rs[1:-1].split(",")))
mh = list(map(lambda x : float(x), mh[1:-1].split(",")))

print(mh)

plt.hist(rs, bins=25, alpha=0.5, label="RS")
plt.hist(mh, bins=25, alpha=0.5, label="GMH")
plt.legend(loc="upper right")

plt.show()
