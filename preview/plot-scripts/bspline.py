import matplotlib.pyplot as plt
import numpy as np
from scipy import interpolate

# read x and y from file
x = []
y = []
with open('data/test/zone-16.txt', 'r') as f:
    for line in f:
        x.append(float(line.split()[0]))
        y.append(float(line.split()[1]))


tck,u = interpolate.splprep([x, y], k=5, s=0)
u=np.linspace(0,1,num=5000,endpoint=True)
out = interpolate.splev(u,tck)

plt.figure()
plt.plot(x, y, 'ro', out[0], out[1], 'b')
plt.legend(['Points', 'Interpolated B-spline', 'True'],loc='best')
plt.axis([min(x)-1, max(x)+1, min(y)-1, max(y)+1])
plt.title('B-Spline interpolation')
plt.savefig('path.png', dpi=300)
