import matplotlib.pyplot as plt
import numpy as np
from scipy import interpolate
import sys, os

# read x and y from file
x = []
y = []

input_file = sys.argv[1]
output_folder = sys.argv[2]

if not os.path.exists(output_folder):
    os.makedirs(output_folder)

with open(input_file, 'r') as f:
    while True:
        x = []
        y = []
        header = f.readline().split()
        if header == []:
            exit()
        zone_id = header[0]
        n = int(header[1])
        for i in range(n):
            line = f.readline()
            x.append(float(line.split()[0]))
            y.append(float(line.split()[1])) 

        tck,u = interpolate.splprep([x, y], k=3, s=0)
        u=np.linspace(0,1,num=5000,endpoint=True)
        out = interpolate.splev(u,tck)

        plt.figure()
        plt.plot(x, y, 'ro', out[0], out[1], 'b')
        plt.legend(['Points', 'Interpolated B-spline', 'True'],loc='best')
        plt.axis([min(x)-1, max(x)+1, min(y)-1, max(y)+1])
        plt.title('B-Spline interpolation')
        plt.savefig(f"{output_folder}/path-{zone_id}.png", dpi=300)
        plt.close()
