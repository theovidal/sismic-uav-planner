import matplotlib.pyplot as plt
import numpy as np
import sys

if len(sys.argv) < 3:
    print("Syntax: python3 score.py <input_file> <output_file>")
    exit()

input_file = sys.argv[1]
output_file = sys.argv[2]

scores = np.loadtxt(input_file)
plt.stem(range(scores.size), scores, markerfmt="")
plt.xlabel('Opérations')
plt.ylabel('Score')
plt.savefig(output_file, dpi=300)
plt.close()
