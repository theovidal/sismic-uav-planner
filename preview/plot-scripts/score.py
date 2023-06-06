import matplotlib.pyplot as plt
import numpy as np

scores = np.loadtxt('scores.csv')
plt.stem(range(scores.size), scores, markerfmt="")
plt.xlabel('Operations')
plt.ylabel('Score')
plt.savefig('score.png', dpi=300)
plt.close()
