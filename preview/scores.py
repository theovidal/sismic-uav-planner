# scores are stored in a file as : id,heuristic,score
# Generate one graph for each id, with heuristic as x-axis and score as y-axis

import matplotlib.pyplot as plt
import numpy as np
import os

def plot_graph(id, heuristic, score):
    plt.scatter(heuristic, score)
    plt.xlabel('Heuristic')
    plt.ylabel('Score')
    plt.title('Score vs Heuristic for id: '+str(id))
    plt.savefig('data/test/scores/'+str(id)+'.png')
    plt.close()

def main():

    # Read scores from file
    scores = np.loadtxt('data/test/scores.csv', delimiter=',', dtype=int)
    scores = scores[scores[:,0].argsort()]

    # Create directory for storing graphs
    if not os.path.exists('data/test/scores'):
        os.makedirs('data/test/scores')

    # Plot graphs
    for i in range(1, 101):
        id = i
        heuristic = scores[scores[:,0]==id][:,1]
        score = scores[scores[:,0]==id][:,2]
        plot_graph(id, heuristic, score)

if __name__ == '__main__':
    main()