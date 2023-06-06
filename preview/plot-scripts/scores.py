# scores are stored in a file as : id,heuristic,score
# Generate one graph for each id, with heuristic as x-axis and score as y-axis

import matplotlib.pyplot as plt
import numpy as np
import os, sys


def plot_graph(output, id, heuristic, score):
    plt.scatter(heuristic, score)
    plt.xlabel('Heuristic')
    plt.ylabel('Score')
    plt.title('Score vs Heuristic for id: '+str(id))
    plt.savefig(output+'/'+str(id)+'.png')
    plt.close()

def main():

    # Get the two parameters passed by the user : input file and output folder
    input_file = sys.argv[1]
    output_folder = sys.argv[2]

    # Read scores from file
    scores = np.loadtxt(input_file, delimiter=',', dtype=int)
    scores = scores[scores[:,0].argsort()]

    # Create directory for storing graphs
    if not os.path.exists('data/test/scores'):
        os.makedirs('data/test/scores')

    # Plot graphs
    for i in range(1, 101):
        id = i
        heuristic = scores[scores[:,0]==id][:,1]
        score = scores[scores[:,0]==id][:,2]
        plot_graph(output_folder, id, heuristic, score)

if __name__ == '__main__':
    main()