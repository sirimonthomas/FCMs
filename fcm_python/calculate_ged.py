import pandas as pd
import networkx as nx
from networkx.algorithms import similarity
import os

# Define the directory containing your edgelists
#edgelist_dir = 'C:/Users/s1512187/OneDrive - University of Edinburgh/Documents/PhD/FCMs/fcm_python/edgelists'
edgelist_dir = 'edgelists'

# List all CSV files in the directory
csv_files = [f for f in os.listdir(edgelist_dir) if f.endswith('.csv')]

graphs = []

# Load each CSV as a graph
for file in csv_files:
    file_path = os.path.join(edgelist_dir, file)
    df = pd.read_csv(file_path)
    G = nx.from_pandas_edgelist(df, 'from', 'to', ['weight'], create_using=nx.DiGraph())
    graphs.append(G)

# Initialize a distance matrix
num_graphs = len(graphs)
distance_matrix = [[0] * num_graphs for _ in range(num_graphs)]

# Compute the Graph Edit Distances
for i in range(num_graphs):
    for j in range(i + 1, num_graphs):
        dist = similarity.graph_edit_distance(graphs[i], graphs[j])
        distance_matrix[i][j] = dist
        distance_matrix[j][i] = dist  # Symmetric matrix

# Convert the distance matrix to a DataFrame for easier inspection
distance_df = pd.DataFrame(distance_matrix, index=csv_files, columns=csv_files)

print(distance_df)

# Save the DataFrame to a CSV file
#distance_df.to_csv('C:/Users/s1512187/OneDrive - University of Edinburgh/Documents/PhD/FCMs/fcm_python/graph_edit_distances.csv', index=True)
distance_df.to_csv('graph_edit_distances.csv', index=True)

