import pandas as pd
import igraph as ig
import leidenalg as la
import numpy as np

# Load the edgelist from a CSV file
# Ensure your CSV has columns named 'source', 'target', and 'weight'
edgelist = pd.read_csv("C:/Users/s1512187/OneDrive - University of Edinburgh/Documents/PhD/FCMs/output/edgelists/aggregated_sum.csv")
# Ensure correct column names
edgelist = edgelist[['from', 'to', 'weight']]
# Create a directed graph from the edgelist
#g = ig.Graph.DataFrame(edgelist, directed=True, vertices=pd.concat([edgelist['from'], edgelist['to']]).unique())
g = ig.Graph.TupleList(edgelist.itertuples(index=False), directed=True, edge_attrs=['weight'])

# Set a resolution parameter to influence community size
resolution_parameter = 1 

# Run the Leiden algorithm
partition = la.find_partition(
    g, 
    la.RBConfigurationVertexPartition, 
    resolution_parameter=resolution_parameter, 
    weights=g.es['weight']
)

# Get membership
membership = partition.membership
# Print the number of communities
print("Number of Communities:", len(set(membership)))

# Calculate the participant coefficient for each node
def participant_coefficient(g, membership):
    num_nodes = len(g.vs)
    memberships = pd.Series(membership, index=range(num_nodes))
    community_count = memberships.nunique()

    k_i = np.zeros(num_nodes)
    for edge in g.es:
        k_i[edge.source] += 1
        k_i[edge.target] += 1

    p_coefficients = np.zeros(num_nodes)
    for v in g.vs:
        k_i_in = np.zeros(community_count)
        for neighbor in g.neighbors(v, mode='all'):
            if membership[v.index] != membership[neighbor]:
                k_i_in[membership[neighbor]] += 1
        sum_squares = np.sum((k_i_in / k_i[v.index]) ** 2)
        p_coefficients[v.index] = 1 - sum_squares

    return p_coefficients

participant_coeffs = participant_coefficient(g, membership)

# Get node names
node_names = g.vs['name']

# Save to a CSV file with node names and participant coefficients
output_df = pd.DataFrame({'node': node_names, 'community': membership, 'pc_leiden': participant_coeffs})
output_df.to_csv('C:/Users/s1512187/OneDrive - University of Edinburgh/Documents/PhD/FCMs/fcm_python/leiden_output.csv', index=False)
