
import os
import pandas as pd
import networkx as nx
import numpy as np
from pathlib import Path

# Subfolder "data" relative to the script
folder_path = Path(__file__).parent / "edgelists"
# 📂 Folder containing your Excel edgelist files
# Build a path to a subfolder called "data" inside the script directory
folder_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
#folder_path = r"C:\Users\s1512187\OneDrive - University of Edinburgh\Documents\PhD\FCMs\fcm_python\edgelists"

# 🗂️ Collect all Excel files
excel_files = [f for f in os.listdir(folder_path) if f.endswith(".xlsx")]

graphs = {}

# 🔄 Import each Excel file and build a directed, weighted graph
for file in excel_files:
    file_path = os.path.join(folder_path, file)
    df = pd.read_excel(file_path)

    # Expecting columns: source, target, weight
    G = nx.DiGraph()
    for _, row in df.iterrows():
        G.add_edge(row['source'], row['target'], weight=row['weight'])

    graphs[file] = G

# 📊 Initialize GED matrix
n = len(graphs)
ged_matrix = np.zeros((n, n))

files = list(graphs.keys())

# 🔢 Compute Graph Edit Distance between all pairs
for i in range(n):
    for j in range(n):
        if i == j:
            ged_matrix[i, j] = 0
        else:
            # networkx provides an *approximate* GED
            ged = nx.graph_edit_distance(graphs[files[i]], graphs[files[j]])
            ged_matrix[i, j] = ged

# 📝 Convert to DataFrame for readability
ged_df = pd.DataFrame(ged_matrix, index=files, columns=files)

# 💾 Save results to Excel or CSV
ged_df.to_excel("graph_edit_distance_matrix.xlsx")
print(ged_df)
