# functions for FCM processing
# Sirimon Thomas
#07/2025


## function to average all FCM adjacency matrices

average_adjacency_matrices <- function(mat_list) {
  # Convert sparse matrices to regular ones
  mat_list <- lapply(mat_list, function(m) {
    if (inherits(m, "Matrix")) as.matrix(m) else m
  })
  
  # Get all unique node names
  all_names <- unique(unlist(lapply(mat_list, function(m) union(rownames(m), colnames(m)))))
  n <- length(all_names)
  
  # Create 3D array of NAs
  arr <- array(NA, dim = c(n, n, length(mat_list)),
               dimnames = list(all_names, all_names, NULL))
  
  for (k in seq_along(mat_list)) {
    m <- mat_list[[k]]
    
    # Create full-size NA matrix and fill it
    tmp <- matrix(NA, nrow = n, ncol = n,
                  dimnames = list(all_names, all_names))
    tmp[rownames(m), colnames(m)] <- m
    
    #add full matrix to 3d array
    arr[, , k] <- tmp
  }
  
  # Average across third dimension, ignoring NA
  avg_mat <- apply(arr, c(1, 2), function(x) mean(x, na.rm = TRUE))
  avg_mat[is.nan(avg_mat)] <- NA  # Clean up empty entries
  
  return(avg_mat)
}


# Function to compute indices with a specified weight attribute 
# compute_indices <- function(g, weight_attr = NULL) {
#   
#   # If a weight attribute is specified, extract it
#   weights <- if (!is.null(weight_attr)) {
#     edge_attr(g, weight_attr)
#   } else {
#     NULL
#   }
#   
#   data.frame(
#     density = edge_density(g),
#     avg_in_degree = mean(degree(g, mode = "in")),
#     avg_out_degree = mean(degree(g, mode = "out")),
#     avg_strength_in = mean(strength(g, mode = "in", weights = weights)),
#     avg_strength_out = mean(strength(g, mode = "out", weights = weights)),
#     transitivity = transitivity(g, type = "global"),
#     avg_path_length = average.path.length(g, directed = TRUE, weights = weights),
#     diameter = diameter(g, directed = TRUE, weights = weights)
#   )
# }

compute_indices <- function(g, weight_attr = NULL) {
  
  # Extract weights if attribute exists
  weights <- if (!is.null(weight_attr) && weight_attr %in% edge_attr_names(g)) {
    edge_attr(g, weight_attr)
  } else {
    NULL
  }
  
  data.frame(
    n_nodes = length(V(g)),
    n_edges = length(E(g)),
    #modularity = modularity(g),
    avg_degree = mean(degree(g, mode = "all")),
    #avg_in_degree = mean(degree(g, mode = "in")),
    #avg_out_degree = mean(degree(g, mode = "out")),
    avg_strength = mean(strength(g, mode = "all", weights = weights)),
    #avg_strength_in = mean(strength(g, mode = "in", weights = weights)),
    #avg_strength_out = mean(strength(g, mode = "out", weights = weights)),
    density = edge_density(g),
    transitivity = transitivity(g, type = "global"),
    avg_path_length = mean_distance(g, directed = TRUE, weights = weights),
    diameter = diameter(g, directed = TRUE, weights = weights),
    #connectivity = edge_connectivity(g),   # minimum number of edges to disconnect graph
    avg_closeness = mean(closeness(g, mode = "all", weights = weights, normalized = TRUE)),
    avg_betweenness = mean(betweenness(g, directed = TRUE, weights = weights, normalized = TRUE)),
    avg_eigenvector = mean(eigen_centrality(g, directed = TRUE, weights = weights)$vector)
  )
}


delta_con <- function(A1, A2, epsilon = 1e-3) {
  # Compute fast belief propagation similarity
  S1 <- solve(diag(nrow(A1)) + epsilon^2 * diag(nrow(A1)) - epsilon * A1)
  S2 <- solve(diag(nrow(A2)) + epsilon^2 * diag(nrow(A2)) - epsilon * A2)
  
  # Root Euclidean distance
  dist <- sqrt(sum((sqrt(S1) - sqrt(S2))^2))
  
  # Convert to similarity score
  sim <- 1 / (1 + dist)
  return(sim)
}


#participation coefficient both degree and strength

participation_coefficient <- function(g, membership, weighted.degree=T) {
  pc <- numeric(vcount(g))
  
  for (v in V(g)) {
    # --- Degree-based ---
    if(!weighted.degree){
      ki <- degree(g, v)
      if (ki > 0) {
        frac <- sapply(unique(membership), function(c) {
          sum(membership[neighbors(g, v)] == c)
        }) / ki
        pc[v] <- 1 - sum(frac^2)
      } else {
        pc[v] <- NA
      }
    } else {
      # --- Strength-based (weighted degree) ---
      ki <- strength(g, v, weights = E(g)$weight)
      if (ki > 0) {
        frac <- sapply(unique(membership), function(c) {
          neighs <- neighbors(g, v)
          # sum weights of edges to neighbors in community c
          sum(E(g)[.from(v) & .to(neighs[membership[neighs] == c])]$weight)
        }) / ki
        pc[v] <- 1 - sum(frac^2)
      } else {
        pc[v] <- NA
      }
    }
    
  }
  return(pc)
}


#compute and add all vertex attributes
add_vertex_attributes <- function(g,
                                  attributes = c("strength", "strength_in", "strength_out",
                                                 "degree", "degree_in", "degree_out",
                                                 "betweenness", "katz",
                                                 "pagerank", "closeness", "eigen",
                                                 "transivity",
                                                 #"cluster_louvain", "pc_louvain",
                                                 "cluster_infomap", "pc_infomap",
                                                 "cluster_leiden", "pc_leiden"),
                                  weighted.degree = T) {
  # Strength (weighted degree)
  if ("strength" %in% attributes) {
    V(g)$strength <- strength(g, vids = V(g), weights = E(g)$weight)
    
    V(g)$strength_norm <- V(g)$strength/max(V(g)$strength)
    
    if (is_directed(g)) {
      if ("strength_in" %in% attributes) {
        V(g)$strength_in <- strength(g, vids = V(g), mode = "in", weights = E(g)$weight)
      }
      if ("strength_out" %in% attributes) {
        V(g)$strength_out <- strength(g, vids = V(g), mode = "out", weights = E(g)$weight)
      }
      
      V(g)$strength_ratio <- V(g)$strength_in/V(g)$strength_out
    }
  } 
  # Degree (unweighted)
  if ("degree" %in% attributes) {
    V(g)$degree <- degree(g, mode = "all")
    
    V(g)$degree_norm <- V(g)$degree/max(V(g)$degree)
    
    if (is_directed(g)) {
      if ("degree_in" %in% attributes) {
        V(g)$degree_in <- degree(g, mode = "in")
      }
      if ("degree_out" %in% attributes) {
        V(g)$degree_out <- degree(g, mode = "out")
      }
      
      V(g)$degree_ratio <- V(g)$degree_in/V(g)$degree_out
    }
  }
  # Betweenness centrality
  if ("betweenness" %in% attributes) {
    V(g)$betweenness <- betweenness(g, directed = is_directed(g), weights = E(g)$weight)
  }
  
  # Katz centrality
  if ("katz" %in% attributes) {
    V(g)$katz <- alpha_centrality(g, weights = E(g)$weight, alpha = 0.1, exo = 1)
  }
  
  # PageRank
  if ("pagerank" %in% attributes) {
    V(g)$pagerank <- page_rank(g, weights = E(g)$weight)$vector
  }
  
  # Closeness centrality
  if ("closeness" %in% attributes) {
    V(g)$closeness <- closeness(g, mode = "out", weights = E(g)$weight)
  }
  
  # Eigenvector centrality
  if ("eigen" %in% attributes) {
    V(g)$eigen <- eigen_centrality(g, directed = TRUE, weights = E(g)$weight)$vector
  }
  
  # Local clustering coefficient
  if ("transivity" %in% attributes) {
    V(g)$transivity <- transitivity(g, type = "local")
  }
  
  # Community detection (Louvain on undirected projection)
  if ("cluster_louvain" %in% attributes) {
    comm_louvain <- cluster_louvain(as_undirected(g, mode = "collapse"))
    V(g)$cluster_louvain <- membership(comm_louvain)
  }
  
  # Participation coefficient (Louvain) 
  if ("pc_louvain" %in% attributes) {
    V(g)$pc_louvain <- participation_coefficient(g, 
                                                 V(g)$cluster_louvain, 
                                                 weighted.degree = weighted.degree) 
  }
  
  # Infomap clustering (directed, weighted)
  if ("cluster_infomap" %in% attributes) {
    comm_infomap <- cluster_infomap(g, e.weights = E(g)$weight)
    V(g)$cluster_infomap <- membership(comm_infomap)
  }
  
  # Participation coefficient (Infomap) 
  if ("pc_infomap" %in% attributes) {
    V(g)$pc_infomap <- participation_coefficient(g, 
                                                 V(g)$cluster_infomap, 
                                                 weighted.degree = weighted.degree) 
  }
  
  # Leiden clustering (directed, weighted)
  # if ("cluster_leiden" %in% attributes) {
  #   comm_leiden <- cluster_leiden(g, weights = E(g)$weight,
  #                                 resolution = 1,
  #                                 objective_function = "modularity")#"CPM"
  #   V(g)$cluster_leiden <- membership(comm_leiden)
  # }
  
  if ("cluster_leiden" %in% attributes) {
    comm_leiden <- leidenAlg::leiden.community(g,#leidenAlg::find_partition
                                             #edge_weights = E(g)$weight,
                                             #partition_type = "",
                                             resolution = 1)
    V(g)$cluster_leiden <- membership(comm_leiden)
  }
  # Participation coefficient (leiden) 
  if ("pc_leiden" %in% attributes) {
    V(g)$pc_leiden <- participation_coefficient(g, 
                                                V(g)$cluster_leiden, 
                                                weighted.degree = weighted.degree) 
  }
  return(list(graph = g, 
              louv = if (exists("comm_louvain")) comm_louvain else NULL, 
              infomap = if (exists("comm_infomap")) comm_infomap else NULL,
              leiden = if (exists("comm_leiden")) comm_leiden else NULL))
}


# top_nodes_by_attribute <- function(graphs, attribute, top_n = 10, decreasing = TRUE) {
#   # graphs: list of igraph objects
#   # attribute: vertex attribute name (string)
#   # top_n: number of nodes to return (default 10)
#   # decreasing: sort order (TRUE = largest first)
#   
#   results <- lapply(graphs, function(g) {
#     if (!(attribute %in% vertex_attr_names(g))) {
#       stop(paste("Attribute", attribute, "not found in graph vertices."))
#     }
#     
#     values <- vertex_attr(g, attribute)
#     names <- if ("name" %in% vertex_attr_names(g)) {
#       vertex_attr(g, "name")
#     } else {
#       as.character(V(g))
#     }
#     
#     df <- data.frame(node = names, value = values, stringsAsFactors = FALSE)
#     df <- df[order(df$value, decreasing = decreasing), ]
#     head(df, top_n)
#   })
#   
#   return(results)
# }

top_nodes_by_attribute <- function(g, attribute, top_n = 10, decreasing = TRUE) {
  #check attribute is in graph
  if (!(attribute %in% vertex_attr_names(g))) {
    stop(paste("Attribute", attribute, "not found in graph vertices."))
  }
  df <- data.frame(
    node = vertex_attr(g, "name"),
    value = vertex_attr(g, attribute),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(value)) %>%
    head(top_n)
  
  #df <- df[order(df$value, decreasing = decreasing), ]
  
  return(df)
}


# Function to compute inter-community connectivity matrix
community_connectivity <- function(g, membership) {
  if (!is_directed(g)) {
    stop("Graph must be directed for this function.")
  }
  if (is.null(E(g)$weight)) {
    stop("Graph must have edge weights.")
  }
  
  k <- length(unique(membership))
  inter_mat <- matrix(0, nrow = k, ncol = k)
  rownames(inter_mat) <- colnames(inter_mat) <- paste0("C", 1:k)
  
  # Loop over edge indices
  for (i in seq_along(E(g))) {
    ends_e <- ends(g, i)              # vertices at edge i
    v1 <- membership[ends_e[1]]
    v2 <- membership[ends_e[2]]
    w  <- E(g)$weight[i]              # weight of edge i
    
    inter_mat[v1, v2] <- inter_mat[v1, v2] + w
  }
  
  return(inter_mat)
}


merge_small_clusters <- function(g, membership, min_size = 5) { 
  # membership: vector of community assignments 
  # min_size: minimum allowed community size 
  comm_sizes <- table(membership) 
  small_clusters <- as.integer(names(comm_sizes[comm_sizes < min_size])) 
  
  # Copy membership so we can modify 
  new_membership <- membership 
  for (sc in small_clusters) { 
    verts <- which(membership == sc) 
    
    for (v in verts) { 
      # Outgoing edges from v 
      out_edges <- E(g)[.from(v)] 
      if (length(out_edges) == 0) next 
      # Target communities of neighbors 
      targets <- membership[ends(g, out_edges)[,2]] 
      weights <- E(g)$weight[out_edges] 
      # Exclude same small cluster 
      valid <- targets != sc 
      if (any(valid)) { 
        max_w <- max(weights[valid]) 
        best_targets <- targets[valid][weights[valid] == max_w] 
        
        # Resolve ties by choosing the largest community 
        sizes <- comm_sizes[as.character(best_targets)] 
        best_target <- best_targets[which.max(sizes)] 
        
        new_membership[v] <- best_target 
      } 
    } 
  } 
  return(new_membership) 
}


#test 1
merge_small_clusters1 <- function(g, membership, min_size = 5) {
  # membership: vector of community assignments
  # min_size: minimum allowed community size
  
  new_membership <- membership
  
  repeat {
    comm_sizes <- table(new_membership)
    small_clusters <- as.integer(names(comm_sizes[comm_sizes < min_size]))
    
    # Stop if no small clusters remain
    if (length(small_clusters) == 0) break
    
    for (sc in small_clusters) {
      verts <- which(new_membership == sc)
      
      for (v in verts) {
        out_edges <- E(g)[.from(v)]
        if (length(out_edges) == 0) next
        
        targets <- new_membership[ends(g, out_edges)[,2]]
        weights <- E(g)$weight[out_edges]
        
        valid <- targets != sc
        if (any(valid)) {
          max_w <- max(weights[valid])
          best_targets <- targets[valid][weights[valid] == max_w]
          
          # Resolve ties by choosing the largest community
          sizes <- comm_sizes[as.character(best_targets)]
          best_target <- best_targets[which.max(sizes)]
          
          new_membership[v] <- best_target
        }
      }
    }
  }
  
  # Relabel to remove empty clusters (drop unused IDs)
  factor_membership <- factor(new_membership)
  as.integer(factor_membership)
  
  return(new_membership)
}


# function to normalise node attributes for plotting
normalise <- function (x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}


# # function to create subgraph of x nodes around a given node
# create_subgraph <- function(graph, node_name, degrees = 2) {
#   human_health_id <- V(graph)[name == node_name]
#   
#   if (length(human_health_id) == 0) {
#     stop("Node not found in the graph.")
#   }
#   
#   # Initialize the list of nodes to include
#   all_nodes <- human_health_id
#   
#   # Collect nodes up to the specified degree
#   current_frontier <- human_health_id
#   
#   for (i in seq_len(degrees)) {
#     neighbors_list <- unlist(lapply(current_frontier, function(node) neighbors(graph, node, mode = "all")))
#     new_frontier <- setdiff(unique(neighbors_list), all_nodes)
#     
#     # Update the list of all encountered nodes
#     all_nodes <- unique(c(all_nodes, new_frontier))
#     current_frontier <- new_frontier
#   }
#   
#   # Create the subgraph
#   subgraph <- induced_subgraph(graph, vids = all_nodes)
#   
#   return(subgraph)
# }
# 
# 
# create_subgraph <- function(graph, node_name, degrees = 2) {
#   node_id <- V(graph)[name == node_name]
#   
#   if (length(node_id) == 0) {
#     stop(paste("Node", node_name, "not found in the graph."))
#   }
#   
#   all_nodes <- node_id
#   current_frontier <- node_id
#   
#   for (i in seq_len(degrees)) {
#     neighbors_list <- unlist(lapply(current_frontier, function(node) neighbors(graph, node, mode = "all")))
#     new_frontier <- setdiff(unique(neighbors_list), all_nodes)
#     all_nodes <- unique(c(all_nodes, new_frontier))
#     current_frontier <- new_frontier
#   }
#   
#   subgraph <- induced_subgraph(graph, vids = all_nodes)
#   
#   return(subgraph)
# }

# function to create subgraph around a given node, from edgelists
subgraph_nodes_from_edgelist <- function(edgelist, node_name, degrees = 2) {
  nodes <- node_name
  
  for (i in seq_len(degrees)) {
    # Filter edgelist for current nodes
    g <- edgelist %>% filter(from %in% nodes | to %in% nodes)
    
    # Update the list of nodes
    nodes <- unique(c(nodes, g$from, g$to))
  }
  
  # Filter for final subgraph edgelist
  final_subgraph <- edgelist %>% filter(from %in% nodes & to %in% nodes)
  
  final_nodes <- c(final_subgraph$from, final_subgraph$to) %>% unique
  
  return(final_nodes)
}



### scenario testing####

#function to run the FCM simulation

library(igraph)

#' Run an FCM simulation to convergence
#'
#' @param graph     igraph object with edge attribute "weight"
#' @param initial_state  Named numeric vector of starting activations
#' @param clamped_nodes  Named numeric vector of nodes to hold fixed throughout
#' @param transfer_fn    "sigmoid", "tanh", or "bivalent"
#' @param lambda         Steepness of sigmoid (ignored for tanh/bivalent)
#' @param max_iter       Maximum number of iterations
#' @param threshold      Convergence threshold (max change between steps)

run_fcm <- function(graph,
                    initial_state,
                    clamped_nodes = NULL,
                    ag.mode       = 'sum',
                    transfer_fn   = "sigmoid",
                    lambda        = 5,
                    max_iter      = 100,
                    threshold     = 1e-6) {
  
  # Extract weight matrix (W[i,j] = weight of edge i -> j)
  weight_col <- ifelse(ag.mode == "sum", "weight_sum", "weight_mean")
  W <- as.matrix(as_adjacency_matrix(graph, attr = weight_col, sparse = FALSE))
  node_names <- rownames(W)
  
  # Align initial state with igraph's node ordering
  A <- initial_state[node_names]
  if (anyNA(A)) stop("initial_state is missing activations for some nodes.")
  
  # Define transfer function
  f <- switch(transfer_fn,
              "sigmoid"  = function(x) 1 / (1 + exp(-lambda * x)),
              "tanh"     = tanh,
              "bivalent" = function(x) as.numeric(x > 0),
              stop("transfer_fn must be 'sigmoid', 'tanh', or 'bivalent'")
  )
  
  # Initialise history matrix
  history <- matrix(
    NA, nrow = max_iter + 1, ncol = length(node_names),
    dimnames = list(NULL, node_names)
  )
  history[1, ] <- A
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    
    # Core FCM update: column sums of W weighted by A (incoming signal per node)
    A_new <- f(as.vector(t(W) %*% A))
    names(A_new) <- node_names
    
    # Pin clamped nodes to their fixed values
    if (!is.null(clamped_nodes)) {
      A_new[names(clamped_nodes)] <- clamped_nodes
    }
    
    history[iter + 1, ] <- A_new
    
    # Check for convergence
    if (max(abs(A_new - A)) < threshold) {
      converged <- TRUE
      history   <- history[seq_len(iter + 1), , drop = FALSE]
      message("Converged after ", iter, " iteration(s).")
      break
    }
    
    A <- A_new
  }
  
  if (!converged) warning("Did not converge within ", max_iter, " iterations.")
  
  list(
    final_state = A_new,
    history     = as.data.frame(history[!is.na(history[, 1]), , drop = FALSE]),
    iterations  = if (converged) iter else max_iter,
    converged   = converged
  )
}

#function to do scenario testing by comparing a baseline run to scenario with edited nodes

#' Compare a baseline FCM run to a scenario where node(s) are changed
#'
#' @param graph            igraph object
#' @param baseline_state   Named numeric vector of baseline activations
#' @param scenario_changes Named numeric vector: nodes to change and their new values
#' @param clamp            If TRUE, changed nodes are held fixed during simulation.
#'                         If FALSE, they are only nudged at t=0 and evolve freely.
#' @param ...              Additional arguments passed to run_fcm

run_scenario <- function(graph,
                         baseline_state,
                         scenario_changes,
                         clamp = TRUE,
                         ...) {
  
  node_names <- V(graph)$name
  
  # --- Baseline ---
  message("--- Running Baseline ---")
  baseline_result <- run_fcm(graph, initial_state = baseline_state, ...)
  
  # --- Scenario ---
  # Modify the starting state
  scenario_init <- baseline_state
  scenario_init[names(scenario_changes)] <- scenario_changes
  
  # Optionally clamp the intervened nodes at their new value
  clamped <- if (clamp) scenario_changes else NULL
  
  message("--- Running Scenario ---")
  scenario_result <- run_fcm(
    graph,
    initial_state = scenario_init,
    clamped_nodes = clamped,
    ...
  )
  
  # --- Comparison table ---
  comparison <- data.frame(
    Node       = node_names,
    Baseline   = round(baseline_result$final_state[node_names],  4),
    Scenario   = round(scenario_result$final_state[node_names],  4),
    Delta      = round(scenario_result$final_state[node_names] -
                         baseline_result$final_state[node_names],  4),
    Intervened = node_names %in% names(scenario_changes),
    row.names  = NULL
  )
  
  list(
    baseline   = baseline_result,
    scenario   = scenario_result,
    comparison = comparison
  )
}


#function to run an ensemble on FCM network from random uniform initial states

library(igraph)

#' Run an FCM ensemble from random uniform initial states
#'
#' @param graph       igraph object with edge attribute "weight"
#' @param n_runs      Number of random restarts
#' @param seed        Random seed for reproducibility
#' @param ...         Arguments passed to run_fcm() (transfer_fn, lambda, etc.)

run_fcm_ensemble <- function(graph,
                             n_runs = 1000,
                             seed   = 42,
                             ...) {
  
  if (!is.null(seed)) set.seed(seed)
  
  nodes <- V(graph)$name
  n     <- length(nodes)
  
  # Storage
  final_states <- matrix(
    NA_real_, nrow = n_runs, ncol = n,
    dimnames = list(NULL, nodes)
  )
  n_iterations <- integer(n_runs)
  did_converge <- logical(n_runs)
  
  pb <- txtProgressBar(min = 0, max = n_runs, style = 3)
  
  for (i in seq_len(n_runs)) {
    
    # Random uniform draw from [0, 1] for every node
    init <- setNames(runif(n, min = 0, max = 1), nodes)
    
    res  <- suppressMessages(
      run_fcm(graph, initial_state = init, ...)
    )
    
    final_states[i, ] <- res$final_state[nodes]
    n_iterations[i]   <- res$iterations
    did_converge[i]   <- res$converged
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # Report non-convergence
  n_fail <- sum(!did_converge)
  if (n_fail > 0) {
    warning(sprintf(
      "%d of %d runs did not converge — consider increasing max_iter.",
      n_fail, n_runs
    ))
  } else {
    message(sprintf("All %d runs converged.", n_runs))
  }
  
  # Node-level summary statistics
  node_summary <- data.frame(
    Node    = nodes,
    Mean    = round(colMeans(final_states),                    4),
    SD      = round(apply(final_states, 2, sd),                4),
    CV      = round(apply(final_states, 2, sd) /
                      colMeans(final_states),                  4),
    Median  = round(apply(final_states, 2, median),            4),
    Q25     = round(apply(final_states, 2, quantile, 0.25),    4),
    Q75     = round(apply(final_states, 2, quantile, 0.75),    4),
    IQR     = round(apply(final_states, 2, IQR),               4),
    Min     = round(apply(final_states, 2, min),               4),
    Max     = round(apply(final_states, 2, max),               4),
    # Stability: 1 = perfectly stable, 0 = maximally variable
    Stability_Score = round(1 - apply(final_states, 2, sd) /
                              0.5, 4),
    row.names = NULL
  )
  
  message(sprintf(
    "Mean iterations to convergence: %.1f (range %d–%d)",
    mean(n_iterations), min(n_iterations), max(n_iterations)
  ))
  
  list(
    final_states = as.data.frame(final_states),
    node_summary = node_summary,
    did_converge = did_converge,
    n_iterations = n_iterations,
    n_runs       = n_runs,
    n_converged  = sum(did_converge)
  )
}

run_fcm_ensemble2 <- function(graph,
                             n_runs = 1000,
                             seed   = 42,
                             ...) {
  
  if (!is.null(seed)) set.seed(seed)
  
  nodes <- V(graph)$name
  n     <- length(nodes)
  
  final_states   <- matrix(NA_real_, nrow = n_runs, ncol = n,
                           dimnames = list(NULL, nodes))
  initial_states <- matrix(NA_real_, nrow = n_runs, ncol = n,
                           dimnames = list(NULL, nodes))
  n_iterations   <- integer(n_runs)
  did_converge   <- logical(n_runs)
  
  pb <- txtProgressBar(min = 0, max = n_runs, style = 3)
  
  for (i in seq_len(n_runs)) {
    
    init <- setNames(runif(n, min = 0, max = 1), nodes)
    initial_states[i, ] <- init
    
    res  <- suppressMessages(
      run_fcm(graph, initial_state = init, ...)
    )
    
    final_states[i, ] <- res$final_state[nodes]
    n_iterations[i]   <- res$iterations
    did_converge[i]   <- res$converged
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # Report convergence
  n_converged <- sum(did_converge)
  n_fail      <- sum(!did_converge)
  
  message(sprintf(
    "%d of %d runs converged (%.1f%%)",
    n_converged, n_runs, n_converged / n_runs * 100
  ))
  
  if (n_fail > 0) {
    message(sprintf(
      "%d runs did not converge and will be excluded from analysis.",
      n_fail
    ))
  }
  
  # ---- Exclude non-converged runs ----
  final_states_clean   <- final_states[did_converge, , drop = FALSE]
  initial_states_clean <- initial_states[did_converge, , drop = FALSE]
  
  # Node summary on converged runs only
  node_summary <- data.frame(
    Node            = nodes,
    Mean            = round(colMeans(final_states_clean),                 4),
    SD              = round(apply(final_states_clean, 2, sd),             4),
    Median          = round(apply(final_states_clean, 2, median),         4),
    Q25             = round(apply(final_states_clean, 2, quantile, 0.25), 4),
    Q75             = round(apply(final_states_clean, 2, quantile, 0.75), 4),
    IQR             = round(apply(final_states_clean, 2, IQR),            4),
    Min             = round(apply(final_states_clean, 2, min),            4),
    Max             = round(apply(final_states_clean, 2, max),            4),
    Stability_Score = round(1 - apply(final_states_clean, 2, sd) / 0.5,  4),
    row.names       = NULL
  )
  
  message(sprintf(
    "Mean iterations to convergence: %.1f (range %d-%d)",
    mean(n_iterations[did_converge]),
    min(n_iterations[did_converge]),
    max(n_iterations[did_converge])
  ))
  
  list(
    final_states        = as.data.frame(final_states_clean),
    initial_states      = as.data.frame(initial_states_clean),
    final_states_all    = as.data.frame(final_states),    # includes non-converged
    initial_states_all  = as.data.frame(initial_states),  # all runs, pre-filtering
    did_converge        = did_converge,
    n_iterations        = n_iterations,
    n_runs              = n_runs,
    n_converged         = n_converged,
    node_summary        = node_summary
  )
}






#function ot detect multiple attractors

library(factoextra)  # for fviz_nbclust

#' Check whether the ensemble reveals multiple attractors
#' using k-means clustering on the full final state vectors

detect_attractors <- function(ensemble_result, max_k = 6) {
  
  fs <- ensemble_result$final_states
  
  # Elbow / silhouette plot to suggest number of clusters
  p <- fviz_nbclust(fs, kmeans, method = "silhouette",
                    k.max = min(max_k, nrow(fs) - 1)) +
    labs(title = "Silhouette Analysis — How Many Attractors?") +
    theme_minimal(base_size = 12)
  
  print(p)
  
  # Fit k=2 as a baseline comparison
  km2 <- kmeans(fs, centers = 2, nstart = 25)
  km1 <- kmeans(fs, centers = 1, nstart = 25)
  
  message(sprintf(
    "Within-cluster SS: k=1: %.4f | k=2: %.4f  (lower = tighter clusters)",
    km1$tot.withinss, km2$tot.withinss
  ))
  
  invisible(list(km2 = km2, km1 = km1))
}

detect_attractors2 <- function(ensemble_result, max_k = 10) {
  
  fs <- ensemble_result$final_states
  
  # Silhouette plot
  p <- fviz_nbclust(fs, kmeans, method = "silhouette",
                    k.max = min(max_k, nrow(fs) - 1)) +
    labs(title = "Silhouette Analysis — How Many Attractors?") +
    theme_minimal(base_size = 12)
  print(p)
  
  # Within-SS across all k values
  message("Within-cluster SS by k:")
  wss_values <- sapply(seq_len(max_k), function(k) {
    km <- kmeans(fs, centers = k, nstart = 25)
    km$tot.withinss
  })
  
  # Print a table
  wss_df <- data.frame(
    k              = seq_len(max_k),
    Within_SS      = round(wss_values, 4),
    Pct_Drop       = round(c(NA, -diff(wss_values) / wss_values[-max_k] * 100), 1)
  )
  print(wss_df)
  
  # Flag the biggest drops
  biggest_drop <- which.max(abs(diff(wss_values))) + 1
  message(sprintf(
    "Largest within-SS drop occurs at k=%d (%.1f%% reduction from k=%d)",
    biggest_drop,
    wss_df$Pct_Drop[biggest_drop],
    biggest_drop - 1
  ))
  
  invisible(list(wss = wss_df))
}

#function to plot stability score for all nodes
library(ggplot2)

#' Bar chart of stability score for all nodes
#' Highlights the most and least stable nodes

plot_stability_overview <- function(ensemble_result,
                                    n_label   = 15,
                                    title     = "Node Stability Across Random Restarts") {
  
  df <- ensemble_result$node_summary
  df <- df[order(df$SD), ]
  df$Rank <- seq_len(nrow(df))
  
  # Label the n_label most and least stable
  n_each      <- floor(n_label / 2)
  label_idx   <- c(seq_len(n_each), (nrow(df) - n_each + 1):nrow(df))
  df$Label    <- ifelse(df$Rank %in% label_idx, df$Node, "")
  
  ggplot(df, aes(x = Rank, y = SD, colour = SD)) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = mean(df$SD), linetype = "dashed",
               colour = "grey50") +
    ggrepel::geom_text_repel(aes(label = Label), size = 3,
                             max.overlaps = 20, show.legend = FALSE) +
    scale_colour_gradient(low = "#2ca02c", high = "#d62728",
                          name = "SD") +
    ggplot2::annotate("text", x = nrow(df) * 0.02, y = mean(df$SD),
             label = "Mean SD", vjust = -0.5, colour = "grey50", size = 3) +
    labs(title = title,
         x     = "Nodes (ranked by stability, left = most stable)",
         y     = "SD of Final Activation Across Runs") +
    theme_minimal(base_size = 12)
}


# function to draw density plots for chosed nodes

library(tidyr)

#' Density plots for a chosen subset of nodes
#' Defaults to the top and bottom N by stability

plot_node_distributions <- function(ensemble_result,
                                    nodes = NULL,
                                    n_each = 6,
                                    title  = "Distribution of Final Activations") {
  
  df      <- ensemble_result$final_states
  summary <- ensemble_result$node_summary
  
  # Default: most and least stable nodes
  if (is.null(nodes)) {
    ordered <- summary$Node[order(summary$SD)]
    nodes   <- c(
      head(ordered, n_each),   # most stable
      tail(ordered, n_each)    # least stable
    )
    message("Showing ", n_each, " most stable and ", n_each, " least stable nodes.")
  }
  
  # Attach stability label for facet ordering
  stability_order <- summary$Node[order(summary$SD)]
  label_df <- summary[summary$Node %in% nodes,
                      c("Node", "SD", "Mean")]
  label_df$Facet_Label <- sprintf("%s\n(SD = %.3f)", label_df$Node, label_df$SD)
  
  long <- pivot_longer(df[, nodes, drop = FALSE],
                       cols      = everything(),
                       names_to  = "Node",
                       values_to = "Final_Activation")
  long <- merge(long, label_df[, c("Node", "Facet_Label", "SD", "Mean")],
                by = "Node")
  
  # Order facets by SD
  long$Facet_Label <- factor(
    long$Facet_Label,
    levels = label_df$Facet_Label[order(label_df$SD)]
  )
  
  ggplot(long, aes(x = Final_Activation, fill = SD)) +
    geom_density(alpha = 0.7, colour = NA) +
    geom_vline(aes(xintercept = Mean), linetype = "dashed",
               colour = "grey30", linewidth = 0.6) +
    scale_fill_gradient(low = "#2ca02c", high = "#d62728",
                        name = "SD") +
    scale_x_continuous(limits = c(0, 1)) +
    facet_wrap(~Facet_Label, scales = "free_y") +
    labs(title    = title,
         subtitle = "Dashed line = mean final activation | Green = stable, Red = variable",
         x        = "Final Activation",
         y        = "Density") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
}
