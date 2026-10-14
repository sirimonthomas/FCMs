#FCM network analysis
#Sirimon Thomas
#20/07/2025

#remotes::install_github("drostlab/edgynode")

#setup####
pacman::p_load(
  tidyverse,
  here,
  igraph,
  ggnetwork,
  ggraph,
  readxl,
  FCMapper,
  RColorBrewer,
  viridis,
  edgynode,
  reshape2,
  tm,
  topicmodels,
  slam,
  reticulate,
  leidenAlg,
  factoextra
  #ged
  #nevada
)


#import functions
source(here('functions.R'))

#define if FCM should be aggregated by averaging or summing

#ag.mode <- 'mean'
ag.mode <- 'sum'

#import FCM metadata
fcm.metadata <- read_excel(here('interviews','interviews.xlsx')) %>% select(1:10) %>%
  mutate(stakeholder = case_when(str_detect(`Internal/External`, 'Internal') ~ 'primary',
                                 str_detect(`Internal/External`, 'External') ~ 'secondary'),
         Gender = case_when(Gender == 'M' ~ 'men',
                            Gender == 'F' ~ 'women')) %>%
  slice(-1) # remove the pilot FCM

### import all edge list ####
fcm.edgelist.all <- read_excel(here('input','FCM_all_edges_edits.xlsx'))

#split FCMs into a list of each FCM
fcm.edgelist.indiv <- fcm.edgelist.all %>% split(.$FCM) %>%
  lapply(function(x) x[ , !(names(x) %in% "FCM")])

### create individual graphs and cumulative concepts ####
#create storage objects
concepts.all <- character()
concepts <- data.frame(
  index = integer(),
  FCM = integer(),
  number.of.concepts = integer(),
  cumulative.concepts = integer(),
  stringsAsFactors = FALSE
)

fcm.graph.indiv <- list()
#fcm.adj.matrix <- list()

for (fcm in seq_along(fcm.edgelist.indiv)) {
  
  #create graph object
  fcm.graph.indiv[[fcm]] <- graph_from_data_frame(fcm.edgelist.indiv[[fcm]],
                                                  directed = T)
  # #create adjacency matrices
  # fcm.adj.matrix[[fcm]] <- as_adjacency_matrix(fcm.graph[[fcm]],
  #                                              attr = 'weight',
  #                                              names = T,
  #                                              sparse = T)
  
  # write.csv(as.matrix(fcm.adj.matrix[[fcm]]), here('output','adjacency_matrices',paste0(names(fcm.edgelist[fcm]),'_adjacency_matrix.csv')),
  #           row.names = T)
  
  #for calculating cumulative number of concepts
  concepts.all <- c(concepts.all, V(fcm.graph.indiv[[fcm]])$name) 
  
  concepts <- rbind(
    concepts,
    data.frame(
      index = fcm,
      FCM = as.numeric(names(fcm.edgelist.indiv[fcm])),
      number.of.concepts = length(V(fcm.graph.indiv[[fcm]])$name),
      cumulative.concepts = length(concepts.all%>% unique()) ,
      stringsAsFactors = FALSE
    )
  )
}


# calculate frequency of concepts
concepts.all <- as.data.frame(concepts.all) %>%
  group_by(concepts.all) %>%
  summarise(n=n())

#### plot cumulative concepts ####
#plot cumulative concepts to check for saturation (leveling off)
cum.concept.graph <- ggplot(concepts, aes(x = index, y = cumulative.concepts)) +
  geom_line(color = '#F8766D') +
  geom_point(size = 2, color = '#00BFC4') +
  labs(
    title = "Cumulative Number of Concepts",
    x = "FCM Index",
    y = "Cumulative Concepts"
  ) + theme_minimal()

ggsave(here('output','cumulative_concepts_graph.jpeg'),cum.concept.graph, dpi = 300)

#average number of concepts
mean(concepts$number.of.concepts)
sd(concepts$number.of.concepts)

#histogram of number of concepts
concepts.hist <- ggplot(concepts, 
                        aes(x = number.of.concepts)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(
    title = "Distribution of Number of Concepts",
    x = "Number of concepts",
    y = "Count"
  )
ggsave(here('output','num_concepts_hist.jpeg'),concepts.hist, dpi =300)


### create edgelists and graph objects ####

### edgelists ####

#### aggregated ####
#add polarity and absolute weights

fcm.edgelist.ag <- fcm.edgelist.all %>%
  group_by(from,to) %>%
  summarise(weight_mean = mean(weight, na.rm =T),
            weight_sum = sum(weight, na.rm = T),
            sd_weight = sd(weight, na.rm = T),
            frequency = sum(weight != 0, na.rm = T),
            .groups = 'drop') %>%
  mutate(polarity = case_when(
    ag.mode == 'mean' & weight_mean < 0 ~ 'negative',
    ag.mode == 'mean' & weight_mean > 0 ~ 'positive',
    ag.mode == 'sum'  & weight_sum  < 0 ~ 'negative',
    ag.mode == 'sum'  & weight_sum  > 0 ~ 'positive',
    TRUE ~ NA_character_
  ),
  weight = if (ag.mode == 'sum') abs(weight_sum) else abs(weight_mean)
  )

#check for edges that negate each other when summed or averaged
#should be empty
if (ag.mode == "mean") {
  edgelist.cancelling <- fcm.edgelist.all %>%
    inner_join(
      fcm.edgelist.ag %>% filter(weight_mean == 0 & sd_weight != 0),
      by = c("from", "to")
    ) %>%
    arrange(from, to)
  print(paste('Edges aggergating to 0:', nrow(edgelist.cancelling)))
} else if (ag.mode == "sum") {
  edgelist.cancelling <- fcm.edgelist.all %>%
    inner_join(
      fcm.edgelist.ag %>% filter(weight_sum == 0),
      by = c("from", "to")
    ) %>%
    arrange(from, to)
  print(paste('Edges aggergating to 0:', nrow(edgelist.cancelling)))
}

#### stakeholder ####

fcm.edgelist.disag.stakeholder <- fcm.edgelist.all %>%
  left_join(fcm.metadata, by = c('FCM'='Code')) %>%
  group_by(stakeholder,from,to) %>%
  summarise(weight_mean = mean(weight, na.rm =T),
            weight_sum = sum(weight, na.rm = T),
            sd_weight = sd(weight, na.rm = T),
            frequency = sum(weight != 0, na.rm = T)) %>%
  mutate(polarity = case_when(
    ag.mode == 'mean' & weight_mean < 0 ~ 'negative',
    ag.mode == 'mean' & weight_mean > 0 ~ 'positive',
    ag.mode == 'sum'  & weight_sum  < 0 ~ 'negative',
    ag.mode == 'sum'  & weight_sum  > 0 ~ 'positive',
    TRUE ~ NA_character_
  ),
  weight = case_when(
    ag.mode == 'mean' ~ abs(weight_mean),
    ag.mode == 'sum'  ~ abs(weight_sum)
  ),
  weight = ifelse(weight == 0,1e-6, weight)) %>% #made o weights a tiny positive value to be able to calculate closeness and shortest path algorithms
  ungroup() %>%
  group_by(stakeholder) %>% 
  split(.$stakeholder) %>% # split by stakeholder group
  lapply(function(df) df %>% ungroup() %>% select(-stakeholder)) #remove 'stakeholder' column

#### gender ####

fcm.edgelist.disag.gender <- fcm.edgelist.all %>%
  left_join(fcm.metadata, by = c('FCM'='Code')) %>%
  group_by(Gender,from,to) %>%
  summarise(weight_mean = mean(weight, na.rm =T),
            weight_sum = sum(weight, na.rm = T),
            sd_weight = sd(weight, na.rm = T),
            frequency = sum(weight != 0, na.rm = T)) %>%
  mutate(polarity = case_when(
    ag.mode == 'mean' & weight_mean < 0 ~ 'negative',
    ag.mode == 'mean' & weight_mean > 0 ~ 'positive',
    ag.mode == 'sum'  & weight_sum  < 0 ~ 'negative',
    ag.mode == 'sum'  & weight_sum  > 0 ~ 'positive',
    TRUE ~ NA_character_
  ),
  weight = case_when(
    ag.mode == 'mean' ~ abs(weight_mean),
    ag.mode == 'sum'  ~ abs(weight_sum)
  ),
  weight = ifelse(weight == 0,1e-6, weight)) %>% ##made o weights a tiny positive value to be able to calculate closeness and shortest path algorithms
  ungroup() %>%
  group_by(Gender) %>% 
  split(.$Gender) %>% # split by stakeholder group
  lapply(function(df) df %>% ungroup() %>% select(-Gender)) #remove 'gender' column

#### gender & stakeholder ####
fcm.edgelist.disag.stake.gender <- fcm.edgelist.all %>%
  left_join(fcm.metadata, by = c('FCM'='Code')) %>%
  group_by(stakeholder,Gender,from,to) %>%
  summarise(weight_mean = mean(weight, na.rm =T),
            weight_sum = sum(weight, na.rm = T),
            sd_weight = sd(weight, na.rm = T),
            frequency = sum(weight != 0, na.rm = T)) %>%
  mutate(polarity = case_when(
    ag.mode == 'mean' & weight_mean < 0 ~ 'negative',
    ag.mode == 'mean' & weight_mean > 0 ~ 'positive',
    ag.mode == 'sum'  & weight_sum  < 0 ~ 'negative',
    ag.mode == 'sum'  & weight_sum  > 0 ~ 'positive',
    TRUE ~ NA_character_
  ),
  weight = case_when(
    ag.mode == 'mean' ~ abs(weight_mean),
    ag.mode == 'sum'  ~ abs(weight_sum)
  ),
  weight = ifelse(weight == 0,1e-6, weight),#made o weights a tiny positive value to be able to calculate closeness and shortest path algorithms
  stakeholder.gender = paste(stakeholder, Gender)) %>%
  ungroup() %>%
  group_by(stakeholder.gender) %>% 
  split(.$stakeholder.gender) %>% # split by stakeholder group
  lapply(function(df) df %>% ungroup() %>% select(-Gender, -stakeholder, -stakeholder.gender)) #remove grouping columns

#combine all edgelists
fcm.edgelists <- c(list('aggregated' = fcm.edgelist.ag),fcm.edgelist.disag.stakeholder, fcm.edgelist.disag.gender, fcm.edgelist.disag.stake.gender)

# write each edgelist to a separate CSV for import to python to compute GED
for (i in seq_along(fcm.edgelists)) {
  write.csv(fcm.edgelists[[i]],
            here('output','edgelists',paste0(str_replace_all(names(fcm.edgelists)[i],' ','_'),'_',ag.mode,'.csv')), 
            row.names = FALSE)
  
  write.csv(fcm.edgelists[[i]],
            here('fcm_python','edgelists',paste0(str_replace_all(names(fcm.edgelists)[i],' ','_'),'_',ag.mode,'.csv')), 
            row.names = FALSE,
            na='')
}

# #### create graphs
# in python, run 'leiden_community_detection.py' to calculate communitites using leiden algorithm

### create graph objects ####
#adds all graph metrics and calculated louvain and infomap clusters
fcm.graphs   <- list()
#fcm.louv     <- list()
fcm.infomap  <- list()
fcm.leiden   <- list()

# #import leiden communities
# fcm.leiden <- read.csv(here('fcm_python','leiden_output.csv')) %>%
#   mutate(community = community +1)

for (nm in names(fcm.edgelists)) {
  g <- graph_from_data_frame(fcm.edgelists[[nm]], directed = TRUE) %>%
    add_vertex_attributes()

  #save to lists
  fcm.graphs[[nm]]  <- g$graph
  #fcm.louv[[nm]]    <- g$louv
  fcm.infomap[[nm]] <- g$infomap
  fcm.leiden[[nm]]  <- g$leiden
}

### calculate graph metrics ####

#### vertex attributes table ####
vertex.attr <- list()
for (nm in names(fcm.graphs)) {
  # add degree, strength and classify as 'driver' 'mediator' or 'outcome'
  vertex.attr[[nm]] <- as_data_frame(fcm.graphs[[nm]], what = "vertices") %>%
    mutate(strength_ratio = ifelse(is.infinite(strength_ratio),NA,strength_ratio),
           strength_class = case_when(
             strength_ratio == 0 ~ "input",
             strength_ratio > 0 & strength_ratio < 1 ~ "driver",
             strength_ratio == 1 ~ "mediator",
             strength_ratio > 1 ~ "driven node",
             is.na(strength_ratio) ~ "outcome",
             TRUE ~ NA_character_   # catch-all for anything unexpected
           ), .after = strength_ratio
    ) %>%
    arrange(desc(strength))
  #write to csv
  write.csv(vertex.attr[[nm]],
            file = here('output','vertex_attributes',paste0(str_replace_all(names(fcm.graphs[nm]),' ','_'),'_vertex_attributes_',ag.mode,'.csv')),
            row.names = F)
}

#### normalised strength difference ####
#calculate difference in normalised strength for shared nodes between different stakeholders
strength.comparison.stakeholder <- inner_join(vertex.attr$primary %>% select('name',strength_norm_pri = strength_norm),
                                              vertex.attr$secondary %>% select('name',strength_norm_sec = strength_norm),
                                              by = 'name') %>%
  mutate(norm_strength_diff = strength_norm_pri - strength_norm_sec) %>%
  arrange(desc(norm_strength_diff))

write.csv(strength.comparison.stakeholder,
          here('output',paste0('normalised_strength_difference_stakeholders_',ag.mode,'.csv')),
          row.names = F)

#plot graphically
strength.comp.stake.g <- strength.comparison.stakeholder %>%
  # Select bottom 10 and top 10
  slice(c(1:10, (n()-9):n())) %>%
  # Plot
  ggplot(aes(x = reorder(name, norm_strength_diff),
             y = norm_strength_diff,
             fill = norm_strength_diff > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "cyan", "FALSE" = "salmon")) +
  #coord_flip() +
  labs(x = "Concept",
       y = "Normalised Strength Difference (primary - secondary)",
       title = "Top & Bottom 10 Nodes by Normalised Strength Difference Between Stakeholders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 12),
        legend.position = "none")

ggsave(strength.comp.stake.g,
       filename = here('output',paste0('normalised_strength_diff_stakeholder_',ag.mode,'.jpg')),
       width = 8, height = 6)


#calculate difference in normalised strength for shared nodes between different genders
strength.comparison.gender <- inner_join(vertex.attr$men %>% select('name',strength_norm_men = strength_norm),
                                         vertex.attr$women %>% select('name',strength_norm_women = strength_norm),
                                         by = 'name') %>%
  mutate(norm_strength_diff = strength_norm_men - strength_norm_women) %>%
  arrange(desc(norm_strength_diff))

write.csv(strength.comparison.gender,
          here('output',paste0('normalised_strength_difference_gender_',ag.mode,'.csv')),
          row.names = F)

#plot graphically
strength.comparison.gender.g <- strength.comparison.gender %>%
  # Select bottom 10 and top 10
  slice(c(1:10, (n()-9):n())) %>%
  # Plot
  ggplot(aes(x = reorder(name, norm_strength_diff),
             y = norm_strength_diff,
             fill = norm_strength_diff > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "cyan", "FALSE" = "salmon")) +
  #coord_flip() +
  labs(x = "Concept",
       y = "Normalised Strength Difference (men - women)",
       title = "Top & Bottom 10 Nodes by Normalised Strength Difference Between Genders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 12),
        legend.position = "none")

ggsave(strength.comparison.gender.g,
       filename = here('output',paste0('normalised_strength_diff_gender_',ag.mode,'.jpg')),
       width = 8, height = 6)

#### graph metrics ####
graph.indeces <- lapply(fcm.graphs, compute_indices, weight_attr = 'weight') %>% bind_rows(.id = 'graph') %>%
  mutate(n_graphs = case_when(
    graph == 'aggregated' ~ nrow(fcm.metadata),
    graph == 'primary' ~ sum(fcm.metadata$stakeholder == 'primary'),
    graph == 'secondary' ~ sum(fcm.metadata$stakeholder == 'secondary'),
    graph == 'men' ~ sum(fcm.metadata$Gender == 'men'),
    graph == 'women' ~ sum(fcm.metadata$Gender == 'women'),
    graph == 'primary men' ~ sum(fcm.metadata$stakeholder == 'primary' &
                                   fcm.metadata$Gender == 'men'),
    graph == 'primary women' ~ sum(fcm.metadata$stakeholder == 'primary' &
                                     fcm.metadata$Gender == 'women'),
    graph == 'secondary men' ~ sum(fcm.metadata$stakeholder == 'secondary' &
                                     fcm.metadata$Gender == 'men'),
    graph == 'secondary women' ~ sum(fcm.metadata$stakeholder == 'secondary'&
                                       fcm.metadata$Gender == 'women')
    
  ),.after = graph)
# %>%
#   pivot_longer(cols = -graph, names_to = "metric", values_to = "value") %>%
#   pivot_wider(names_from = graph, values_from = value) %>%
#   # #add metrics for aggregated FCM
#   # left_join(compute_indices(fcm.ag.graph, weight_attr = 'weight') %>%
#   #             pivot_longer(cols = colnames(.),names_to = "metric", values_to = "value")) %>%
#   select(metric,all = value,everything())

write.csv(graph.indeces, here('output',paste0('graph_indeces_summary_',ag.mode,'.csv')), row.names = F)

#### top 10 nodes ####
top.nodes.strength <- lapply(
  fcm.graphs,
  top_nodes_by_attribute,
  attribute = "strength",
  top_n = 10
) %>% bind_cols() %>% select_if(is.character) %>% set_names(str_to_title(names(fcm.graphs))) %>%
  bind_cols(tibble(Rank = 1:10), .)

write.csv(top.nodes.strength, here('output',paste0('top_nodes_strength_',ag.mode,'.csv')), row.names = F)

#### strength ratio - drivers, mediators, outcomes ####
#inputs
fcm.inputs <- vertex.attr$aggregated %>% arrange(strength_ratio, desc(strength_out)) %>% slice(1:10)

#drivers
fcm.drivers <- vertex.attr$aggregated %>% arrange(strength_ratio, desc(strength_out)) %>% filter(strength_ratio>0) %>% slice(1:10)

#mediators
fcm.mediators <- vertex.attr$aggregated %>% arrange(strength_ratio, desc(strength_out)) %>% filter(strength_ratio==1)%>% slice(1:10)

#driven nodes
fcm.driven <- vertex.attr$aggregated %>% arrange(desc(strength_ratio), desc(strength_in)) %>% filter(!is.na(strength_ratio)) %>% slice(1:10)

#outcomes
fcm.outcomes <- vertex.attr$aggregated %>% arrange(desc(strength_in)) %>% filter(is.na(strength_ratio)) %>% slice(1:10)

fcm.strength.ratio <- bind_cols(fcm.inputs%>%select(inputs = name),
                                fcm.drivers%>%select(drivers = name),
                                fcm.mediators%>%select(mediators = name),
                                fcm.driven%>%select(driven_nodes = name),
                                fcm.outcomes%>%select(outcomes = name)) %>% remove_rownames()

write.csv(fcm.strength.ratio,
          file = here('output',paste0('strength_ratio_node_classes_',ag.mode,'.csv')),
          row.names = F)

#### jaccard similarity ####
#FCMapper::comp.maps(V(fcm.graphs$primary)$name, V(fcm.graphs$secondary)$name)
#FCMapper::comp.maps(V(fcm.graphs$men)$name, V(fcm.graphs$women)$name)

#Jaccard for all graphs
# initialize an empty matrix
jaccard.matrix <- matrix(NA, nrow = length(fcm.graphs), ncol = length(fcm.graphs),
                         dimnames = list(names(fcm.graphs), names(fcm.graphs)))

# fill the matrix with jaccard results
for (i in seq_len(length(fcm.graphs)-1)) {
  for (j in seq((i+1),length(fcm.graphs))) {
    jaccard <- FCMapper::comp.maps(
      V(fcm.graphs[[i]])$name,
      V(fcm.graphs[[j]])$name
    )
    jaccard.matrix[i, j] <- jaccard$Jaccard
  }
}

#Jaccard for only main dissaggregations (stakeholder and gender)
# initialize an empty matrix
jaccard.matrix <- matrix(NA, nrow = length(fcm.graphs[1:5]), ncol = length(fcm.graphs[1:5]),
                         dimnames = list(names(fcm.graphs[1:5]), names(fcm.graphs[1:5])))

# fill the matrix with jaccard results
for (i in seq_len(length(fcm.graphs[1:5])-1)) {
  for (j in seq((i+1),length(fcm.graphs[1:5]))) {
    jaccard <- FCMapper::comp.maps(
      V(fcm.graphs[[i]])$name,
      V(fcm.graphs[[j]])$name
    )
    jaccard.matrix[i, j] <- jaccard$Jaccard
  }
}

#write to csv
write.csv(jaccard.matrix,
          here('output',paste0('jaccard_similarity_matrix.csv')),
          na = '',
          row.names = T)

## plot heatmap

jaccard.heat.map <-  ggplot(as.data.frame(as.table(jaccard.matrix)), 
                            aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 4, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd")) +
  scale_x_discrete(position = "top") + # move x-axis to top
  scale_y_discrete(limits = rev(levels(as.data.frame(as.table(jaccard.matrix))$Var1))) + #invert y-axis
  labs(title = 'Jaccard Similarity Matrix', x=NULL, y=NULL,
       fill = 'Jaccard\nSimilarity\nScore') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) # vertical labels

ggsave(here('output','jaccard_similarity_heatmap_stakeholder_gender.jpg'),
       jaccard.heat.map,
       width = 6,
       height = 5)

# GED (do it in python on ILRI HPC) ####
# ged_between <- function(g1, g2) {
#   graph_edit_distance(g1, g2,
#                       edge.cost = function(e1, e2) abs(E(g1)[e1]$weight - E(g2)[e2]$weight),
#                       edge.insert.cost = function(e) E(g2)[e]$weight,
#                       edge.delete.cost = function(e) E(g1)[e]$weight,
#                       vertex.cost = function(v1, v2) 0) # no cost for matched nodes
# }
# 
# # Example: GED between first two graphs
# ged_between(fcm.graphs[[1]], fcm.graphs[[2]])
# 
# 
# library(reticulate)
# use_condaenv("base", 
#              conda = "C:/Users/s1512187/AppData/Local/anaconda3/Scripts/conda.exe",
#              required = TRUE)
# py_config()   # check which Python is being used
# nx <- import("networkx")
# py_get_attr(nx, "__version__")
# 
# fcm.graphs.py <- list()
# for (j in seq_along(fcm.edgelist.disag)) {
#   gr <- fcm.edgelist.disag[[j]]
#   py.g <- nx$DiGraph()
#   for (i in 1:nrow(gr)) {
#     py.g$add_edge(gr[[i,1]], gr[[i,2]], weight = gr[[i,8]])
#   }
#   # nm <- names(fcm.edgelist.disag)[j] 
#   # fcm.graphs.py[[nm]] <- py.g
#   fcm.graphs.py[[names(fcm.edgelist.disag)[j]]] <- py.g
# }
# 
# ged <- nx$graph_edit_distance(fcm.graphs.py$`primary male`, fcm.graphs.py$`secondary male`)
# 
# 
# 
# # 
# 
# delta con #################################################################
# 
# # Compute fast belief propagation affinity matrix
# affinity_matrix <- function(g, epsilon = 1e-3) {
#   A <- as_adj(g, sparse = FALSE, attr = "weight")
#   n <- vcount(g)
#   I <- Diagonal(n)
#   D <- Diagonal(x = rowSums(A))
#   # Affinity: (I + epsilon^2 * D - epsilon * A)^(-1)
#   solve(I + epsilon^2 * D - epsilon * A)
# }
# 
# # DeltaCon similarity between two graphs
# delta_con <- function(g1, g2, epsilon = 1e-3) {
#   S1 <- affinity_matrix(g1, epsilon)
#   S2 <- affinity_matrix(g2, epsilon)
#   # Root Euclidean distance
#   dist <- sqrt(sum((sqrt(S1) - sqrt(S2))^2))
#   # Convert to similarity score
#   1 / (1 + dist)
# }
# 
# #### similarity matrices ####
# 
# #for loop to build matrix
# 
# for (i in 1:nrow(similarity.matrix)) {
#   
# }
# 
# similarity.matrix <- 
#   
#   #working
#   edgynode::metric_dist_delta_con(make_adjacency(as_adjacency_matrix(stake.gender.disag.graph[[1]], sparse = F)), 
#                                   make_adjacency(as_adjacency_matrix(stake.gender.disag.graph[[2]], sparse = F)))
# 
# nevada::dist_spectral(fcm.graphs$primary,
#                       fcm.graphs$secondary,
#                       representation = 'adjacency')
# 
# 
#


# plot graphs ####

for (i in seq_along(fcm.graphs)) {
  
  #plot entire network
  g <- ggraph(fcm.graphs[[i]], layout = "fr") +
    geom_edge_fan(
      aes(edge_colour = polarity,
          edge_width  = weight),
      #alpha = 0.8,
      arrow = arrow(angle = 30, length = unit(1.5, "mm"),
                    ends = "last", type = "closed"),
      end_cap = circle(1, "mm")
    ) +
    scale_edge_colour_manual(values = c("#F8766D", "#00BFC4"),
                             labels = c("Negative", "Positive")) +
    scale_edge_width(range = c(0.05, 1), guide = "none") +
    geom_node_point(size = 1, colour = "black", alpha = 0.8) +
    geom_node_text(aes(label = name), repel = T, size = 1.5) +
    labs(edge_width = 'Weight', edge_colour = 'Edge polarity',
         title = paste0('Fuzzy Cognitive Map: ',str_to_title(names(fcm.graphs[i])))) +
    theme_void()
  
  ggsave(plot = g, filename = here('output','graph_plots',paste0(str_replace_all(names(fcm.graphs[i]),' ','_'),'_graph_',ag.mode,'.jpg')),
         height = 300, width = 350, units = 'mm')
  
  
  #histogram of degree
  ggsave(plot = ggplot(as_tibble(degree(fcm.graphs[[i]])), aes(x = value)) +
           geom_histogram(binwidth = 2, fill = "steelblue", color = "black", boundary = 0) +
           labs(title = paste0('Histogram of Degree: ',str_to_title(names(fcm.graphs[i]))),
                x = "Degree",
                y = "Count") +
           theme_minimal(),
         filename = here('output','histograms',paste0(str_replace_all(names(fcm.graphs[i]),' ','_'),'_histogram_degree_',ag.mode,'.jpg')))
  
  #histogram of strength
  ggsave(plot = ggplot(as_tibble(strength(fcm.graphs[[i]])), aes(x = value)) +
           geom_histogram(binwidth = 2, fill = "steelblue", color = "black", boundary = 0) +
           labs(title = paste0('Histogram of Strength: ',str_to_title(names(fcm.graphs[i]))),
                x = "Strength",
                y = "Count") +
           theme_minimal(),
         filename = here('output','histograms',paste0(str_replace_all(names(fcm.graphs[i]),' ','_'),'_histogram_strength_',ag.mode,'.jpg')))
  
  
  #### filtered map
  #induce subgraph with only top 5% of strength
  g.filt <- induced_subgraph(fcm.graphs[[i]], 
                             V(fcm.graphs[[i]])[strength >= quantile(V(fcm.graphs[[i]])$strength, probs = 0.95)])
  
  #normalise the strengths to a defined range for plotting
  V(g.filt)$strength <- normalise(V(g.filt)$strength, to = c(3, 10))
  
  #plot top 5% strength
  g <- ggraph(g.filt, layout = "linear", circular = T) + #layout = "stress"
    geom_edge_fan(
      aes(edge_colour = polarity,
          edge_width  = weight,
          end_cap = circle(node2.strength +2, "pt")),
      #alpha = 0.8,
      arrow = arrow(angle = 30, length = unit(1.5, "mm"),
                    ends = "last", type = "closed"),
      
    ) +
    scale_edge_colour_manual(values = c("#F8766D", "#00BFC4"),
                             labels = c("Negative", "Positive")) +
    scale_edge_width(range = c(0.05, 1.2), guide = "legend") +
    #add an invisible node layer to get the legend
    geom_node_point(aes(size = strength), colour = "black", alpha = 0) +
    #add nodes mapped to the 
    geom_node_point(aes(size = I(strength)), colour = "black") +
    geom_node_text(aes(label = str_to_sentence(name)), repel = T, size = 3, box.padding = unit(0.7,'lines')) +
    labs(edge_width = 'Weight', edge_colour = 'Edge Influence',
         size = 'Strength',
         title = paste0('Top 5% of nodes by strength: ',str_to_title(names(fcm.graphs[i]))), 'FCM') +
    theme_void()
  
  ggsave(plot = g, filename = here('output','graph_plots',paste0(str_replace_all(names(fcm.graphs[i]),' ','_'),'_top5_nodes_strength_',ag.mode,'.jpg')),
         height = 200, width = 220, units = 'mm')
  
  
}


#spherical layout for full aggregated graph
aggregated.graph.sphere <- ggraph(fcm.graphs$aggregated, layout = layout_on_sphere(fcm.graphs$aggregated)) +
  geom_edge_fan(
    aes(edge_colour = polarity,
        edge_width  = weight),
    alpha = 0.8,
    arrow = arrow(angle = 30, length = unit(1.5, "mm"),
                  ends = "last", type = "closed"),
    end_cap = circle(1, "mm")
  ) +
  scale_edge_colour_manual(
    values = c("negative" = "#F8766D", "positive" = "#00BFC4"),
    labels = c("negative" = "Negative", "positive" = "Positive")
  ) +
  scale_edge_width(range = c(0.05, 1), guide = "none") +
  geom_node_point(size = 0.8, colour = "black", alpha = 0.8) +
  geom_node_text(aes(label = name), repel = T, size = 1.5) +
  labs(edge_width = 'Weight', edge_colour = 'Edge polarity',
       title = 'Aggregated Fuzzy Cognitive Map') +
  theme_void()

ggsave(plot = aggregated.graph.sphere, filename = here('output','graph_plots',paste0('aggregated_graph_sphere_',ag.mode,'.jpg')),
       height = 300, width = 350, units = 'mm')


# community clustering ####

#remove cluster graphs - different number of clusters each time so it might not overwrite everything
file.remove(list.files(here('output','cluster_plots'), recursive = T, full.names = T))

# Define the methods you want to iterate over
methods <- list(
  infomap = fcm.infomap$aggregated,
  #louvain = fcm.louv$aggregated,
  leiden = fcm.leiden$aggregated
)

for (method_name in names(methods)) {
  method <- methods[[method_name]]
  
  cat("\n###", method_name, "###\n")
  
  # Community connectivity matrix
  comm.connectivity.matrix <- community_connectivity(
    fcm.graphs$aggregated,
    membership(method)
  )
  
  # Row normalisation (transition probabilities)
  comm.connectivity.matrix.prop <- comm.connectivity.matrix / rowSums(comm.connectivity.matrix)
  comm.connectivity.matrix.prop[is.na(comm.connectivity.matrix.prop)] <- 0 # for clusters with no internal connections where the rowSum = 0
  
  # Symmetrical matrix
  comm.connectivity.matrix.sym <- comm.connectivity.matrix + t(comm.connectivity.matrix)
  #diag(comm.connectivity.matrix.sym) <- diag(comm.connectivity.matrix)
  
  # Normalise symmetrical matrix
  comm.connectivity.matrix.sym.prop <- comm.connectivity.matrix.sym / rowSums(comm.connectivity.matrix.sym)
  
  # Modularity score
  mod.score <- modularity(
    fcm.graphs$aggregated,
    membership = membership(method),
    weights = E(fcm.graphs$aggregated)$weight
  )
  
  #### name clusters based on strongest nodes in cluster
  
  cluster.names <- vertex.attr$aggregated %>%# arrange(cluster_leiden, desc(strength)) %>%
    group_by(!!sym(paste0('cluster_',method_name))) %>%
    #summarise(n=n())
    slice_max(order_by = strength, n=3) %>%
    summarise(labels = paste(name, collapse = ", ")) %>%
    rename('cluster' = !!sym(paste0('cluster_',method_name)))
  
  write.csv(cluster.names,
            here('output','cluster_plots',method_name,paste0('cluster_labels_',method_name,'.csv')),
            row.names = F)  
  

  # Print results
  cat("Modularity score for", method_name, ":", mod.score, "\n")
  
  #### heatmap the clusters ####
  heatmap.matrices <- list(comm.connectivity.matrix,
                           comm.connectivity.matrix.prop,
                           comm.connectivity.matrix.sym,
                           comm.connectivity.matrix.sym.prop)
  titles <- c('Community Connectivity Heatmap',
              'Normalised Community Connectivity Heatmap',
              'Undirected Community Connectivity Heatmap',
              'Normalised Undirected Community Connectivity Heatmap')
  
  #create and print heatmaps
  for (mat in seq_along(heatmap.matrices)) {
    df <- melt(heatmap.matrices[[mat]])
    colnames(df) <- c("Community_i", "Community_j", "Value")
    
    heat.map <-  ggplot(df, aes(x = Community_j, y = Community_i, fill = Value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Value, 2)), size = ifelse(method_name == 'infomap',1,5), color = "black") +
      scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd")) +
      scale_x_discrete(position = "top") + # move x-axis to top
      scale_y_discrete(limits = rev(levels(df$Community_i))) + #invert y-axis
      labs(title = paste(titles[mat],':',str_to_title(method_name)), x = "Community j", y = "Community i") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) # vertical labels
    
    ggsave(here('output','heatmaps',method_name,paste0(tolower(str_replace_all(titles[mat],' ','_')),'_',method_name,'.jpg')),
           heat.map,
           width = 8,
           height = 5)
  }
  
  ## plot clusters ####
  
  # Loop over each cluster ID in this method
  membership_vec <- vertex.attributes(fcm.graphs$aggregated)[[paste0('cluster_',method_name)]]
  
  for (clust in seq_along(unique(membership_vec))) {
    
    graph.clust <- induced_subgraph(
      fcm.graphs$aggregated,
      V(fcm.graphs$aggregated)[membership_vec == clust]
    )
    
    # scale strength to control end point of arrows
    V(graph.clust)$strength <- normalise(V(graph.clust)$strength, to = c(3, 10))
    
    #plot top 5% strength
    gg.clust <- ggraph(graph.clust, layout = "linear", circular = T) + #layout = "stress"
      geom_edge_fan(
        aes(edge_colour = polarity,
            edge_width  = weight,
            end_cap = circle(node2.strength +2, "pt")),
        #alpha = 0.8,
        arrow = arrow(angle = 30, length = unit(1.5, "mm"),
                      ends = "last", type = "closed"),
        
      ) +
      scale_edge_colour_manual(values = c("#F8766D", "#00BFC4"),
                               labels = c("Negative", "Positive")) +
      scale_edge_width(range = c(0.05, 1.5), guide = "legend") +
      #add an invisible node layer to get the legend
      geom_node_point(aes(size = strength), colour = "black", alpha = 0) +
      #add nodes mapped to the 
      geom_node_point(aes(size = I(strength)), colour = "black") +
      geom_node_text(aes(label = str_to_sentence(name)), repel = T, size = 3, box.padding = unit(0.7,'lines')) +
      labs(edge_width = 'Weight',
           edge_colour = 'Edge Influence',
           size = 'Strength',
           title = paste0('Cluster ', clust, ' by ', str_to_title(method_name), ' Algorithm')) +
      theme_void()
    
    
    ggsave(
      plot = gg.clust,
      filename = here("output", "cluster_plots",method_name,
                      paste0("aggregated_graph_", method_name, "_cluster_", clust, ".jpg")),
      height = 300, width = 350, units = "mm" 
    )
  }
  
}

## explore participant coefficient & bridging nodes ####

#to do




# #reassign small clusters to reduce the total number of clusters - doesn't reduce number of clusters
# reduced.members <- merge_small_clusters1(fcm.ag.graph,
#                                          membership = membership(fcm.ag.infomap),
#                                          min_size = 5)
# hist(reduced.members)
# hist(membership(fcm.ag.infomap))
# 
# fcm.ag.infomap$codelength
# 
# ## OR just take first 15 clusters?????

# ####add tags to each cluster - topic modelling (LDA) for cluster naming####
# 
# #create list of cluster vertex names
# cluster.text <- split(names(membership(fcm.leiden$aggregated)), membership(fcm.leiden$aggregated))
# 
# # 2) Build a corpus and Document-Term Matrix (DTM) per cluster
# make_dtm <- function(texts) {
#   corp <- VCorpus(VectorSource(texts))
#   corp <- tm_map(corp, removePunctuation)
#   #corp <- tm_map(corp, removeNumbers)
#   corp <- tm_map(corp, removeWords, stopwords("english"))
#   #corp <- tm_map(corp, stripWhitespace)
#   DocumentTermMatrix(corp, control = list(wordLengths = c(3, Inf)))
# }
# 
# dtms <- lapply(cluster.text, make_dtm)
# 
# #Fit LDA per cluster (choose K topics; often 1–3 for naming)
# fit_lda <- function(dtm, k = 2) {
#   # Remove empty rows to avoid errors
#   dtm <- dtm[row_sums(dtm) > 0, ]
#   LDA(dtm, k = k, method = "Gibbs",
#       control = list(seed = 1234, burnin = 1000, iter = 2000, thin = 100))
# }
# 
# # Fit LDA models for each cluster 
# lda_models <- lapply(dtms, fit_lda, k = 2) 
# # Extract top terms per topic (fixed version) 
# topic_terms <- lapply(lda_models, function(model) terms(model, 3)) 
# # Format cluster tags (take first topic’s top words as a tag) 
# cluster_tags <- lapply(topic_terms, function(tt) { 
#   apply(tt, 2, paste, collapse = ", ")[1] 
# }) 
# # Show suggested tags 
# cluster.tags <-  data.frame(values = unlist(cluster_tags)) %>% mutate(cluster = row_number()) %>%
#   rename(tags = values) %>% remove_rownames()
# 
# #### name clusters based on strongest nodes in cluster
# 
# clusters.leiden <- vertex.attr$aggregated %>%# arrange(cluster_leiden, desc(strength)) %>%
#   group_by(cluster_leiden) %>%
#   slice_max(order_by = strength, n=3) %>%
#   summarise(labels = paste(name, collapse = ", ")) %>%
#   rename(cluster = cluster_leiden)
# 
# write.csv(clusters.leiden,
#           here('output','labelled_leiden_clusters.csv'),
#           row.names = F)  

#One Health sub graphs ####

# one health clusters comparison
one.health.edgelists <- list()
one.health.graphs <- list()

for (sector in c('human health','livestock health','environmental health')) {
  #filter edgelists for subgraphs
  one.health.edgelists[[sector]] <- subgraph_nodes_from_edgelist(fcm.edgelists$aggregated, sector, degrees = 2)
  
  #create subgraph
  one.health.graphs[[sector]] <- induced_subgraph(fcm.graphs$aggregated,
                                                  V(fcm.graphs$aggregated)$name %in% one.health.edgelists[[sector]])
  
  
  #write edgelists to csv for GED calculation using python
  write.csv(as_data_frame(one.health.graphs[[sector]], what = "edges"),
            here('output','one_health_subgraphs',paste0('one_health_',str_replace_all(sector,' ','_'),'.csv')),
            row.names = F,
            na='')
}

#Jaccard for One Health subgraphs graphs
# initialize an empty matrix
jaccard.matrix.onehealth <- matrix(NA, nrow = length(one.health.edgelists), ncol = length(one.health.edgelists),
                                   dimnames = list(names(one.health.edgelists), names(one.health.edgelists)))

# fill the matrix with jaccard results
for (i in seq_len(length(one.health.graphs)-1)) {
  for (j in seq((i+1),length(one.health.graphs))) {
    jaccard <- FCMapper::comp.maps(
      V(one.health.graphs[[i]])$name,
      V(one.health.graphs[[j]])$name
    )
    jaccard.matrix.onehealth[i, j] <- jaccard$Jaccard
  }
}

#jaccard results as table
jaccard.onehealth.table <- as.table(jaccard.matrix.onehealth) %>% as.data.frame() %>% filter(!is.na(Freq)) %>%
  rename('Graph 1'=Var1, 'Graph 2'=Var2, 'Jaccard Similarity Score'=Freq)

## plot heatmap

jaccard.onehealth.heat.map <-  ggplot(as.data.frame(as.table(jaccard.matrix.onehealth)), 
                            aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 4, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd")) +
  scale_x_discrete(position = "top",# move x-axis to top
                   labels = function(x) str_wrap(str_to_title(x), width = 11)) + 
  scale_y_discrete(limits = rev(levels(as.data.frame(as.table(jaccard.matrix.onehealth))$Var1)),#invert y-axis
                   labels = function(x) str_wrap(str_to_title(x), width = 11)) + 
  labs(title = 'Jaccard Similarity Matrix', x=NULL, y=NULL,
       fill = 'Jaccard\nSimilarity\nScore') +
  theme_minimal() + 
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) # vertical labels

ggsave(here('output','jaccard_similarity_heatmap_one_health.jpg'),
       jaccard.onehealth.heat.map,
       width = 6,
       height = 5)

# FCM modelling ####
# Run 10000 random restarts
# run_fcm_ensemble2() recognizes non-converging runs and removes them from the final output so clusters of converged runs can be better identified
ensemble <- run_fcm_ensemble2(
  graph       = fcm.graphs$aggregated,
  n_runs      = 10000,
  seed        = 42,
  transfer_fn = "sigmoid",
  lambda      = 1,
  max_iter    = 100,
  threshold   = 1e-6
)


## basins of attraction ####
# Check if there are multiple attractors
detect_attractors2(ensemble, max_k = 5)

# Elbow / silhouette plot to suggest number of clusters
p <- fviz_nbclust(ensemble$final_states, kmeans, method = "silhouette",
                  k.max = min(max_k, nrow(ensemble$final_states) - 1)) +
  labs(title = "Silhouette Analysis — How Many Attractors?") +
  theme_minimal(base_size = 12)

print(p)

# Fit k=2
km2 <- kmeans(ensemble$final_states, centers = 2, nstart = 25)
table(km2$cluster)
# Fit k=3
km3 <- kmeans(ensemble$final_states, centers = 3, nstart = 25)
table(km3$cluster)

# Full attractor profiles (nodes as rows, attractors as columns)
attractor_profiles <- as.data.frame(t(km2$centers))
names(attractor_profiles) <- paste0("Attractor_", 1:2)
attractor_profiles$Node <- rownames(attractor_profiles)

# Which nodes differ most across the 2 attractors?
attractor_profiles$Max_Range <- apply(
  attractor_profiles[, 1:2], 1, function(x) max(x) - min(x)
)

# Compare the two attractor profiles - values are the average final activation for that node, within that attractor
#attractor_profiles <- as.data.frame(t(km2$centers))
#names(attractor_profiles) <- c("Attractor_1", "Attractor_2")
#attractor_profiles$Node <- rownames(attractor_profiles)
attractor_profiles$Difference <- round(
  attractor_profiles$Attractor_1 - 
    attractor_profiles$Attractor_2, 4
)

# top 20 nodes that differ most in their final activations between attractors
attractor.profile.table <- head(attractor_profiles[
  order(abs(attractor_profiles$Difference), decreasing = TRUE), 
], 20) %>%
  select(Node, Attractor_1, Attractor_2, Difference)

write.csv(attractor.profile.table,
          here('output','system_modelling','attractor_profile_topnodes.csv'),
          row.names = F)

## Explore initial conditions that lead to the two different attractors

# Identify which runs went to which attractor
attractor_1_cluster <- which.max(table(km2$cluster))
attractor_2_cluster <- which.min(table(km2$cluster))
is_attractor_2      <- km2$cluster == attractor_2_cluster

# Split initial states into two groups
initial_attractor_1 <- ensemble$initial_states[!is_attractor_2, ]
initial_attractor_2 <- ensemble$initial_states[is_attractor_2, ]

message(sprintf("Attractor 1: %d runs | Attractor 2: %d runs",
                nrow(initial_attractor_1),
                nrow(initial_attractor_2)))

# Compare mean initial activation for every node
initial_comparison <- data.frame(
  Node        = V(fcm.graphs$aggregated)$name,
  Attractor_1 = round(colMeans(initial_attractor_1), 4),
  Attractor_2 = round(colMeans(initial_attractor_2), 4),
  Difference  = round(colMeans(initial_attractor_2) - 
                        colMeans(initial_attractor_1), 4)
)

# Top 20 nodes with largest difference in starting values
top_nodes <- initial_comparison$Node[
  order(abs(initial_comparison$Difference), decreasing = TRUE)
][1:20]

# Build a long format dataframe of initial states for these nodes
att1_df            <- as.data.frame(initial_attractor_1[, top_nodes])
att1_df$Attractor  <- sprintf("Attractor 1 (n=%d)", nrow(initial_attractor_1))

att2_df            <- as.data.frame(initial_attractor_2[, top_nodes])
att2_df$Attractor  <- sprintf("Attractor 2 (n=%d)", nrow(initial_attractor_2))

combined <- rbind(att1_df, att2_df)

long <- pivot_longer(combined,
                     cols      = -Attractor,
                     names_to  = "Node",
                     values_to = "Initial_Activation")

# Labels for plot (generated dynamically from actual n)
att1_label <- sprintf("Attractor 1 (n=%d)", nrow(initial_attractor_1))
att2_label <- sprintf("Attractor 2 (n=%d)", nrow(initial_attractor_2))

# Box plot — shows individual points for the small attractor 2 group
initial.activations.cluster.plot <- ggplot(long, aes(x = Attractor, y = Initial_Activation,
                 fill = Attractor)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  # geom_jitter(data  = long[long$Attractor == att2_label, ],
  #             width = 0.1, size = 2, colour = "#d62728") +
  scale_fill_manual(values = setNames(
    c("#F8766D", "#00BFC4"),
    c(att1_label, att2_label)
  )) +
  facet_wrap(~Node, scales = "free_y",
             labeller = labeller(Node = label_wrap_gen(width = 15))) +  # <-- change 1
  labs(title    = "Initial Activation by Attractor — Top 20 Nodes",
       subtitle = "Red dots = individual Attractor 2 runs",
       x        = NULL,
       y        = "Initial Activation") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        axis.text.x     = element_blank(),
        strip.text      = element_text(size = 8))  # <-- change 2

ggsave(initial.activations.cluster.plot,
       file = here('output','system_modelling','initial_activation_cluster_difference.jpeg'))



## stability of nodes ####
# Stability overview (all 450 nodes)
plot_stability_overview(ensemble, n_label = 20)

# Density plots — 6 most and 6 least stable
plot_node_distributions(ensemble, n_each = 6)

# Or specify nodes of interest directly
plot_node_distributions(ensemble,
                        nodes = c('human health','livestock health','livestock disease','grazing','milk availability','schooling'))

# Inspect the summary table for least stable nodes
head(ensemble$node_summary[order(ensemble$node_summary$SD,
                                 decreasing = TRUE), ], 20)



## non-converging runs ####
# Basic summary
n_total       <- ensemble$n_runs
n_converged   <- ensemble$n_converged
n_failed      <- n_total - n_converged

message(sprintf(
  "%d of %d runs did not converge (%.1f%%)",
  n_failed, n_total, n_failed / n_total * 100
))

# get index of runs that did not converge
failed_idx <- which(!ensemble$did_converge)

# Extract their initial states
initial_failed <- ensemble$initial_states_all[failed_idx, ] %>%
  summarise(across(everything(), ~mean(.x, na.rm=T))) %>% t() %>% as.data.frame() %>%
  mutate(node = row.names(.)) %>% select(node, mean_activation = V1)


# FCM scenario testing ####


# Baseline: all nodes at moderate activation
nodes    <- V(fcm.graphs$aggregated)$name
baseline <- setNames(rep(0.5, length(nodes)), nodes)

# Scenario: increase Economy activation from 0.5 to 0.9
result <- run_scenario(
  graph            = fcm.graphs$aggregated,
  baseline_state   = baseline,
  scenario_changes = c(`drought` = 1),
  clamp            = T,          # hold scenario_changes at the same level through iterations 
  transfer_fn      = "sigmoid",
  lambda           = 1
)

# Inspect results
scenario.delta <- result$comparison

# Visualise
plot_scenario_delta(result, title = "Effect of High Economy Activation")
plot_trajectories(result$scenario, title = "Scenario Convergence")







################### END #####################################################








# ## aggregated map ####
# #### create aggregated edgelist for combined FCM ####
# #add polarity and absolute weights
# fcm.edgelist.ag <- fcm.edgelist.all %>%
#   group_by(from,to) %>%
#   summarise(weight_mean = mean(weight, na.rm =T),
#             weight_sum = sum(weight, na.rm = T),
#             sd_weight = sd(weight, na.rm = T),
#             frequency = sum(weight != 0, na.rm = T)) %>%
#   mutate(polarity = case_when(
#     ag.mode == 'mean' & weight_mean < 0 ~ 'negative',
#     ag.mode == 'mean' & weight_mean > 0 ~ 'positive',
#     ag.mode == 'sum'  & weight_sum  < 0 ~ 'negative',
#     ag.mode == 'sum'  & weight_sum  > 0 ~ 'positive',
#     TRUE ~ NA_character_
#   ),
#   weight = case_when(
#     ag.mode == 'mean' ~ abs(weight_mean),
#     ag.mode == 'sum'  ~ abs(weight_sum)
#   )
#   )
# 
# write.csv(fcm.edgelist.ag, here('output',paste0('FCM_edgelist_aggregated_',ag.mode,'.csv')), row.names = F)
# 
# #check for edges that negate each other when summed or averaged
# #should be empty
# edgelist.cancelling <- fcm.edgelist.all %>% inner_join(fcm.edgelist.ag %>% ifelse(ag.mode = 'mean',
#                                                                                   filter(weight_mean ==0 & sd_weight !=0),
#                                                                                   filter(weight_sum ==0)),
#                                                        by = c('from','to')) %>%
#   arrange(from,to)
# 
# 
# #### create aggregated graph ####
# #create graph object
# fcm.ag <- graph_from_data_frame(fcm.edgelist.ag,
#                                 directed = T) %>%
#   add_vertex_attributes()
# 
# fcm.ag.graph <- fcm.ag$graph
# fcm.ag.infomap <- fcm.ag$infomap
# 
# #### plot aggregated graph ####
# ### remove 0 weighted edges for now - to remove once edges are corrected
# #fcm.ag.graph <- delete_edges(fcm.ag.graph, E(fcm.ag.graph)[weight == 0])
# 
# #drl layout 
# aggregated.graph.drl <- ggraph(fcm.ag.graph, layout = "drl") +
#   geom_edge_fan(
#     aes(edge_colour = polarity,
#         edge_width  = weight),
#     #alpha = 0.8,
#     arrow = arrow(angle = 30, length = unit(1.5, "mm"),
#                   ends = "last", type = "closed"),
#     end_cap = circle(1, "mm")
#   ) +
#   scale_edge_colour_manual(
#     values = c("negative" = "#F8766D", "positive" = "#00BFC4"),
#     labels = c("negative" = "Negative", "positive" = "Positive")
#   ) +
#   scale_edge_width(range = c(0.05, 1), guide = "none") +
#   geom_node_point(size = 1, colour = "black", alpha = 0.8) +
#   geom_node_text(aes(label = name), repel = T, size = 1.5) +
#   labs(edge_width = 'Weight', edge_colour = 'Edge polarity',
#        title = 'Aggregated Fuzzy Cognitive Map') +
#   theme_void()
# 
# ggsave(plot = aggregated.graph.drl, filename = here('output',paste0('aggregated_graph_',ag.mode,'.jpg')),
#        height = 300, width = 350, units = 'mm')
# 
# #spherical layout
# aggregated.graph.sphere <- ggraph(fcm.ag.graph, layout = layout_on_sphere(fcm.ag.graph)) +
#   geom_edge_fan(
#     aes(edge_colour = polarity,
#         edge_width  = weight),
#     alpha = 0.8,
#     arrow = arrow(angle = 30, length = unit(1.5, "mm"),
#                   ends = "last", type = "closed"),
#     end_cap = circle(1, "mm")
#   ) +
#   scale_edge_colour_manual(
#     values = c("negative" = "#F8766D", "positive" = "#00BFC4"),
#     labels = c("negative" = "Negative", "positive" = "Positive")
#   ) +
#   scale_edge_width(range = c(0.05, 1), guide = "none") +
#   geom_node_point(size = 0.8, colour = "black", alpha = 0.8) +
#   geom_node_text(aes(label = name), repel = T, size = 1.5) +
#   labs(edge_width = 'Weight', edge_colour = 'Edge polarity',
#        title = 'Aggregated Fuzzy Cognitive Map') +
#   theme_void()
# 
# ggsave(plot = aggregated.graph.sphere, filename = here('output',paste0('aggregated_graph_sphere_',ag.mode,'.jpg')),
#        height = 300, width = 350, units = 'mm')
# 
# #circular layout
# combined.graph.circle <- ggraph(fcm.ag.graph, layout = 'circle') +
#   geom_edge_fan(
#     aes(edge_colour = polarity,
#         edge_width  = weight),
#     alpha = 0.8,
#     arrow = arrow(angle = 30, length = unit(1.5, "mm"),
#                   ends = "last", type = "closed"),
#     end_cap = circle(1, "mm")
#   ) +
#   scale_edge_colour_manual(
#     values = c("negative" = "#F8766D", "positive" = "#00BFC4"),
#     labels = c("negative" = "Negative", "positive" = "Positive")
#   ) +
#   scale_edge_width(range = c(0.05, 1), guide = "none") +
#   geom_node_point(size = 0.8, colour = "black", alpha = 0.8) +
#   geom_node_text(aes(label = name), repel = T, size = 1.5) +
#   labs(edge_width = 'Weight', edge_colour = 'Edge polarity',
#        title = 'Aggregated Fuzzy Cognitive Map') +
#   theme_void()
# 
# ggsave(plot = combined.graph.circle, filename = here('output',paste0('aggregated_graph_circular_',ag.mode,'.jpg')),
#        height = 300, width = 350, units = 'mm')
# 
