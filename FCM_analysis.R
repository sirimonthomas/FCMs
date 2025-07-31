#FCM network analysis
#Sirimon Thomas
#20/07/2025

pacman::p_load(
  tidyverse,
  here,
  igraph,
  ggnetwork,
  readxl,
  FCMapper
)

source(here('functions.R'))

#load FCMs
fcm <- list()
fcm.graph <- list()
fcm.adj.matrix <- list()
concepts.all <- character()
concepts.cumulative <- numeric()
for (file in list.files(here('input'), pattern = '.xlsx', full.names = F)) {
  obj.name <- paste0('fcm_',str_sub(file, start = 1, end = 2))
  #import edge list
  fcm[[obj.name]] <- read_excel(here('input',file), sheet = 'edge_list')
  
  #create graph object
  fcm.graph[[obj.name]] <- graph_from_data_frame(fcm[[obj.name]],
                                                 directed = T)
  #create adjacency matrices
  fcm.adj.matrix[[obj.name]] <- as_adjacency_matrix(fcm.graph[[obj.name]],
                                                    attr = 'weight',
                                                    names = T,
                                                    sparse = T)
  
  write.csv(as.matrix(fcm.adj.matrix[[obj.name]]), here('output',paste0(obj.name,'_adjacency_matrix.csv')),
            row.names = T)
  
  #for calculating cumulative number of concepts
  concepts.all <- c(concepts.all,colnames(fcm.adj.matrix[[obj.name]])) %>% unique()
  #concepts.all <- c(concepts.all,V(fcm.graph[[obj.name]])$name) %>% unique()
  
  concepts.cumulative[obj.name] <- length(concepts.all)
  names(concepts.cumulative) <- 1:length(concepts.cumulative)
}

#plot cumulative concepts to check for saturation (leveling off)
plot(1:length(fcm),concepts.cumulative,
     xlab = 'FCM number',
     ylab = 'Cumulative Number of Unique Concepts')


#aggregate all FCMs
combined.adj.mat <- average_adjacency_matrices(fcm.adj.matrix)

#create igraph
combined.fcm <- graph_from_adjacency_matrix(combine)

#basic plot
plot(combined_fcm, edge.label = E(combined_fcm)$weight)

  
  