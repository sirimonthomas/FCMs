#FCM network analysis
#Sirimon Thomas
#20/07/2025

pacman::p_load(
  tidyverse,
  here,
  igraph,
  ggnetwork,
  readxl
)

#load FCMs
fcm <- list()
fcm.graph <- list()
fcm.adj.matrix <- list()
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
}

#plot
#basic plot
plot(fcm.graph$fcm_02, edge.label = E(fcm.graph$fcm_02)$weight)

#aggregate all FCMs
fcm$all <- 
  