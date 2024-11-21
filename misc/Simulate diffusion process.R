#install.packages("igraph")
#install.packages("MASS")
library(igraph)
library(animation)
library(igraph)
library(MASS)

# Step 1: initialize network
set.seed(42) #reproduction
num_nodes <- 25  # Number of nodes in the network

# Generate a random corr matrix
cormatrix <- matrix(runif(num_nodes^2, min = -1, max = 1), num_nodes) #consider negative association later
cormatrix[lower.tri(cormatrix)] <- t(cormatrix)[lower.tri(cormatrix)] #Make symmetric
diag(cormatrix) <- 0 #set diag as 0

# transfer corr matrix to adjacency matrix
threshold <- 0.7 # Only keep edges with a correlation > 0.7 based on the correlation cut-off
adj_matrix <- abs(cormatrix) > threshold
adj_matrix[upper.tri(adj_matrix)] <- FALSE

#initial weights
initial_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)
edge_weights <- cormatrix[adj_matrix]

#define the parameters of the network layout
fixed_layout <- layout_with_graphopt(initial_graph)
E(initial_graph)$weight <- edge_weights
E(initial_graph)$width <- abs(E(initial_graph)$weight) * 4
E(initial_graph)$color <- ifelse(E(initial_graph)$weight > 0,  "#000080", "#B22222")
V(initial_graph)$size  <- 20
V(initial_graph)$color <- "gray"
V(initial_graph)$label.color <- "black"
#plot(initial_graph, layout = fixed_layout)

# step 2: define the number of nodes in each phase based on diffusion of innovation theory
n_innovators <- round(num_nodes*0.025)
n_early_adopters <- round(num_nodes*0.135)
n_early_majority <- round(num_nodes*0.34)
n_late_majority <- round(num_nodes*0.34)
n_laggards <-  round(num_nodes*0.16)
numer_phases <- c(n_innovators, n_early_adopters,n_early_majority, n_late_majority, n_laggards)

#Step3: simulate the diffusion process

#calculate weights of each node
node_strength <- strength(initial_graph, vids = V(initial_graph), mode = "all", weights = E(initial_graph)$weight)

simulate_diffusion <- function(initial_graph, total_phase, node_strength, numer_phases, node_colors, phase_colors){
  total_phase <- total_phase
  top_indices <- NaN
  used_nodes <- list()
  adoption_history <- list()
  
  for (i in 1:total_phase){
    num_phase <- numer_phases[i]
    
    if (i == 1){
      top_indices <- order(node_strength, decreasing = TRUE)[1:num_phase]
      
    }else{
      connected_nodes <- unique(unlist(lapply(top_indices, function(x) {
        c(neighbors(initial_graph, x, mode = "all"))
      })))
      
      #print(connected_nodes)
      #remove the used nodes
      if (any((used_nodes) %in% connected_nodes)){
        connected_nodes <- setdiff(connected_nodes, used_nodes)
      }else{
        connected_nodes <- connected_nodes
      }
      weights_connected_nodes <- node_strength[connected_nodes]
      medium <- order(weights_connected_nodes, decreasing = TRUE)[1:num_phase]
      top_indices <-connected_nodes[medium]
      #print(top_indices)
    }
    # Save the adoption picture
    top_indices <- top_indices
    used_nodes <- unlist(append(used_nodes, top_indices))
    adoption_history[i] <- list(top_indices)
  }

  return(adoption_history)
}

total_phase <- 5
adoption_history <- simulate_diffusion(initial_graph, total_phase, node_strength, numer_phases, node_colors, phase_colors)
adoption_history

# Step 4: create GIF changing the color of the nodes
#initial and phase color
node_colors <- rep("gray", vcount(initial_graph))
phase_colors <- c("#AA77E9","#F7A24F","#FBEB66", "#4EA660", "#5292F7")

phases <- c("Innovators", "Early Adopters", "Early Majority", "Late Majority", "Laggards")

saveGIF({
  for (i in 1:total_phase){
    color_phase <- phase_colors[i]
    top_indices <- unlist(adoption_history[i])
    phase <- phases[i]
    node_colors[top_indices] <- color_phase  # Change the color of other nodes with different color
    
    plot(initial_graph, 
         vertex.color = node_colors,
         layout = fixed_layout,
         vertex.frame.color = "gray",
         main = paste("Technology Diffusion - Phase:", phase))
    legend("topright", legend = phases, col = phase_colors, pch = 19, bty = "n")
  }
}, movie.name = "technology_diffusion_revised_20_nodes.gif", interval = 2, ani.width = 800, ani.height = 600)

