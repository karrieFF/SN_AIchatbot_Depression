
library(doipkg)
library(igraph) #use to create graph
library(animation) #use to create GIF file

#parameters
num_agents <- 100
method <- c("counts",  "closeness") #"betweeness",
stages <- c(1, 2, 3, 4, 5) # five stages
stages_name <- c("stage1", "stage2", "stage3", "stage4", "stage5")
p_prior <- 0.45
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16) #probability of adoption at each stage based on DOI
adoption_efficacy <- c(1500, 1200, 900, 600, 300) #unit: step
non_adoption_efficacy <- 250 #unit：step, increase only 250, how to set this
n_simulations <- 10

####################################
#---------------------simulations
#################################

n_counts <- 0
n_closeness <- 0

for (i in 1:n_simulations){
  #---------create network data
  data <- doipkg::generate_network_matrices(num_agents)

  original_data <- data$original_data
  dajacency_matrix <- data$adjacency_matrix
  similarity_matrix <- data$similarity_matrix
  final_matrix <- data$final_matrix

  #-------------------Compare the results from two approaches
  compare_table <- data.frame(counts = numeric(num_agents), closeness = numeric(num_agents))

  for (i in 1:2){
    approach <- method[i]
    original_data[ ,approach] <- NaN #initial a new column
    output <- doipkg::detect_adopters(
      num_agents,
      adj_matrix,
      final_matrix,
      original_data,
      stages,
      stages_name,
      ps_theory,
      adoption_efficacy,
      non_adoption_efficacy,
      approach,
      p_prior
    )
    compare_table[i] <- output$original_data[,approach]
  }

  #-----------calculate the probability of each approach larger than another approach
  largest_counts <- apply(compare_table, 1, function(row) row["counts"] > row["closeness"])
  largest_closeness <- apply(compare_table, 1, function(row) row["closeness"] > row["counts"])

  prob_counts <- mean(largest_counts)
  prob_closeness <- mean(largest_closeness)
  all_probabilities <- c(prob_counts,prob_closeness)

  if (max(all_probabilities) == prob_counts) {
    n_counts <- n_counts + 1
  } else if (max(all_probabilities) == prob_closeness) {
    n_closeness <- n_closeness + 1
  }

}

#The final probability have higher
final_pro_counts <- n_counts/n_simulations
final_pro_close <- n_closeness/n_simulations

########################################################
#---------------Step 6. Visualize the diffusion process
#######################################################

data <- doipkg::generate_network_matrices(num_agents)

original_data <- data$original_data
dajacency_matrix <- data$adjacency_matrix
similarity_matrix <- data$similarity_matrix
final_matrix <- data$final_matrix

#select closeness
approach <- "closeness"
original_data[ ,approach] <- NaN #initial a new column
output <- doipkg::detect_adopters(
  num_agents,
  adj_matrix,
  final_matrix,
  original_data,
  stages,
  stages_name,
  ps_theory,
  adoption_efficacy,
  non_adoption_efficacy,
  approach,
  p_prior
)

final_data <- output$original_data

cormatrix <- 1 / standardized_distance_matrix  # Convert to similarity matrix
cormatrix[lower.tri(cormatrix)] <- t(cormatrix)[lower.tri(cormatrix)]  # Make symmetric
diag(cormatrix) <- 0  # Set diagonal to 0

# Ensure adjacency matrix is undirected
adj_matrix[upper.tri(adj_matrix)] <- FALSE  # Remove upper triangle to ensure symmetry
edge_weights <- cormatrix[adj_matrix]  # Extract edge weights

# Create the graph object
initial_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Assign edge weights
E(initial_graph)$weight <- edge_weights

# Define the layout
fixed_layout <- layout_with_graphopt(initial_graph)  # Repulsion layout for better spacing

# Set edge properties
E(initial_graph)$width <- abs(E(initial_graph)$weight) * 3  # Scale edge width
E(initial_graph)$color <- "#BBBCB6"  # Set edge color (gray)

# Set vertex properties
V(initial_graph)$size <- 10  # Node size
V(initial_graph)$label.color <- "black"  # Label color
V(initial_graph)$label.cex <- 0.7  # Label font size
V(initial_graph)$label.dist <- 0  # Label distance

# Normalize data for vertex colors
base_pa <- final_data$closeness
normalized_base_pa <- (base_pa - min(base_pa)) / (max(base_pa) - min(base_pa))  # Min-max normalization

# Create gradient red colors based on normalized data
gradient_red <- rgb(1, 1 - normalized_base_pa, 1 - normalized_base_pa)  # Gradient red (higher values = more intense red)

follow_up_pa <-  final_data$closeness
normalized_follow_pa <- (follow_up_pa - min(follow_up_pa)) / (max(follow_up_pa) - min(follow_up_pa))  # Min-max normalization

# Create gradient red colors based on normalized data
gradient_follow_red <- rgb(1, 1 - normalized_follow_pa, 1 - normalized_follow_pa)  # Gradient red (higher values = more intense red)

# Define pie chart colors for each node
pie_colors <- mapply(function(red, follow_red) {
  c(red, follow_red)  # Left is gradient red (baseline), right is gradient follow-up red
}, gradient_red, gradient_follow_red, SIMPLIFY = FALSE)

V(initial_graph)$pie <- list(rep(1, 2))  # Equal proportions for pie slices
V(initial_graph)$pie.color <- pie_colors  # Assign pie colors dynamically

# Set additional vertex properties
V(initial_graph)$frame.color <- "white"  # Node border color

# Remove plot margins
par(mar = c(0, 0, 0, 0))

# Plot the graph with pie chart nodes
plot(
  initial_graph,
  layout = fixed_layout,
  vertex.shape = "pie",  # Use pie charts as node shapes
  vertex.size = V(initial_graph)$size,  # Node size
  vertex.label = V(initial_graph)$label,  # Node labels
  vertex.label.cex = V(initial_graph)$label.cex,  # Label font size
  vertex.label.dist = V(initial_graph)$label.dist  # Label distance
)

############################################
#-------Step 7. Simulate diffusion process with the increase in pA
###########################################

# Define node frame colors and phase colors
node_frame_colors <- rep("white", vcount(initial_graph))  # Default frame color
node_size <- rep(10, vcount(initial_graph))

phase_colors <- c("#FBEB66", "#EF8A43", "#7F00FF", "#4EA660", "#193E8F")  # Phase colors
phases <- c("Innovators", "Early Adopters", "Early Majority", "Late Majority", "Laggards")  # Phase names

# Get all stages and their names
all_stages <- output$adoption_lst  # List of nodes for each phase
stages_name <- names(all_stages)   # Phase names (keys in the list)
total_stages <- length(stages_name)

#get the change efficacy
all_stages_efficacy <- output$update_efficacy

# Save the animation as a GIF
saveGIF({
  for (i in 1:total_stages) {
    # Get the current phase and its corresponding nodes
    phase <- stages_name[i]
    top_indices <- all_stages[[phase]]

    # Update node frame colors for the current phase
    node_frame_colors[top_indices] <- phase_colors[i]
    node_size[top_indices] <- 15

    # Plot the graph for the current phase
    plot(
      initial_graph,
      vertex.shape = "pie",
      layout = fixed_layout,                  # Fixed layout for consistency
      vertex.frame.color = node_frame_colors, # Use frame colors dynamically
      vertex.label.color = "black",           # Node label color
      vertex.label.cex = 0.8,                 # Node label size
      vertex.size =node_size ,                       # Node size
      main = paste("Technology Diffusion - Phase:", phases[i])  # Dynamic title
    )

    # Add a legend for phases
    legend(
      "topright",
      legend = phases,
      col = phase_colors,
      pch = 19,    # Legend points as circles
      bty = "n"    # No legend box
    )
  }
}, movie.name = "AI chatbot diffusion process.gif", interval = 4, ani.width = 1000, ani.height = 800)

