
library(doipkg)
library(igraph) #use to create graph
library(animation) #use to create GIF file

num_agents <- 100
#---------------Step 1.  Generate Dataframe
id <- seq(1, num_agents)
gender <- sample(c(0, 1), num_agents, replace = TRUE)  # 0 represents female; 1 represents male
age <- runif(num_agents, min = 18, max = 30)
socio_economic_status <- sample(c(0, 1, 2), num_agents, replace = TRUE)  # 0 = low, 1 = middle, 2 = high
baseline_PA <- runif(num_agents, min = 1000, max = 15000)  # 0 = did not reach PA goal, 1 = reached PA goal

# Create the data frame
original_data <- data.frame(
  ID = id,
  Gender = gender,
  Age = age,
  SES = socio_economic_status,
  Baseline_PA = baseline_PA
)

#-------------------Step 2. Generate the link between two nodes
#original matrix
agent1 <- original_data$ID
agent2 <- original_data$ID

# Create an adjacency matrix with random connections (0 or 1)
# 0.9 probability for no connection, 0.1 for connection
adj_matrix <- matrix(sample(0:1, num_agents^2, replace = TRUE, prob = c(0.9, 0.1)),
                     nrow = num_agents,
                     ncol = num_agents)

adj_matrix[lower.tri(adj_matrix)] <- t(adj_matrix)[lower.tri(adj_matrix)] #Make symmetric
diag(adj_matrix) <- 0 # Remove self-loops
rownames(adj_matrix) <- agent1
colnames(adj_matrix) <- agent2

#---------------Step 3. Generate similarity matrix
# Initialize the similarity matrix
similarity_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)

# Loop through each pair of individuals to populate the similarity matrix
for (i1 in 1:num_agents) {
  for (i2 in 1:num_agents) {
    # Extract individual data as numeric vectors, excluding the ID column
    individual1 <- as.numeric(original_data[i1, -1])
    individual2 <- as.numeric(original_data[i2, -1])

    # Calculate similarity using the specified method (change method as needed)
    similarity_matrix[i1, i2] <- doipkg::calculate_distance(individual1, individual2, method = "Euclidean") #this used the doipkg
  }
}
#standardize similarity matrix: Z-score standardized

min_val <- min(similarity_matrix)
max_val <- max(similarity_matrix)
standardized_similarity_matrix <- (similarity_matrix - min_val) / (max_val - min_val)

# Assign strength to the matrix
final_matrix <- standardized_similarity_matrix * adj_matrix

#---------------Step 4. detect adopter at each stage
stages <- c(1, 2, 3, 4, 5) # five stages
stages_name <- c("stage1", "stage2", "stage3", "stage4", "stage5")
p_prior <- 0.45
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16) #probability of adoption at each stage based on DOI
adoption_efficacy <- c(1500, 1200, 900, 600, 300) #unit: step
non_adoption_efficacy <- -250 #unit：step
original_data[,'Follow_up_PA'] <- NaN #initial a new column
method <- "closeness" # "counts, betweeness, closeness"

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
  method,
  p_prior
)

#---------------Step 5. Calculate the effectiveness
final_data <- output$original_data
t_test_result <- t.test(final_data$Baseline_PA, final_data$Follow_up_PA, paired = TRUE)
print(t_test_result)

#---------------Step 6. Visualize the diffusion process
# Convert similarity matrix to symmetric form
cormatrix <- 1 / standardized_similarity_matrix  # Convert to similarity
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
base_pa <- final_data$Baseline_PA
normalized_base_pa <- (base_pa - min(base_pa)) / (max(base_pa) - min(base_pa))  # Min-max normalization

# Create gradient red colors based on normalized data
gradient_red <- rgb(1, 1 - normalized_base_pa, 1 - normalized_base_pa)  # Gradient red (higher values = more intense red)

follow_up_pa <-  final_data$Follow_up_PA
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


#-----------------------Step 7. Simulate diffusion process with the increase in pA

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
}, movie.name = "technology_diffusion_revised_20_nodes.gif", interval = 4, ani.width = 1000, ani.height = 800)

