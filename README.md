DOI-package
================

# doipkg

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/doipkg)](https://CRAN.R-project.org/package=doipkg)
[![R-CMD-check.yaml](https://github.com/karrieFF/SN_AIchatbot_Physical_Activity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/karrieFF/SN_AIchatbot_Physical_Activity/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/karrieFF/SN_AIchatbot_Physical_Activity/blob/main/LICENSE.md)

<!-- badges: end -->

This package can be used to simulate how social network intervention can
affect a specific outcome by adopting digital technology.

## Installation

You can install the development version of doipkg from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("SN_AIchatbot_Physical_Activity/doipkg")
```

## Examples

This example is to run social network simulation to determine which
approach for selecting innovators can help achieve large effectiveness.

#### Simulate social network data

This function generates a dataset with multiple characteristics and
creates an adjacent matrix to generate a network.

``` r
install.packages("doipkg")

#generate the data for network analysis
data <- generate_network_matrices(
  num_agents = 100) 

data
```

#### Calculate the distance between two agents.

This function helps you calculate the distance between two agents based
on the agents’ characteristics.

``` r

num_agents = 100

similarity_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)

original_data <- data$original_data

individual1 <- as.numeric(original_data[i1, -1])
individual2 <- as.numeric(original_data[i2, -1])
      
similarity_matrix[i1, i2] <- calculate_distance(
  individual1, 
  individual2, 
  method = "Euclidean") #Cosine, Hamming, Euclidean, Jaccard
```

#### Calculate the effectiveness of adoption

This function helps you output the change of outcome after the social
network intervention.

``` r

output <- detect_adopters(
  num_agents = 100 ,
  adj_matrix = data$adjacency_matrix,
  final_matrix = data$final_matrix,
  original_data = data$original_data,
  stages = c(1, 2, 3, 4, 5),
  stages_name = c("stage1", "stage2", "stage3", "stage4", "stage5"),
  ps_theory = c(0.025, 0.135, 0.34, 0.34, 0.16),
  adoption_efficacy = c(1500, 1200, 900, 600, 300),
  non_adoption_efficacy = 250,
  approach = 'closeness',
  p_prior = 0.45
)
```

#### Plot the network

``` r
final_data <- output$output_data

min_val <- min(similarity_matrix)
max_val <- max(similarity_matrix)
standardized_distance_matrix <- (similarity_matrix - min_val) / (max_val - min_val)

cormatrix <- 1 / standardized_distance_matrix  # Convert to similarity matrix
cormatrix[lower.tri(cormatrix)] <- t(cormatrix)[lower.tri(cormatrix)]  # Make symmetric
diag(cormatrix) <- 0  # Set diagonal to 0

# Ensure adjacency matrix is undirected
adj_matrix[upper.tri(adj_matrix)] <- FALSE  # Remove upper triangle to ensure symmetry
edge_weights <- cormatrix[adj_matrix]  # Extract edge weights
initial_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE) # Create the graph object
E(initial_graph)$weight <- edge_weights # Assign edge weights

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
normalized_base_pa <- (base_pa - min(base_pa)) / (max(base_pa) - min(base_pa))

# Create gradient red colors based on normalized data
gradient_red <- rgb(1, 1 - normalized_base_pa, 1 - normalized_base_pa)

follow_up_pa <-  final_data$closeness
normalized_follow_pa <- (follow_up_pa - min(follow_up_pa)) / (max(follow_up_pa) - min(follow_up_pa))

# Create gradient red colors based on normalized data
gradient_follow_red <- rgb(1, 1 - normalized_follow_pa, 1 - normalized_follow_pa)

# Define pie chart colors for each node
pie_colors <- mapply(function(red, follow_red) {
  c(red, follow_red)
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
```

#### Visualize the diffusion process

``` r
# Define node frame colors and phase colors
node_frame_colors <- rep("white", vcount(initial_graph))  # Default frame color
node_size <- rep(10, vcount(initial_graph))

phase_colors <- c("#FBEB66", "#EF8A43", "#7F00FF", "#4EA660", "#193E8F")  # Phase colors
phases <- c("Innovators", "Early Adopters", "Early Majority", "Late Majority", "Laggards")  # Phase names

# Get all stages and their names
all_stages <- output$output_lst  # List of nodes for each phase
stages_name <- names(all_stages)   # Phase names (keys in the list)
total_stages <- length(stages_name)

#get the change efficacy
all_stages_efficacy <- output$update_effi

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
}, movie.name = "ai_chatbot_diffusion_process.gif", interval = 4, ani.width = 1000, ani.height = 800)
```

#### Multiple simulations to compare the effectiveness of different approach

``` r
num_agents <- 100
method <- c("counts",  "closeness")
stages <- c(1, 2, 3, 4, 5) # five stages
stages_name <- c("stage1", "stage2", "stage3", "stage4", "stage5")
p_prior <- 0.45
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16) #probability of adoption at each stage based on DOI
adoption_efficacy <- c(1500, 1200, 900, 600, 300) #unit: step
non_adoption_efficacy <- 250 #unit：step, increase only 250, how to set this
n_simulations <- 100000

results <- run_simulation(num_agents,
                                  n_simulations,
                                  method,
                                  stages,
                                  stages_name,
                                  p_prior,
                                  ps_theory,
                                  adoption_efficacy,
                                  non_adoption_efficacy)

-final_pro_counts
0.47263

-final_pro_closeness
0.47067
```
