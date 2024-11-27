
library(doipkg)
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

# Assign strength to the matrix
final_matrix <- similarity_matrix * adj_matrix

#---------------Step 4. detect adopter at each stage

stages <- c(1, 2, 3, 4, 5) # five stages
stages_name <- c("stage1", "stage2", "stage3", "stage4", "stage5")
p_prior <- 0.45
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16) #probability of adoption at each stage based on DOI
adoption_efficacy <- c(1500, 1200, 900, 600, 300) #unit: step
non_adoption_efficacy <- -250 #unit：step
method <- "betweeness" # "counts, betweeness, closeness"
original_data[,'Follow_up_PA'] <- NaN #initial a new column

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

#define the parameters of the network layout
fixed_layout <- layout_with_graphopt(initial_graph)
E(initial_graph)$weight <- edge_weights
E(initial_graph)$width <- abs(E(initial_graph)$weight) * 4
E(initial_graph)$color <- ifelse(E(initial_graph)$weight > 0,  "#000080", "#B22222")
V(initial_graph)$size  <- 20
V(initial_graph)$color <- "gray"
V(initial_graph)$label.color <- "black"
#plot(initial_graph, layout = fixed_layout)


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
















