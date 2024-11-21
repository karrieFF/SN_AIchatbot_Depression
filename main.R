
library(doipkg)
num_agents <- 100

# Generate data
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

# Initialize the similarity matrix
similarity_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)

# Loop through each pair of individuals to populate the similarity matrix
for (i1 in 1:num_agents) {
  for (i2 in 1:num_agents) {
    # Extract individual data as numeric vectors, excluding the ID column
    individual1 <- as.numeric(original_data[i1, -1])  
    individual2 <- as.numeric(original_data[i2, -1])
    
    # Calculate similarity using the specified method (change method as needed)
    similarity_matrix[i1, i2] <- doipkg::calculate_similarity(individual1, individual2, method = "Euclidean") #this used the doipkg
  }
}

#standardize the matrix
min_val <- min(similarity_matrix)
max_val <- max(similarity_matrix)

# Standardize the similarity matrix to range [0, 1]
standardized_similarity_matrix <- (similarity_matrix - min_val) / (max_val - min_val)

#original matrix
agent1 <- sample(1:num_agents, 100, replace = TRUE)
agent2 <- sample(1:num_agents, 100, replace = TRUE)

# Create an adjacency matrix with random connections (0 or 1)
# 0.9 probability for no connection, 0.1 for connection
adj_matrix <- matrix(sample(0:1, num_agents^2, replace = TRUE, prob = c(0.9, 0.1)), 
                     nrow = num_agents, 
                     ncol = num_agents)

adj_matrix[lower.tri(adj_matrix)] <- t(adj_matrix)[lower.tri(adj_matrix)] #Make symmetric
diag(adj_matrix) <- 0 # Remove self-loops

#assign strength to the matrix
final_matrix <- standardized_similarity_matrix * adj_matrix

#-----------------detect innovators

# Function to calculate top innovators based on centrality
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)

#the reason why we select 300 - 1500 is based on the theory, the increase for the total 60 days is 1500,
#if we split them into 5 parts, that should be 12days, 24days, 36days, 48days, 60days.
#the icnrease is relatively linear, the step increase for each time split should 300, 600, 900, 1200
#people at the final stage may adopt the chatbot the shorted time
#thus the increase of the steps at the final stage is 300 steps
#if we hypothesis is not enirely linear

effectivenss <- c(300, 600, 900, 1200, 1500)

# Replace final_matrix with your matrix
p_innovators <- ps_theory[1]  # Proportion of innovators
method <- "closeness"  # Method: "in-degree", "betweeness", or "closeness"
result <- get_top_innovators(final_matrix, p_innovators, method)

# Print the results
innovators_indice <- result$indices # Top indices
innovators_values <- result$values   # Corresponding centrality values

#effectiveness, adoption increase 1000 steps, non-adoption increase 250 steps

adoption_efficacy <- effectivenss[1]
original_data[i,'Follow_up_PA'] <- NaN

original_data[innovators_indice, 'Follow_up_PA'] <- original_data[innovators_indice, 'Baseline_PA'] + adoption_efficacy


#-----------------adoption_effectiveness

#early adopter, the rest of 13.5% (DOI), adoption rate is 45% (prior knowledge)
select <- c()

#p_theory <- 0.135
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)
p_prior <- 0.45
stages <- seq(2:5)

effectivenss <- c(300, 600, 900, 1200, 1500)
non_adoption_efficacy <- -250 #finally

result <- simulate_adoption(
  final_matrix = final_matrix,
  result = result,
  stages = stages,
  ps_theory = ps_theory,
  p_prior = p_prior,
  effectiveness = effectivenss,
  non_adoption_efficacy = non_adoption_efficacy,
  original_data = original_data
)
result
# Access outputs
adoption_index <- result$adoption_index
non_adoption_index <- result$non_adoption_index
updated_data <- result$updated_data

updated_data
