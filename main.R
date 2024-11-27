
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

for (stage in stages) {
  if (stage == 1) {
    # Proportion of innovators
    p_DOI <- ps_theory[stage]
    total_n <- num_agents

    # Determine adopters
    result <- determine_adopter(total_n, adj_matrix, final_matrix, p_DOI, method)
    stage_indice <- result$indices  # Top indices
    stage_values <- result$values   # Corresponding centrality values
    print(stage_indice)
    previous_indices <- c(previous_indices, stage_indice)

  } else {
    # Proportion of adopters based on prior stage
    p_DOI <- ps_theory[stage] * p_prior
    total_n <- num_agents - length(previous_indices)

    # Collect all connected nodes
    connect_indices <- c()
    for (index in previous_indices) {
      select_column <- adj_matrix[, index]
      new_indices <- setdiff(which(select_column != 0), c(previous_indices, connect_indices))
      connect_indices <- c(connect_indices, new_indices)
    }

    # Subset matrices for connected nodes
    adj_matrix_sta <- adj_matrix[connect_indices, connect_indices]
    final_matrix_sta <- final_matrix[connect_indices, connect_indices]

    # Determine adopters in the connected sub-network
    result_sta <- determine_adopter(total_n, adj_matrix_sta, final_matrix_sta, p_DOI, method)
    stage_indice <- result_sta$indices  # Top indices
    print(stage_indice)
    stage_values <- result_sta$values   # Corresponding centrality values
    previous_indices <- c(previous_indices, stage_indice)
  }

  # Efficacy of this stage
  stage_efficacy <- adoption_efficacy[stage]

  # Update adoption index and efficacy
  adoption_lst[[stages_name[stage]]] <- stage_indice
  update_efficacy[[stages_name[stage]]] <- original_data[stage_indice, 'Baseline_PA'] + stage_efficacy

  # Update original data with new efficacy
  original_data[stage_indice, 'Follow_up_PA'] <- original_data[stage_indice, 'Baseline_PA'] + stage_efficacy
}

#non-adoption efficacy
# Identify non-adopters
non_adoption_index <- setdiff(1:num_agents, previous_indices)
# Update effectiveness for non-adopters
original_data[non_adoption_index, 'Follow_up_PA'] <- original_data[non_adoption_index, 'Baseline_PA'] + non_adoption_efficacy



detect_adopters <- function(num_agents,adj_matrix,final_matrix,original_data,stages,stages_name,ps_theory,adoption_efficacy,non_adoption_efficacy,method,p_prior) {
  # Initialize variables
  adoption_lst <- list()
  update_efficacy <- list()
  previous_indices <- c()
  original_data[,'Follow_up_PA'] <- NaN # Initialize a new column

  for (stage in stages) {
    if (stage == 1) {
      # Proportion of innovators
      p_DOI <- ps_theory[stage]
      total_n <- num_agents

      # Determine adopters
      result <- determine_adopter(total_n, adj_matrix, final_matrix, p_DOI, method)
      stage_indice <- result$indices  # Top indices
      stage_values <- result$values   # Corresponding centrality values
      print(stage_indice)
      previous_indices <- c(previous_indices, stage_indice)

    } else {
      # Proportion of adopters based on prior stage
      p_DOI <- ps_theory[stage] * p_prior
      total_n <- num_agents - length(previous_indices)

      # Collect all connected nodes
      connect_indices <- c()
      for (index in previous_indices) {
        select_column <- adj_matrix[, index]
        new_indices <- setdiff(which(select_column != 0), c(previous_indices, connect_indices))
        connect_indices <- c(connect_indices, new_indices)
      }

      # Subset matrices for connected nodes
      adj_matrix_sta <- adj_matrix[connect_indices, connect_indices]
      final_matrix_sta <- final_matrix[connect_indices, connect_indices]

      # Determine adopters in the connected sub-network
      result_sta <- determine_adopter(total_n, adj_matrix_sta, final_matrix_sta, p_DOI, method)
      stage_indice <- result_sta$indices  # Top indices
      print(stage_indice)
      stage_values <- result_sta$values   # Corresponding centrality values
      previous_indices <- c(previous_indices, stage_indice)
    }

    # Efficacy of this stage
    stage_efficacy <- adoption_efficacy[stage]

    # Update adoption index and efficacy
    adoption_lst[[stages_name[stage]]] <- stage_indice
    update_efficacy[[stages_name[stage]]] <- original_data[stage_indice, 'Baseline_PA'] + stage_efficacy

    # Update original data with new efficacy
    original_data[stage_indice, 'Follow_up_PA'] <- original_data[stage_indice, 'Baseline_PA'] + stage_efficacy
  }

  # Handle non-adopters
  non_adoption_index <- setdiff(1:num_agents, previous_indices)
  original_data[non_adoption_index, 'Follow_up_PA'] <- original_data[non_adoption_index, 'Baseline_PA'] + non_adoption_efficacy

  # Return results
  return(list(
    adoption_lst = adoption_lst,
    update_efficacy = update_efficacy,
    original_data = original_data
  ))
}


#---------------Step 5. Calculate the effectiveness

