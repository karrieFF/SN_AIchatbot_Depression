#' Generate network data
#'
#' This function is used to generate a dataset with multiple characteristics and create an adjacent matrix to generate network.
#' @param num_agents A numeric scalar indicating the number of agents to generate.
#' @return A list containing:
#' \item{original_data}{A data frame of agent demographic data.}
#' \item{adjacency_matrix}{A symmetric adjacency matrix indicating random connections between agents.}
#' \item{similarity_matrix}{A standardized similarity matrix based on Euclidean distance.}
#' \item{final_matrix}{A weighted matrix combining the adjacency and similarity matrices.}
#' @export

generate_network_matrices <- function(num_agents) {

  #################################################################
  # Step 1: Generate Dataframe
  #################################################################

  id <- seq(1, num_agents)
  gender <- sample(c(0, 1), num_agents, replace = TRUE)  # 0 = female, 1 = male
  age <- runif(num_agents, min = 18, max = 30)
  socio_economic_status <- sample(c(0, 1, 2), num_agents, replace = TRUE)  # 0 = low, 1 = middle, 2 = high
  baseline_PA <- runif(num_agents, min = 1000, max = 15000)

  # Create the data frame
  original_data <- data.frame(
    ID = id,
    Gender = gender,
    Age = age,
    SES = socio_economic_status,
    Baseline_PA = baseline_PA
  )

  #################################################################
  # Step 2: Generate Adjacency Matrix
  #################################################################
  agent1 <- original_data$ID
  agent2 <- original_data$ID

  # Random connections: 90% probability of no connection, 10% connection
  adj_matrix <- matrix(sample(0:1, num_agents^2, replace = TRUE, prob = c(0.9, 0.1)),
                       nrow = num_agents,
                       ncol = num_agents)
  adj_matrix[lower.tri(adj_matrix)] <- t(adj_matrix)[lower.tri(adj_matrix)] # Make symmetric
  diag(adj_matrix) <- 0 # Remove self-loops
  rownames(adj_matrix) <- agent1
  colnames(adj_matrix) <- agent2

  #################################################################
  # Step 3: Generate Similarity Matrix
  #################################################################
  # Initialize the similarity matrix
  difference_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)

  # Calculate pairwise similarity using a nested loop
  for (i1 in 1:num_agents) {
    for (i2 in 1:num_agents) {
      # Extract individual data, excluding the ID column
      individual1 <- as.numeric(original_data[i1, -1])
      individual2 <- as.numeric(original_data[i2, -1])

      # Compute Euclidean distance
      difference_matrix[i1, i2] <- doipkg::calculate_distance(individual1, individual2, method = "Euclidean")
    }
  }

  # Standardize similarity matrix (min-max normalization)
  min_val <- min(difference_matrix)
  max_val <- max(difference_matrix)
  standardized_distance_matrix <- (difference_matrix - min_val) / (max_val - min_val)

  #################################################################
  # Step 4: Generate Final Weighted Matrix
  #################################################################
  # Combine standardized similarity with the adjacency matrix
  final_matrix <- standardized_distance_matrix * adj_matrix

  # Return all matrices and data frame
  list(
    original_data = original_data,
    adjacency_matrix = adj_matrix,
    similarity_matrix = standardized_distance_matrix,
    final_matrix = final_matrix
  )
}
