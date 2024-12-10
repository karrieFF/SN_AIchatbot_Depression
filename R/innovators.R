#' Get adopters
#'
#' This function determines adopters in a network using one of three centrality measures: counts, betweenness, or closeness.
#' @param total_n Numeric scalar. Total number of nodes in the network.
#' @param adj_matrix Numeric matrix. The adjacency matrix representing the network.
#' @param final_matrix Numeric matrix. An additional matrix related to the network (optional for this function).
#' @param indices Numeric vector. Indices of the nodes to consider for adoption.
#' @param p_innovators Numeric scalar. Proportion of total nodes to be classified as innovators.
#' @param approach String. The centrality method to use: "counts", "betweeness", or "closeness".
#' @details Determines the top innovators in the network based on the specified centrality measure.
#' @return A list containing:
#' \describe{
#'   \item{indices}{Numeric vector. Indices of the top adopters.}
#'   \item{values}{Numeric vector. Centrality values of the top adopters.}
#'   \item{approach}{String. The centrality measure used.}
#' }
#' @export

determine_adopter <- function(total_n, adj_matrix, final_matrix, indices, p_innovators, approach) {
  # -----------------------Subset matrices for connected nodes
  adj_matrix_sta <- adj_matrix[, indices]
  final_matrix_sta <- final_matrix[, indices]

  # Calculate the number of innovators to select
  top_n <- round(total_n * p_innovators)

  # Counts centrality (number of connections, number of non-zero connections for each column)
  nan_0_counts <- colSums(adj_matrix_sta != 0)

  # -------------------------Betweenness centrality
  total_close <- c()

  for (i0 in 1:length(indices)){
    col_first <- adj_matrix_sta[, i0]  # Extract column for the current agent
    non_0_index <- which(col_first != 0)  # Get indices of non-zero values in the column
    length_non_0_index <- length(non_0_index)
    num_close <- (length_non_0_index*(length_non_0_index-1))/2
    total_close <- c(total_close, num_close)  # append the result
  }

  # Closeness centrality (sum of similarity values for each column)
  sum_distance <- colSums(adj_matrix_sta)

  # Determine top innovators based on the selected method
  if (approach == "counts") {
    top_indices <- order(-nan_0_counts)[1:top_n]
    top_values <- nan_0_counts[top_indices]

  } else if (approach == "betweeness") {
    top_indices <- order(-total_close)[1:top_n]
    top_values <- total_close[top_indices]

  } else if (approach == "closeness") {
    top_indices <- order(sum_distance)[1:top_n] #The short distance, rank from low to high
    top_values <- sum_distance[top_indices] #top_indices represent short distance

  } else {
    stop("Invalid approach Choose from 'in-degree', 'betweeness', or 'closeness'.")
  }

  real_top_indices <- indices[top_indices] #real indices returned in connect_indices

  # Return the results as a list
  return(list(
    indices = real_top_indices,
    values = top_values,
    approach = approach
  ))
}
