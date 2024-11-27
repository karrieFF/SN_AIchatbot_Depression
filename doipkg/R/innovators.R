#' The title of -get innovators-
#'
#' Here is a brief description
#'
#' @param final_matrix A Numeric Matrix.
#' @param p_innovators A Numeric scalar.
#' @param method A string.
#'
#' @details Get the innovators of \code{final_matrix} and \code{p_innovators}.
#' @return A Numeric Vector \code{doipkg_innovators}:
#' \item{final_matrix}{Numeric Matrix.}
#' \item{p_innovators}{Numeric Vector.}
#' \item{final_matrixp_innovators}{Numeric scalar. the calculation of  \code{final_matrix} and \code{p_innovators}}
#' @export


determine_adopter <- function(total_n, adj_matrix, final_matrix, p_innovators, method) {
  # Calculate the number of innovators to select
  top_n <- round(total_n * p_innovators)

  # Counts centrality (number of connections, number of non-zero connections for each column)
  nan_0_counts <- colSums(adj_matrix != 0)

  # Betweenness centrality
  total_close <- c()  # Vector to store betweenness centrality
  for (i0 in 1:nrow(final_matrix)) {
    num_close <- 0
    col_first <- final_matrix[, i0]  # Extract column for the current agent
    non_0_index <- which(col_first != 0)  # Get indices of non-zero values in the column

    # Loop over each pair of non-zero indices
    for (i1 in non_0_index) {
      except_i <- setdiff(non_0_index, i1)  # Exclude the current index i1
      for (i2 in except_i) {
        value <- final_matrix[i1, i2]  # Get value at (i1, i2) in the matrix
        if (value != 0) {  # Increment if non-zero
          num_close <- num_close + 1
        }
      }
    }
    total_close <- c(total_close, num_close)  # append the result
  }

  # Closeness centrality (sum of similarity values for each column)
  sum_distance <- colSums(final_matrix)

  # Determine top innovators based on the selected method
  if (method == "counts") {
    top_indices <- order(-nan_0_counts)[1:top_n]
    top_values <- nan_0_counts[top_indices]
  } else if (method == "betweeness") {
    top_indices <- order(-total_close)[1:top_n]
    top_values <- total_close[top_indices]
  } else if (method == "closeness") {
    top_indices <- order(sum_distance)[1:top_n] #The short distance, rank from low to high
    top_values <- sum_distance[top_indices] #top_indices represent short distance
  } else {
    stop("Invalid method. Choose from 'in-degree', 'betweeness', or 'closeness'.")
  }

  # Return the results as a list
  return(list(
    indices = top_indices,
    values = top_values,
    method = method
  ))
}
