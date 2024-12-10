#' Calculate Distance Between Two Agents
#'
#' Computes the distance between two numeric vectors based on a specified method.
#'
#' @param individual1 Numeric vector. The first individual.
#' @param individual2 Numeric vector. The second individual.
#' @param method String. The method used for calculation: "Cosine", "Hamming", "Euclidean", or "Jaccard".
#' @details Computes the similarity or distance between \code{individual1} and \code{individual2} using one of the supported methods.
#' @return Numeric scalar. The calculated similarity or distance.
#' @export

# Function to calculate similarity between two individuals
# distance represents how far the two nodes, but 1/distance represent the similarity,

calculate_distance <- function(individual1, individual2, method) {
  if (method == "Cosine") {
    cosine_similarity <- sum(individual1 * individual2) /
      (sqrt(sum(individual1^2)) * sqrt(sum(individual2^2)))     # Calculate cosine distance
    return(cosine_similarity)

  } else if (method == "Hamming") {
    hamming_distance <- sum(individual1 != individual2)     # Calculate Hamming distance
    return(hamming_distance)

  } else if (method == "Euclidean") {
    euclidean_distance <- sqrt(sum((individual1 - individual2)^2))     # Calculate Euclidean distance
    return(euclidean_distance)

  } else if (method == "Jaccard") {
    jaccard_similarity <- sum(individual1 & individual2) / sum(individual1 | individual2)
    return(jaccard_similarity)

  } else {
    stop("Invalid method. Choose 'Cosine', 'Hamming', 'Euclidean', or 'Jaccard'.")
  }
}
