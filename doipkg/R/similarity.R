#' The title of -calculate similarity-
#'
#' Here is a brief description
#'
#' @param individual1 A Numeric Vector.
#' @param individual2 A Numeric Vector.
#' @param method A string.
#'
#' @details Computes the similarity of \code{individual1} and \code{individual2}.
#' @return A Numeric scalar \code{doipkg_similarity}:
#' \item{individual1}{Numeric Vector.}
#' \item{individual2}{Numeric Vector.}
#' \item{individual1individual2}{Numeric scalar. the sum of \code{individual1} and \code{individual2}}
#' @export

# Function to calculate similarity between two individuals
calculate_similarity <- function(individual1, individual2, method) {
  if (method == "Cosine") {
    cosine_similarity <- sum(individual1 * individual2) /
      (sqrt(sum(individual1^2)) * sqrt(sum(individual2^2)))     # Calculate cosine similarity
    return(cosine_similarity)

  } else if (method == "Hamming") {
    hamming_distance <- sum(individual1 != individual2)     # Calculate Hamming distance and convert to similarity
    return(hamming_distance)

  } else if (method == "Euclidean") {

    euclidean_distance <- sqrt(sum((individual1 - individual2)^2))     # Calculate Euclidean distance and convert to similarity
    return(euclidean_distance)

  } else if (method == "Jaccard") {

    jaccard_similarity <- sum(individual1 & individual2) / sum(individual1 | individual2)
    return(jaccard_similarity)

  } else {
    stop("Invalid method. Choose 'Cosine', 'Hamming', 'Euclidean', or 'Jaccard'.")
  }
}
