#' The title of -adoption effectiveness-
#'
#' Here is a brief description
#'
#' @param final_matrix A Numeric Matrix.
#' @param result A list.
#' @param stages A list.
#' @param ps_theory A list.
#' @param p_prior A Numeric scalar.
#' @param effectiveness A list.
#' @param non_adoption_efficacy A Numeric scalar.
#' @param original_data A Numeric Matrix.
#' @export


simulate_adoption <- function(final_matrix, result, stages, ps_theory, p_prior, effectiveness, non_adoption_efficacy, original_data) {
  # Initialize variables
  n_total <- nrow(final_matrix)
  innovators <- result$indices
  adoption_index <- c(innovators)

  for (stage in stages) {
    n_rest <- n_total - length(adoption_index)

    # Initialize total data as an empty data frame
    total_data <- data.frame(Index = integer(), Value = numeric())

    for (i in seq_along(adoption_index)) {
      index <- adoption_index[i]  # Select index
      select_column <- final_matrix[, index]  # Select specific column
      nonzero_indices <- which(select_column != 0)  # Find indices of non-zero elements
      filtered_indices <- nonzero_indices[!nonzero_indices %in% adoption_index]  # Exclude already adopted
      nonzero_values <- select_column[filtered_indices]  # Get the corresponding values

      # Create a data frame for the current innovator's indices and values
      new_data <- data.frame(Index = filtered_indices, Value = nonzero_values)

      if (nrow(new_data) > 0) {
        total_data <- merge(total_data, new_data, by = "Index", all = TRUE)  # Merge with cumulative data
        total_data$Value <- rowSums(total_data[, c("Value.x", "Value.y")], na.rm = TRUE)
        total_data <- total_data[, c("Index", "Value")]  # Retain final structure
      }
    }

    # Theoretical number of adoption at this stage
    num_adoption <- round(n_rest * ps_theory[stage] * p_prior)

    # Sort and select top indices for adoption
    sorted_data <- total_data[order(-total_data$Value), ]
    top_indices <- head(sorted_data$Index, num_adoption)

    # Update adoption index
    adoption_index <- c(adoption_index, top_indices)

    # Update effectiveness for adopters
    adoption_efficacy <- effectiveness[stage]
    original_data[top_indices, 'Follow_up_PA'] <- original_data[top_indices, 'Baseline_PA'] + adoption_efficacy
  }

  # Identify non-adopters
  non_adoption_index <- setdiff(1:n_total, adoption_index)

  # Update effectiveness for non-adopters
  original_data[non_adoption_index, 'Follow_up_PA'] <- original_data[non_adoption_index, 'Baseline_PA'] + non_adoption_efficacy

  # Return results
  return(list(
    adoption_index = adoption_index,
    non_adoption_index = non_adoption_index,
    updated_data = original_data
  ))
}
