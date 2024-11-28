#' The title of -detect_adopters-
#'
#' Here is a brief description
#'
#' @param num_agents A Numeric scala.
#' @param adj_matrix A Numeric Matrix.
#' @param final_matrix A Numeric Matrix.
#' @param result A list.
#' @param stages A list.
#' @param stages_name A list.
#' @param method A str.
#' @param ps_theory A list.
#' @param p_prior A Numeric scalar.
#' @param adoption_efficacy A list.
#' @param non_adoption_efficacy A Numeric scalar.
#' @param original_data A Numeric Matrix.
#' @export


detect_adopters <- function(num_agents, adj_matrix, final_matrix, original_data, stages, stages_name, ps_theory, adoption_efficacy, non_adoption_efficacy, method, p_prior) {

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
      result <- determine_adopter(total_n, adj_matrix, final_matrix, sequence(num_agents), p_DOI, method)
      stage_indice <- result$indices  # Top indices
      stage_values <- result$values   # Corresponding centrality values
      #print(stage_indices)
      previous_indices <- c(previous_indices, stage_indice)
      #print(previous_indices)

    } else {
      # Proportion of adopters based on prior stage
      p_DOI <- ps_theory[stage] * p_prior
      total_n <- num_agents - length(previous_indices)

      # Collect all connected nodes
      connect_indices <- c()
      for (index in previous_indices) {
        select_column <- adj_matrix[, index]
        select_index <- which(select_column != 0)
        select_index <- setdiff(select_index, connect_indices)#the indices are not in the connect indices
        select_index <- setdiff(select_index, previous_indices)#the indices are not in the previous indices
        connect_indices <- c(connect_indices, select_index)
      }

      # Determine adopters in the connected sub-network
      result_sta <- determine_adopter(total_n, adj_matrix, final_matrix, connect_indices, p_DOI, method)
      stage_indice <- result_sta$indices  # indices of the matrix

      #print(stage_indice)
      stage_values <- result_sta$values   # Corresponding centrality values
      previous_indices <- c(previous_indices, stage_indice)
      #print(previous_indices)
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
