#' Detect Adopters in the Diffusion Process
#'
#' This function identifies adopters across multiple stages based on centrality measures, updates their efficacy,
#' and adjusts data for non-adopters.
#'
#' @param num_agents Numeric scalar. Total number of agents in the network.
#' @param adj_matrix Numeric matrix. Adjacency matrix representing the network.
#' @param final_matrix Numeric matrix. Additional network-related matrix (optional for this function).
#' @param original_data Numeric matrix. Data containing baseline and other attributes for agents.
#' @param stages List. A sequence of stages for the adoption process.
#' @param stages_name List. Names corresponding to each stage.
#' @param ps_theory List. Proportions of adopters for each stage.
#' @param adoption_efficacy List. Efficacy values added for adopters at each stage.
#' @param non_adoption_efficacy Numeric scalar. Efficacy value for non-adopters.
#' @param approach String. Centrality measure approach: "counts", "betweeness", or "closeness".
#' @param p_prior Numeric scalar. Proportion adjustment for subsequent stages.
#' @return A list containing:
#' \describe{
#'   \item{output_lst}{List of adopter indices for each stage.}
#'   \item{update_effi}{List of updated efficacy values for each stage.}
#'   \item{output_data}{Original data updated with new efficacy values for adopters and non-adopters.}
#' }
#' @export

detect_adopters <- function(num_agents, adj_matrix, final_matrix, original_data, stages, stages_name, ps_theory, adoption_efficacy, non_adoption_efficacy, approach, p_prior) {

  # Initialize variables
  adoption_lst <- list()
  update_efficacy <- list()
  previous_indices <- c()

  for (stage in stages) {
    if (stage == 1) {
      # Proportion of innovators
      p_DOI <- ps_theory[stage]
      total_n <- num_agents

      # Determine adopters
      result <- determine_adopter(total_n, adj_matrix, final_matrix, sequence(num_agents), p_DOI, approach)
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
      result_sta <- determine_adopter(total_n,
                                      adj_matrix,
                                      final_matrix,
                                      connect_indices,
                                      p_DOI,
                                      approach)

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
    original_data[stage_indice, approach] <- original_data[stage_indice, 'Baseline_PA'] + stage_efficacy
  }

  # Handle non-adopters
  non_adoption_index <- setdiff(1:num_agents, previous_indices)
  original_data[non_adoption_index, approach] <- original_data[non_adoption_index, 'Baseline_PA'] + non_adoption_efficacy

  # Return results
  return(list(
    output_lst = adoption_lst,
    update_effi = update_efficacy,
    output_data = original_data
  ))
}
