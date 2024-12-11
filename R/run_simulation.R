#' Run Simulation
#'
#' @param num_agents Number of agents
#' @param n_simulations Number of simulations
#' @param methods Methods for comparison
#' @param stages Stages of the process
#' @param stages_name Names of the stages
#' @param p_prior Prior probability of adoption
#' @param ps_theory Theoretical probabilities
#' @param adoption_efficacy Adoption efficacy values
#' @param non_adoption_efficacy Non-adoption efficacy value
#' @return A list of final probabilities
#' @export
#' @useDynLib doipkg, .registration = TRUE

run_simulation <- function(
    num_agents, n_simulations, methods, stages,
    stages_name, p_prior, ps_theory, adoption_efficacy, non_adoption_efficacy
) {
  run_simulation_cpp(
    num_agents, n_simulations, methods, stages,
    stages_name, p_prior, ps_theory, adoption_efficacy, non_adoption_efficacy
  )
}
