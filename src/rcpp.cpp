#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List run_simulation_cpp(
    int num_agents,
    int n_simulations,
    Rcpp::CharacterVector methods,
    Rcpp::IntegerVector stages,
    Rcpp::CharacterVector stages_name,
    double p_prior,
    Rcpp::NumericVector ps_theory,
    Rcpp::NumericVector adoption_efficacy,
    double non_adoption_efficacy
) {
  // Initialize counters
  int n_counts = 0;
  int n_closeness = 0;

 //Simulation loop
  for (int i = 0; i < n_simulations; i++) {
    // Step 1: Generate mock network data
    NumericMatrix compare_table(num_agents, methods.size());

    for (int j = 0; j < methods.size(); j++) {
      std::string approach = as<std::string>(methods[j]);

      NumericVector output_data(num_agents); // Generate adoption results for this approach

      for (int k = 0; k < num_agents; k++) {
        // Simulate adoption or non-adoption
        if (R::runif(0, 1) < p_prior) {
          output_data[k] = adoption_efficacy[stages[k % stages.size()] - 1]; // -1 for 0-based index
        } else {
          output_data[k] = non_adoption_efficacy;
        }
      }

      // Store output in the comparison table
      compare_table(_, j) = output_data;
    }

    // Step 2: Compare the results between methods
    int largest_counts = 0;
    int largest_closeness = 0;

    for (int k = 0; k < num_agents; k++) {
      if (compare_table(k, 0) > compare_table(k, 1)) {
        largest_counts++;
      } else if (compare_table(k, 1) > compare_table(k, 0)) {
        largest_closeness++;
      }
    }

    // Calculate probabilities
    double prob_counts = (double)largest_counts / num_agents;
    double prob_closeness = (double)largest_closeness / num_agents;

    // Update counters
    if (prob_counts > prob_closeness) {
      n_counts++;
    } else if (prob_closeness > prob_counts) {
      n_closeness++;
    }
  }

  // Calculate final probabilities
  double final_pro_counts = (double)n_counts / n_simulations;
  double final_pro_closeness = (double)n_closeness / n_simulations;

  return List::create(
    Named("final_pro_counts") = final_pro_counts,
    Named("final_pro_closeness") = final_pro_closeness
  );
}
