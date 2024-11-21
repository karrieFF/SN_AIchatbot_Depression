
# Function to calculate top innovators based on centrality
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)

#the reason why we select 300 - 1500 is based on the theory, the increase for the total 60 days is 1500,
#if we split them into 5 parts, that should be 12days, 24days, 36days, 48days, 60days.
#the icnrease is relatively linear, the step increase for each time split should 300, 600, 900, 1200
#people at the final stage may adopt the chatbot the shorted time
#thus the increase of the steps at the final stage is 300 steps
#if we hypothesis is not enirely linear

effectivenss <- c(300, 600, 900, 1200, 1500)

# Replace final_matrix with your matrix
p_innovators <- ps_theory[1]  # Proportion of innovators
method <- "closeness"  # Method: "in-degree", "betweeness", or "closeness"
result <- get_top_innovators(final_matrix, p_innovators, method)

# Print the results
innovators_indice <- result$indices # Top indices
innovators_values <- result$values   # Corresponding centrality values

#effectiveness, adoption increase 1000 steps, non-adoption increase 250 steps

adoption_efficacy <- effectivenss[1]
original_data[i,'Follow_up_PA'] <- NaN
  
original_data[innovators_indice, 'Follow_up_PA'] <- original_data[innovators_indice, 'Baseline_PA'] + adoption_efficacy



