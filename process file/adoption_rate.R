
#early adopter, the rest of 13.5% (DOI), adoption rate is 45% (prior knowledge)
select <- c()

#p_theory <- 0.135
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)
p_prior <- 0.45
stages <- seq(2:5)

effectivenss <- c(300, 600, 900, 1200, 1500)
non_adoption_efficacy <- -250 #finally

result <- simulate_adoption(
  final_matrix = final_matrix,
  result = result,
  stages = stages,
  ps_theory = ps_theory,
  p_prior = p_prior,
  effectiveness = effectivenss,
  non_adoption_efficacy = non_adoption_efficacy,
  original_data = original_data
)
result
# Access outputs
adoption_index <- result$adoption_index
non_adoption_index <- result$non_adoption_index
updated_data <- result$updated_data

updated_data
