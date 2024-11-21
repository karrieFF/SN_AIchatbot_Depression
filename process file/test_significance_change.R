
#finally, we can determine if step counts can be significnatly changed or not

# Perform paired t-test
t_test_result <- t.test(original_data$Baseline_PA, original_data$Follow_up_PA, paired = TRUE)

# Print the results
print(t_test_result)