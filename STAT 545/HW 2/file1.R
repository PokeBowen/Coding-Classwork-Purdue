source("file2.R")

# Read data
score <- read.csv("scores.csv", header = TRUE)

# Just calculate the grades
final_results <- calculate_letter_grades(score)

# Check our work
# print(final_results)

# Write to a CSV
write.csv(final_results, "final_letter_grades.csv", row.names = FALSE)
