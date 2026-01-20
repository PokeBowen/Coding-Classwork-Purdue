# Clear the workspace
rm(list=ls())

# Reading a data frame from an Excel file
score=read.csv("scores_new.csv", header =T)

# Simply use apply and median with na.rm=TRUE to find our student medians
# First select the columns we want to use
hw_cols <- c("HW_1", "HW_2", "HW_3", "HW_4", "HW_5")
student_medians = apply(score[, hw_cols], 1, median, na.rm = TRUE)

# Add scores to data using column bind
score_with_medians = cbind(score, Median_HW = student_medians)

# Now we sort using order
score_with_medians_final <- score_with_medians[order(score_with_medians$Median_HW, decreasing = TRUE), ]

print(score_with_medians)
print(score_with_medians_final)