
# Clear the workspace
rm(list=ls())

# Reading a data frame from an Excel file
score=read.csv("scores_new.csv", header =T)

# Simply use apply and median with na.rm=TRUE to find our homework medians
# First select the columns we want to use
hw_cols <- c("HW_1", "HW_2", "HW_3", "HW_4", "HW_5")
hw_medians = apply(score[, hw_cols], 2, median, na.rm = TRUE)
print(hw_medians)



# Let us only look at Alice's row
alice_row = score[score$First.Name == "Alice", ]
# Calculate median (using same hw columns as before)
alice_median = median(as.numeric(alice_row[, hw_cols]), na.rm = TRUE)
print(alice_median)

