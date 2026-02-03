# We create our function, basically copied and pasted

calculate_letter_grades <- function(score) {
  #unsubmitted hw/quiz/exam/project counts as 0 score
  score[is.na(score)] <- 0
  
  #Calculate component-wise scores according to the syllabus
  hw.component = (score$HW_maxscore_100/100)*30
  midterm.component = (score$Midterm_maxscore_50/50)*25
  final.component = (score$Final_maxscore_50/50)*25
  project.component=(score$Project_maxscore_100/100)*20
  
  #Calculate total score
  total = cbind(hw.component, midterm.component, final.component, project.component)
  totalscore = apply(total,1,sum)
  
  #Assign grades
  grades = rep(NA,nrow(score))
  
  for ( i in 1:nrow(score)){
    if (totalscore[i]< 59) grades[i] = "F"
    else if ((totalscore[i]>= 60) & (totalscore [i] <70)) grades[i] = "D"
    else if ((totalscore[i]>= 70) & (totalscore [i] < 80)) grades[i] = "C"
    else if ((totalscore[i]>= 80) & (totalscore [i] < 90)) grades[i] = "B"
    else if ((totalscore[i]>= 90) & (totalscore [i] <100)) grades[i] = "A"
    else grades[i] = "ERROR"
  }
  
  finalscore = cbind(as.character(score[,1]), totalscore, grades)
  finalscore
}