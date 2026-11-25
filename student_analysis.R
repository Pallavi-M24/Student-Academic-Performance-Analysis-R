# ============================================
# MINI PROJECT: STUDENT ACADEMIC PERFORMANCE
# DATA ANALYSIS USING R
# ============================================


# STEP 1: CREATE THE DATASET

students <- data.frame(
  Student = paste("Student", 1:10),
  
  Mathematics = c(85, 72, 90, 65, 78, 88, 56, 95, 74, 82),
  
  Statistics = c(80, 75, 92, 68, 79, 85, 60, 96, 70, 84),
  
  Computer_Science = c(92, 80, 95, 70, 85, 90, 62, 98, 78, 88),
  
  English = c(78, 82, 88, 72, 80, 86, 65, 91, 76, 84),
  
  Electronics = c(83, 70, 94, 66, 77, 89, 58, 93, 72, 81)
)


# SAVE DATASET AS CSV

write.csv(students, "student_marks.csv", row.names = FALSE)


# STEP 2: CALCULATE TOTAL AND AVERAGE MARKS

subjects <- students[, 2:6]

students$Total <- rowSums(subjects)

students$Average <- students$Total / 5

cat("\n========== STUDENT DATASET ==========\n")

print(students)


# STEP 3: STATISTICAL ANALYSIS

cat("\n========== STATISTICAL ANALYSIS ==========\n")

cat("\nMEAN:\n")
print(round(sapply(subjects, mean), 2))

cat("\nMEDIAN:\n")
print(round(sapply(subjects, median), 2))


# FUNCTION TO CALCULATE MODE

find_mode <- function(x) {
  
  frequency <- table(x)
  
  highest_frequency <- max(frequency)
  
  modes <- names(frequency)[frequency == highest_frequency]
  
  if (highest_frequency == 1 || length(modes) > 1) {
    return("No unique mode")
  } else {
    return(modes)
  }
}


cat("\nMODE:\n")
print(sapply(subjects, find_mode))


cat("\nVARIANCE:\n")
print(round(sapply(subjects, var), 2))


cat("\nSTANDARD DEVIATION:\n")
print(round(sapply(subjects, sd), 2))


# STEP 4: IDENTIFY HIGHEST AND LOWEST SCORING STUDENTS

highest_student <- which.max(students$Total)

lowest_student <- which.min(students$Total)

cat("\n========== STUDENT PERFORMANCE ==========\n")

cat("\nHighest Scoring Student:\n")
print(students[highest_student, c("Student", "Total", "Average")])

cat("\nLowest Scoring Student:\n")
print(students[lowest_student, c("Student", "Total", "Average")])


# STEP 5: SUBJECT-WISE AVERAGE

subject_average <- colMeans(subjects)

cat("\n========== SUBJECT-WISE AVERAGE ==========\n")

print(round(subject_average, 2))


# STEP 6: CREATE OUTPUT FOLDER

if (!dir.exists("output")) {
  dir.create("output")
}


# STEP 7: GRAPH 1 - SUBJECT-WISE AVERAGE MARKS

png("output/subject_averages.png",
    width = 1000, height = 700)

barplot(
  subject_average,
  main = "Average Marks in Each Subject",
  xlab = "Subjects",
  ylab = "Average Marks",
  ylim = c(0, 100),
  col = "skyblue",
  border = "black"
)

dev.off()


# STEP 8: GRAPH 2 - TOTAL MARKS OF STUDENTS

png("output/student_totals.png",
    width = 1000, height = 700)

barplot(
  students$Total,
  names.arg = students$Student,
  main = "Total Marks of Students",
  xlab = "Students",
  ylab = "Total Marks",
  col = "lightgreen",
  border = "black",
  las = 2
)

dev.off()


# STEP 9: GRAPH 3 - DISTRIBUTION OF SUBJECT MARKS

png("output/marks_distribution.png",
    width = 1000, height = 700)

boxplot(
  subjects,
  main = "Distribution of Marks Across Subjects",
  xlab = "Subjects",
  ylab = "Marks",
  col = "lightpink"
)

dev.off()


# STEP 10: FINAL INTERPRETATION

cat("\n========== INTERPRETATION ==========\n")

cat("1. Computer Science has the highest subject average.\n")

cat("2. Student 8 has obtained the highest total marks.\n")

cat("3. Student 7 has obtained the lowest total marks.\n")

cat("4. The mode is not unique for the selected subject marks.\n")

cat("5. The boxplot helps compare the distribution of marks.\n")

cat("6. The graphs provide a visual comparison of student performance.\n")


cat("\nPROJECT EXECUTION COMPLETED SUCCESSFULLY!\n")