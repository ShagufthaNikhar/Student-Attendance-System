# List of student names
student_names <- c("Ankitha-CD006", "Samruddhi-CD084", "Sanjana-CD085", 
                   "Shaguftha-CD087", "Shreya-CD091", "Shreyas-CD090", 
                   "Shrisha-CD088", "Shivamani-CD089", "Sumanth-CD072", "Rida-CD076")

# Function to take attendance
take_attendance <- function() {
  attendance_status <- character(length(student_names))
  
  for (i in 1:length(student_names)) {
    cat(paste("Is", student_names[i], "present or absent (P/A): "))
    attendance_status[i] <- toupper(trimws(readline()))
    
    while (!(attendance_status[i] %in% c("P", "A"))) {
      cat("Invalid input. Enter 'P' for present or 'A' for absent.\n")
      cat(paste("Re-enter attendance for", student_names[i], "(P/A): "))
      attendance_status[i] <- toupper(trimws(readline()))
    }
  }
  
  data <- data.frame(Student = student_names, Attendance = attendance_status, stringsAsFactors = FALSE)
  return(data)
}

# Function to plot attendance summary
plot_attendance <- function(attendance_data) {
  attendance_summary <- table(attendance_data$Attendance)
  
  if (!"P" %in% names(attendance_summary)) attendance_summary["P"] <- 0
  if (!"A" %in% names(attendance_summary)) attendance_summary["A"] <- 0
  
  attendance_summary <- attendance_summary[c("P", "A")]
  
  barplot(attendance_summary,
          main = "Student Attendance",
          xlab = "Attendance Status",
          ylab = "Number of Students",
          col = c("green", "red"),
          names.arg = c("Present", "Absent"))
}

# Main Execution
attendance_system <- function() {
  attendance_data <- take_attendance()
  cat("\nAttendance Record:\n")
  print(attendance_data)
  plot_attendance(attendance_data)
}

# Run the system
attendance_system()
