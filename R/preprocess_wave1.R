source(here::here("R/utils.R"))

labels_to_string <- function(data, index){
  tryCatch(
    {
      obj <- get_labels(data[index])
      res = ""
      for (i in 1:length(row(obj[,"name"]))){
        temp <- paste(obj[i,"value"], obj[i, "name"], sep=":")
        if(i == 1){
          res <- temp
        }else{
          res <- paste(res, temp, sep = ", ")
        }
      }
      return(res)
    },
    error=function(e){
      return("")
    }
  )
}

write_cell <- function(file_path, value) {
  # Get the current column position and total columns
  current_column <- getOption("current_column")
  total_columns <- getOption("total_columns")
  
  # Choose separator based on whether it's the last cell in the row
  sep <- if (current_column == total_columns) "\n" else ";"
  
  # Write the cell value with the appropriate separator
  cat(value, sep, file = file_path, append = TRUE)
  
  # Update the column counter
  if (current_column == total_columns) {
    options(current_column = 1)  # Reset to the first column at the end of a row
  } else {
    options(current_column = current_column + 1)  # Move to the next column
  }
}

documentation <- function(data){
  output_path <- file.path(Sys.getenv("USERPROFILE"), "Documents/Lemanique Panel/Wave_1", "documentation_wave1.csv")
  
  write.table(t(c("question_code", "question_text", "question_labels")),
              file = output_path, 
              sep = ";", 
              col.names = FALSE,
              row.names = FALSE, 
              append = TRUE)
  options(current_column = 1)
  options(total_columns = 3)
  
  #reads raw data and writes parsed data
  for (i in 1:ncol(data)){
    question_code <- colnames(data[i])
    write_cell(output_path, question_code)
    write_cell(output_path, gsub("[\r\n]", " ", attr(get(question_code, data), "label")))
    write_cell(output_path, labels_to_string(data, i))
  }
}


full_data <- function(data){
  
  output_path <- file.path(Sys.getenv("USERPROFILE"), "Documents/Lemanique Panel/Wave_1", "full_data_wave1.csv")
  
  
  
  write.table(t(colnames(data)),
              file = output_path, 
              sep = ";", 
              col.names = FALSE,
              row.names = FALSE, 
              append = TRUE)
  options(current_column = 1)
  options(total_columns = length(colnames(data)))
  options(total_rows = length(rownames(data)))
  
  for (i in 1:length(rownames(data))){
    for (ii in 1:length(colnames(data))){
      write_cell(output_path, t(data[i,ii]))
    }
  }
  
}

main <- function(){
  
  #Gets raw data => needs read access to the LAUSR drive
folder <- "//enac1files.epfl.ch/LASUR/common/LaSUR/06 - Recherche/Dossier de travail IT4R/Enquête mobilité"
  file <- "EPFL_vague1_pond_clean_240319.sav"
  
  if (!dir.exists(folder)) {
    stop(
      sprintf("Error: dir '%s' is not accessible. ", folder),
      "Read rights to the LASUR drive are necessary to preprocess the data. "
    )
  }
  
  wave1_data <- haven::read_sav(file.path(folder, file))
  
  documentation(wave1_data)
  
  #full_data(wave1_data)
}


main()