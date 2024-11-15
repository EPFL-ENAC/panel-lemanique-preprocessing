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


documentation <- function(data){
  output_path <- file.path(Sys.getenv("USERPROFILE"), "Documents/Lemanique Panel/Wave_1", "documentation_wave1.csv")
  
  question_codes <- colnames(wave1_data)
  question_text <- character(length(question_codes))
  question_labels <- character(length(question_codes))
  for (i in seq_along(question_codes)) {
    question_text[i] <- gsub(
      "[\r\n]", 
      " ", 
      ifelse(is.null(attr(get(question_codes[i], wave1_data), "label")), 
             NA_character_, 
             attr(get(question_codes[i], wave1_data), "label"))
    )
    
    question_labels[i] <- labels_to_string(data, i)
  }
  result <- tibble::tibble(question_code = question_codes, question_text = question_text, question_labels = question_labels)
  write.csv(result, output_path)
  
}

full_data <- function(data){
  output_path <- file.path(Sys.getenv("USERPROFILE"), "Documents/Lemanique Panel/Wave_1", "full_data_wave1.csv")
  write.csv(data, output_path)
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
  
  full_data(wave1_data)
}


main()