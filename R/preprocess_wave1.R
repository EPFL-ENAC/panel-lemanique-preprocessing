source(here::here("R/utils.R"))

#' Creates a string from le labels of a question
#'
#' @param data SPSS dataset
#' @param index int representing index if the question
#' 
#' @return string

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

#' gets section name for a question
#' @param question_number number of the question : Q10 => 10
#' 
#' @return string containing the section name for the question

get_section <- function (question_number){
  sections_start <- c(0, 12, 30, 37, 40, 61, 75, 77, 81, 85, 90, 107, 109, 115, 123, 129)
  sections <- c("Permis, véhicules et abbonements", "Multi-résidence", "Pratiques et moyens de déplacements", "Loop Leman Express", "Mobilités pour le travail et études", "Stationnement et financement", "Subventionnement abo", "Mobilités hors du travail", 
                "excursions", "Séjours", "Opinions", "Variables sociodémographiques 1/2", "Variabes sociodémographiques 2/2", "Variables sociodémographiques_ménage", "Opt-out")
  for (i in 1:length(sections_start)){
    if(question_number < sections_start[i]) return(sections[i])
  }
}

#' Outputs csv documentation for a dataset incluing question_code, question_label and question_labels
#' 
#' @param data SPSS dataset

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

#' Outputs whole dataset without labels in a csv file
#' 
#' @param data SPSS dataset

full_data <- function(data) {
  output_path <- file.path(Sys.getenv("USERPROFILE"), "Documents/Lemanique Panel/Wave_1", "full_data_wave1.csv")
  write.csv(data, output_path)
}

#' preprocess participants data
#' 
#' @param data SPSS dataset

write_files_participants <- function(data){
  output_path <- "data/wave1/paricipants.tsv"

  columns <- c("IDNO","Pays","Groupe","GP_Age_source","Numéro_INSEE","Numéro_OFS","CP_source","Localité_source")
  NAs <- rep(NA_character_,nrow(data["IDNO"]))
  
  participants_code <- data$IDNO
  pays <- data$Pays
  group <- data$Groupe
  gp_age_source <- data$GP_Age_source
  numero_INSEE <- data$Numéro_INSEE
  numero_OFS <- data$Numéro_OFS
  weight <- NAs 
  titre_source <- NAs 
  cp_source <- data$CP_source
  localite_source <- data$Localité_source
  
  result <- tibble::tibble(
                           participants_code =  participants_code, 
                           pays = pays,
                           group = group,
                           gp_age_source = gp_age_source,
                           numero_INSEE = numero_INSEE,
                           numero_OFS = numero_OFS,
                           weight = weight,
                           titre_source = titre_source,
                           cp_source = cp_source,
                           localite_source = localite_source
                           )
  missing_names <- c("weight", "titre_source")
  
  readr::write_tsv(result, output_path)
  
  something_label(data, columns, "participant")
}

#' preprocess question data
#' @param data SPSS dataset

write_files_questions <- function(data){
  output_path <- "data/wave1/questions.tsv"
  
  columns <- c()
  question_text <- c()
  section_name <- c()
  
  for(i in colnames(data)){
    if(stringr::str_detect(i, "^Q\\d*.*$")){
      columns <- c(columns, i)
      question_text <- c(question_text, gsub("\n","",attr(get(i, data), "label")))
      section_name <- c(section_name, get_section(as.integer(stringr::str_extract(i,"\\d+"))))
    }
  }
  result <- tibble::tibble(
    question_code = columns,
    question_text = question_text,
    section_name =  section_name
  )
  
  readr::write_tsv(result, output_path)
  
  something_label(data, columns, "questions")
  
}

#' preprocess survey completion data
#' 
#' @param data SPSS dataset

write_files_survey_completion <- function(data){
  output_path <- "data/wave1/survey_completion.tsv"
  
  columns <- c("IDNO")
  NAs <- rep(NA_character_,nrow(data["IDNO"]))
  
  participant_code <- data$IDNO
  
  result <- tibble::tibble(
                          participant_code = participant_code,
                          count_miss1 = NAs,
                          count_miss2 = NAs,
                          progress = NAs,
                          start_date = NAs,
                          end_date = NAs,
                          temps_minute = NAs,
                          flag_troll = NAs
  )
  missing_names <- c("count_miss1", "count_miss2", "progress", "start_date", "end_date", "temps_minute", "flag_troll")
  
  readr::write_tsv(result, output_path)
  
  something_label(data, columns, "survey_completion")
}


#' outputs tsv file containing questions an their label
#' 
#' @param data SPSS dataset
#' @param cols list of all columns to be processed
#' @param name string containing name of the output tsv file

something_label <- function(data, cols, name){
  output_path <- paste("data/wave1/",name,"_labels.tsv",sep="")
  
  cols_selected <- data |>
    dplyr::select(tidyselect::all_of(cols))
  
  result <- get_labels(cols_selected)
  readr::write_tsv(result, output_path)
}


#' writes tsv file with all the section names inside
write_file_section <- function(){
  output_path <- "data/wave1/sections.tsv"
  
  result <- tibble::tibble(
    section_name = c("Permis, véhicules et abbonements", "Multi-résidence", "Pratiques et moyens de déplacements", "Loop Leman Express", "Mobilités pour le travail et études", "Stationnement et financement", "Subventionnement abo", "Mobilités hors du travail", 
                     "excursions", "Séjours", "Opinions", "Variables sociodémographiques 1/2", "Variabes sociodémographiques 2/2", "Variables sociodémographiques_ménage", "Opt-out")
  )
  
  readr::write_tsv(result, output_path)
}

#' reads dataset and calls specific documentation functions

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
  
  #documentation(wave1_data)
  
  #full_data(wave1_data)
  
  if (!dir.exists(here::here("data/wave1"))) {
    dir.create(here::here("data/wave1"), recursive = TRUE)
  }
  
  
  write_files_participants(wave1_data)
  write_files_survey_completion(wave1_data)
  write_files_questions(wave1_data)
  write_file_section()
}


main()