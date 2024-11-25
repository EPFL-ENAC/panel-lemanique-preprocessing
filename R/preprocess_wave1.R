# This script is used to preprocess the "raw" data from the first wave of the Lemanique Panel.
# Running it will result in a collection of .tsv files that can be ingested in the PostgreSQL
# database via the python ingestion script.

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

  columns <- c("participant_code","pays","pays","gp_age_source","numero_insee","numero_ofs","CP_source","Localité_source")
  NAs <- rep(NA_character_,nrow(data["participant_code"]))
  
  participants_code <- data$participant_code
  pays <- data$pays
  group <- data$group
  gp_age_source <- data$gp_age_source
  numero_INSEE <- data$numero_insee
  numero_OFS <- data$numero_ofs
  weight <- NAs 
  titre_source <- NAs 
  cp_source <- data$CP_source
  localite_source <- data$Localité_source
  
  result <- tibble::tibble(
                           participants_code =  participants_code, 
                           pays = pays,
                           group = group,
                           gp_age_source = gp_age_source,
                           numero_insee = numero_INSEE,
                           numero_ofs = numero_OFS,
                           weight = weight,
                           titre_source = titre_source,
                           cp_source = cp_source,
                           localite_source = localite_source
                           )
  missing_names <- c("weight", "titre_source")
  
  readr::write_tsv(result, output_path)
  
  write_label_file(data, columns, "participant")
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
  
  write_label_file(data, columns, "questions")
  
}

#' preprocess survey completion data
#' 
#' @param data SPSS dataset

write_files_survey_completion <- function(data){
  output_path <- "data/wave1/survey_completion.tsv"
  
  columns <- c("participant_code")
  NAs <- rep(NA_character_,nrow(data["participant_code"]))
  
  participant_code <- data$participant_code
  
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
  
  write_label_file(data, columns, "survey_completion")
}


#' outputs tsv file containing questions an their label
#' 
#' @param data SPSS dataset
#' @param cols list of all columns to be processed
#' @param name string containing name of the output tsv file

write_label_file <- function(data, cols, name){
  output_path <- paste("data/wave1/",name,"_labels.tsv",sep="")
  
  cols_selected <- data |>
    dplyr::select(tidyselect::all_of(cols))
  
  result <- get_labels(cols_selected)
  
  if(name == "questions"){
    result <- result |>
      dplyr::rename(
        question_code = variable_name,
        label = name
      )
  }
  
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

#' preprocess answers of all participants
#' 
#' @param data SPSS dataset
write_file_answers <- function(data){
  output_path <- "data/wave1/responces.tsv"
  
  participants_colnames <- c("participant_code","pays","pays","gp_age_source","numero_insee","numero_ofs","CP_source","Localité_source")
  extra_colnames <- c("wgt_socio",	"wgt_cant_trim",	"wgt_agg_trim",	"wgt_cant_trim_gps",	"wgt_agg_trim_gps",	"wgt_cant_trim_v2",	"wgt_agg_trim_v2")
  responses <- data |>
    dplyr::select(
      -all_of(c(participants_colnames[-1], extra_colnames))
    ) |>
    zap_all()
  
  response_texts <- pivot_responses(
    responses,
    id_column = "participant_code",
    selection_type = "character", remove_nas = TRUE, names_to = "question_code",
    values_to = "response_text"
  ) |>
    remove_escapeseqs()
  
  response_values <- pivot_responses(
    responses,
    id_column = "participant_code",
    selection_type = "numeric", remove_NAs = TRUE, names_to = "question_code",
    values_to = "response_value"
  )
  
  responses <- response_values |>
    dplyr::bind_rows(response_texts)
  
  readr::write_tsv(responses, output_path)
  
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
  
  wave1_data <- wave1_data |>
    dplyr::rename(
      participant_code = IDNO,
      pays = Pays,
      group = Groupe,
      gp_age_source = GP_Age_source,
      numero_insee = Numéro_INSEE,
      numero_ofs = Numéro_OFS
    )
  
  if (!dir.exists(here::here("data/wave1"))) {
    dir.create(here::here("data/wave1"), recursive = TRUE)
  }
  
  #documentation(wave1_data)
  #full_data(wave1_data)
  
  write_files_participants(wave1_data)
  write_files_survey_completion(wave1_data)
  write_files_questions(wave1_data)
  write_file_section()
  write_file_answers(wave1_data)
}


main()

#' TODO(Q)
#' 
#' - fix col names in survey_completion_labels, (survey_completion), (section), question_labels(V), (question) => placement or name or both lol
#' - fix (or not) double col bc of coma in section name
#' - check if responce_text in responces.tsv works as intended