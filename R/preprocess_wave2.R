# This script is used to preprocess the "raw" data from the second wave of the Lemanique Panel.
# Running it will result in a collection of .tsv files that can be ingested in the PostgreSQL
# database via the python ingestion script.

here::i_am("R/preprocess_wave2.R")

source(here::here("R/utils.R"))

# The "raw" data is stored on the LASUR drive. To be able to run the preprocessing, you will need
# (at least) read access to the drive.
folder <- "/Volumes/LASUR/common/LaSUR/06 - Recherche/Dossier de travail IT4R/"

if (!dir.exists(folder)) {
  stop(
    sprintf("Error: dir '%s' is not accessible. ", folder),
    "Read rights to the LASUR drive are necessary to preprocess the data. "
  )
}

wave2_data <- haven::read_sav(
  file.path(
    folder,
    "Enquête consommation/Data/Database from FORS/EPFL Panel LÇmanique Vague 2 _FINAL_EPFL.sav"
  )
)

# Most of the column names are messy, janitor helps clean them up automatically, forcing snake_case
# column names that are also limited to ASCII characters. Column names included "Localité_source".
names(wave2_data) <- janitor::make_clean_names(names(wave2_data), case = "snake", ascii = TRUE)

# The names of four columns are slightly different in the database, so we change them here
wave2_data <- wave2_data |>
  dplyr::rename(
    participant_code = idno,
    group = groupe,
    count_miss1 = countmiss1,
    count_miss2 = countmiss2,
    numero_insee = insee,
    numero_ofs = ofs
  )

# Columns to be included in the participants file. This is currently more liberal than the table
# definition
participants_colnames <- c(
  "participant_code", "pays", "group", "gp_age_source", "numero_insee", "numero_ofs", "weight",
  "titre_source", "cp_source", "localite_source"
)

# Columns to be included in the survey_metadata (also named survey_completion) table
survey_metadata_colnames <- c(
  "count_miss1", "count_miss2", "progress", "start_date", "end_date", "temps_minute"
)

# These are additional column names that are currently simply discarded for the MVP
# TODO: figure out with Panel team what to do with these additional variables.
extra_colnames <- c(
  "titre_actuel", "cp_actuel", "localite_actuel", "code_raison_contact_1_v2",
  "code_raison_contact_2_v2", "code_raison_contact_3_v2", "particip_avant_changements",
  "flag_troll", "particip_v2", "suppression_suite_v2", "flag_chgmt_pays", "mobile_ordi",
  "avant_chgmt_vet", "flag_chgmt_localite_v2"
)

# Prepare the questions and question_labels output
questions <- wave2_data |>
  dplyr::select(
    -tidyselect::all_of(participants_colnames),
    -tidyselect::all_of(survey_metadata_colnames),
    -tidyselect::all_of(extra_colnames)
  )

question_labels <- get_labels(questions, names_to = "question_code") |>
  dplyr::rename(label = name)
question_labels <- remove_escapeseqs(question_labels)

questions <- questions |>
  purrr::map(~ attr(.x, "label")) |>
  unlist() |>
  tibble::enframe(name = "question_code", value = "question_text")

questions <- questions |>
  dplyr::mutate(section_name = stringr::str_extract(question_code, "^[:alpha:]+(?=\\_)")) |>
  dplyr::mutate(section_name = dplyr::case_match(
    section_name,
    "ali" ~ "Alimentation",
    "con" ~ "Consommation",
    "end" ~ "Satisfaction",
    "ene" ~ "Energie",
    "equ" ~ "Equipement",
    "log" ~ "Logement",
    "rep" ~ "Rep",
    "temp" ~ "Temperature",
    "vot" ~ "Votation"
  ))

# Some of the questions include escape characters (\n) that need to be removed
questions <- remove_escapeseqs(questions)

# Prepare the sections output
sections <- questions |>
  dplyr::select(section_name) |>
  dplyr::group_by(section_name) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

# Prepare the participants and participant_labels output
participants <- wave2_data |>
  dplyr::select(tidyselect::all_of(participants_colnames))

participant_labels <- get_labels(participants)

participants <- participants |>
  zap_all() |>
  dplyr::mutate(numero_insee = strtoi(numero_insee, base = 10L))

# Prepare survey_completion data
survey_completion <- wave2_data |>
  dplyr::select(
    participant_code, count_miss1, count_miss2, progress, start_date, end_date, temps_minute,
    flag_troll
  )

survey_completion_labels <- get_labels(survey_completion)

survey_completion <- survey_completion |>
  zap_all()

# Prepare the responses table
responses <- wave2_data |>
  dplyr::select(
    -all_of(c(participants_colnames[-1], survey_metadata_colnames, extra_colnames))
  ) |>
  zap_all()

response_texts <- pivot_responses(
  responses,
  selection_type = "character", remove_NAs = TRUE, names_to = "question_code",
  values_to = "response_text"
) |>
  remove_escapeseqs()

response_values <- pivot_responses(
  responses,
  selection_type = "numeric", remove_NAs = TRUE, names_to = "question_code",
  values_to = "response_value"
)

responses <- response_values |>
  dplyr::bind_rows(response_texts)

# Write everything to file
output_folder <- "data/wave2"

if (!dir.exists(here::here(output_folder))) {
  dir.create(here::here(output_folder), recursive = TRUE)
}

output_folder <- here::here(output_folder)

readr::write_tsv(participant_labels, here::here(output_folder, "participant_labels.tsv"))
readr::write_tsv(participants, here::here(output_folder, "participants.tsv"))
readr::write_tsv(question_labels, here::here(output_folder, "question_labels.tsv"))
readr::write_tsv(questions, here::here(output_folder, "questions.tsv"))
readr::write_tsv(sections, here::here(output_folder, "sections.tsv"))
readr::write_tsv(survey_completion, here::here(output_folder, "survey_completion.tsv"))
readr::write_tsv(
  survey_completion_labels, here::here(output_folder, "survey_completion_labels.tsv")
)
readr::write_tsv(responses, here::here(output_folder, "responses.tsv"))
