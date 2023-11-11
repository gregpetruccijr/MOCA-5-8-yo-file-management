# MOCA data file management code for Clevenger and Pfeiffer
# Written by G. Petrucci Jr.
# Created: 10-25-2023
# Edited: 11-9-2023


rm(list = ls())

# Check if running Windows or Mac, updated 11-3-23----
system = Sys.info()[1]
if (system == 'Windows') {
  moca_drive = 'X:/MOCA'
  sirard_lab_one_drive <-
    'C:/Users/Greg J Petrucci Jr/OneDrive - University of Massachusetts/Sirard Lab'
  petrucci_personal_one_drive = 'C:/Users/Greg J Petrucci Jr/OneDrive - University of Massachusetts/BoxData/Petrucci_Personal/Univeristy of Massachusetts/University of Massachusetts'
  teams_dir <-
    'C:/Users/Greg J Petrucci Jr/OneDrive - University of Massachusetts'
  
  ## need to update this on home PC
} else {
  moca_drive = '/Volumes/moca/MOCA'
  sirard_lab_one_drive = '/Users/gregpetruccijr./Library/CloudStorage/OneDrive-UniversityofMassachusetts/Sirard Lab'
  petrucci_personal_one_drive <-
    '/Users/gregpetruccijr./Library/CloudStorage/OneDrive-UniversityofMassachusetts/BoxData/Petrucci_Personal/Univeristy of Massachusetts/University of Massachusetts'
}

# load necessary libraries----
library(tidyverse)
library(readxl)
library(PhysicalActivity)
library(lubridate)
library(beepr)
library(lme4)
library(psych)
library(irr)
library(CTT)
library(data.table)
library(tidyverse)
library(lubridate)
library(GGIR) # for ENMO metrics
library(slider) # needed for Crouter 2010 method
library(randomForest) # needed for Staudenmayer 2015 method
library(meta)
library(readxl)

# Set working directory----
sirard_lab_one_drive_path <-
  paste(sirard_lab_one_drive,
        'MOCA - R01/Data for Clevenger and Pfeiffer',
        sep = '/')
files <- list.files(sirard_lab_one_drive_path)
# Read in excel sheet with 5-8 year old MOCA participants----
MOCA_5_8_yo_participants <-
  read_xlsx(paste(
    sirard_lab_one_drive_path,
    "MOCA 5-8 yo participants.xlsx",
    sep = '/'
  ))
# String together participant and session columns so I can use to find accel file in directory
MOCA_5_8_yo_participants$`Participant ID` <-
  as.character(MOCA_5_8_yo_participants$`Participant ID`)
colnames(MOCA_5_8_yo_participants)[1] <- 'Participant_ID'
colnames(MOCA_5_8_yo_participants)[10] <- 'Comments2'
# get rid of extra comments cols
MOCA_5_8_yo_participants <-
  MOCA_5_8_yo_participants %>%  dplyr::select(c(Participant_ID:Comments, Comments2))
# unite ID and session cols
MOCA_5_8_yo_participants <-
  MOCA_5_8_yo_participants %>% unite(Participant_ID_Session,
                                     Participant_ID:Session,
                                     sep = '_',
                                     remove = F)
MOCA_5_8_yo_participants <-
  MOCA_5_8_yo_participants %>% relocate(Participant_ID_Session, .after =
                                          Session)

overall_n_row <- nrow(MOCA_5_8_yo_participants) # use this later?
# Build master df for all 5-8yo participants----
# New thought: do similar process as below, for each age group, row.bind after, and that will be the master_df
# master_df_MOCA_5_8_yo_participants <- data.frame(Participant_ID=MOCA_5_8_yo_participants$Participant_ID,
#                                                   Session=MOCA_5_8_yo_participants$Session,
#                                                   Participant_ID_Session=MOCA_5_8_yo_participants$Participant_ID_Session,
#                                                   Filepath=NA,
#                                                   Gender=MOCA_5_8_yo_participants$Gender,
#                                                   Age=MOCA_5_8_yo_participants$Age,
#                                                   Comment1=MOCA_5_8_yo_participants$Comments,
#                                                   Comment2=MOCA_5_8_yo_participants$Comments2
#                                                  ) # add hip+wrist and raw+1sec epoch?

# Five year old participants----
# 5 yo only df b/c they are in a separate directory from others
MOCA_5_yo_participants <-
  MOCA_5_8_yo_participants %>%  dplyr::filter(Age == 5)
# Change path and search MOCA drive for 5yo final output files
moca_drive_5yo_path <-
  paste(moca_drive, 'Final Output Files/MOCA_1.5to5.9', sep = '/')

all_MOCA_drive_1.5_5.9_yo_files <-
  list.files(moca_drive_5yo_path) # this list all 1.5 to 5.9 yo files,
# how does list.files() sort? I want the files in order that they appear on MOCA drive. Ans: alphabetical order

# subset all_MOCA_drive_1.5_5.9_yo_files to only include 5yo Participant_ID_Session's in MOCA_5_yo_participants
MOCA_5_yo_Participant_ID_Sessions <-
  c(MOCA_5_yo_participants$Participant_ID_Session) #strings to search for
all_MOCA_drive_1.5_5.9_yo_files_df <-
  data.frame(File_name = all_MOCA_drive_1.5_5.9_yo_files) # use for loop or str_which to get index location of each file on the MOCA drive?
# Order that the files appears in can vary. How deal w/ this?
MOCA_drive_5_yo_files_df <- all_MOCA_drive_1.5_5.9_yo_files_df %>%
  filter(str_detect(
    all_MOCA_drive_1.5_5.9_yo_files,
    paste(MOCA_5_yo_Participant_ID_Sessions, collapse = "|")
  ))

# Str_split Partic_Sess info from .rds files and make column that resembles Partic_Sess col in master df
n_5_files <- nrow(MOCA_drive_5_yo_files_df)

MOCA_5_yo_master_df <-
  data.frame(
    Participant_ID = rep(NA, times = n_5_files),
    Session = rep(NA, times = n_5_files),
    Wear_Location = rep(NA, times =
                          n_5_files),
    Data_Type = rep(NA, times =
                      n_5_files),
    File_name = rep(NA, times =
                      n_5_files),
    File_path = rep(NA, times =
                      n_5_files),
    i = 1:n_5_files
  )
# List of lists
MOCA_5_yo_file_name_pieces <-
  str_split(MOCA_drive_5_yo_files_df$File_name, '_')

# Loop to extract pieces and fill MOCA_drive_5_yo_file_pieces
for (i in 1:n_5_files) {
  MOCA_5_yo_master_df$Participant_ID[i] <-
    MOCA_5_yo_file_name_pieces[[i]][1]
  MOCA_5_yo_master_df$Session[i] <-
    MOCA_5_yo_file_name_pieces[[i]][2]
  #MOCA_drive_5_yo_file_pieces$Participant_Session <-  fill this with unite after the loop
  MOCA_5_yo_master_df$Wear_Location[i] <-
    MOCA_5_yo_file_name_pieces[[i]][3]
  MOCA_5_yo_master_df$Data_Type[i] <-
    MOCA_5_yo_file_name_pieces[[i]][4]
  MOCA_5_yo_master_df$File_name[i] <-
    MOCA_drive_5_yo_files_df$File_name[i]
  MOCA_5_yo_master_df$File_path[i] <-
    paste(moca_drive_5yo_path,
          MOCA_drive_5_yo_files_df$File_name[i],
          sep = '/')
}

# unite ID and session cols
MOCA_5_yo_master_df <-
  MOCA_5_yo_master_df %>% unite(Participant_ID_Session,
                                Participant_ID:Session,
                                sep = '_',
                                remove = F)
MOCA_5_yo_master_df <-
  MOCA_5_yo_master_df %>% relocate(Participant_ID_Session, .after =
                                     Session)

# inner join MOCA_5_yo_master_df (file names and paths) w/ MOCA_5_8_yo_participants (demographic data) for 5 yos
master_df_MOCA_5_yo_participants <-
  MOCA_5_yo_master_df %>% inner_join(MOCA_5_8_yo_participants, by =
                                       'Participant_ID_Session')
master_df_MOCA_5_yo_participants <-
  master_df_MOCA_5_yo_participants %>%  dplyr::select(!c(i, Participant_ID.y, Session.y))
master_df_MOCA_5_yo_participants <-
  master_df_MOCA_5_yo_participants %>%  dplyr::rename(Participant_ID = 'Participant_ID.x', Session =
                                                        'Session.x')
# Questions:
# 1) why are master_df_MOCA_5_yo_participants (n_row=204) and
#      MOCA_5_yo_participants (n_row=60) not the same length?
expected_MOCA_5_yo_participant_ID_Sessions <-
  unique(MOCA_5_yo_participants$Participant_ID_Session) # length=60
actual_MOCA_5_yo_participant_ID_Sessions <-
  unique(master_df_MOCA_5_yo_participants$Participant_ID_Session)

# determine missing in actual_MOCA_5_yo_participant_ID_Sessions from expected_MOCA_5_yo_participant_ID_Sessions (n=9)
missing_MOCA_5_yo_participant_df <-
  data.frame(
    Participant_ID_Session =
      setdiff(
        expected_MOCA_5_yo_participant_ID_Sessions,
        actual_MOCA_5_yo_participant_ID_Sessions
      ),
    Reason = NA
  )
# 327_C
missing_MOCA_5_yo_participant_df[1, 2] <-
  paste(MOCA_5_yo_participants[5, 6], MOCA_5_yo_participants[5, 7], sep =
          ',')
# 331_C, 331_E, 331_S
missing_MOCA_5_yo_participant_df[c(2:4), 2] <- 'Missing session'
# 336_C, 336_E, 336_S
missing_MOCA_5_yo_participant_df[c(5:7), 2] <- 'Missing session'
# 811_C, 811_E
missing_MOCA_5_yo_participant_df[c(8:9), 2] <- 'Missing session'

# 2) rename .rds files with "MOCA' prefix? Better for loading into RStudio?
# Copy .rds files to Teams for sharing

batch_rename_and_copy <- function(target_dir) {
  # List files in the source directory
  source_files <-
    master_df_MOCA_5_yo_participants$File_path
  
  # Loop through files, rename and copy
  for (i in 1:length(source_files)) {
    source_file <- source_files[i]
    new_name <-
      paste('MOCA',
            master_df_MOCA_5_yo_participants$File_name[i],
            sep = '_')
    
    # check to make sure source_file and new_name relate to the same file MOCA_5_yo_file_name_pieces[[i]][1]
    source_file_info <-
      str_split(source_file, 'MOCA_1.5to5.9/')[[1]][2]
    new_name_info <- str_split(new_name, 'MOCA_')[[1]][2]
    
    if (source_file_info == new_name_info) {
      # Construct the target file path
      target_file <- file.path(target_dir, new_name)
      # Rename and copy file
      file.copy(source_file, target_file)
      c(paste("Renamed and copied:",
              source_file,
              "to",
              target_file))
      beep()
    } else {
      c(paste(
        "Failed to rename and copy:",
        source_file,
        "to",
        target_file
      ))
      break
      print(i)
    } # end if statement
    
    
  } # end for loop
} # end fn

# Inputs to batch_rename_and_copy fn:
teams_dir_5_yo <-
  paste(teams_dir, 'Documents - MOCA 5-8 Year Old Data/Five yo files', sep =
          '/')

# Moving to MOCA 5-8 Year Old Data Microsoft Teams dir, filepath only works w/ GP Macbook rn
batch_rename_and_copy(target_dir = teams_dir_5_yo)
# check to make sure that all expected files made the move
moved_5_yo_files <- list.files(teams_dir_5_yo)
n_moved_5_yo_files <- length(moved_5_yo_files)

# Moved files list of lists
moved_5_yo_files_pieces <- str_split(moved_5_yo_files, 'MOCA_')
# add column for moved (1=Yes, 0=No)
master_df_MOCA_5_yo_participants$Moved <- NA

for (f in 1:n_moved_5_yo_files) {
  temp_inds <-
    str_which(master_df_MOCA_5_yo_participants$File_name,
              moved_5_yo_files_pieces[[f]][2])
  master_df_MOCA_5_yo_participants$Moved[f] <- temp_inds
}


# Size to eight year old participants----
# 6-8 yo only df b/c they are in a separate directory from others
MOCA_6_8_yo_participants <-
  MOCA_5_8_yo_participants %>%  dplyr::filter(Age > 5)
# Change path and search MOCA drive for 6-8yo final output files
moca_drive_6_8_yo_path <-
  paste(moca_drive, 'Final Output Files/MOCA_6to9.9', sep = '/')

all_MOCA_drive_6_9.9_yo_files <-
  list.files(moca_drive_6_8_yo_path) # this list all 6 to 9.9 yo files,
# how does list.files() sort? I want the files in order that they appear on MOCA drive. Ans: alphabetical order

# subset all_MOCA_drive_6_9.9_yo_files to only include 6-8yo Participant_ID_Session's in MOCA_6_8_yo_participants
MOCA_6_8_yo_Participant_ID_Sessions <-
  c(MOCA_6_8_yo_participants$Participant_ID_Session) #strings to search for
all_MOCA_drive_6_9.9_yo_files_df <-
  data.frame(File_name = all_MOCA_drive_6_9.9_yo_files) # use for loop or str_which to get index location of each file on the MOCA drive?
# Order that the files appears in can vary. How deal w/ this?
MOCA_drive_6_8_yo_files_df <-
  all_MOCA_drive_6_9.9_yo_files_df %>%
  filter(str_detect(
    all_MOCA_drive_6_9.9_yo_files,
    paste(MOCA_6_8_yo_Participant_ID_Sessions, collapse = "|")
  ))

# Str_split Partic_Sess info from .rds files and make column that resembles Partic_Sess col in master df
n_6_8_files <- nrow(MOCA_drive_6_8_yo_files_df)

MOCA_6_8_yo_master_df <-
  data.frame(
    Participant_ID = rep(NA, times = n_6_8_files),
    Session = rep(NA, times = n_6_8_files),
    Wear_Location = rep(NA, times =
                          n_6_8_files),
    Data_Type = rep(NA, times =
                      n_6_8_files),
    File_name = rep(NA, times =
                      n_6_8_files),
    File_path = rep(NA, times =
                      n_6_8_files),
    i = 1:n_6_8_files
  )
# List of lists
MOCA_6_8_yo_file_name_pieces <-
  str_split(MOCA_drive_6_8_yo_files_df$File_name, '_') #maybe should have included this in the loop but everything appears to be working correctly

# Loop to extract pieces and fill MOCA_drive_5_yo_file_pieces
for (i in 1:n_6_8_files) {
  MOCA_6_8_yo_master_df$Participant_ID[i] <-
    MOCA_6_8_yo_file_name_pieces[[i]][1]
  MOCA_6_8_yo_master_df$Session[i] <-
    MOCA_6_8_yo_file_name_pieces[[i]][2]
  #MOCA_drive_5_yo_file_pieces$Participant_Session <-  fill this with unite after the loop
  MOCA_6_8_yo_master_df$Wear_Location[i] <-
    MOCA_6_8_yo_file_name_pieces[[i]][3]
  MOCA_6_8_yo_master_df$Data_Type[i] <-
    MOCA_6_8_yo_file_name_pieces[[i]][4]
  MOCA_6_8_yo_master_df$File_name[i] <-
    MOCA_drive_6_8_yo_files_df$File_name[i]
  MOCA_6_8_yo_master_df$File_path[i] <-
    paste(moca_drive_6_8_yo_path,
          MOCA_drive_6_8_yo_files_df$File_name[i],
          sep = '/')
}

# unite ID and session cols
MOCA_6_8_yo_master_df <-
  MOCA_6_8_yo_master_df %>% unite(Participant_ID_Session,
                                  Participant_ID:Session,
                                  sep = '_',
                                  remove = F)
MOCA_6_8_yo_master_df <-
  MOCA_6_8_yo_master_df %>% relocate(Participant_ID_Session, .after =
                                       Session)
# inner join MOCA_6_8_yo_master_df (file names and paths) w/ MOCA_5_8_yo_participants (demographic data) for 6-8 yos
master_df_MOCA_6_8_yo_participants <-
  MOCA_6_8_yo_master_df %>% inner_join(MOCA_5_8_yo_participants, by =
                                         'Participant_ID_Session')
master_df_MOCA_6_8_yo_participants <-
  master_df_MOCA_6_8_yo_participants %>%  dplyr::select(!c(i, Participant_ID.y, Session.y))
master_df_MOCA_6_8_yo_participants <-
  master_df_MOCA_6_8_yo_participants %>%  dplyr::rename(Participant_ID = 'Participant_ID.x', Session =
                                                          'Session.x')
# 1) why are master_df_MOCA_6_8_yo_participants (n_row=476) and
#      MOCA_6_8_yo_participants (n_row=132) not the same length?
expected_MOCA_6_8_yo_participant_ID_Sessions <-
  unique(MOCA_6_8_yo_participants$Participant_ID_Session) # length=132
actual_MOCA_6_8_yo_participant_ID_Sessions <-
  unique(master_df_MOCA_6_8_yo_participants$Participant_ID_Session)

# determine missing in actual_MOCA_6_8_yo_participant_ID_Sessions from expected_MOCA_6_8_yo_participant_ID_Sessions (n=13)
missing_MOCA_6_8_yo_participant_df <-
  data.frame(
    Participant_ID_Session =
      setdiff(
        expected_MOCA_6_8_yo_participant_ID_Sessions,
        actual_MOCA_6_8_yo_participant_ID_Sessions
      ),
    Reason = NA
  )
# 317_C
missing_MOCA_6_8_yo_participant_df[1, 2] <-
  MOCA_6_8_yo_participants[53, 6]
# 317_S
missing_MOCA_6_8_yo_participant_df[2, 2] <-
  MOCA_6_8_yo_participants[56, 6]
# 324_C,_E & _S
missing_MOCA_6_8_yo_participant_df[c(3:5), 2] <- 'Missing session'
# 285_C & _S
missing_MOCA_6_8_yo_participant_df[c(6:7), 2] <- 'Missing session'
# 289_C,_E & _S
missing_MOCA_6_8_yo_participant_df[c(8:10), 2] <-
  'Missing session'
# 294_C,_H & _S
missing_MOCA_6_8_yo_participant_df[c(11:13), 2] <-
  'Missing session'

# 2) rename .rds files with "MOCA' prefix? Better for loading into RStudio?
# Copy .rds files to Teams for sharing

batch_rename_and_copy_6_8_yo_files <-
  function(target_dir, new_names) {
    # List files in the source directory
    source_files_6_8_yo <-
      master_df_MOCA_6_8_yo_participants$File_path
    
    # Loop through files, rename and copy
    for (i in 1:length(source_files_6_8_yo)) {
      source_file_6_8 <- source_files_6_8_yo[i]
      new_name_6_8 <-
        paste('MOCA',
              master_df_MOCA_6_8_yo_participants$File_name[i],
              sep = '_')
      
      # check to make sure source_file and new_name relate to the same file MOCA_5_yo_file_name_pieces[[i]][1]
      source_file_6_8_info <-
        str_split(source_file_6_8, 'MOCA_6to9.9/')[[1]][2]
      new_name_6_8_info <- str_split(new_name_6_8, 'MOCA_')[[1]][2]
      
      if (source_file_6_8_info == new_name_6_8_info) {
        # Construct the target file path
        target_file_6_8 <- file.path(target_dir, new_name_6_8)
        # Rename and copy file
        file.copy(source_file_6_8, target_file_6_8)
        c(paste(
          "Renamed and copied:",
          source_file_6_8,
          "to",
          target_file_6_8
        ))
        beep()
        print(i)
      } else {
        c(paste(
          "Failed to rename and copy:",
          source_file_6_8,
          "to",
          target_file_6_8
        ))
        break
        beep(sound = 7)
        print(i)
      } # end if statement
      
      
    } # end for loop
  } # end fn

# Inputs to batch_rename_and_copy fn:
teams_dir_6_8_yo <-
  paste(teams_dir,
        'Documents - MOCA 5-8 Year Old Data/Six to eight yo files',
        sep = '/')

# Moving to MOCA 6-8 Year Old Data Microsoft Teams dir, filepath only works w/ GP Macbook rn
batch_rename_and_copy_6_8_yo_files(target_dir = teams_dir_6_8_yo)
# check to make sure that all expected files made the move
moved_6_8_yo_files <- list.files(teams_dir_6_8_yo)
n_moved_6_8_yo_files <- length(moved_6_8_yo_files)

# Moved files list of lists
moved_6_8_yo_files_pieces <-
  str_split(moved_6_8_yo_files, 'MOCA_')
# add column for moved (1=Yes, 0=No)
master_df_MOCA_6_8_yo_participants$Moved <- NA

for (f in 1:n_moved_6_8_yo_files) {
  temp_inds <-
    str_which(master_df_MOCA_6_8_yo_participants$File_name,
              moved_6_8_yo_files_pieces[[f]][2])
  master_df_MOCA_6_8_yo_participants$Moved[f] <- temp_inds
}



# Final cleaning----
master_df_MOCA_5_8_yo_participants <-
  master_df_MOCA_5_yo_participants %>% bind_rows(master_df_MOCA_6_8_yo_participants)
# write this to .csv for my own use?
current_wd_files <- list.files(sirard_lab_one_drive_path)
if (!file.exists(
  paste(
    sirard_lab_one_drive_path,
    'master_df_MOCA_5_8_yo_participants.csv',
    sep = '/'
  )
)) {
  write.csv(master_df_MOCA_5_8_yo_participants,
            'master_df_MOCA_5_8_yo_participants.csv')
} else {
  print('File already exists')
}
# write MOCA_5_8_yo_participants to csv in Teams dir
outer_teams_dir <-
  paste(teams_dir, 'Documents - MOCA 5-8 Year Old Data', sep = '/')
if (!file.exists(paste(outer_teams_dir, 'MOCA_5_8_yo_participants.csv', sep =
                       '/'))) {
  write.csv(
    MOCA_5_8_yo_participants,
    paste(outer_teams_dir, 'MOCA_5_8_yo_participants.csv', sep = '/')
  )
} else {
  print('File already exists')
}

# read in data collection info files from MOCA drive to get demo and anthro data
demo_anthro_data <-
  read_xlsx(paste(moca_drive, 'MOCA Data Collection Info 5_20_20.xlsx', sep =
                    '/'),
            sheet = 2)
demo_anthro_data_only <-
  demo_anthro_data %>% dplyr::select(!names(demo_anthro_data)[2:4]) %>%
  dplyr::rename(Participant_ID = 'Participant ID')
demo_anthro_data_only$Participant_ID <-
  as.character(demo_anthro_data_only$Participant_ID)
# join demo_anthro_data_only to MOCA_5_8_yo_participants
MOCA_5_8_yo_participants_anthro <-
  MOCA_5_8_yo_participants %>% left_join(demo_anthro_data_only, by = 'Participant_ID') %>%
  dplyr::select(!'Gender.y') %>%
  dplyr::rename(Gender = 'Gender.x')
# write.csv
if (!file.exists(paste(outer_teams_dir, 'MOCA_5_8_yo_participants_anthro.csv', sep =
                       '/'))) {
  write.csv(
    MOCA_5_8_yo_participants_anthro,
    paste(
      outer_teams_dir,
      'MOCA_5_8_yo_participants_anthro.csv',
      sep = '/'
    )
  )
} else {
  print('File already exists')
}

# tidy MOCA_5_8_yo_participants_anthro. Don't want: Session, Participant_ID_Session, Comments, Comments2
library(janitor) #used to clean column names
MOCA_5_8_yo_participants_anthro <- MOCA_5_8_yo_participants_anthro %>% 
                                      janitor::clean_names()
MOCA_5_8_yo_participants_anthro <- MOCA_5_8_yo_participants_anthro %>% 
                                    dplyr::rename(hispanic_or_latino=hispanic_or_lati_false) %>% 
                                      dplyr::rename(refuse_consent=refuse_dont_kfals_ew)
# god damn tidyverse
tidy_MOCA_5_8_yo_participants_anthro <- MOCA_5_8_yo_participants_anthro %>%
                                            gather(variable, value, -participant_id,-session,
                                                   -participant_id_session, -comments, -comments2) %>% 
                                              dplyr::select(participant_id, variable, value) %>% 
                                                group_by(participant_id) %>% 
                                                  reframe(variable=variable, value=value) %>% 
                                                  dplyr::distinct() %>% 
                                                  group_by(participant_id) %>% 
                                                  spread(key=variable, value=value)

# write.csv
if (!file.exists(paste(outer_teams_dir, 'tidy_MOCA_5_8_yo_participants_anthro.csv', sep =
                       '/'))) {
  write.csv(
    tidy_MOCA_5_8_yo_participants_anthro,
    paste(
      outer_teams_dir,
      'tidy_MOCA_5_8_yo_participants_anthro.csv',
      sep = '/'
    )
  )
} else {
  print('File already exists')
}

