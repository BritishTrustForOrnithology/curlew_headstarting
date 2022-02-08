#######################################################################################
#
#
#   SOURCE CODE:
#   
#   Header code to install and load packages, set directory paths, print session info
#   when WITHIN a .Rproj
#
#######################################################################################



# ======================  SET SEED & PROJECT DETAILS  ====================

set.seed(seed_number)

# name of project folder
project_name <- project_details$project_name

# output and workspaces sub-directory (e.g. for original analysis, revisions, accepted versions, etc)
output_version_date <- project_details$output_version_date
workspace_version_date <- project_details$workspace_version_date



# =================================  INSTALL and LOAD PACKAGES =================================


list.of.packages <- package_details

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only=TRUE)



# =================================  SET DIRECTORY PATHS  ================================

# ---- Parent working directory, depending on workstation (usually the Git folder) ----
parentwd <- "../" # one level up from project folder is Git folder

# ---- Create sub-directories ----

projectwd <- getwd() # the .Rproj working directory
codewd <- "./code"
datawd <- "./data"

if (is.null(output_version_date)) {
  top_outputwd <- NULL
  outputwd <- "./output"
} else {
  top_outputwd <- "./output"
  outputwd <- file.path("./output", output_version_date)
}

if (is.null(output_version_date)) {
  top_workspacewd <- NULL
  workspacewd <- "./workspaces"
} else {
  top_workspacewd <- "./workspaces"
  workspacewd <- file.path("./workspaces", workspace_version_date)
}

# create sub-directories if they don't exist (invisible() suppresses [[i]] NULL output from lapply)
dirs <- c(codewd, datawd, top_outputwd, top_workspacewd, outputwd, workspacewd)
lapply(dirs, function(x) {
  if (!dir.exists(x)) dir.create(x)
}) %>% invisible

options(digits=6)


# ================================  SESSION INFO  ===============================

# creates new session info .txt, stored in project working directory
# session info is captured in the most recent run of the code
# if using version control, then can see what the session_info was on each day you ran some code

session_info_file <- "session_info.txt"

if (!file.exists(session_info_file)) {  # if file does not exist
  
  #create file
  file.create(session_info_file)
  
  # today's date
  today_date <- format(Sys.Date(), "%d-%b-%Y")
  
  # add session info text to file
  sink(session_info_file)
  cat("\n# =============== Session info ==================\n", sep="\n")
  cat("# ----  Date: ", today_date, " --------\n\n")
  print(sessionInfo())
  sink()
  
} else {  # if file does exist
  
  # get last modified timestamp
  file_date_last_modified <- file.info(session_info_file)$mtime %>% format(., "%d-%b-%Y")
  
  # today's date
  today_date <- format(Sys.Date(), "%d-%b-%Y")
  
  # update session_text only if the file was last modified previous to today's date
  if(!file_date_last_modified %in% today_date) {
    sink(session_info_file)
    cat("\n# =============== Session info ==================\n", sep="\n")
    cat("# ----  Date: ", today_date, " --------\n\n")
    print(sessionInfo())
    sink()
  }
}

