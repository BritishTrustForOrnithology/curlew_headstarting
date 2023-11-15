
########################################################################
#
#   Functions analyses
#
#   --SOURCE ALL FUNCTION SCRIPTS
#             
########################################################################

# all function scripts (apart from run_all_functions.R)
all_files <- dir(paste(codewd, "functions", sep="/")) %>% .[-grep("run_all_functions.R", .)]

# loop through and source all function scripts
for (i_file in all_files) {
  source(paste(codewd, "functions", i_file, sep="/"))
}
