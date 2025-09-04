

# SOURCE CODE to download latest files from Google Drive

# individual metadata
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/10mZu0PAjuIXtCZMSYFUFxa2j-2QxlDDlTQDfNTArf-8/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_individual_metadata.csv")
)

# biometric data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1FqUZHwe2aLJlw-SiwJtdORtuISnk7tIHlUIT_ZR7pJg/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_biometric_data.csv")
)

# gps tag deployment data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1V3QMfRHw1Y_yxKeV83CgV-dojuqG0CfIiIy2-D4lK4k/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_tag_deployment_data_gps.csv")
)

# radio tag deployment data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1_3v4_vnuXvshp3kjDC-U_hzY9-uMuGH4qpJnmmugbNQ/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_tag_deployment_data_radio.csv")
)

# resighting data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1v-U_m87Nza0P3fcQiEkwOaELwlbwTdaiepFyXUJQhpU/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_resighting_data.csv")
)

# egg collection data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/18JzojuDbjbuqsboZ5clSNgtX5iOXu1ikR_J8FQptzUA/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_egg_collection_data.csv")
)

# rearing data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1cpXhZwueeV5TqP4wANnWIxW6U8KSdmCvClKf4E-To2Y/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_rearing_data.csv")
)

# study month data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1cHDUD_W7jz0udB-dOt6KBTNC12zPKUdp37ZirjDRFIg/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_study_month.csv")
  
)