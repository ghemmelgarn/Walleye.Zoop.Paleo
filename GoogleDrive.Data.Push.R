#' You'll need to have access to the google drive folder where these data are
#' stored before running this script. Use this script to update the shared
#' folder with new input or output data as needed.
#'
#' WARNING: THIS SCRIPT WILL OVERWRITE THE SHARED DATA FOLDER WITH YOUR LOCAL COPY
#'

  # load library
  library(googledrive)
  library(purrr)
  library(data.table)

  #drive authorization request

  # link to google drive & force new token auth (can change to 1 to use existing account)
  drive_auth(email = FALSE)


  # store folder url as a file ID
  # identify this folder on Drive
  # let googledrive know this is a file ID or URL, as opposed to file name
    outfolder <- drive_get(as_id("https://drive.google.com/drive/folders/1SpUY4K8Ytg0iNth-9Z01eoUqcJl0lZH2"))
    #pushes to "Processed Data" in google drive - manually put in 'To Pull to R" folder if you want to pull the new version into input folder

  # update the drive folder with your local folder version:
  #identify a list of files in your current folder
  # identify the csv files in each folder
  list.files("Data/Output", recursive = T)

  # store these for use as a list of files to update
  outputfolder_files <- data.table(list.files("Data/Output/", pattern = "*.csv", recursive = T))


  # export/upload them, overwriting the currrent shared drive data folder:
  walk(outputfolder_files$V1, ~ drive_upload(paste("Data/Output", .x, sep = "/"), path = outfolder, name = .x, overwrite = T, type = "csv" ))
