#' #' # Dataset Push for Macroniche (NOT UPDATED FOR THIS PROJECT)
#' 
#' #' You'll need to have access to the google drive folder where these data are 
#' #' stored before running this script. Use this script to update the shared
#' #' folder with new input or output data as needed.
#' #' 
#' #' WARNING: THIS SCRIPT WILL OVERWRITE THE SHARED DATA FOLDER WITH YOUR LOCAL COPY
#' #' 
#'   
#'   # load library
#'   library(googledrive)
#'   library(purrr)
#'   library(data.table)
#'   
#'   #drive authorization request
#' 
#'   # link to google drive & force new token auth (can change to 1 to use existing account)
#'   drive_auth(email = FALSE)
#'   
#'   
#'   # store folder url as a file ID
#'   # identify this folder on Drive
#'   # let googledrive know this is a file ID or URL, as opposed to file name
#'     infolder <- drive_get(as_id("https://drive.google.com/open?id=18pifxDCG85yhFSskJSCYhJVN3tTRl9wY"))
#'     outfolder <- drive_get(as_id("https://drive.google.com/open?id=18s1G78WA9Hu4_6k_fIPMROV9DVo-dI_5"))
#'     
#'   # update the drive folder with your local folder version:
#'   #identify a list of files in your current folder
#'   # identify the csv files in each folder
#'   list.files("data", recursive = T)
#'   
#'   # store these for use as a list of files to update
#'   inputfolder_files <- data.table(list.files("data/input/", pattern = "*.csv", recursive = T))
#'   outputfolder_files <- data.table(list.files("data/output/", pattern = "*.csv", recursive = T))
#'   
#'   
#'   # export/upload them, overwriting the currrent shared drive data folder:
#'   walk(inputfolder_files$V1, ~ drive_upload(paste("data/input", .x, sep = "/"), path = infolder, name = .x, overwrite = T, type = "csv" ))
#'   walk(outputfolder_files$V1, ~ drive_upload(paste("data/output", .x, sep = "/"), path = outfolder, name = .x, overwrite = T, type = "csv" ))
#'   