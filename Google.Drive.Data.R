
---
  title: "r_data"
author: "Mike Verhoeven"
date: "1October2024"
output: html_document
---
  
  Think carefully about whether you want to execute this script. It will overwrite same-named data in your project Data folder.

#NOT UPDATED, COPIED FROM MIKE

```{r}
library(purrr)
library("googledrive")
library(data.table)
```


# Create data folder on your local machine
```{r}
ifelse(!dir.exists("R and stan code/Data"), 
       dir.create("R and stan code/Data"), 
       "Folder exists already")
```

# Download files from Gdrive
```{r}
drive_auth(email = FALSE)


# store folder url as a file ID
# identify this folder on Drive
# let googledrive know this is a file ID or URL, as opposed to file name
datafolder <- drive_get(
  as_id("https://drive.google.com/drive/u/1/folders/1MHMK0BsUYe-C6B_bjjkIoIE0rft8iygA")) #ZM/WAE - HK Data folder in Hansen GDrive


# identify the csv files in each folder
input_files <- data.table(drive_ls(datafolder))

#input_files <-  input_files %>% dplyr::filter(str_detect(name, ".csv"))

# download them, overwriting your currrent project data folder:
walk(input_files$id, ~ drive_download(as_id(.x), path = paste("data", input_files[id==.x, name], sep = "/"), overwrite = T ))

```
