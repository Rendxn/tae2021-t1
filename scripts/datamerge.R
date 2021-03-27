library(dplyr)

file_paths <-
  list.files(path = './data',
             pattern = '*.csv',
             full.names = TRUE) # All files with directory

file_names <-
  list.files(path = './data',
             pattern = '*.csv') # All file names (without directory)

merge_cols <- c('ï..DIRECTORIO', 'SECUENCIA_ENCUESTA')
# , 'SECUENCIA_ENCUESTA', 'SECUENCIA_P', 'ORDEN', 'FEX_C'

files <- lapply(
  file_paths,
  read.table,
  header = TRUE,
  sep = ';',
  dec = ','
) # Read all files and store each one in a separate data frame

multiple_full <- function(...){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  joined = full_join(..., by = merge_cols)
  return(joined)
}

all_df <- Reduce(multiple_full, files) # Full join all of the data frames
names(all_df)[1] = 'DIRECTORIO' # Rename from ï..DIRECTORIO to DIRECTORIO

filtered_col_names <-
  c(
    'DIRECTORIO',
    'SECUENCIA_ENCUESTA',
    'ORDEN',
    'P8587',
    'P6218',
    'P6090',
    'P6390S2',
    'P8624',
    'P415'
  )

filtered_df <- select(all_df, all_of(filtered_col_names))
