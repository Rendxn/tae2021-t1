library(dplyr)

# All files with directory
file_paths <-
  list.files(path = './data',
             pattern = '*.csv',
             full.names = TRUE)

# All file names (without directory)
file_names <-
  list.files(path = './data',
             pattern = '*.csv')

# Cols that will be passed in the full_join
merge_cols <- c('ï..DIRECTORIO', 'SECUENCIA_ENCUESTA')
# , 'SECUENCIA_ENCUESTA', 'SECUENCIA_P', 'ORDEN', 'FEX_C'

# Read all files and store each one in a separate data frame
files <- lapply(
  file_paths,
  read.table,
  header = TRUE,
  sep = ';',
  dec = ','
)

# Function that returns the full join
# of two given dataframes by
# `merge_cols`
multiple_full <- function(...){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  joined = full_join(..., by = merge_cols)
  return(joined)
}

# Full join all of the data frames using Reduce.
all_df <- Reduce(multiple_full, files)
# Rename from ï..DIRECTORIO to DIRECTORIO
names(all_df)[1] = 'DIRECTORIO'

# Columns to be filtered from the whole DB
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
