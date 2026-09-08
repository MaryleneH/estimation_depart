library(arrow)

parquet <- read_parquet("/home/onyxia/work/chaine/data/bts_2024.parquet")

print(names(parquet))