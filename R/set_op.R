#' build_protein_sets
#' Builds protein sets from the DIA-NN report, based on a sample_id and replicate_id column
#' Output can be forwarded to UpsetR's UpSetR::fromList() to build an UpSet plot.
#'
#'@return A list of unique protein groups to each id in supplied sample_id_col of df
#'@export

build_protein_sets <- function(df, sample_id_col){

  metadata <- df %>%
  select(all_of(c(sample_id_col))) %>%
  group_by(across(all_of(c(sample_id_col))))%>%
  summarise()

sample_ids <- metadata %>%
  pull(sample_id_col) %>%
  unique()

pg_list <- vector(
  mode="list", length = length(sample_ids)
)

names(pg_list) <- sample_ids

# Construct a map over the sample and replicate IDs of the dataset

for(i in 1:length(sample_ids)){

    sample_id <- sample_ids[[i]]

    distinct_pgs <- df %>%
      filter(!!sym(sample_id_col) == sample_id) %>%
      pull(Protein.Group)%>%
      unique()

    pg_list[[sample_id]] <- distinct_pgs

  }

  return(pg_list)


}


#' get_jaccard_matrix
#' Builds jaccard matrix over protein group identifications in the supplied DIA-NN report
#'
#'@return A matrix of jaccard indices.
#'@export


