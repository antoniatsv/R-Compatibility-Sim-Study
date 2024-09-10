library(dplyr)

read_rds <- function(df_name) {
  readr::read_rds(file = df_name)
}

df <- list.files(path = "~/Missing\ Data\ Compatibility/", pattern = ".RDS", full.names = TRUE) %>% 
  purrr::map(.f = ~read_rds(.)) %>% 
  purrr::list_rbind()

readr::write_rds(df, file = "~/Missing\ Data\ Compatibility/simulation_results_all.RDS")
