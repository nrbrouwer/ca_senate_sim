# scripts/make_senator_pages.R

library(tidyverse)

senator_csv <- "files/csvs/senator_list.csv"
senator_dir  <- "files/pdfs/role_profiles/senators"
s_pages_dir  <- "senator-pages"
b_pages_dir  <- "bills-pages"

if (!dir.exists(s_pages_dir)) dir.create(s_pages_dir, recursive = TRUE)

# Read your spreadsheet
senator_list <- read.csv(senator_csv) 

for (i in seq_len(nrow(senator_list))) {
  senator_id <- as.character(senator_list$District[i])
  name   <- paste(senator_list$First.Name[i], senator_list$Last.Name[i], sep = " ")
  
  s_qmd_path <- file.path(s_pages_dir, paste0("district_", senator_id, ".qmd"))
  pdf_rel  <- file.path("..", senator_dir, paste0("district_", senator_id, "_profile.pdf"))
  
  yaml <- c(
    "---",
    sprintf('title: "%s"', name),
    "format:",
    "  html:",
    "    page-layout: full",  # Uses full page width
    "---"
  )
  body <- c(
    "",
    sprintf("[View the PDF](%s)", pdf_rel)
  )
  
  cat(paste(c(yaml, body), collapse = "\n"), file = s_qmd_path)
}