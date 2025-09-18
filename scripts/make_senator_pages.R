# scripts/make_senator_pages.R

senator_csv <- "files/csvs/senator_list.csv"
senator_dir  <- "files/pdfs/role_profiles"
pages_dir  <- "senator-pages"

if (!dir.exists(pages_dir)) dir.create(pages_dir, recursive = TRUE)

# Read your spreadsheet
senator_list <- read.csv(senator_csv) 

for (i in seq_len(nrow(senator_list))) {
  senator_id <- as.character(senator_list$District[i])
  name   <- as.character(senator_list$Name[i])
  
  qmd_path <- file.path(pages_dir, paste0("district_", senator_id, ".qmd"))
  pdf_rel  <- file.path("..", senator_dir, paste0("district_", senator_id, "_profile.pdf"))
  
  yaml <- c(
    "---",
    sprintf('title: "%s"', name),
    "---"
  )
  body <- c(
    "",
    sprintf("[View the PDF](%s)", pdf_rel)
  )
  
  cat(paste(c(yaml, body), collapse = "\n"), file = qmd_path)
}