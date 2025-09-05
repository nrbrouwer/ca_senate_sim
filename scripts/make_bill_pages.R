# scripts/make_bill_pages.R

# Load libraries
library(readxl)

bills_xlsx <- "bill_list.xlsx"
bills_dir  <- "bills"
pages_dir  <- "bills-pages"

if (!dir.exists(pages_dir)) dir.create(pages_dir, recursive = TRUE)

# Read your spreadsheet
bill_list <- read_excel(bills_xlsx) 

for (i in seq_len(nrow(bill_list))) {
  bill_id <- as.character(bill_list$bill_number[i])
  title   <- as.character(bill_list$title[i])
  
  qmd_path <- file.path(pages_dir, paste0("bill_", bill_id, ".qmd"))
  pdf_rel  <- file.path("..", bills_dir, paste0("bill_", bill_id, ".pdf"))
  
  yaml <- c(
    "---",
    sprintf('title: "%s"', title),
    "---"
  )
  body <- c(
    "",
    sprintf("[View the PDF](%s)", pdf_rel)
  )
  
  cat(paste(c(yaml, body), collapse = "\n"), file = qmd_path)
}