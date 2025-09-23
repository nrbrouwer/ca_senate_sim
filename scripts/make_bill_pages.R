# scripts/make_bill_pages.R
library(tidyverse)

bills_csv <- "files/csvs/bill_list.csv"
senator_csv <- "files/csvs/senator_list.csv"
bills_dir  <- "files/pdfs/bills"
b_pages_dir  <- "bills-pages"
s_pages_dir  <- "senator-pages"

if (!dir.exists(b_pages_dir)) dir.create(b_pages_dir, recursive = TRUE)

# Read your spreadsheet
bill_list <- read.csv(bills_csv) 
senator_list <- read.csv(senator_csv)

bill_list <- bill_list %>%
  mutate(name = paste(First.Name, Last.Name, sep = " "),
        name_join = tolower(paste0(First.Name, Last.Name)),
        name_join = gsub(" ", "", name_join),
        bill_measure = paste0("SB-", bill_number),
        url = paste0(toupper(Last.Name), "_", "SB", bill_number),
        committee = ifelse(committee == "" | is.na(committee), "Unassigned", committee),
        appropriations = ifelse(appropriations == 1, "Yes", "No")) 

senator_list <- senator_list %>%
  mutate(name_join = tolower(paste0(First.Name, Last.Name)),
          name_join = gsub(" ", "", name_join))%>%
  select(-First.Name, -Last.Name )

bill_list <- left_join(bill_list, senator_list, by = "name_join")


for (i in seq_len(nrow(bill_list))) {
  bill_id <- as.character(bill_list$bill_measure[i])
  title   <- paste(bill_list$bill_measure[i], bill_list$title[i], sep = " ")
  committee <- as.character(bill_list$committee[i])
  appropriations <- as.character(bill_list$appropriations[i])
  header <- paste(bill_id, title, sep = ": ")
  author <- paste(bill_list$name[i])
  district <- as.character(bill_list$District[i])
  url <- as.character(bill_list$url[i])
  
  b_qmd_path <- file.path(b_pages_dir, paste0(url, ".qmd"))
  pdf_rel  <- file.path("..", bills_dir, paste0(url, ".pdf"))
  s_qmd_path <- file.path("..", s_pages_dir, paste0("district_", district, ".qmd"))
  
  yaml <- c(
    "---",
    sprintf('title: "%s"', title),
    "---"
  )
  body <- c(
    "",
    "::: {.panel-tabset}",
    "",
    "## Details",
    "",
    sprintf("**Author:** [%s](%s)", author, s_qmd_path),
    "",
    sprintf("**Committee:** %s", committee),
    "",
    sprintf("**Appropriations:** %s", appropriations),
    "",
    sprintf("**Text:** [View Bill](%s)", pdf_rel),
    "",
    sprintf('<iframe src="%s" width="100%%" height="600px"></iframe>', pdf_rel),
    "",
    "## Vote History",
    "",
    "This is temporary content for the second tab. More details about the bill will go here.",
    "",
    ":::",
    ""
  )
 
  cat(paste(c(yaml, body), collapse = "\n"), file = b_qmd_path)
}