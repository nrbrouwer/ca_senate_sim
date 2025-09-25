# scripts/make_senator_pages.R

library(tidyverse)

senator_csv <- "files/csvs/senator_list.csv"
bills_csv <- "files/csvs/bill_list.csv"
senator_dir  <- "files/pdfs/role_profiles/senators"
s_pages_dir  <- "senator-pages"
b_pages_dir  <- "bills-pages"

if (!dir.exists(s_pages_dir)) dir.create(s_pages_dir, recursive = TRUE)

# Read your spreadsheet
senators <- read.csv(senator_csv) 
senators <- senators %>%
  mutate(name_join = tolower(paste0(First.Name, Last.Name)),
          name_join = gsub(" ", "", name_join))

bills <- read.csv(bills_csv)
bills <- bills %>%
  mutate(name = paste(First.Name, Last.Name, sep = " "),
        name_join = tolower(paste0(First.Name, Last.Name)),
        name_join = gsub(" ", "", name_join),
        bill_measure = paste0("SB-", bill_number))

bill_add <- data.frame(name_join = "scottwiener",
                      bill_measure = "SB-10",
                      bill_number = 10)
bills <- bind_rows(bills, bill_add)

senator_bills <- senators %>%
  left_join(bills, by = "name_join") %>%
  group_by(name_join) %>%
  mutate(bill_count = row_number()) %>%
  select(name_join, bill_count, bill_number, bill_measure) %>%
  pivot_wider(
    names_from = bill_count, 
    values_from = bill_number,
    names_prefix = "bill_"
  ) %>%
  mutate(across(bill_1:bill_2, ~ as.integer(.)))

for (i in seq_len(nrow(senators))) {
  senator_id <- as.character(senators$District[i])
  lastname <- tolower(senators$Last.Name[i])
  name   <- paste(senators$First.Name[i], senators$Last.Name[i], sep = " ")
  bill1 <- as.character(senator_bills$bill_1[i])
  bill_measure1 <- if(!is.na(bill1)) paste0("SB-", bill1) else NULL
  bill2 <- as.character(senator_bills$bill_2[i])
  bill_measure2 <- if(!is.na(bill2)) paste0("SB-", bill2) else NULL

  
  s_qmd_path <- file.path(s_pages_dir, paste0("district_", senator_id, ".qmd"))
  pdf_rel  <- file.path("..", senator_dir, paste0(lastname, "_", senator_id, "_profile.pdf"))

  b_qmd_path1 <- file.path(b_pages_dir, paste0(toupper(lastname), "_SB", bill1, ".qmd"))
  b_qmd_path2 <- file.path(b_pages_dir, paste0(toupper(lastname), "_SB", bill2, ".qmd"))

  bill_links <- c()
  if(!is.na(bill1)) {
    bill_links <- c(bill_links, sprintf("[%s](%s)", bill_measure1, b_qmd_path1))
  }
  if(!is.na(bill2)) {
    bill_links <- c(bill_links, sprintf("[%s](%s)", bill_measure2, b_qmd_path2))
  }

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
    "::: {.panel-tabset}",
    "",
    "## Details",
    "",
    sprintf("**District:** %s", senator_id),
    "",
    sprintf("**Committee Assignments:**"),
    "",
    sprintf("**Bills:**"),
    "",
    bill_links,
    "",
    sprintf("[View Profile](%s)", pdf_rel),
    "",
    "## Vote History",
    "",
    "Placeholder text",
    "",
    ":::"
  )
  
  cat(paste(c(yaml, body), collapse = "\n"), file = s_qmd_path)
}