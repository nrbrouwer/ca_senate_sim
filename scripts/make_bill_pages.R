# scripts/make_bill_pages.R
library(tidyverse)
library(purrr)

bills_csv <- "files/csvs/bill_list.csv"
senator_csv <- "files/csvs/senator_list.csv"
bills_dir  <- "files/pdfs/bills"
b_pages_dir  <- "bills-pages"
s_pages_dir  <- "senator-pages"

if (!dir.exists(b_pages_dir)) dir.create(b_pages_dir, recursive = TRUE)

# Read your spreadsheet
bills <- read.csv(bills_csv) 
senators <- read.csv(senator_csv)

bills <- bills %>%
  mutate(name = paste(First.Name, Last.Name, sep = " "),
        name_join = tolower(paste0(First.Name, Last.Name)),
        name_join = gsub(" ", "", name_join),
        bill_measure = paste0("SB-", bill_number),
        url = paste0(toupper(Last.Name), "_", "SB", bill_number),
        committee = ifelse(committee == "" | is.na(committee), "Unassigned", committee),
        appropriations = ifelse(appropriations == 1, "Yes", "No")) 

senators <- senators %>%
  mutate(name_join = tolower(paste0(First.Name, Last.Name)),
          name_join = gsub(" ", "", name_join))

senators$Name <- paste(senators$First.Name, senators$Last.Name, sep = " ")
senators$Name <- trimws(gsub("\\s+", " ", senators$Name)) 
s_names <- senators$Name
d_sen <- senators$Name[senators$Party == "D"]
r_sen <- senators$Name[senators$Party == "R"]

senators <- senators %>%
  select(-First.Name, -Last.Name, -Name )

bills <- left_join(bills, senators, by = "name_join")

# Vote data

clean_votes <- function(votes_df){
  dat <- votes_df %>%
    rename_with(~ gsub("\\.", " ", .x)) %>%  
    rowwise() %>%
    mutate(across(any_of(s_names), ~ case_when(
            .x == 1 ~ "Aye",
            .x == 0 ~ "No",
            .default = "—"
          )),   
          Bill = as.character(Bill),
          yes = sum(c_across(any_of(s_names)) == "Aye", na.rm = TRUE),
          no = sum(c_across(any_of(s_names)) == "No", na.rm = TRUE),
          absent = sum(c_across(any_of(s_names)) == "—", na.rm = TRUE),
          Vote = paste(yes, no, absent, sep = "-"),
          d_yes = sum(c_across(any_of(d_sen)) == "Aye", na.rm = TRUE),
          d_no = sum(c_across(any_of(d_sen)) == "No", na.rm = TRUE),
          d_absent = sum(c_across(any_of(d_sen)) == "—", na.rm = TRUE),
          Dem_vote = paste(d_yes, d_no, d_absent, sep = "-"),
          Dem_percent = round((d_yes/(d_yes + d_no + d_absent)*100)),
          Dem_percent_sign = paste0(Dem_percent, "%"),
          r_yes = sum(c_across(any_of(r_sen)) == "Aye", na.rm = TRUE),
          r_no = sum(c_across(any_of(r_sen)) == "No", na.rm = TRUE),
          r_absent = sum(c_across(any_of(r_sen)) == "—", na.rm = TRUE),
          Rep_vote = paste(r_yes, r_no, r_absent, sep = "-"),
          Rep_percent = round((r_yes/(r_yes + r_no + r_absent)*100)),
          Rep_percent_sign = paste0(Rep_percent, "%")
          ) %>%
    select(Date, Bill, Vote, Result, Dem_percent, Rep_percent, any_of(s_names))
}

votes_lgl <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSWsxVKMyPrvGZW1VFD0_DdTsMmH-dzITniXvWusbbG34FPwj7uWsIDB6B_6Sb5AdK94SbZ75eL0vTT/pub?gid=0&single=true&output=csv") 
votes_anr <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSWsxVKMyPrvGZW1VFD0_DdTsMmH-dzITniXvWusbbG34FPwj7uWsIDB6B_6Sb5AdK94SbZ75eL0vTT/pub?gid=1077921709&single=true&output=csv") 
votes_blh <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSWsxVKMyPrvGZW1VFD0_DdTsMmH-dzITniXvWusbbG34FPwj7uWsIDB6B_6Sb5AdK94SbZ75eL0vTT/pub?gid=1971997036&single=true&output=csv") 
votes_app <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSWsxVKMyPrvGZW1VFD0_DdTsMmH-dzITniXvWusbbG34FPwj7uWsIDB6B_6Sb5AdK94SbZ75eL0vTT/pub?gid=578574660&single=true&output=csv") 
votes_floor <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSWsxVKMyPrvGZW1VFD0_DdTsMmH-dzITniXvWusbbG34FPwj7uWsIDB6B_6Sb5AdK94SbZ75eL0vTT/pub?gid=192921423&single=true&output=csv")

votes <- list(lgl = votes_lgl,
              anr = votes_anr,
              blh = votes_blh,
              app = votes_app,
              floor = votes_floor)
votes <- lapply(votes, clean_votes)


for (i in seq_len(nrow(bills))) {
  bill_id <- as.character(bills$bill_measure[i])
  title   <- paste(bills$bill_measure[i], bills$title[i], sep = " ")
  committee <- as.character(bills$committee[i])
  appropriations <- as.character(bills$appropriations[i])
  header <- paste(bill_id, title, sep = ": ")
  author <- paste(bills$name[i])
  district <- as.character(bills$District[i])
  url <- as.character(bills$url[i])

  matches <- map_dfr(votes, ~ {
    if(nrow(.x) == 0) return(NULL)
    .x %>% filter(Bill == bill_id)
  }, .id = "source")
  
  matches <- matches %>%
    select(Date, source, Vote, Result, Dem_percent, Rep_percent) %>%
    arrange(desc(Date)) %>%
    mutate(source = case_when(source == "lgl" ~ "Local Government and Labor",
      source == "anr" ~ "Agriculture and Natural Resources",
      source == "blh" ~ "Business, Law, and Health",
      source == "app" ~ "Appropriations",
      source == "floor" ~ "Floor",
    ))
  
  matches_code <- if(nrow(matches) > 0) {
    matches_str <- capture.output(dput(matches))
    paste(matches_str, collapse = "\n")
  } else {
    "data.frame()"
  }  
  
  b_qmd_path <- file.path(b_pages_dir, paste0(url, ".qmd"))
  pdf_rel  <- file.path("..", bills_dir, paste0(url, ".pdf"))
  s_qmd_path <- file.path("..", s_pages_dir, paste0("district_", district, ".qmd"))
  
  yaml <- c(
    "---",
    sprintf('title: "%s"', title),
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
    "```{r}",
    "#| echo: false",
    "#| warning: false",
    "#| message: false",
    "",
    "library(dplyr)",
    "library(gt)",
    "",
    paste("matches <-", matches_code),
    "",
    "if(nrow(matches) > 0) {",
    "  matches %>%",
    "    gt() %>%",
    "    cols_label(",
    "      Date = 'Date',",
    "      source = 'Committee',",
    "      Vote = 'Vote',",
    "      Result = 'Result',",
    "      Dem_percent_sign = 'Democratic Support',",
    "      Rep_percent_sign = 'Republican Support'",
    "    ) %>%",
    "    tab_header(",
    "      title = 'Vote History',",
    "      subtitle = 'Sorted by newer to older votes'",
    "    ) %>%",
    "    opt_interactive(",
    "      use_sorting = TRUE,",
    "      use_highlight = TRUE",
    "    ) %>%",
    "    opt_row_striping()",
    "} else {",
    "  cat('No vote history available for this bill.')",
    "}",
    "",
    "```",
    "",
    ":::",
    ""
  )
 
  cat(paste(c(yaml, body), collapse = "\n"), file = b_qmd_path)
}
