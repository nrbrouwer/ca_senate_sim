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
        Last.Name_link = gsub(" ", "_", Last.Name),
        name_join = tolower(paste0(First.Name, Last.Name)),
        name_join = gsub(" ", "", name_join),
        bill_measure = paste0("SB-", bill_number),
        url = paste0(toupper(Last.Name_link), "_", "SB", bill_number),
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
  select(-First.Name, -Last.Name, -Name)

bills <- left_join(bills, senators, by = "name_join")

# Vote data

clean_votes <- function(votes_df){

  dat <- votes_df

  if(nrow(dat) == 0) {
    return(NULL)
  }

  dat <- dat %>%
    rename_with(~ gsub("\\.", " ", .x)) %>%  
    rowwise() %>%
    mutate(
          Bill = as.character(Bill),
          yes = sum(c_across(any_of(s_names)) == "Aye", na.rm = TRUE),
          no = sum(c_across(any_of(s_names)) == "No", na.rm = TRUE),
          abstain = sum(c_across(any_of(s_names)) == "Abstain", na.rm = TRUE),
          absent = sum(c_across(any_of(s_names)) == "" | is.na(c_across(any_of(s_names)))),
          Vote = paste(yes, no, abstain, absent, sep = "-"),
          d_yes = sum(c_across(any_of(d_sen)) == "Aye", na.rm = TRUE),
          d_no = sum(c_across(any_of(d_sen)) == "No", na.rm = TRUE),
          d_abstain = sum(c_across(any_of(d_sen)) == "Abstain", na.rm = TRUE),
          d_absent = sum(c_across(any_of(d_sen)) == "" | is.na(c_across(any_of(d_sen)))),
          Dem_vote = paste(d_yes, d_no, d_abstain, d_absent, sep = "-"),
          Dem_percent = round((d_yes/(d_yes + d_no + d_abstain)*100)),
          Dem_percent_sign = paste0(Dem_percent, "%"),
          r_yes = sum(c_across(any_of(r_sen)) == "Aye", na.rm = TRUE),
          r_no = sum(c_across(any_of(r_sen)) == "No", na.rm = TRUE),
          r_abstain = sum(c_across(any_of(r_sen)) == "Abstain", na.rm = TRUE),
          r_absent = sum(c_across(any_of(r_sen)) == "" | is.na(c_across(any_of(r_sen)))),
          Rep_vote = paste(r_yes, r_no, r_abstain, r_absent, sep = "-"),
          Rep_percent = round((r_yes/(r_yes + r_no + r_abstain)*100)),
          Rep_percent_sign = paste0(Rep_percent, "%")
          ) %>%
    select(Date, Bill, Vote, Result, Dem_percent_sign, Rep_percent_sign, any_of(s_names))
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
  number <- as.character(bills$bill_number[i])
  bill_id <- as.character(bills$bill_measure[i])
  title   <- paste(bills$bill_measure[i], bills$title[i], sep = " ")
  committee <- as.character(bills$committee[i])
  appropriations <- as.character(bills$appropriations[i])
  header <- paste(bill_id, title, sep = ": ")
  author <- paste(bills$name[i])
  district <- as.character(bills$District[i])
  url <- as.character(bills$url[i])

matches <- map_dfr(votes, ~ {
    if(is.null(.x) || nrow(.x) == 0) return(NULL)
    .x %>% filter(Bill == bill_id)
  }, .id = "source")

# Check if matches has any rows before processing
if(!is.null(matches) && nrow(matches) > 0) {
  matches <- matches %>%
    select(Date, source, Vote, Result, Dem_percent_sign, Rep_percent_sign) %>%
    arrange(desc(Date)) %>%
    mutate(source = case_when(
      source == "lgl" ~ "Local Government and Labor",
      source == "anr" ~ "Agriculture and Natural Resources",
      source == "blh" ~ "Business, Law, and Health",
      source == "app" ~ "Appropriations",
      source == "floor" ~ "Floor",
    ))
  
  matches_code <- capture.output(dput(matches)) %>%
    paste(collapse = "\n")
} else {
  matches_code <- "data.frame()"
}
  
b_qmd_path <- file.path(b_pages_dir, paste0(url, ".qmd"))
pdf_rel  <- file.path("..", bills_dir, paste0(url, ".pdf"))
s_qmd_path <- file.path("..", s_pages_dir, paste0("district_", district, ".qmd"))

  
# Finding older verions of bills 
prev_bills_dir <- "files/pdfs/previous_bills"
prev_pattern <- paste0("^", url, "_v[0-9]+\\.pdf$")

if (dir.exists(prev_bills_dir)) {
  prev_files <- list.files(prev_bills_dir, pattern = prev_pattern, full.names = FALSE)
  
  # Extract version numbers and sort descending (newest first)
  if (length(prev_files) > 0) {
    prev_versions <- as.numeric(gsub(paste0("^", url, "_v([0-9]+)\\.pdf$"), "\\1", prev_files))
    prev_df <- data.frame(
      file = prev_files,
      version = prev_versions
    ) %>%
      arrange(desc(version))
  } else {
    prev_df <- data.frame()
  }
} else {
  prev_df <- data.frame()
}
  
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
  "## Previous Text",
  "",
  if (nrow(prev_df) > 0) {
    c(
      "::: {.panel-tabset}",
      "",
      # Generate sub-tabs for each previous version
      unlist(lapply(seq_len(nrow(prev_df)), function(j) {
        v_num <- prev_df$version[j]
        v_file <- prev_df$file[j]
        v_path <- file.path("..", prev_bills_dir, v_file)
        
        c(
          sprintf("### Version %d", v_num),
          "",
          sprintf('<iframe src="%s" width="100%%" height="600px"></iframe>', v_path),
          ""
        )
      })),
      ":::",
      ""
    )
  } else {
    "No previous versions available for this bill."
  },
  "",
"## Lobbyist Letters",
"",
"```{r}",
"#| echo: false",
"#| warning: false",
"#| message: false",
"",
"library(dplyr)",
"library(gt)",
"",
"# Create lookup table for lobby codes",
"lobby_names <- c(",
"  'COC' = 'CA Chamber of Commerce',",
"  'PGE' = 'PG&E/Edison/Sempra',",
"  'FED' = 'CA Labor Federation',",
"  'CTA' = 'CA Teachers Association',",
"  'BANK' = 'CA Bankers Association/Personal Insurance Federation',",
"  'ALT' = 'Altria (tobacco)',",
"  'FB' = 'Farm Bureau',",
"  'SC' = 'Sierra Club',",
"  'UC' = 'University of California',",
"  'LCC' = 'League of CA Cities'",
")",
"",
"# Find lobbyist letters for this bill",
"lobbyist_dir <- file.path('..', 'files', 'pdfs', 'lobbyist_letters')",
"",
sprintf("bill_pattern <- '_SB%s_(support|oppose)\\\\.pdf$'", number),
"",
"if (dir.exists(lobbyist_dir)) {",
"  letter_files <- list.files(lobbyist_dir, pattern = bill_pattern, full.names = FALSE)",
"  ",
"  if (length(letter_files) > 0) {",
"    # Parse letter info",
"    letter_df <- data.frame(",
"      file = letter_files",
"    ) %>%",
"      mutate(",
"        org_code = gsub('_.*', '', file),",
"        organization = ifelse(org_code %in% names(lobby_names), lobby_names[org_code], org_code),",
"        position = ifelse(grepl('_support\\\\.pdf$', file), 'Support', 'Oppose'),",
"        link_path = file.path('..', lobbyist_dir, file)",
"      ) %>%",
"      arrange(position, organization)",
"    ",
"    # Split into support and oppose",
"    support_letters <- letter_df %>% filter(position == 'Support')",
"    oppose_letters <- letter_df %>% filter(position == 'Oppose')",
"    ",
"    # Display Support Letters",
"    if (nrow(support_letters) > 0) {",
"      cat('\\n### Support Letters\\n\\n')",
"      for (j in seq_len(nrow(support_letters))) {",
"        cat(sprintf('[%s](%s)  \\n', support_letters$organization[j], support_letters$link_path[j]))",
"      }",
"      cat('\\n')",
"    }",
"    ",
"    # Display Oppose Letters",
"    if (nrow(oppose_letters) > 0) {",
"      cat('\\n### Opposition Letters\\n\\n')",
"      for (j in seq_len(nrow(oppose_letters))) {",
"        cat(sprintf('[%s](%s)  \\n', oppose_letters$organization[j], oppose_letters$link_path[j]))",
"      }",
"      cat('\\n')",
"    }",
"  } else {",
"    cat('No lobbyist letters available for this bill.')",
"  }",
"} else {",
"  cat('Lobbyist letters directory not found.')",
"}",
"",
"```",
"",
  ":::",
  ""
)
 
  cat(paste(c(yaml, body), collapse = "\n"), file = b_qmd_path)
}
