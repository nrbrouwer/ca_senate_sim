# scripts/make_senator_pages.R

library(tidyverse)
library(purrr)

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

senators$Name <- paste(senators$First.Name, senators$Last.Name, sep = " ")
senators$Name <- trimws(gsub("\\s+", " ", senators$Name)) 
s_names <- senators$Name
d_sen <- senators$Name[senators$Party == "D"]
r_sen <- senators$Name[senators$Party == "R"]
          

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

#votes data

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
    ungroup()
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
  

# Loop to create pages

for (i in seq_len(nrow(senators))) {
  senator_id <- as.character(senators$District[i])
  lastname <- tolower(senators$Last.Name[i])
  name   <- paste(senators$First.Name[i], senators$Last.Name[i], sep = " ")
  bill1 <- as.character(senator_bills$bill_1[i])
  bill_measure1 <- if(!is.na(bill1)) paste0("SB-", bill1) else NULL
  bill2 <- as.character(senator_bills$bill_2[i])
  bill_measure2 <- if(!is.na(bill2)) paste0("SB-", bill2) else NULL

  name <- senators$Name[i]
  party <- senators$Party[i]

party_match <- ifelse(party == "D", "Dem_percent", "Rep_percent")

committees <- sapply(votes, function(df) name %in% names(df))
committees <- which(committees)
committee_names <- names(votes)[committees]

committee_fullnames <- c(
  "lgl" = "Local Government and Labor",
  "anr" = "Agriculture and Natural Resources", 
  "blh" = "Business, Law, and Health",
  "app" = "Appropriations",
  "floor" = "Floor"
)
committee_names <- committee_fullnames[committee_names]
committee_names <- committee_names[committee_names != "Floor"]

committee_names <- paste(committee_names,  collapse = "; ")

votes_including_s <- votes[committees]
# Keep only non-empty dataframes
votes_including_s <- Filter(function(df) {
  !is.null(df) && is.data.frame(df) && nrow(df) > 0
}, votes_including_s)
  

# Check if we have any dataframes left
if(length(votes_including_s) > 0) {
  # Debug: check what columns each dataframe has
  cat("Columns in each dataframe:\n")
  lapply(names(votes_including_s), function(name) {
    cat(name, ":", paste(colnames(votes_including_s[[name]]), collapse = ", "), "\n")
  })
  
  votes_including_s <- bind_rows(votes_including_s, .id = "Committee")
  
  # Check if Date column exists after binding
  if("Date" %in% colnames(votes_including_s) && name %in% colnames(votes_including_s)) {
    votes_including_s <- votes_including_s %>%
      select(Date, Bill, Committee, all_of(name), all_of(party_match)) %>%  # Use all_of() for variables
      mutate(party_percent = paste0(.data[[party_match]], "%"),
        party_vote = ifelse(.data[[party_match]] >= 50, 1, 0),
        party_aligned = case_when(
          party_vote == 1 & .data[[name]] == "Aye" ~ "Yes",
          party_vote == 0 & .data[[name]] == "No" ~ "Yes",
          party_vote == 1 & .data[[name]] == "No" ~ "No",
          party_vote == 0 & .data[[name]] == "Aye" ~ "No",
          TRUE ~ ""
        ),
        Committee = case_when(
          Committee == "lgl" ~ "Local Government and Labor",
          Committee == "anr" ~ "Agriculture and Natural Resources", 
          Committee == "blh" ~ "Business, Law, and Health",
          Committee == "app" ~ "Appropriations",
          Committee == "floor" ~ "Floor",
          TRUE ~ ""
        )) %>%
      select(Date, Bill, Committee, all_of(name), party_percent, party_aligned) %>%  # Use all_of() here too
      rename("Vote" = all_of(name),  # And here
        "Party Vote" = party_percent,
        "Party Aligned" = party_aligned)  %>%
      arrange(desc(Date))
  } else {
    # Handle case where columns don't exist
    votes_including_s <- data.frame()
  }
} else {
  # No dataframes with data for this senator
  votes_including_s <- data.frame()
}


  
vote_table <- if(nrow(votes_including_s) > 0) {
  votes_str <- capture.output(dput(votes_including_s))
  paste(votes_str, collapse = "\n")
} else {
  "data.frame()"
}
  
  s_qmd_path <- file.path(s_pages_dir, paste0("district_", senator_id, ".qmd"))
  pdf_rel  <- file.path("..", senator_dir, paste0(lastname, "_", senator_id, "_profile.pdf"))

  b_qmd_path1 <- file.path("..", b_pages_dir, paste0(toupper(lastname), "_SB", bill1, ".qmd"))
  b_qmd_path2 <- file.path("..", b_pages_dir, paste0(toupper(lastname), "_SB", bill2, ".qmd"))

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
    sprintf("**Committee Assignments:** %s", committee_names),
    "",
    sprintf("**Bills:**"),
    "",
    bill_links,
    "",
    sprintf("[View Profile](%s)", pdf_rel),
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
    "library(tidyverse)",
    "library(gt)",
    "",
    paste("votes_including_s <-", vote_table),
    "",
    "if(nrow(votes_including_s) > 0) {",
    "",
    "  # Create complete factor levels to ensure all categories appear",
    "  votes_including_s$`Party Aligned` <- factor(votes_including_s$`Party Aligned`, levels = c('Yes', 'No', ''))",
    "",
    "  party_df <- data.frame(table(votes_including_s$`Party Aligned`))",
    "  names(party_df) <- c('alignment', 'count')",
    "  party_df$percentage <- round(party_df$count / sum(party_df$count) * 100, 1)",
    "",
    "  # Filter out zero counts for the pie chart display, but keep them for legend",
    "  party_df_display <- party_df",
    "  party_df_display$alignment <- as.character(party_df_display$alignment)",
    "",
    "  pie_chart <- ggplot(party_df_display, aes(x = '', y = count, fill = alignment)) +",
    "    geom_bar(stat = 'identity', width = 1) +",
    "    coord_polar('y', start = 0) +",
    "    theme_void() +",
    "    labs(title = 'Senator-Party Alignment Breakdown') +",
    "    scale_fill_manual(name = 'Alignment', 
                      values = c('Yes' = 'green', 'No' = 'red', '' = 'gray'),
                      limits = c('Yes', 'No', ''), 
                      drop = FALSE) +",
    "    geom_text(aes(label = ifelse(count > 0, paste0(percentage, '%'), '')),",
    "              position = position_stack(vjust = 0.5))",
    "",
    "  print(pie_chart)",
    "",
    "  votes_including_s %>%",
    "    gt() %>%",
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
  
  cat(paste(c(yaml, body), collapse = "\n"), file = s_qmd_path)
}