# scripts/make_lobby_pages.R

library(tidyverse)

lobby_csv <- "files/csvs/lobbyist_list.csv"
bills_csv <- "files/csvs/bill_list.csv"
letters_dir  <- "files/pdfs/lobbyist_letters"
b_pages_dir  <- "bills-pages"
l_pages_dir <- "lobby-pages"

if (!dir.exists(l_pages_dir)) dir.create(l_pages_dir, recursive = TRUE)

lobbys <- read.csv(lobby_csv) 

bills <- read.csv(bills_csv)
bills <- bills %>%
  mutate(author_name = toupper(gsub(" ", "_", Last.Name)),
        bill_measure = paste0("SB", bill_number),
        bill_link = paste(author_name, bill_measure, sep = "_"))

# Creating pages

for (i in seq_len(nrow(lobbys))) {
  lobby <- lobbys$Lobby[i]
  lobby_code <- toupper(lobbys$Code[i])

  # Find all position letters for this lobby group
  if (dir.exists(letters_dir)) {
    all_letters <- list.files(letters_dir, pattern = paste0("^", lobby_code, "_SB[0-9]+_(support|oppose)\\.pdf$"), full.names = FALSE)
    
    if (length(all_letters) > 0) {
      # Parse the filenames to extract bill numbers and positions
      letters_df <- data.frame(
        filename = all_letters,
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          bill_number = as.integer(gsub(paste0("^", lobby_code, "_SB([0-9]+)_(support|oppose)\\.pdf$"), "\\1", filename)),
          position = gsub(paste0("^", lobby_code, "_SB([0-9]+)_(support|oppose)\\.pdf$"), "\\2", filename),
          bill_measure = paste0("SB-", bill_number),
          Position = tools::toTitleCase(position),
          letter_path = file.path("..", letters_dir, filename),
          Letter_Link = paste0("[View Letter](", letter_path, ")")
        ) %>%
        left_join(bills %>% select(bill_number, bill_link), by = "bill_number") %>%
        mutate(
          Bill_Link = if_else(
            !is.na(bill_link),
            paste0("[", bill_measure, "](../", b_pages_dir, "/", bill_link, ")"),
            bill_measure
          )
        ) %>%
        select(Bill_Link, Position, Letter_Link) 
      
      # Convert to dput format for embedding in qmd
      letters_code <- capture.output(dput(letters_df)) %>%
        paste(collapse = "\n")
      
    } else {
      letters_code <- "data.frame()"
    }
  } else {
    letters_code <- "data.frame()"
  }
  
  # Create the QMD file path
  lobby_qmd_path <- file.path(make_lobby_pages, paste0(lobby_code, ".qmd"))
  
  # Build YAML header
  yaml <- c(
    "---",
    sprintf('title: "%s"', lobby),
    "format:",
    "  html:",
    "    page-layout: full",
    "---"
  )
  
  # Build body
  body <- c(
    "",
    "::: {.panel-tabset}",
    "",
    "## Position Letters",
    "",
    "```{r}",
    "#| echo: false",
    "#| warning: false",
    "#| message: false",
    "",
    "library(dplyr)",
    "library(gt)",
    "",
    paste("letters_df <-", letters_code),
    "",
    "if(nrow(letters_df) > 0) {",
    "  letters_df %>%",
    "    gt() %>%",
    "    cols_label(",
    "      bill_measure = 'Bill',",
    "      Position = 'Position',",
    "      Letter_Link = 'Letter'",
    "    ) %>%",
    "    fmt_markdown(columns = Letter_Link) %>%",
    "    tab_header(",
    "      title = 'Position Letters'",
    "    ) %>%",
    "    opt_interactive(",
    "      use_sorting = TRUE,",
    "      use_search = TRUE,",
    "      use_filters = TRUE",
    "    ) %>%",
    "    opt_row_striping()",
    "} else {",
    "  cat('No position letters available for this lobby group.')",
    "}",
    "",
    "```",
    "",
    "## Spending",
    "",
    "Spending data coming soon.",
    "",
    ":::",
    ""
  )
  
  # Write the file
  cat(paste(c(yaml, body), collapse = "\n"), file = lobby_qmd_path)
}
