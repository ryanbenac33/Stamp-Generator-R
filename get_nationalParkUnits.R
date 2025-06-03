library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Define regions with states
regions <- list(
  NorthAtlantic = c("Maine", "New Hampshire", "Vermont", "Massachusetts",
                    "Rhode Island", "Connecticut", "New York", "New Jersey"),
  MidAtlantic = c("Pennsylvania", "Delaware", "Maryland", "Virginia", "West Virginia"),
  NationalCapital = c("District of Columbia"),
  Southeast = c("Kentucky", "North Carolina", "South Carolina", "Tennessee",
                "Georgia", "Alabama", "Mississippi", "Florida"),
  Midwest = c("Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin", "Minnesota",
              "Iowa", "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas"),
  PacificNW_Alaska = c("Washington", "Oregon", "Idaho", "Alaska"),
  Western = c("California", "Nevada", "Hawaii"),
  RockyMountain = c("Montana", "Wyoming", "Utah", "Colorado"),
  Southwest = c("Arizona", "New Mexico", "Oklahoma", "Texas")
)

# Define colors for each region
region_colors <- c(
  NorthAtlantic = "#003366",
  MidAtlantic = "#0066CC",
  NationalCapital = "#66CCFF",
  Southeast = "#339966",
  Midwest = "#FF9933",
  PacificNW_Alaska = "#9966CC",
  Western = "#CC3333",
  RockyMountain = "#996633",
  Southwest = "#009999"
)

# Build a lookup table: state -> region
state_to_region <- unlist(lapply(names(regions), function(region) {
  setNames(rep(region, length(regions[[region]])), regions[[region]])
}))

# Function to assign color by state
assign_color <- function(state_vec) {
  regions <- state_to_region[state_vec]
  colors <- region_colors[regions]
  colors[is.na(colors)] <- "#000000"  # default black for unknown states
  return(colors)
}

# Scrape function
scrape_national_parks <- function() {
  url <- "https://en.wikipedia.org/wiki/List_of_the_United_States_National_Park_System_official_units"
  response <- httr::GET(url)
  page <- httr::content(response, as = "parsed", encoding = "UTF-8")
  
  tables <- rvest::html_nodes(page, "table.sortable.wikitable")
  table <- tables[2]  # second table
  
  rows <- rvest::html_nodes(table, "tr")[-1]  # skip header
  
  national_parks_data <- lapply(rows, function(row) {
    cols <- rvest::html_nodes(row, "td")
    if (length(cols) >= 2) {
      park_name <- rvest::html_text(cols[1]) %>% stringr::str_trim()
      park_name <- stringr::str_replace_all(park_name, fixed('"'), "")
      park_name <- stringr::str_replace_all(park_name, fixed("'"), "")
      state <- rvest::html_text(cols[2]) %>% stringr::str_trim()
      data.frame(`National Park` = park_name, State = state, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  dplyr::bind_rows(national_parks_data)
}

# Identify type function
identify_type <- function(name) {
  m <- stringr::str_match(name, "\\bNational(?: [A-Z][a-z]+){0,3}")
  if (!is.na(m[1])) m[1] else "Site"
}

# Main execution
national_parks_df <- scrape_national_parks()

# Rename if needed
if ("National.Park" %in% colnames(national_parks_df)) {
  national_parks_df <- national_parks_df %>%
    rename(`National Park` = National.Park)
}

national_parks_df <- national_parks_df %>%
  arrange(`National Park`) %>%
  mutate(
    `National Park` = `National Park` %>%
      stringr::str_replace_all(fixed("'"), "") %>%
      stringr::str_replace_all(",", "") %>%
      stringr::str_trim(),
    Type = sapply(`National Park`, identify_type),
    default_color = assign_color(State)
  )

output_file <- "./data/national_sites.csv"
readr::write_csv(national_parks_df, output_file)

