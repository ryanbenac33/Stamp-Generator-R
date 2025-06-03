library(httr)
library(rvest)
library(dplyr)

# Helper: check if a Wikipedia page exists
page_exists <- function(title) {
  url <- paste0("https://en.wikipedia.org/wiki/", title)
  res <- httr::GET(url)
  if (res$status_code != 200) return(FALSE)
  page <- read_html(res)
  # Check if this is a redirect or missing page message
  no_page_text <- page %>% html_node("div.noarticletext")
  if (!is.null(no_page_text)) return(FALSE)
  TRUE
}

# Scrape landmarks from a single state page
scrape_state_landmarks <- function(state_name) {
  page_title <- paste0("List_of_National_Natural_Landmarks_in_", state_name)
  
  if (!page_exists(page_title)) {
    message("Skipping state (page missing or redirect): ", state_name)
    return(NULL)
  }
  
  url <- paste0("https://en.wikipedia.org/wiki/", page_title)
  res <- httr::GET(url)
  if (res$status_code != 200) {
    message("Failed to retrieve page: ", state_name)
    return(NULL)
  }
  
  page <- read_html(res)
  tables <- html_nodes(page, "table.wikitable, table.wikitable.sortable")
  
  if (length(tables) == 0) {
    message("No tables found for: ", state_name)
    return(NULL)
  }
  
  for (tbl in tables) {
    rows <- html_nodes(tbl, "tr")
    if (length(rows) < 2) next
    
    # Extract header text
    headers <- html_nodes(rows[1], "th") %>% html_text(trim=TRUE)
    if (length(headers) == 0) {
      # fallback: use first row td as header if no th found
      headers <- html_nodes(rows[1], "td") %>% html_text(trim=TRUE)
      data_rows <- rows[-1]
    } else {
      data_rows <- rows[-1]
    }
    
    # Extract all column data
    cols_data <- vector("list", length(headers))
    names(cols_data) <- headers
    
    for (i in seq_along(data_rows)) {
      cells <- html_nodes(data_rows[i], "td")
      if (length(cells) != length(headers)) next
      for (j in seq_along(cells)) {
        text <- html_text(cells[j], trim=TRUE)
        cols_data[[j]] <- c(cols_data[[j]], text)
      }
    }
    
    # Score each column for likelihood of being landmark names
    score_col <- sapply(cols_data, function(col) {
      if (length(col) == 0) return(0)
      unique_vals <- unique(col)
      prop_unique <- length(unique_vals) / length(col)
      prop_non_num <- mean(!grepl("^\\d+$", col))
      prop_unique * prop_non_num
    })
    
    max_score <- max(score_col, na.rm=TRUE)
    if (max_score < 0.5) next
    
    best_col <- which.max(score_col)
    landmarks <- cols_data[[best_col]]
    landmarks <- landmarks[landmarks != ""]
    
    if (length(landmarks) > 0) {
      return(data.frame(
        Natural_Landmark = landmarks,
        State = gsub("_", " ", state_name),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  message("No landmarks extracted for: ", state_name)
  return(NULL)
}

# Main loop over all states
scrape_all_landmarks <- function(states_vector) {
  all_landmarks <- list()
  total_states <- length(states_vector)
  
  for (i in seq_along(states_vector)) {
    state <- states_vector[i]
    message(sprintf("Scraping states [%s] %d/%d ( %2d%%) ETA: ...", 
                    paste(rep("=", i), collapse=""), 
                    i, total_states, floor(i/total_states*100)))
    
    landmarks <- scrape_state_landmarks(state)
    if (!is.null(landmarks)) {
      all_landmarks[[state]] <- landmarks
    }
  }
  
  do.call(rbind, all_landmarks)
}

# List of states with underscores (must match Wikipedia page format)
states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
  "New_Hampshire", "New_Jersey", "New_Mexico", "New_York",
  "North_Carolina", "North_Dakota", "Ohio", "Oklahoma", "Oregon",
  "Pennsylvania", "Rhode_Island", "South_Carolina", "South_Dakota",
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West_Virginia", "Wisconsin", "Wyoming"
)

# Run scraping
landmarks_df <- scrape_all_landmarks(states)

# View/save results
print(head(landmarks_df))
# write.csv(landmarks_df, "national_natural_landmarks.csv", row.names=FALSE)
