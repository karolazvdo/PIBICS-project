###################################################################
###################################################################
##  Languages available and pageviews from Wikipedia            ##
##  Karoline Azevedo, for PIBICS project 05 de Janeiro de 2025  ##
##################################################################
##################################################################
##
# Install and load packages 
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(dplyr, tidyr, stringr, tidywikidatar, readxl, pageviews, lubridate, purrr)

# Import dataset
data <- read_excel("C:/Users/drive/Downloads/Dataset da Wikipedia.xlsx") 

df <- data %>%
  select(Nome_Cientifico, Wikidata_ID) %>%
  filter(!is.na(Wikidata_ID)) # Remove lines without wikidata_id

# --- 1. Get the number of languages available by Wikidata ID ---
cat("Buscando número de idiomas para cada Wikidata ID...\n")

# Get information of languages available
itens <- tw_get(df$Wikidata_ID, language = "all_available", wait = 30)

# Filter columns by "label_" (representing the languages) or "sitelink"
item_df <- itens %>%
  filter(str_starts(property, "label") | str_starts(property, "sitelink"))

# Counting the number of languages by Wikidata ID
all_languages <- item_df %>%
  filter(str_starts(property, "label")) %>%  # Only labels
  group_by(id) %>%
  summarise(n_languages = n()) %>%
  rename(Wikidata_ID = id)

# Join with original data and replace NA for 0
df <- df %>%
  left_join(all_languages, by = "Wikidata_ID") %>%
  replace_na(list(n_languages = 0))

# --- 2. Get the article from Wikipedia portuguese page ---
cat("Buscando títulos de artigos na Wikipedia em português...\n")

df <- df %>%
  mutate(article = map_chr(Wikidata_ID, ~ {
    wiki_link <- tw_get_wikipedia(.x, language = "pt")
    if (!is.na(wiki_link) && length(wiki_link) > 0) {
      sub("^.*/wiki/", "", wiki_link) %>%
        str_replace_all(" ", "_")
    } else {
      NA_character_
    }
  }))

# --- 3. Get daily pageviews between 2016-2024 ---
cat("Buscando visualizações diárias (2016-2024) para artigos em português...\n")

# Only presenting articles
df_views <- df %>% filter(!is.na(article))

# Function for getting the pageviews
get_views <- function(article) {
  tryCatch({
    article_pageviews(
      project = "pt.wikipedia",
      article = article,
      user_type = "user",
      start = "2016010100",
      end = "2024010100",
      granularity = "daily"
    ) %>%
      mutate(article = article)
  }, error = function(e) {
    cat("Erro ao buscar:", article, "\n")
    return(NULL)
  })
}

# Applying the funciton
views_pt <- map_df(df_views$article, get_views)

# --- 4. Process the data and add information into original data ---
if (nrow(views_pt) > 0) {
  views_pt <- views_pt %>%
    mutate(date = as.Date(date)) %>%
    group_by(article) %>%
    summarise(
      total_views = sum(views, na.rm = TRUE),
      difTime_pt = as.numeric(difftime(max(date, na.rm = TRUE), min(date, na.rm = TRUE), units = "days")),
      averageViews_pt = round(total_views / difTime_pt, 2),
      .groups = "drop"
    )
  
  # Save the primarly data
  write.csv(views_pt, "viewsBruto_pt.csv", row.names = FALSE)
  cat("Dados brutos de visualizações salvos em 'viewsBruto_pt.csv'\n")
  
  # Add `averageViews_pt` to original data
  df <- df %>%
    left_join(views_pt %>% select(article, averageViews_pt), by = "article")
  
} else {
  cat("Nenhuma visualização encontrada para os artigos.\n")
  df <- df %>% mutate(averageViews_pt = NA_real_)  # NA into absent data
}

# --- 5. Save final data ---
write.csv(df, "df_final.csv", row.names = FALSE)
cat("Dados finais salvos em 'df_final.csv'\n")