## Language distribution of less and more responses

Before proceeding with the substantive text analysis, the language composition of the
responses was examined separately for the *less* and *more* items. The distribution
is reported both for all observations and for unique response wordings. Language
classification was refined using the detected language together with script and
language-specific textual markers.

```{r language-distribution, message=FALSE, warning=FALSE}

library(dplyr)
library(stringr)
library(tidyr)
library(knitr)

# ------------------------------------------------------------
# Patterns used to refine language classification
# ------------------------------------------------------------

french_pattern <- paste0(
  "\\b(",
  "dans|avec|pour|mais|puisque|durant|",
  "aucun|aucune|toutes|tous|",
  "région|régions|élections?|",
  "sont|étaient|était|ailleurs|partout",
  ")\\b"
)

spanish_pattern <- paste0(
  "\\b(",
  "donde|aunque|porque|pero|",
  "zonas|regiones|rurales|urbanas|",
  "elecciones?|menos|más|todos|todas|",
  "ninguna|ninguno|fuera|dentro|país",
  ")\\b"
)


# ------------------------------------------------------------
# Refine language classification
# ------------------------------------------------------------

language_data <- text_english_analysis %>%
  filter(tag %in% c("v2elsnless", "v2elsnmore")) %>%
  mutate(
    item = recode(
      tag,
      "v2elsnless" = "Less",
      "v2elsnmore" = "More"
    ),
    
    language_refined = case_when(
      
      # Arabic script
      str_detect(text_answer_spacing, "\\p{Arabic}") ~ "Arabic",
      
      # Cyrillic script
      str_detect(text_answer_spacing, "\\p{Cyrillic}") ~ "Russian/Cyrillic",
      
      # Spanish wording
      str_detect(
        str_to_lower(text_answer_spacing),
        regex(spanish_pattern, ignore_case = TRUE)
      ) ~ "Spanish",
      
      # French wording
      str_detect(
        str_to_lower(text_answer_spacing),
        regex(french_pattern, ignore_case = TRUE)
      ) ~ "French",
      
      # Everything else
      TRUE ~ "English/Other"
    )
  )

# ------------------------------------------------------------
# Function for producing distributions
# ------------------------------------------------------------

lang_levels <- c(
  "English",
  "Arabic",
  "Russian/Cyrillic",
  "Spanish",
  "French",
  "Other"
)

language_summary <- function(data, level_name) {
  
  data %>%
    count(item, language_refined, name = "n") %>%
    group_by(item) %>%
    complete(
      language_refined = lang_levels,
      fill = list(n = 0)
    ) %>%
    mutate(
      total = sum(n),
      value = sprintf("%d (%.1f%%)", n, 100 * n / total),
      level = level_name
    ) %>%
    ungroup()
}


# All observations
language_all <- language_summary(
  language_data,
  "All observations"
)


# Unique response wordings
language_unique <- language_summary(
  language_data %>%
    distinct(item, text_answer_spacing, .keep_all = TRUE),
  "Unique wordings"
)


# ------------------------------------------------------------
# Final supplementary table
# ------------------------------------------------------------

language_table <- bind_rows(
  language_all,
  language_unique
) %>%
  select(item, level, language_refined, value, total) %>%
  distinct() %>%
  pivot_wider(
    names_from = language_refined,
    values_from = value
  ) %>%
  arrange(
    factor(item, levels = c("Less", "More")),
    factor(level, levels = c("All observations", "Unique wordings"))
  )

kable(
  language_table,
  caption = "Supplementary Table: Language distribution of less and more responses. Cells report N (%).",
  align = "llccccccc"
)
```

**Note:** Arabic and Russian/Cyrillic are identified from the writing system. French and Spanish are identified using the detected-language result together with selected language-specific textual markers. The refinement therefore also captures cases classified by the automated detector as English but containing clear non-English linguistic features.
