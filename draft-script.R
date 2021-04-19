library(tidyverse)

PL_raw_data <- read_csv("sportsref_download_PL_210419.csv")

glimpse(PL_raw_data)

# Matches that haven't been played are filtered out

Big6 <- c("Arsenal", "Chelsea", "Liverpool", "Manchester City", "Manchester Utd", "Tottenham")

PL_tidy <- PL_raw_data %>%
  filter(!is.na(Home)) %>%
  select(Home, Away, Score) %>%
  filter(!is.na(Score)) %>%
  mutate("Home_Score" = str_extract(Score, "^[:digit:]"),
         "Away_Score" = str_extract(Score, "[:digit:]$"),
         "Home_Points" = if_else(Home_Score > Away_Score, 3,
                                 if_else(Home_Score < Away_Score, 0, 1)),
         "Away_Points" = if_else(Home_Score < Away_Score, 3,
                                 if_else(Home_Score > Away_Score, 0, 1)),
         "Big6_ind" = if_else(Home %in% Big6 & Away %in% Big6, "B", 
                              if_else(Home %in% Big6, "H", 
                                      if_else(Away %in% Big6, "A", "N"))),
         "O14_Home_Points" = if_else())

glimpse(PL_tidy)

Current_Table <- PL_tidy %>%
  select("Team" = Home, "Points" = Home_Points) %>%
  rbind(select(PL_tidy, "Team" = Away, "Points" = Away_Points)) %>%
  group_by(Team) %>%
  summarise("Games" = n(),
            "Points" = sum(Points)) %>%
  arrange(desc(Points))

Other14_Table <- 


