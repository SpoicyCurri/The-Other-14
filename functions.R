# Matches that haven't been played are filtered out

Big6 <- c("Arsenal", "Chelsea", "Liverpool", "Manchester City", "Manchester Utd", "Tottenham")
Spa3 <- c("Real Madrid", "Barcelona", "Atlético Madrid")
Ser3 <- c("Juventus", "Milan", "Inter")
Super12 <- c(Big6, Spa3, Ser3)

Tidy_Data <- function(League_Data){
  League_Data %>%
    filter(!is.na(Home)) %>%
    select(Home, Away, Score) %>%
    filter(!is.na(Score)) %>%
    mutate("Home_Score" = str_extract(Score, "^[:digit:]"),
           "Away_Score" = str_extract(Score, "[:digit:]$"),
           "Home_Points" = if_else(Home_Score > Away_Score, 3,
                                   if_else(Home_Score < Away_Score, 0, 1)),
           "Away_Points" = if_else(Home_Score < Away_Score, 3,
                                   if_else(Home_Score > Away_Score, 0, 1)),
           "Super12_ind" = if_else(Home %in% Super12 & Away %in% Super12, "B", 
                                   if_else(Home %in% Super12, "H", 
                                           if_else(Away %in% Super12, "A", "N"))),
           "O14_Home_Points" = if_else(Super12_ind %in% c("H","B"), 0, 
                                       if_else(Super12_ind == "A", 3, Home_Points)),
           "O14_Away_Points" = if_else(Super12_ind %in% c("A","B"), 0, 
                                       if_else(Super12_ind == "H", 3, Away_Points)))
}

Updated_League_Table <- function(League_Data){
  League_Data %>%
  select("Team" = Home, "Points" = Home_Points, "O14_Points" = O14_Home_Points) %>%
  rbind(select(League_Data, "Team" = Away, "Points" = Away_Points, "O14_Points" = O14_Away_Points)) %>%
  group_by(Team) %>%
  summarise("Games" = n(),
            "Points" = sum(Points),
            "O14_Points" = sum(O14_Points),
            "Points_Gain" = O14_Points - Points) %>%
  arrange(desc(O14_Points))
}

Create_DataTable <- function(League_Table){
  DT::datatable(League_Table,
                fillContainer = T,
                rownames = T,
                extensions = c("Buttons", "Scroller"),
                options = list(pageLength = 20,
                               dom = 'Bfrti',
                               buttons = c('copy', 'csv'))
  )
}
