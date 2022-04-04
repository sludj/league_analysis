library(tidyverse)
library(rvest)
library(devtools)


# 2021 summer season data
pull_data_leaguepedia <- function(url, name_for_table) {
  html <- read_html(url)
  
  table_we_want <- html %>% 
    html_node("table") %>% 
    html_table()

  colnames(table_we_want) <- as.character(unlist(table_we_want[2,]))
  
  colnames(table_we_want)[2] <- "Player"
  colnames(table_we_want)[3] <- "Games"
  
  table_dropped <- table_we_want %>% 
    tail(-2) %>% 
    .[-1] %>% 
    mutate(table_name = name_for_table)
}

# pull summer 2021 by role
summer_2021_top <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSummer+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=top&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                         "summer_2021_top")
summer_2021_mid <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSummer+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Mid&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                         "summer_2021_mid")
summer_2021_bot <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSummer+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Bot&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                         'summer_2021_bot')
summer_2021_jung <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSummer+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Jungle&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "summer_2021_jung")
summer_2021_supp <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSummer+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Support&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "summer_2021_supp")

# pull lock-in tourney by role
lock_in_2022_top <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Brole%5D=Top&TS%5Btournament%5D=LCS%2F2022+Season%2FLock+In&pfRunQueryFormName=TournamentStatistics",
                                          "lock_in_22_top")
lock_in_2022_jung <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2022+Season%2FLock+In&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Jungle&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                           "lock_in_22_jung")
lock_in_2022_mid <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2022+Season%2FLock+In&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Mid&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "lock_in_22_mid")
lock_in_2022_bot <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2022+Season%2FLock+In&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Bot&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "lock_in_22_bot")
lock_in_2022_supp <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2022+Season%2FLock+In&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Support&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                           "lock_in_22_supp")

# pull spring 2021
spring_2021_top <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSpring+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=top&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "spring_2021_top")
spring_2021_jung <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSpring+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Jungle&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                           "spring_2021_jung")
spring_2021_mid <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSpring+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=mid&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "spring_2021_mid")
spring_2021_bot <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSpring+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=bot&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                          "spring_2021_bot")
spring_2021_supp <- pull_data_leaguepedia("https://lol.fandom.com/wiki/Special:RunQuery/TournamentStatistics?pfRunQueryFormName=TournamentStatistics&TS%5Bpreload%5D=TournamentByPlayerRole&TS%5Btournament%5D=LCS%2F2021+Season%2FSpring+Season&TS%5Blink%5D=&TS%5Bchampion%5D=&TS%5Brole%5D=Support&TS%5Bteam%5D=&TS%5Bpatch%5D=&TS%5Byear%5D=&TS%5Bregion%5D=&TS%5Btournamentlevel%5D=&TS%5Bwhere%5D=&TS%5Bincludelink%5D%5Bis_checkbox%5D=true&TS%5Bshownet%5D%5Bis_checkbox%5D=true&wpRunQuery=Run+query&pf_free_text=",
                                           "spring_2021_supp")
# get our data together
data_combined <- mget(ls(pattern = "summer*|lock_in*|spring*")) %>% 
  bind_rows()

# need to coerce to numeric for calcs
glimpse(data_combined)

data_coerced <- data_combined %>% 
  mutate(across(Games:CP, ~ str_remove(., "%"))) %>% 
  mutate(across(Games:CP, as.numeric))

compare_kpar <- data_coerced %>% 
  group_by(table_name) %>% 
  summarize(avg_kpar = mean(KPAR))

names(data_coerced)

fantasy_avg <- data_coerced %>% 
  mutate(fantasy_kda_only = K * 2 + D * -1 + A * 2 + CS / 100)

roles <- as.list(c("top", "jung", "mid", "bot", "supp"))

names(roles) <- c("top", "jung", "mid", "bot", "supp")

data_by_role_fun <- function(role) {
  fantasy_avg %>% 
    filter(str_detect(table_name, role))
}

data_by_role <- lapply(roles, data_by_role_fun)

list2env(data_by_role, envir = .GlobalEnv)

hist(mid$fantasy_kda_only, breaks=20)
par(mar=c(9,5,1,1))
boxplot(fantasy_avg$fantasy_kda_only ~ fantasy_avg$table_name, las=2)

fantasy_avg <- fantasy_avg %>% filter(Games > 2)

ggplot(fantasy_avg, aes(x = table_name, y = fantasy_kda_only, label = Player)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(vjust = -.7, hjust = -.3, check_overlap = TRUE)

fantasy_avg_no_lock <- fantasy_avg %>% filter(
  str_detect(table_name, "summer*|spring*"),
  Games > 12
) %>% 
  separate(table_name, c("a", "b", "c"), remove = FALSE) %>% 
  arrange(c, a)

ggplot(fantasy_avg_no_lock, aes(x = table_name, y = fantasy_kda_only, label = Player)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(vjust = -.7, hjust = -.3, check_overlap = TRUE)
