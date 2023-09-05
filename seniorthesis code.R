library(tidyverse)
library(baseballr)
require(caTools)
library(ggplot2)
library(psych)
library(lubridate)
library(zoo)
library(randomForest)
library(Boruta)
library(xgboost)
library(caret)
library(caTools)
library(ranger)
library(randomForest)
library(vip)
library(ggthemes)
library(DiagrammeR)
library(rpart)
library(stargazer)
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")
library(stargazer)




options(scipen = 999)
data <- read_csv("statcast_data.csv")
data$Count <- paste(data$balls, '-', data$strikes)
#data <- data %>% 
#  filter(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b) | !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) | is.na(on_1b) & !is.na(on_2b) & is.na(on_3b))
data <- data %>% 
  filter(description != 'intent_ball')
data <- data %>% 
  mutate(score_dif = home_score - away_score)
data <- data %>% 
  filter(score_dif < 10 & score_dif > -10)
data <- data %>% 
  mutate(swinging_strike = if_else(description == 'swinging_strike', 1, 0))
data$on_1b[is.na(data$on_1b)] = 0
data$on_2b[is.na(data$on_2b)] = 0
data$on_3b[is.na(data$on_3b)] = 0
data2 <- data %>% 
  select(c(player_name, pitcher, batter, game_pk,description, stand, p_throws, type, balls, strikes, on_1b, on_2b, on_3b, outs_when_up, inning, inning_topbot, pitch_number, home_score, away_score, swinging_strike, release_speed, release_pos_x, release_pos_y, pfx_x, pfx_z, spin_axis, release_extension, pitcher))

 
data2 <- na.omit(data2)
#data2 <- arrange(data2, inning)
#data2 <- data2 %>% 
#  group_by(pitcher, game_pk) %>% 
#  mutate(pitch_count = seq(n()))
model_data <- data %>% 
  select(c(swinging_strike, release_speed, release_pos_x, release_pos_y, pfx_x, pfx_z, spin_axis, release_extension, pitcher))
model_data <- na.omit(model_data)

set.seed(1)
split_index <- sample.split(model_data$swinging_strike, SplitRatio = 0.7)
train_data  <- subset(model_data, split_index == TRUE)
test_data   <- subset(model_data, split_index == FALSE)

train_matrix <- as.matrix(train_data)
test_matrix <- as.matrix(test_data)

train_matrix <- na.omit(train_matrix)
test_matrix <- na.omit(test_matrix)

model <- xgboost(data = train_matrix[, 2:8], label = train_matrix[,1], nrounds = 100, max.depth = 6,
                 eta = .25, objective = "reg:squarederror",
                 early_stopping_rounds = 3)
vip(model, num_features = 12, horizontal = TRUE, 
    aesthetics = list(color = "black", size = 1)) + ggtitle("Stuff+ Feature Importance")
xgb.plot.tree(model = model, trees = 1)
predictions <- predict(model, test_matrix[,2:8])
preds <- as.data.frame(
  matrix(predict(model, as.matrix(model_data %>% select(-swinging_strike, -pitcher))))
) %>% 
  dplyr::rename(Stuff = V1)
stuff_projs <- cbind(model_data, preds)
stuff_projs <- stuff_projs %>% 
  mutate(StuffAdj = (Stuff + 2))
stuff_projs <- stuff_projs %>% 
  mutate(`Stuff+` = (StuffAdj - mean(StuffAdj)/mean(StuffAdj))*100)

mean(stuff_projs$`Stuff+`)
stuff_projs$`Stuff+` <- stuff_projs$`Stuff+` - 10.0989

stuff_projs <- stuff_projs %>% 
  select(c(Stuff, StuffAdj, `Stuff+`))
final_df <- cbind(data2, stuff_projs)

id_map <- read_csv('player_id_map.csv')
#razz <- read_csv('razzball.csv')
id_map <- id_map %>% 
  select(c(PLAYERNAME, MLBID, IDFANGRAPHS))
player_speed <- read_csv('player_speed.csv')
player_speed$playerid <- as.character(player_speed$playerid)
player_speed <- player_speed %>% 
  left_join(id_map, by = c('playerid' = 'IDFANGRAPHS'))
player_speed <- player_speed %>% 
  select(c(Name, BsR, SB, CS, Spd, MLBID))

final_df <- final_df %>% 
  left_join(player_speed, by = c('on_1b' = 'MLBID'))
final_df <- final_df %>%
  rename(on_1b_name = Name,
         on_1b_BsR = BsR,
         on_1b_SB = SB,
         on_1b_CS = CS,
         on_1b_Spd = Spd)
final_df <- final_df %>% 
  left_join(player_speed, by = c('on_2b' = 'MLBID'))
final_df <- final_df %>%
  rename(on_2b_name = Name,
         on_2b_BsR = BsR,
         on_2b_SB = SB,
         on_2b_CS = CS,
         on_2b_Spd = Spd)

runner_1st <- final_df %>% 
  filter(on_1b > 0 & on_2b == 0)
runner_1st <- runner_1st %>% 
  select(-c(on_2b_name, on_2b_BsR, on_2b_SB, on_2b_CS, on_2b_Spd))
runner_1st <- na.omit(runner_1st)

stargazer(as.data.frame(runner_1st[c('inning','outs_when_up', 'home_score', 'away_score', 'swinging_strike', 'release_speed', 'release_pos_x', 'release_pos_y', 'pfx_x', 'pfx_z', 'spin_axis', 'release_extension', 'Stuff', 'Stuff+', 'on_1b_BsR',
                                   'on_1b_SB', 'on_1b_CS', 'on_1b_Spd')]), covariate.labels = c('Inning', 'Outs', 'Home Score', 'Away Score', 'Swinging Strike (Binary)', 'Pitch Speed', 'Pitcher Release Position (X)', 'Pitcher Release Position (Y)', 'Horizontal movement in feet', 'Vertical movement in feet',
                                                                                                'Spin Axis', 'Release Extension (Ft.)', 'Raw Stuff', 'Stuff+', '1st Base Runner BsR', '1st Base Runner SB', '1st Base Runner CS', '1st Base Runner Spd'), summary.stat = c("min", "median", "mean", "max", "sd"), type = "html", title="Summary Statistics (Runner on 1st Only)", digits=1, out="runner_1st.html")
runner_2nd <- final_df %>% 
  filter(on_1b == 0 & on_2b > 0)
runner_2nd <- runner_2nd %>% 
  select(-c(on_1b_name, on_1b_BsR, on_1b_SB, on_1b_CS, on_1b_Spd))
runner_2nd <- na.omit(runner_2nd)

stargazer(as.data.frame(runner_2nd[c('inning','outs_when_up', 'home_score', 'away_score', 'swinging_strike', 'release_speed', 'release_pos_x', 'release_pos_y', 'pfx_x', 'pfx_z', 'spin_axis', 'release_extension', 'Stuff', 'Stuff+', 'on_2b_BsR',
                                     'on_2b_SB', 'on_2b_CS', 'on_2b_Spd')]), covariate.labels = c('Inning', 'Outs', 'Home Score', 'Away Score', 'Swinging Strike (Binary)', 'Pitch Speed', 'Pitcher Release Position (X)', 'Pitcher Release Position (Y)', 'Horizontal movement in feet', 'Vertical movement in feet',
                                                                                                  'Spin Axis', 'Release Extension (Ft.)', 'Raw Stuff', 'Stuff+', '2nd Base Runner BsR', '2nd Base Runner SB', '2nd Base Runner CS', '2nd Base Runner Spd'), summary.stat = c("min", "median", "mean", "max", "sd"), type = "html", title="Summary Statistics (Runner on 2nd Only)", digits=1, out="runner_2nd.html")
runner_1st_2nd <- final_df %>% 
  filter(on_1b > 0 & on_2b > 0)
runner_1st_2nd <- na.omit(runner_1st_2nd)
stargazer(as.data.frame(runner_1st_2nd[c('inning','outs_when_up', 'home_score', 'away_score', 'swinging_strike', 'release_speed', 'release_pos_x', 'release_pos_y', 'pfx_x', 'pfx_z', 'spin_axis', 'release_extension', 'Stuff', 'Stuff+', 'on_1b_BsR',
                                     'on_1b_SB', 'on_1b_CS', 'on_1b_Spd', 'on_2b_BsR', 'on_2b_SB', 'on_2b_CS', 'on_2b_Spd')]), covariate.labels = c('Inning', 'Outs', 'Home Score', 'Away Score', 'Swinging Strike (Binary)', 'Pitch Speed', 'Pitcher Release Position (X)', 'Pitcher Release Position (Y)', 'Horizontal movement in feet', 'Vertical movement in feet',
                                                                                                  'Spin Axis', 'Release Extension (Ft.)', 'Raw Stuff', 'Stuff+', '1st Base Runner BsR', '1st Base Runner SB', '1st Base Runner CS', '1st Base Runner Spd', '2nd Base Runner BsR', '2nd Base Runner SB', '2nd Base Runner CS', '2nd Base Runner Spd'), summary.stat = c("min", "median", "mean", "max", "sd"), type = "html", title="Summary Statistics (Runner on 1st and 2nd)", digits=1, out="runner_1st_2nd.html")
no_runners_on <- final_df %>% 
  filter(on_1b == 0 & on_2b == 0 & on_3b == 0)

#batting revert
runner_1st <- runner_1st %>% 
  left_join(batting, by = c('batter' = 'MLBID'))
runner_1st <- runner_1st %>% 
  left_join(pitching, by = c('pitcher' = 'MLBID'))
runner_1st <- runner_1st %>% 
  filter(PA > 100)
runner_1st <- runner_1st %>% 
  filter(IP > 10)
runner_1st <- runner_1st %>% 
  mutate(score_dif = home_score - away_score)
runner_1st$Count <- paste(runner_1st$balls, '-', runner_1st$strikes)
runner_1st <- na.omit(runner_1st)

runner_1st <- runner_1st %>% 
  filter(Count != "4 - 2")

runner_1st_model <- lm(`Stuff+` ~ `wRC+` + xFIP  + as.factor(outs_when_up) + inning + on_1b_Spd + on_1b_SB + score_dif + on_1b_SB*score_dif + on_1b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st)
summary(runner_1st_model)
vif(runner_1st_model)

runner_2nd <- runner_2nd %>% 
  left_join(batting, by = c('batter' = 'MLBID'))
runner_2nd <- runner_2nd %>% 
  left_join(pitching, by = c('pitcher' = 'MLBID'))
runner_2nd <- runner_2nd %>% 
  filter(PA > 100)
runner_2nd <- runner_2nd %>% 
  filter(IP > 10)
runner_2nd <- runner_2nd %>% 
  mutate(score_dif = home_score - away_score)
runner_2nd$Count <- paste(runner_2nd$balls, '-', runner_2nd$strikes)
runner_2nd <- na.omit(runner_2nd)

runner_2nd_model <- lm(`Stuff+` ~ `wRC+` + xFIP  + as.factor(outs_when_up) + inning + on_2b_Spd + on_2b_SB + score_dif + on_2b_SB*score_dif + on_2b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_2nd)
summary(runner_2nd_model)
vif(runner_2nd_model)


runner_1st_2nd <- runner_1st_2nd %>% 
  left_join(batting, by = c('batter' = 'MLBID'))
runner_1st_2nd <- runner_1st_2nd %>% 
  left_join(pitching, by = c('pitcher' = 'MLBID'))
runner_1st_2nd <- runner_1st_2nd %>% 
  filter(PA > 100)
runner_1st_2nd <- runner_1st_2nd %>% 
  filter(IP > 10)
runner_1st_2nd <- runner_1st_2nd %>% 
  mutate(score_dif = home_score - away_score)
runner_1st_2nd$Count <- paste(runner_1st_2nd$balls, '-', runner_1st_2nd$strikes)
runner_1st_2nd <- na.omit(runner_1st_2nd)

runner_1st_2nd_model <- lm(`Stuff+` ~ `wRC+` + xFIP  + as.factor(outs_when_up) + inning + on_2b_Spd + on_2b_SB + score_dif + on_2b_SB*score_dif + on_2b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st_2nd)
summary(runner_1st_2nd_model)
vif(runner_1st_2nd_model)



#model results
stargazer(runner_1st_model, runner_2nd_model, runner_1st_2nd_model, title = 'Table 1', column.labels = c("Runner on 1st Only","Runner on 2nd Only","Runners on 1st and 2nd"),
          dep.var.labels = "Stuff+", #keep = c('home_ft', 'Attend', 'playoff', 'atts_game:', 'experience_fts:','running.ft_perc:'),
          omit = ("Constant"), 
          covariate.labels = c('wRC+', 'xFIP', '1 Out', '2 Outs', 'Inning', 'Runner on 1st Spd', 'Runner on 1st Stolen Bases', 'Runner on 2nd Spd', 'Runner on 2nd Stolen Bases', 'Score Dif', 'Pitcher Throws (Right)', 'Batter Stance (Right)', 'Count: 0-1', 'Count: 0-2', 'Count: 1-0', 'Count: 1-1', 'Count: 1-2', 'Count: 2-0', 'Count: 2-1', 'Count: 2-2', 'Count: 3-0', 'Count: 3-1', 'Count: 3-2', 'Pitch Number', 'Runner on 1st Stolen Bases X Score Dif', 'Runner on 1st Spd X Score Dif', 'Runner on 2nd Stolen Bases X Score Dif', 'Runner on 2nd Spd X Score Dif'),
          #notes = 'All models include OneShot, PrevMade, PrevMiss, Up5_10, Up4, Up3, Up2, Up1, Tied, Down1, Down2, Down3, Down4, and Down5_10', model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table1.html")


#more modelling
runner_1st_7th <- runner_1st %>% 
  filter(inning >= 7)
runner_1st_model_7th <- lm(`Stuff+` ~ `wRC+` + xFIP + on_1b_Spd + on_1b_SB + score_dif + on_1b_SB*score_dif + on_1b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st_7th)
summary(runner_1st_model)
vif(runner_1st_model)

runner_2nd_7th <- runner_2nd %>% 
  filter(inning >= 7)
runner_2nd_model_7th <- lm(`Stuff+` ~ `wRC+` + on_2b_Spd + on_2b_SB + score_dif + on_2b_SB*score_dif + on_2b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_2nd_7th)
summary(runner_2nd_model_7th)
vif(runner_2nd_model_7th)

runner_1st_2nd_7th <- runner_1st_2nd %>% 
  filter(inning >= 7)
runner_1st_2nd_model_7th <- lm(`Stuff+` ~ `wRC+` + on_2b_Spd + on_2b_SB + score_dif + on_2b_SB*score_dif + on_2b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st_2nd_7th)
summary(runner_1st_2nd_model_7th)
vif(runner_1st_2nd_model_7th)

#score difference
runner_1st_score_dif <- runner_1st %>% 
  filter(score_dif <= 1 & score_dif >= -1)
runner_1st_model_score_dif <- lm(`Stuff+` ~ `wRC+` + xFIP + on_1b_Spd + on_1b_SB + score_dif + on_1b_SB*score_dif + on_1b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st_score_dif)
summary(runner_1st_model_score_dif)
vif(runner_1st_model_score_dif)

runner_2nd_score_dif <- runner_2nd %>% 
  filter(score_dif <= 1 & score_dif >= -1)
runner_2nd_model_score_dif <- lm(`Stuff+` ~ `wRC+` + on_2b_Spd + on_2b_SB + score_dif + on_2b_SB*score_dif + on_2b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_2nd_score_dif)
summary(runner_2nd_model_score_dif)
vif(runner_2nd_model_score_dif)

runner_1st_2nd_score_dif <- runner_1st_2nd %>% 
  filter(score_dif <= 1 & score_dif >= -1)
runner_1st_2nd_model_score_dif <- lm(`Stuff+` ~ `wRC+` + on_2b_Spd + on_2b_SB + score_dif + on_2b_SB*score_dif + on_2b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st_2nd_score_dif)
summary(runner_1st_2nd_model_score_dif)
vif(runner_1st_2nd_model_score_dif)

#score dif & past 7th inning
runner_1st_score_dif_7th_inn <- runner_1st %>% 
  filter(score_dif <= 1 & score_dif >= -1 & inning >= 7)
runner_1st_model_score_dif_7th <- lm(`Stuff+` ~ `wRC+` + xFIP + on_1b_Spd + on_1b_SB + score_dif + on_1b_SB*score_dif + on_1b_Spd*score_dif + as.factor(p_throws) + as.factor(stand) + as.factor(Count) + pitch_number, data = runner_1st_score_dif_7th_inn)
summary(runner_1st_model_score_dif)

#more modelling results
stargazer(runner_1st_model, runner_1st_model_7th, runner_1st_model_score_dif, runner_1st_model_score_dif_7th, title = 'Table 2: Analysis of Runner on 1st Only', column.labels = c("All Situations","7th Inning or Later","1 Run Game", "7th Inning or Later and 1 Run Game"),
          dep.var.labels = "Stuff+", #keep = c('home_ft', 'Attend', 'playoff', 'atts_game:', 'experience_fts:','running.ft_perc:'),
          omit = ("Constant"), 
          covariate.labels = c('wRC+', 'xFIP', 'Runner on 1st Spd', 'Runner on 1st Stolen Bases', 'Score Dif', 'Pitcher Throws (Right)', 'Batter Stance (Right)', 'Count: 0-1', 'Count: 0-2', 'Count: 1-0', 'Count: 1-1', 'Count: 1-2', 'Count: 2-0', 'Count: 2-1', 'Count: 2-2', 'Count: 3-0', 'Count: 3-1', 'Count: 3-2', 'Pitch Number', 'Runner on 1st Stolen Bases X Score Dif', 'Runner on 1st Spd X Score Dif'),
          #notes = 'All models include OneShot, PrevMade, PrevMiss, Up5_10, Up4, Up3, Up2, Up1, Tied, Down1, Down2, Down3, Down4, and Down5_10', model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table2.html")

#batters
batting <- read_csv("career_batting.csv")
player_id <- read_csv("player_id_map.csv")
player_id <- player_id %>% 
  select(c(PLAYERNAME, MLBID, IDFANGRAPHS))
batting$playerid <- as.character(batting$playerid)
player_id$IDFANGRAPHS <- as.character(player_id$IDFANGRAPHS)
batting <- batting %>% 
  left_join(player_id, by = c('playerid' = 'IDFANGRAPHS'))
batting <- batting %>% 
  select(c(Name, PA, wOBA, `wRC+`, MLBID))

#pitchers
pitching <- read_csv("career_pitching.csv")
pitching$playerid <- as.character(pitching$playerid)
player_id$IDFANGRAPHS <- as.character(player_id$IDFANGRAPHS)
pitching <- pitching %>% 
  left_join(player_id, by = c('playerid' = 'IDFANGRAPHS'))
pitching <- pitching %>% 
  select(c(Name, IP, ERA, xFIP, MLBID))














###
no_runners_on <- no_runners_on %>% 
  select(c(player_name, `Stuff+`))
mean(no_runners_on$`Stuff+`)
no_runners_on$bases <- 'No Runners On'
runner_1st <- runner_1st %>% 
  select(c(player_name, `Stuff+`))
mean(runner_1st$`Stuff+`)
runner_1st$bases <- 'Runner on First Only'
runner_2nd <- runner_2nd %>% 
  select(c(player_name, `Stuff+`))
mean(runner_2nd$`Stuff+`)
runner_2nd$bases <- 'Runner on Second Only'
runner_1st_2nd <- runner_1st_2nd %>% 
  select(c(player_name, `Stuff+`))
mean(runner_1st_2nd$`Stuff+`)
runner_1st_2nd$bases <- 'Runners on First and Second'

total_runners <- rbind(no_runners_on, runner_1st, runner_2nd, runner_1st_2nd)
ggplot(total_runners, aes(bases, `Stuff+`)) +
  geom_boxplot() +
  geom_jitter() + 
  scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(runner_1st, aes(on_1b_SB, `Stuff+`)) +
  geom_point() +
  ggtitle("Stuff+ Vs. Runner On First Career Stolen Bases") +
  labs(x = "Stolen Bases",
       y = "Stuff+") 

#RF Model
library(pdp)
library(partykit)
install.packages('randomForest')
rf_data <- runner_1st %>% 
  select(c(`Stuff+`, `wRC+`, xFIP, inning, on_1b_Spd, on_1b_SB, score_dif, p_throws, stand, balls, strikes, outs_when_up, pitch_number))
set.seed(123) # For reproducibility
rf_data <- janitor::clean_names(rf_data)
rf_made <- ranger(stuff ~ ., data = rf_data, num.trees = 100, importance = 'impurity')
vip(rf_made, num_features = 12, horizontal = TRUE, 
    aesthetics = list(color = "black", size = 1)) + ggtitle("Figure 2", subtitle = "Random Forest Model - Level of Importance for Each Variable Against Stuff+")



rf_model <- ranger(stuff ~ ., data = rf_data, importance = "permutation")
var_importance <- importance(rf_model)

dim(rf_data)
rf_grid <- expand.grid(mtry = seq(2,8, by = 1),
                       splitrule = 'variance', min.node.size = 5)
rf_made_tune <- train(stuff ~., data = rf_data, method = 'ranger', num.trees = 20,
                      trControl = trainControl(method = 'cv', number = 5), tuneGrid = rf_grid)
rf_made_tune$bestTune
rf_made_best <- ranger(stuff ~ ., data = rf_data, 
                       num.trees = 100, importance = "impurity",
                       mtry = 3)
rf_pred <- predict(rf_made, data = rf_data)$predictions
confusionMatrix(rf_pred, rf_data$stuff)
library(randomForest)
library(reprtree)

model <- randomForest(stuff ~ ., data=rf_data, importance=TRUE, ntree=10, mtry = 5, do.trace=100)

reprtree:::plot.getTree(model)


init_tree <- rpart(formula = `Stuff+` ~ on_1b_Spd + on_1b_SB + score_dif,
                   data = runner_1st_score_dif_7th_inn, method = 'class')
rpart.plot(init_tree)





#visualizations
runner_1st <- runner_1st %>% 
  mutate(spd_group = case_when(
    on_1b_Spd > 0 & on_1b_Spd <= 2.5 ~ '0.1 to 2.5',
    on_1b_Spd > 2.5 & on_1b_Spd <= 5 ~ '2.6 to 5',
    on_1b_Spd > 5 & on_1b_Spd <= 7.5 ~ '5.1 to 7.5',
    on_1b_Spd > 7.5 & on_1b_Spd <= 9.4 ~ '7.6 to 9.4'
  ))

speed_grouping <- runner_1st %>% 
  group_by(spd_group) %>% 
  summarise(avg_spd = mean(on_1b_Spd), avg_sb = mean(on_1b_SB), avg_stuff = mean(`Stuff+`))
speed_grouping %>%
  ungroup() %>% 
  dplyr::select(spd_group,avg_spd, avg_sb, avg_stuff) %>%
  gt() %>%
  gt_theme_538() %>% 
  tab_header(title = md("**Difference in Stuff+ By Baserunner Spd**"), subtitle = '2011 to 2021 Seasons | Runner on 1st Only') %>%
  cols_label(
    spd_group = 'Runner Spd',
    avg_spd = 'Average Spd',
    avg_sb = 'Average SB',
    avg_stuff = "Average Stuff+"
  ) %>%
  cols_align(
    align = "center",
    columns = 1:4
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 2,
    suffixing = TRUE
  ) %>% gtsave('speed_group.png')

runner_1st <- runner_1st %>% 
  mutate(sb_group = case_when(
    on_1b_SB >= 0 & on_1b_SB <= 9 ~ '0 to 9',
    on_1b_SB > 9 & on_1b_SB <= 50 ~ '10 to 50',
    on_1b_SB > 50 & on_1b_SB <= 73 ~ '51 to 73',
    on_1b_SB > 73 & on_1b_SB <= 392 ~ '74 to 392'
  ))

stolenBases_grouping <- runner_1st %>% 
  group_by(sb_group) %>% 
  summarise(avg_spd = mean(on_1b_Spd), avg_sb = mean(on_1b_SB), avg_stuff = mean(`Stuff+`))
stolenBases_grouping %>%
  ungroup() %>% 
  dplyr::select(sb_group,avg_spd, avg_sb, avg_stuff) %>%
  gt() %>%
  gt_theme_538() %>% 
  tab_header(title = md("**Difference in Stuff+ By Career Stolen Bases**"), subtitle = '2011 to 2021 Seasons | Runner on 1st Only') %>%
  cols_label(
    sb_group = 'Runner Stolen Bases',
    avg_spd = 'Average Spd',
    avg_sb = 'Average SB',
    avg_stuff = "Average Stuff+"
  ) %>%
  cols_align(
    align = "center",
    columns = 1:4
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 2,
    suffixing = TRUE
  ) %>% gtsave('sb_group.png')
