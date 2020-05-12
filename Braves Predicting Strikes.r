library(data.table)

baseball <- read.csv(url("https://raw.githubusercontent.com/double-dose-larry/atl_2019_statcast/master/atl_2019_pitches.csv"))
setwd("C:/Users/OWNER/OneDrive - Emory University/Spring 2020/Sports Analytics/Individual Project")
stats <- fread("stats1.csv", header=TRUE)
colnames(stats) <- c("last_name","first_name","player_name","year","ip","er","era")
baseball$player_name <- as.character(baseball$player_name)
stats <- stats[,c(3,5,6)]

library(dplyr)
library(stringr)
merged <- left_join(baseball, stats, by="player_name")
merged$era <- (merged$er / merged$ip) * 9
merged$batter_temp <- word(merged$des, 1, 3)
batter_keys <- na.omit(unique(data.frame(merged$batter, merged$batter_temp)))
batter_keys <- batter_keys[!(word(batter_keys$merged.batter_temp, 2) %in% c("challenged", "reviewed", "Jays", "Tyler")),]
batter_keys <- batter_keys[!(word(batter_keys$merged.batter_temp, 3) %in% c("caught", "out")),]
batter_keys$merged.batter_temp <- word(batter_keys$merged.batter_temp,1,2)
batter_keys <- unique(batter_keys)
length(unique(batter_keys$merged.batter))

hitters <- fread("hitters.csv", header=TRUE)
colnames(hitters) <- c("last_name","first_name","year","OPS", "drop")
hitters$player <- paste(hitters$first_name, hitters$last_name)
colnames(batter_keys) <- c("batter", "player")
hitters <- hitters[,c(4,6)]
hitters$player[hitters$player=="Ronald Acuna Jr."] <- "Ronald Acuna"
batter_keys<- left_join(batter_keys, hitters)
batter_keys$OPS[batter_keys$player=="Austin Riley"] <- mean(batter_keys$OPS, na.rm = TRUE)
batter_keys$OPS[batter_keys$player %in% c("Alex Jackson", "John Ryan")] <- .7 * mean(batter_keys$OPS, na.rm = TRUE)
batter_keys$OPS[is.na(batter_keys$OPS)] <- min(batter_keys$OPS, na.rm = TRUE)
merged <- left_join(merged, batter_keys[,c(1,3)])

# create strike column - 1 indicates strike (foul, called_strike, swinging_strike, foul_tip, swinging_strike_blocked, foul_bunt, missed_bunt)
# 0 indicates everything else
merged$is_strike <- ifelse(merged$description == "foul" | merged$description == "called_strike" | merged$description == "swinging_strike" |
                               merged$description == "foul_tip" | merged$description == "swinging_strike_blocked" | merged$description == "foul_bunt" |
                               merged$description == "missed_bunt", 1, 0)


# choose which columns to use
df <- merged[,c("pitch_type","release_speed","release_pos_x","release_pos_z","pitcher","batter","balls","strikes","outs_when_up","home_team","away_team",
                  "on_3b","on_2b","on_1b","inning","inning_topbot","p_throws","stand","sz_top","sz_bot","effective_speed","release_speed","release_spin_rate",
                  "release_extension","at_bat_number","pitch_number","bat_score","fld_score","era", "OPS", "is_strike")]

# make dummies for on_3b, on_2b, on_1b
df$on_3b_dum <- ifelse(!is.na(df$on_3b), 1, 0)
df$on_2b_dum <- ifelse(!is.na(df$on_2b), 1, 0)
df$on_1b_dum <- ifelse(!is.na(df$on_1b), 1, 0)

# make dummy for whether or not pitcher is home
df$pitcher_is_home <- ifelse(df$inning_topbot == "Top",1,0)

# make dummy for whether pitcher's team is winning or not
df$pitcher_is_winning <- ifelse(df$fld_score>df$bat_score, 1, 0)
df$tied <- ifelse(df$fld_score==df$bat_score, 1, 0)

#intense
df$intense <- ifelse(abs(df$fld_score-df$bat_score)<2 & df$on_2b_dum + df$on_3b_dum>=1, 1, 0)

# measure release points relative to average for each pitcher
df <- df %>% group_by(pitcher) %>%
  mutate(release_pos_x = release_pos_x - mean(release_pos_x)) %>%
  mutate(release_pos_z = release_pos_z - mean(release_pos_z))

# remove columns - since we made dummies, dont need the old on_3b etc or the home_team and away_team
df2 <- df[,c("pitch_type","release_speed","release_pos_x","release_pos_z","balls","strikes","outs_when_up",
             "inning","p_throws","stand","sz_top","sz_bot","release_spin_rate",
             "release_extension","at_bat_number","pitch_number","bat_score","fld_score","on_3b_dum","on_2b_dum","on_1b_dum",
             "pitcher_is_home","pitcher_is_winning","tied","era", "OPS", "intense", "is_strike")]


# remove NAs
df2 <- df2[complete.cases(df2),]

# change R/L to 1/0
df2$stand <- ifelse(df2$stand == "R",1,0)
df2$p_throws <- ifelse(df2$p_throws == "R",1,0)
df2$sz_diff <- df2$sz_top - df2$sz_bot
df2$release_pos_x <- sqrt(abs(df2$release_pos_x+.001))
df2$release_pos_z <- sqrt(abs(df2$release_pos_z+.001))
df2$pitch_type <- as.character(df2$pitch_type)
for (i in 1:nrow(df2)) {
  df2$pitch_type[i] <- ifelse(df2$pitch_type[i]=="FF", "AFF", df2$pitch_type[i])
}
df2$pitch_type <- as.factor(df2$pitch_type)
#df2$count <- paste(df2$balls,df2$strikes, sep="")
#df2$count <- as.factor(df2$count)

df2 <- df2[,c("pitch_type","release_speed","release_pos_x","release_pos_z","balls", "strikes", "outs_when_up",
             "inning","p_throws","stand","sz_diff","release_spin_rate",
             "release_extension","at_bat_number","pitch_number","bat_score","fld_score","on_3b_dum","on_2b_dum","on_1b_dum",
             "pitcher_is_home","pitcher_is_winning","tied","era", "OPS", "intense", "is_strike")]

df2 = df2[df2$pitch_type!= "",]
# logistic regression
library(caret)
train.index <- createDataPartition(df2$pitch_type, p = .7, list = FALSE)
train <- df2[ train.index,]
test  <- df2[-train.index,]

m1 <- glm(is_strike~.,data=train,family=binomial)
pred1 <- predict(m1,newdata=test,type="response")

perf1 = data.frame(pred1,test$is_strike)
perf1$ypred = ifelse(perf1$pred1>=0.5,1,0)
accuracy <- table(perf1$ypred, perf1$test.is_strike)
# model accuracy
sum(diag(accuracy))/sum(accuracy)

df3 <- df2[,c("pitch_type","strikes","sz_diff","release_spin_rate","pitch_number","bat_score",
              "on_2b_dum","pitcher_is_winning","era","OPS","is_strike")]
train.index2 <- createDataPartition(df3$pitch_type, p = .7, list = FALSE)
train2 <- df3[ train.index2,]
test2  <- df3[-train.index2,]

m2 <- glm(is_strike~.,data=train2,family=binomial)
pred2 <- predict(m2,newdata=test2,type="response")

perf2 = data.frame(pred2,test2$is_strike)
perf2$ypred = ifelse(perf2$pred2>=0.5,1,0)
accuracy2 <- table(perf2$ypred, perf2$test2.is_strike)
# model accuracy
sum(diag(accuracy2))/sum(accuracy2)
