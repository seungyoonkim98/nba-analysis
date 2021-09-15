library("ggplot2")
library("tidyverse")

### Tidying Data
# No missing values
summary(player.tracking.data)
player.tracking.data$Drives[is.na(player.tracking.data$Drives),]

drives.dataset <- player.tracking.data$Drives
drives.dataset <- drives.dataset[, 6:ncol(drives.dataset)]
drives.dataset <- drives.dataset[-c(2,3)]
str(drives.dataset)

### Multiple Linear Regression
library(caTools)
set.seed(123)
split = sample.split(drives.dataset$W, SplitRatio = 0.75)
training_set = subset(drives.dataset, split == TRUE)
test_set = subset(drives.dataset, split == FALSE)

regressor = lm(formula = W ~ .,
               data = training_set)
summary(regressor)

# Backward Elimination on Significance Level 0.05
numVars = length(training_set)
for (i in c(1:numVars)){
  regressor = lm(formula = W ~ ., data = training_set)
  maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
  if (maxVar > 0.05){
    j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
    training_set = training_set[, -j]
  }
  numVars = numVars - 1
}

# Lakers' Drives stats
lakers.drives <- filter(player.tracking.data$Drives, TEAM_ABBREVIATION == "LAL")
rest.drives <- filter(player.tracking.data$Drives, TEAM_ABBREVIATION != "LAL")
total.lakers.drives <- sum(select(lakers.drives, DRIVES))

# Find Total Drives of Rest of NBA
rest.by.team <- group_by(rest.drives, TEAM_ABBREVIATION)
rest.by.team
total.rest.drives.by.team <- summarize(rest.by.team, sum(DRIVES))
colnames(total.rest.drives.by.team) <- c("TEAM_ABBREVIATION", "DRIVES")

# Plot comparisons of Lakers Drives to the Rest of the NBA's
# Total number of drives
Total.Drives <- ggplot() +
                geom_bar(data = total.rest.drives.by.team,
                         stat="Identity",
                         color = "Black",
                         aes(x = TEAM_ABBREVIATION, 
                             y = DRIVES,
                             fill = DRIVES > total.lakers.drives)) +
                geom_abline(slope = 0,
                            intercept = total.lakers.drives,
                            color = "Black",
                            lty = 2)
Final.Total.Drives <- Total.Drives +
  xlab("Teams") +
  ylab("Number of Drives per Game") +
  ggtitle("LAKERS VS NBA: DRIVES PER GAME") +
  theme(axis.title.x = element_text(color="Black", size=20),
        axis.title.y = element_text(color="Black", size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title=element_text(size=20),
        legend.text = element_text(size=10),
        legend.justification = c(1,1),
        
        plot.title = element_text(color="Black", 
                                  size=30,
                                  family="Courier")) +
  scale_fill_manual(values = c("grey46", "olivedrab2"),
                    labels = c("Less Drives Per Game than Lakers",
                               "More Drives Per Game than Lakers"),
                    name = "Teams With")
Final.Total.Drives

# Find Total Drives of All NBA (TODO - RUN STATISTICS)
nba.by.team <- group_by(player.tracking.data$Drives, TEAM_ABBREVIATION)
nba.by.team
total.nba.drives.by.team <- summarize(nba.by.team, sum(DRIVES))
total.nba.drives.by.team
colnames(total.nba.drives.by.team) <- c("TEAM_ABBREVIATION", "DRIVES")
summary(total.nba.drives.by.team)

### Compare Top 5 Lakers' Drive Converters to rest of NBA
# Find Lakers' Top 5 Most Frequent Drivers
arrange(lakers.drives, desc(DRIVES))
lakers.frequent.drivers <- head(arrange(lakers.drives, desc(DRIVES)), 5)
lakers.frequent.drivers

# Find Rest of NBA's Top 100 Most Frequent Drivers
arrange(rest.drives, desc(DRIVES))
rest.frequent.drivers <- head(arrange(rest.drives, desc(DRIVES)), 100)
rest.frequent.drivers

frequent.drivers <- bind_rows(lakers.frequent.drivers, rest.frequent.drivers)
frequent.drivers <- frequent.drivers[!duplicated(frequent.drivers$PLAYER_NAME),]

# Amongst the NBA's Top 100 Most Frequeent Drivers, where do the
# Lakers' Top 5 Most Frequent Drivers stand
Drive.Conv.Pts <- ggplot(data = frequent.drivers, aes(x = DRIVES, y = DRIVE_PTS)) +
                  geom_point(aes(color = TEAM_ABBREVIATION == "LAL"),
                             alpha = 0.7) +
                  geom_smooth(data = rest.frequent.drivers)
Final.Drive.Conv.Pts <- Drive.Conv.Pts +
  xlab("Number of Drives per Game") +
  ylab("Points Scored by Driver") +
  ggtitle("LAKERS VS NBA: DRIVES TO POINTS CONVERSIONS") +
  theme(axis.title.x = element_text(color="Black", size=20),
        axis.title.y = element_text(color="Black", size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title=element_text(size=20),
        legend.text = element_text(size=10),
        legend.justification = c(1,1),
        
        plot.title = element_text(color="Black", 
                                  size=30,
                                  family="Courier")) +
  scale_color_manual (name = "Player Team Affiliation",
                      values = c("Black", "Red"),
                      labels = c("Top 100 Frequent NBA Drivers",
                                 "Top 5 Frequent Lakers Drivers"))
Final.Drive.Conv.Pts

Drive.to.Passes <- ggplot(data = lakers.frequent.drivers,
                          aes(x = DRIVES,
                              y = DRIVE_PASSES)) +
                   geom_point(alpha = 1) +
                   geom_smooth(data = rest.frequent.drivers) +
                   geom_text(aes(label=PLAYER_NAME),
                             hjust=-.1, vjust=-1)
Drive.to.Passes

Drive.Conv.Asts <- ggplot(data = lakers.frequent.drivers,
                         aes(x = DRIVE_PASSES,
                             y = DRIVE_AST)) +
                  geom_point(alpha = 1) +
                  geom_smooth(data = rest.frequent.drivers) +
                  geom_text(aes(label=PLAYER_NAME),
                            hjust=-.1, vjust=-1)
Drive.Conv.Asts


