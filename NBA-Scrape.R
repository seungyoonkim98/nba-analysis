library("jsonlite")
library("curl")

getwd()

#Create list containing dataframes of the 12 different types player tracking statistics
player.tracking.data <- list()
pt.json.files <- c("PlayerTrackingDrives.json","PlayerTrackingDefensiveImpact.json","PlayerTrackingCatchAndShoot.json",
                   "PlayerTrackingPassing.json","PlayerTrackingTouches.json","PlayerTrackingPullUp.json",
                   "PlayerTrackingRebounding.json","PlayerTrackingShootingEfficiency.json","PlayerTrackingSpeedDist.json",
                   "PlayerTrackingElbowTouches.json","PlayerTrackingPostUps.json","PlayerTrackingPaintTouches.json")

for (file in 1:length(pt.json.files)) {
  data <- fromJSON(pt.json.files[file])
  data.df <- do.call(rbind.data.frame, data$resultSets$rowSet)
  col.name <- do.call(rbind, data$resultSets$headers)
  colnames(data.df) <- col.name[,]
  player.tracking.data[[file]] <- data.df
  rm(data)
  rm(data.df)
  rm(col.name)
}

rm(file)
rm(pt.json.files)

#Relabel List Elements
names(player.tracking.data) <- c("Drives","Defense","CatchAndShoot","Passing","Touches","PullUp",
                                 "Rebounding","ShootingEff","SpeedDistance","ElbowTouches","PostUps","PaintTouches")

#Refactor Data
for (df in 1:length(player.tracking.data)) {
  l <- length(player.tracking.data[[df]])
  player.tracking.data[[df]][5:l] <- sapply(player.tracking.data[[df]][5:l], as.character)
  player.tracking.data[[df]][5:l] <- sapply(player.tracking.data[[df]][5:l], as.numeric)
}
rm(l)
