
#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("pitchRx")
#install.packages("e1071")
#install.packages("dplyr")

library(pitchRx)
library(dplyr)
library(e1071)

# Analsys with "pitches" data (2011)
rivera = subset(pitches, pitcher == 121250)
hughes = subset(pitches, pitcher == 461833)
dim(hughes)
# 946 70
names(hughes)
# 'url' 'inning_side' 'inning' 'next_' 'num' 'gameday_link' 'row_names' 'des' 'des_es' 'id' 'type' 'tfs' 'tfs_zulu' 'x' 'y' 'sv_id' 'start_speed' 'end_speed' 'sz_top' 'sz_bot' 'pfx_x' 'pfx_z' 'px' 'pz' 'x0' 'y0' 'z0' 'vx0' 'vy0' 'vz0' 'ax' 'ay' 'az' 'break_y' 'break_angle' 'break_length' 'pitch_type' 'type_confidence' 'zone' 'nasty' 'spin_dir' 'spin_rate' 'cc' 'mt' 'on_1b' 'on_2b' 'on_3b' 'count' 'pitcher' 'batter' 'b' 's' 'o' 'start_tfs' 'start_tfs_zulu' 'stand' 'b_height' 'p_throws' 'atbat_des' 'atbat_des_es' 'event' 'score' 'home_team_runs' 'away_team_runs' 'event2' 'event3' 'batter_name' 'pitcher_name' 'event4' 'date'
# rivera[,c(17, 34, 35, 36, 41, 42)]
# start_speed break_y break_angle break_length spin_dir spin_rate

# KMeans Clustering
print("K-Means Clustering")
print("Mariano Rivera")
riveraNSC = sapply(rivera[,c(17, 34, 35, 36, 41, 42)], as.numeric)
riveraSC = scale(riveraNSC)
pclust.NSC.rivera <- kmeans(riveraNSC, 2, nstart=10)
rivera$clusterNSC <- pclust.NSC.rivera$cluster
pclust.SC.rivera <- kmeans(riveraSC, 2, nstart=10)
rivera$clusterSC <- pclust.SC.rivera$cluster
table(rivera$pitch_type, rivera$clusterNSC)
table(rivera$pitch_type, rivera$clusterSC)

print("Phil Hughes")
hughesNSC = sapply(hughes[,c(17, 34, 35, 36, 41, 42)], as.numeric)
hughesSC = scale(hughesNSC)
pclust.NSC.hughes <- kmeans(hughesNSC, 2, nstart=10)
hughes$clusterNSC <- pclust.NSC.hughes$cluster
pclust.SC.hughes <- kmeans(hughesSC, 2, nstart=10)
hughes$clusterSC <- pclust.SC.hughes$cluster
table(hughes$pitch_type, hughes$clusterNSC)
table(hughes$pitch_type, hughes$clusterSC)

# Hierarchical Clustering
# http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html
print("Hierarchical Clustering")
print("Mariano Rivera")
d.rivera = dist(as.matrix(riveraSC), method = 'maximum')
hc.rivera = hclust(d.rivera, method = 'ward.D2')
clustcut.rivera = cutree(hc.rivera, 2)
rivera$hierclust <- clustcut.rivera
table(rivera$pitch_type, rivera$hierclust)
table(rivera$clusterSC, rivera$hierclust)

print("Phil Hughes")
d.hughes = dist(as.matrix(hughesSC), method = 'maximum')
hc.hughes = hclust(d.hughes, method = 'ward.D2')
clustcut.hughes = cutree(hc.hughes, 2)
hughes$hierclust <- clustcut.hughes
table(hughes$pitch_type, hughes$hierclust)
table(hughes$clusterSC, hughes$hierclust)


# SVM
# http://rischanlab.github.io/SVM.html
print("Support Vector Machines")
print("Mariano Rivera")
svm_model.rivera <- svm(riveraSC, as.factor(rivera$pitch_type))
svm_predict.rivera <- predict(svm_model.rivera, riveraSC)
rivera$svmpred <- svm_predict.rivera
print("MLB Classification vs SVM:")
table(rivera$pitch_type, rivera$svmpred)
print("Hierarchical Clustering vs SVM:")
table(rivera$hierclust, rivera$svmpred)
print("K-Means vs SVM:")
table(rivera$clusterSC, rivera$svmpred)

print("Phil Hughes")
svm_model.hughes <- svm(hughesSC, as.factor(hughes$pitch_type), kernel="radial")
svm_predict.hughes <- predict(svm_model.hughes, hughesSC)
hughes$svmpred <- svm_predict.hughes
print("MLB Classification vs SVM:")
table(hughes$pitch_type, hughes$svmpred)
print("Hierarchical Clustering vs SVM:")
table(hughes$hierclust, hughes$svmpred)
print("K-Means vs SVM:")
table(hughes$clusterSC, hughes$svmpred)


# load game ids to filter out 2010 games where Rivera or Hughes pitched
data(gids, package="pitchRx")

# I manually got all the games Phil Hughes and Mariano Rivera pitched by checking their game logs at mlb.com
riveraandhughes2010 = c("2010-10-02","2010-09-26","2010-09-21","2010-09-15","2010-09-10","2010-09-05","2010-08-31","2010-08-25","2010-08-19","2010-08-14","2010-08-09","2010-08-04","2010-07-30","2010-07-25","2010-07-20","2010-07-09","2010-07-04","2010-06-29","2010-06-19","2010-06-13","2010-06-08","2010-06-02","2010-05-28","2010-05-22","2010-05-17","2010-05-12","2010-05-07","2010-05-02","2010-04-27","2010-04-21","2010-04-15","2010-09-28","2010-09-20","2010-09-19","2010-09-17","2010-09-14","2010-09-11","2010-09-04","2010-09-03","2010-09-01","2010-08-29","2010-08-28","2010-08-21","2010-08-18","2010-08-17","2010-08-11","2010-08-10","2010-08-08","2010-08-07","2010-07-31","2010-07-26","2010-07-21","2010-07-18","2010-07-16","2010-07-08","2010-07-05","2010-07-02","2010-07-01","2010-06-27","2010-06-25","2010-06-23","2010-06-20","2010-06-11","2010-06-09","2010-06-06","2010-06-03","2010-06-01","2010-05-30","2010-05-26","2010-05-25","2010-05-21","2010-05-18","2010-05-16","2010-05-14","2010-04-30","2010-04-29","2010-04-20","2010-04-18","2010-04-13","2010-04-11","2010-04-07", "2010-04-06")
yankees10 = gids[grepl("nya", gids) & grepl("2010", gids)]
riveraandhughes2010 = gsub("-","_",riveraandhughes2010)
rh2010.gids = unique(grep(paste(riveraandhughes2010,collapse="|"), yankees10, value=TRUE))
# rh2010.gids # list of game ids

# this took over 5 minutes on my computer
dat <- scrape(game.ids = rh2010.gids, async = FALSE)

rh2010dat = dat$pitch %>% left_join(dat$atbat, by = c("num","gameday_link"))
dim(rh2010dat)
#25692 71
rivera2010 = subset(rh2010dat, pitcher == 121250 & (pitch_type == "FC" | pitch_type == "FF"))
hughes2010 = subset(rh2010dat, pitcher == 461833 & (pitch_type == "FC" | pitch_type == "FF"))
#names(rivera2010[,c(12, 29, 30, 31, 36, 37)])
#unique(rivera2010$pitch_type)
rivera2010NSC = sapply(rivera2010[,c(12, 29, 30, 31, 36, 37)], as.numeric)
rivera2010SC = scale(rivera2010NSC)
hughes2010NSC = sapply(hughes2010[,c(12, 29, 30, 31, 36, 37)], as.numeric)
hughes2010SC = scale(hughes2010NSC)
#table(subset(rh2010dat, pitcher == 121250)$pitch_type)

# KMeans Clustering
print("K-Means Clustering")
print("Mariano Rivera")
pclust.SC.rivera2010 <- kmeans(rivera2010SC, 2, nstart=10)
rivera2010$clusterSC <- pclust.SC.rivera2010$cluster
table(rivera2010$pitch_type, rivera2010$clusterSC)

print("Phil Hughes")
pclust.SC.hughes2010 <- kmeans(hughes2010SC, 2, nstart=10)
hughes2010$clusterSC <- pclust.SC.hughes2010$cluster
table(hughes2010$pitch_type, hughes2010$clusterSC)


# Hierarchical Clustering
# http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html
print("Hierarchical Clustering")
print("Mariano Rivera")
d.rivera2010 = dist(as.matrix(rivera2010SC), method = 'maximum')
hc.rivera2010 = hclust(d.rivera2010, method = 'ward.D2')
clustcut.rivera2010 = cutree(hc.rivera2010, 2)
rivera2010$hierclust <- clustcut.rivera2010
table(rivera2010$pitch_type, rivera2010$hierclust)
table(rivera2010$clusterSC, rivera2010$hierclust)

print("Phil Hughes")
d.hughes2010 = dist(as.matrix(hughes2010SC), method = 'maximum')
hc.hughes2010 = hclust(d.hughes2010, method = 'ward.D2')
clustcut.hughes2010 = cutree(hc.hughes2010, 2)
hughes2010$hierclust <- clustcut.hughes2010
table(hughes2010$pitch_type, hughes2010$hierclust)
table(hughes2010$clusterSC, hughes2010$hierclust)


print("SVM w/2011 Model")

print("Mariano Rivera")
#svm_model.rivera2010 <- svm(rivera2010SC, as.factor(rivera2010$pitch_type)) # this would create a new model, we want to use the old one
svm_predict.rivera2010 <- predict(svm_model.rivera, rivera2010SC)
rivera2010$svmpred <- svm_predict.rivera2010
print("MLB Classification vs SVM:")
table(rivera2010$pitch_type, rivera2010$svmpred)

print("Phil Hughes")
svm_predict.hughes2010 <- predict(svm_model.hughes, hughes2010SC)
hughes2010$svmpred <- svm_predict.hughes2010
print("MLB Classification vs SVM:")
table(hughes2010$pitch_type, hughes2010$svmpred)




