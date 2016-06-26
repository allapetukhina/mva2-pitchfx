
options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages("pitchRx")

library(pitchRx)

rivera = subset(pitches, pitcher == 121250)
hughes = subset(pitches, pitcher == 461833)



dim(hughes)
# 946 70
names(hughes)
# 'url' 'inning_side' 'inning' 'next_' 'num' 'gameday_link' 'row_names' 'des' 'des_es' 'id' 'type' 'tfs' 'tfs_zulu' 'x' 'y' 'sv_id' 'start_speed' 'end_speed' 'sz_top' 'sz_bot' 'pfx_x' 'pfx_z' 'px' 'pz' 'x0' 'y0' 'z0' 'vx0' 'vy0' 'vz0' 'ax' 'ay' 'az' 'break_y' 'break_angle' 'break_length' 'pitch_type' 'type_confidence' 'zone' 'nasty' 'spin_dir' 'spin_rate' 'cc' 'mt' 'on_1b' 'on_2b' 'on_3b' 'count' 'pitcher' 'batter' 'b' 's' 'o' 'start_tfs' 'start_tfs_zulu' 'stand' 'b_height' 'p_throws' 'atbat_des' 'atbat_des_es' 'event' 'score' 'home_team_runs' 'away_team_runs' 'event2' 'event3' 'batter_name' 'pitcher_name' 'event4' 'date'

# KMeans Clustering

# rivera[,c(17, 34, 35, 36, 41, 42)]
# start_speed break_y break_angle break_length spin_dir spin_rate

riveraNSC = sapply(rivera[,c(17, 34, 35, 36, 41, 42)], as.numeric)
riveraSC = scale(riveraNSC)

pclust.NSC.rivera <- kmeans(riveraNSC, 2, nstart=10)
rivera$clusterNSC <- pclust.NSC.rivera$cluster

pclust.SC.rivera <- kmeans(riveraSC, 2, nstart=10)
rivera$clusterSC <- pclust.SC.rivera$cluster

hughesNSC = sapply(hughes[,c(17, 34, 35, 36, 41, 42)], as.numeric)
hughesSC = scale(hughesNSC)

pclust.NSC.hughes <- kmeans(hughesNSC, 2, nstart=10)
hughes$clusterNSC <- pclust.NSC.hughes$cluster

pclust.SC.hughes <- kmeans(hughesSC, 2, nstart=10)
hughes$clusterSC <- pclust.SC.hughes$cluster


table(rivera$pitch_type, rivera$clusterSC)

table(hughes$pitch_type, hughes$clusterSC)

# Hierarchical Clustering
# http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html

d.rivera = dist(as.matrix(riveraSC), method = 'maximum')
hc.rivera = hclust(d.rivera, method = 'ward.D2')
clustcut.rivera = cutree(hc.rivera, 2)
rivera$hierclust <- clustcut.rivera
table(rivera$pitch_type, clustcut.rivera)
table(rivera$hierclust, rivera$clusterSC)


d.hughes = dist(as.matrix(hughesSC), method = 'maximum')
hc.hughes = hclust(d.hughes, method = 'ward.D2')
clustcut.hughes = cutree(hc.hughes, 2)
hughes$hierclust <- clustcut.hughes
table(hughes$pitch_type, clustcut.hughes)
table(hughes$hierclust, hughes$clusterSC)



