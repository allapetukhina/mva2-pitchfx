
#options(repos = c(CRAN = "http://cran.rstudio.com"))
# Install packages if not installed
libraries = c("pitchRx", "e1071", "dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Analsys with "pitches" data (2011)
data(pitches, package="pitchRx")
rivera = subset(pitches, pitcher == 121250)
hughes = subset(pitches, pitcher == 461833)

# Data Preprocessing
riveraNSC         = sapply(rivera[,c("start_speed", "break_y", "break_angle",
                                     "break_length", "spin_dir", "spin_rate")], as.numeric)
riveraSC          = scale(riveraNSC)
hughesNSC         = sapply(hughes[,c("start_speed", "break_y", "break_angle",
                                     "break_length", "spin_dir", "spin_rate")], as.numeric)
hughesSC          = scale(hughesNSC)


# Loading 2010 data from MLB servers to train SVM model
data(gids, package="pitchRx")
# I manually got all the games Phil Hughes and Mariano Rivera pitched by checking their game logs at mlb.com
riveraandhughes2010 = c("2010-10-02","2010-09-26","2010-09-21","2010-09-15","2010-09-10","2010-09-05","2010-08-31","2010-08-25","2010-08-19","2010-08-14","2010-08-09","2010-08-04","2010-07-30","2010-07-25","2010-07-20","2010-07-09","2010-07-04","2010-06-29","2010-06-19","2010-06-13","2010-06-08","2010-06-02","2010-05-28","2010-05-22","2010-05-17","2010-05-12","2010-05-07","2010-05-02","2010-04-27","2010-04-21","2010-04-15","2010-09-28","2010-09-20","2010-09-19","2010-09-17","2010-09-14","2010-09-11","2010-09-04","2010-09-03","2010-09-01","2010-08-29","2010-08-28","2010-08-21","2010-08-18","2010-08-17","2010-08-11","2010-08-10","2010-08-08","2010-08-07","2010-07-31","2010-07-26","2010-07-21","2010-07-18","2010-07-16","2010-07-08","2010-07-05","2010-07-02","2010-07-01","2010-06-27","2010-06-25","2010-06-23","2010-06-20","2010-06-11","2010-06-09","2010-06-06","2010-06-03","2010-06-01","2010-05-30","2010-05-26","2010-05-25","2010-05-21","2010-05-18","2010-05-16","2010-05-14","2010-04-30","2010-04-29","2010-04-20","2010-04-18","2010-04-13","2010-04-11","2010-04-07", "2010-04-06")
yankees10 = gids[grepl("nya", gids) & grepl("2010", gids)]
riveraandhughes2010 = gsub("-","_",riveraandhughes2010)
rh2010.gids = unique(grep(paste(riveraandhughes2010,collapse="|"), yankees10, value=TRUE))

# the commented command below downloads the data from MLB.  This took over 5 minutes on my computer and 
# thus I have included this data in a csv file, which first must be unzipped.
#dat <- scrape(game.ids = rh2010.gids, async = FALSE)
#rh2010dat = dat$pitch %>% left_join(dat$atbat, by = c("num","gameday_link"))

rh2010dat = read.csv2("rh2010dat.csv", row.names = 1, stringsAsFactors = F)

rivera2010 = subset(rh2010dat, pitcher == 121250 & (pitch_type == "FC" | pitch_type == "FF"))
hughes2010 = subset(rh2010dat, pitcher == 461833 & (pitch_type == "FC" | pitch_type == "FF"))

# Preprocess 2010 pitchFx data for Mariano Rivera and Phil Hughes
rivera2010NSC  = sapply(rivera2010[,c("start_speed", "break_y", "break_angle",
                             "break_length", "spin_dir", "spin_rate")], as.numeric)
rivera2010SC   = scale(rivera2010NSC)
hughes2010NSC  = sapply(hughes2010[,c("start_speed", "break_y", "break_angle",
                             "break_length", "spin_dir", "spin_rate")], as.numeric)
hughes2010SC   = scale(hughes2010NSC)


# KMeans Clustering
print("K-Means Clustering")

print("Mariano Rivera")
rivera$kmeansNSC  = kmeans(riveraNSC, 2, nstart=10)$cluster
rivera$kmeansSC   = kmeans(riveraSC, 2, nstart=10)$cluster
print("Not Scaled")
table(rivera$pitch_type, rivera$kmeansNSC)
print("Scaled")
table(rivera$pitch_type, rivera$kmeansSC)

print("Phil Hughes")
hughes$kmeansNSC  = kmeans(hughesNSC, 2, nstart=10)$cluster
hughes$kmeansSC   = kmeans(hughesSC, 2, nstart=10)$cluster
print("Not Scaled")
table(hughes$pitch_type, hughes$kmeansNSC)
print("Scaled")
table(hughes$pitch_type, hughes$kmeansSC)

# SVM
# http://rischanlab.github.io/SVM.html
print("Support Vector Machines")
print("Classify 2011 Pitches w/2010 Training Data")

print("Mariano Rivera")
#radial kernel
svm_model.rivera2010 = svm(rivera2010SC, as.factor(rivera2010$pitch_type),
                           kernel="radial",gamma=1/dim(rivera2010SC)[2], cost=1)

#rivera_svm_tune      = tune(svm, train.x=rivera2010SC, train.y=as.factor(rivera2010$pitch_type), 
#                            kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(1/dim(rivera2010SC)[2],.5,1,2)))
#print(rivera_svm_tune)

rivera$svmpred = predict(svm_model.rivera2010, riveraSC)
table(rivera$pitch_type, rivera$svmpred)

print("Phil Hughes")
svm_model.hughes2010 = svm(hughes2010SC, as.factor(hughes2010$pitch_type),
                            kernel="radial",gamma=1/dim(hughes2010SC)[2], cost=1)
#hughes_svm_tune      = tune(svm, train.x=hughes2010SC, train.y=as.factor(hughes2010$pitch_type), 
#                            kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(1/dim(hughes2010SC)[2],.5,1,2)))
#print(hughes_svm_tune)

hughes$svmpred = predict(svm_model.hughes2010, hughesSC)
table(hughes$pitch_type, hughes$svmpred)

# Pairwise and PCA Analysis

# Pair-Wise Analysis
png("rivera-pairwise-kmeans.png", width=900, height=900)
pairs(riveraSC, main = "Mariano Rivera - Pairwise K-means Clusters", 
      pch=as.numeric(as.factor(rivera$kmeansSC)), col=as.factor(rivera$pitch_type))
dev.off()
#png("rivera-pairwise-hierarchical.png", width=900, height=900)
#pairs(riveraSC, main = "Mariano Rivera - Pairwise Hierarchical Clusters", 
#      pch=as.numeric(as.factor(rivera$hierarchical)), col=as.factor(rivera$pitch_type))
#dev.off()
png("rivera-pairwise-svm.png", width=900, height=900)
pairs(riveraSC, main = "Mariano Rivera - Pairwise SVM Classification", 
      pch=as.numeric(as.factor(rivera$svmpred)), col=as.factor(rivera$pitch_type))
dev.off()

png("hughes-pairwise-kmeans.png", width=900, height=900)
pairs(hughesSC, main = "Phil Hughes - Pairwise - K-means Clusters", 
      pch=as.numeric(as.factor(hughes$kmeansSC)), col=as.factor(hughes$pitch_type))
dev.off()
#png("hughes-pairwise-hierarchical.png", width=900, height=900)
#pairs(riveraSC, main = "Phil Hughes - Pairwise - Hierarchical Clusters", 
#      pch=as.numeric(as.factor(rivera$hierarchical)), col=as.factor(rivera$pitch_type))
#dev.off()
png("hughes-pairwise-svm.png", width=900, height=900)
pairs(hughesSC, main = "Phil Hughes - Pairwise - SVM Classification", 
      pch=as.numeric(as.factor(hughes$svmpred)), col=as.factor(hughes$pitch_type))
dev.off()

# PCA Analysis
cov.rivera   = t(riveraSC) %*% riveraSC
pcaan.rivera = prcomp(riveraSC)

# Scree Plot (Not Currently Used)
#screeplot(pcaan.rivera, type="lines")

png("rivera-pca-kmeans.png", width=900, height=900)
pairs(pcaan.rivera$x[,c("PC1", "PC2", "PC3")], main = "Mariano Rivera - PCA - K-means Clusters", 
      pch=as.numeric(as.factor(rivera$kmeansSC)), col=as.factor(rivera$pitch_type))
dev.off()
#png("rivera-pca-hierarchical.png", width=900, height=900)
#pairs(pcaan.rivera$x[,c("PC1", "PC2", "PC3")], main = "Mariano Rivera - PCA - Hierarchical Clusters", 
#      pch=as.numeric(as.factor(rivera$hierarchical)), col=as.factor(rivera$pitch_type))
#dev.off()
png("rivera-pca-svm.png", width=900, height=900)
pairs(pcaan.rivera$x[,c("PC1", "PC2", "PC3")], main = "Mariano Rivera - PCA - SVM Classification", 
      pch=as.numeric(as.factor(rivera$svmpred)), col=as.factor(rivera$pitch_type))
dev.off()

cov.hughes   = t(hughesSC) %*% hughesSC
pcaan.hughes = prcomp(hughesSC)

# Scree Plot (Not Currently Used)
#screeplot(pcaan.hughes, type="lines")

png("hughes-pca-kmeans.png", width=900, height=900)
pairs(pcaan.hughes$x[,c("PC1", "PC2", "PC3")], main = "Phil Hughes - PCA - K-means Clusters", 
      pch=as.numeric(as.factor(hughes$kmeansSC)), col=as.factor(hughes$pitch_type))
dev.off()
#png("hughes-pca-hierarchical.png", width=900, height=900)
#pairs(pcaan.hughes$x[,c("PC1", "PC2", "PC3")], main = "Phil Hughes - PCA - Hierarchical Clusters", 
#      pch=as.numeric(as.factor(hughes$hierarchical)), col=as.factor(hughes$pitch_type))
#dev.off()
png("hughes-pca-svm.png", width=900, height=900)
pairs(pcaan.hughes$x[,c("PC1", "PC2", "PC3")], main = "Phil Hughes - PCA - SVM Classification", 
      pch=as.numeric(as.factor(hughes$svmpred)), col=as.factor(hughes$pitch_type))
dev.off()


