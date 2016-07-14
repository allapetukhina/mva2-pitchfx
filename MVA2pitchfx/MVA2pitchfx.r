
#options(repos = c(CRAN = "http://cran.rstudio.com"))
# Install packages if not installed
libraries = c("pitchRx", "e1071", "dplyr", "car")
lapply(libraries, function(x) {if (!(x %in% installed.packages())) {
  install.packages(x)
}})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
  

# Some of Jinhua's Code
plot_cmethod = function(x_dat, y_dat, title, x_label, y_label, colors, shapes, 
                        save_name = NA, 
                        legend_names = c("Pitch type: FC","Pitch type: FF",x_label,y_label)) {
    plot(x_dat,y_dat,main = title, xlab = "First PC", ylab = "Second PC", col=colors, pch=shapes)
    legend("bottomleft", legend_names,
           pch=c(20,20,2,1), col = c(1,2,1,1),cex = 0.75)
    if(!is.na(save_name)) {
        dev.copy(png, save_name)
        dev.off()
    }
}

get_ward_cluster = function(dat_scaled, cut_h = 2) {
    d = dist(dat_scaled, "euclidean", p = 2)   # euclidean distance matrix
    w = hclust(d, method = "ward.D")   
    return(cutree(w, h = cut_h))
}


# Analsys with "pitches" data (2011)
data(pitches, package="pitchRx")
rivera.2011 = subset(pitches, pitcher == 121250)
hughes.2011 = subset(pitches, pitcher == 461833)

# Data Preprocessing
rivera.2011.pNSC = sapply(rivera.2011[,c("start_speed", "break_y", "break_angle",
                                    "break_length", "spin_dir", "spin_rate")], as.numeric)
rivera.2011.pSC  = scale(rivera.2011.pNSC)
hughes.2011.pNSC = sapply(hughes.2011[,c("start_speed", "break_y", "break_angle",
                                    "break_length", "spin_dir", "spin_rate")], as.numeric)
hughes.2011.pSC  = scale(hughes.2011.pNSC)


# Loading 2010 data from MLB servers to train SVM model
data(gids, package="pitchRx")
# I manually got all the games Phil Hughes and Mariano Rivera pitched by checking their game logs at mlb.com
rhgames.2010 = c("2010-10-02","2010-09-26","2010-09-21","2010-09-15","2010-09-10","2010-09-05","2010-08-31","2010-08-25","2010-08-19","2010-08-14","2010-08-09","2010-08-04","2010-07-30","2010-07-25","2010-07-20","2010-07-09","2010-07-04","2010-06-29","2010-06-19","2010-06-13","2010-06-08","2010-06-02","2010-05-28","2010-05-22","2010-05-17","2010-05-12","2010-05-07","2010-05-02","2010-04-27","2010-04-21","2010-04-15","2010-09-28","2010-09-20","2010-09-19","2010-09-17","2010-09-14","2010-09-11","2010-09-04","2010-09-03","2010-09-01","2010-08-29","2010-08-28","2010-08-21","2010-08-18","2010-08-17","2010-08-11","2010-08-10","2010-08-08","2010-08-07","2010-07-31","2010-07-26","2010-07-21","2010-07-18","2010-07-16","2010-07-08","2010-07-05","2010-07-02","2010-07-01","2010-06-27","2010-06-25","2010-06-23","2010-06-20","2010-06-11","2010-06-09","2010-06-06","2010-06-03","2010-06-01","2010-05-30","2010-05-26","2010-05-25","2010-05-21","2010-05-18","2010-05-16","2010-05-14","2010-04-30","2010-04-29","2010-04-20","2010-04-18","2010-04-13","2010-04-11","2010-04-07", "2010-04-06")
rhgames.2010 = gsub("-","_",rhgames.2010)

yankees.2010 = gids[grepl("nya", gids) & grepl("2010", gids)]

rhgames.2010.gids = unique(grep(paste(rhgames.2010,collapse="|"), yankees.2010, value=TRUE))

# the commented command below downloads the data from MLB.  This took over 5 minutes on my computer and 
# thus I have included this data in a csv file, which first must be unzipped.
#dat <- scrape(game.ids = rh2010.gids, async = FALSE)
#rh2010dat = dat$pitch %>% left_join(dat$atbat, by = c("num","gameday_link"))

rhgames.2010.dat = read.csv2("rh2010dat.csv", row.names = 1, stringsAsFactors = F)

rivera.2010 = subset(rhgames.2010.dat, pitcher == 121250 & (pitch_type == "FC" | pitch_type == "FF"))
hughes.2010 = subset(rhgames.2010.dat, pitcher == 461833 & (pitch_type == "FC" | pitch_type == "FF"))

# Preprocess 2010 pitchFx data for Mariano Rivera and Phil Hughes
rivera.2010.pNSC  = sapply(rivera.2010[,c("start_speed", "break_y", "break_angle",
                                         "break_length", "spin_dir", "spin_rate")], as.numeric)
rivera.2010.pSC   = scale(rivera.2010.pNSC)
hughes.2010.pNSC  = sapply(hughes.2010[,c("start_speed", "break_y", "break_angle",
                                         "break_length", "spin_dir", "spin_rate")], as.numeric)
hughes.2010.pSC   = scale(hughes.2010.pNSC)


#Mariano

# PCA Analysis
rivera.2011.pca = prcomp(rivera.2011.pSC)
eva1 = (rivera.2011.pca$sdev)^2
vae1 = eva1/sum(eva1)

#plot variance explained
plot(vae1, xlab = "Index", ylab = "Variance Explained", 
     main = "Mariano Rivera", type = "b")

#plot all the observations on first and second PC coordinates
plot(rivera.2011.pca$x[,c("PC1")],rivera.2011.pca$x[,c("PC2")],main = "Pitches from Mariano Rivera",
     xlab = "first PC", ylab = "second PC", col=as.factor(rivera.2011$pitch_type))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF"), 
       pch=1, col = c(1,2), cex = 0.75)

#plot factor loadings
plot(rivera.2011.pca$rotation[, c("PC1")], type = "o", col = "red3", pch = 1,xlab = "Subindex", 
     xaxt = "n", ylab = "Percentage [%]", ylim = c(-0.6,0.9),
     main = "Factor Loadings for Mariano Rivera", lwd = 2)
lines(rivera.2011.pca$rotation[, c("PC2")], col = "blue3", lwd = 2,pch = 1,type = "o")
axis(1, at = c(1:6), las = 0)
legend("topright", c("First PC", "Second PC"), lwd = 2, 
       col = c("red3", "blue3"), cex = 0.8)

#Hughes
# PCA Analysis
hughes.2011.pca = prcomp(hughes.2011.pSC)
eva2 = (hughes.2011.pca$sdev)^2
vae2 = eva2/sum(eva2)

#plot variance explained
plot(vae2, xlab = "Index", ylab = "Variance Explained", 
     main = "Phil Hughes", type = "b")

#plot all the observations on first and second PC coordinates
plot(hughes.2011.pca$x[,c("PC1")],hughes.2011.pca$x[,c("PC2")],main = "Pitches from Phil Hughes",
     xlab = "first PC", ylab = "second PC", col=as.factor(hughes.2011$pitch_type))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF"), 
       pch=1, col = c(1,2), cex = 0.75)

#plot factor loadings
plot(hughes.2011.pca$rotation[, c("PC1")], type = "o", col = "red3", pch = 1,xlab = "Subindex", 
     xaxt = "n", ylab = "Percentage [%]", ylim = c(-0.6,0.9),
     main = "Factor Loadings for Phil Hughes", lwd = 2)
lines(hughes.2011.pca$rotation[, c("PC2")], col = "blue3", lwd = 2,pch = 1,type = "o")
axis(1, at = c(1:6), las = 0)
legend("topright", c("First PC", "Second PC"), lwd = 2, 
       col = c("red3", "blue3"), cex = 0.8)


# Mariano Rivera
rivera.2011.d = dist(rivera.2011.pSC, "euclidean", p = 2)   # euclidean distance matrix
rivera.2011.w = hclust(rivera.2011.d, method = "ward.D")

rivera.2011.ward = cutree(rivera.2011.w, h = 300)

plot(rivera.2011.w, hang = -0.1, labels = FALSE, frame.plot = TRUE, ann = FALSE)
title(main = "Dendrogram for pitches from Mariano Rivera", 
      xlab = "Ward algorithm",
      ylab = "Squared euclidean distance")

plot(rivera.2011.pca$x[,c("PC1")],rivera.2011.pca$x[,c("PC2")],main = "Mariano Rivera_Ward's Method",
     xlab = "First PC", ylab = "Second PC", col=as.factor(rivera.2011$pitch_type),
     pch=as.numeric(as.factor(rivera.2011.ward)))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF","First cluster","Second cluster"),
       pch=c(20,20,2,1), col = c(1,2,1,1),cex = 0.75)

#cluster analysis results compared with original classification
table(rivera.2011$pitch_type, rivera.2011.ward)

# Phil Hughes
hughes.2011.d = dist(hughes.2011.pSC, "euclidean", p = 2)   # euclidean distance matrix
hughes.2011.w = hclust(hughes.2011.d, method = "ward.D")   

hughes.2011.ward = cutree(hughes.2011.w, h = 300)

plot(hughes.2011.w, hang = -0.1, labels = FALSE, frame.plot = TRUE, ann = FALSE)
title(main = "Dendrogram for pitches from Phil Hughes", 
      xlab = "Ward algorithm",
      ylab = "Squared euclidean distance")

plot(hughes.2011.pca$x[,c("PC1")],hughes.2011.pca$x[,c("PC2")],main = "Phil Hughes_Ward's Method",
     xlab = "First PC", ylab = "Second PC", col=as.factor(hughes.2011$pitch_type),
     pch=as.numeric(as.factor(hughes.2011.ward)))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF","First cluster","Second cluster"),
       pch=c(20,20,2,1), col = c(1,2,1,1),cex = 0.75)

#cluster analysis results compared with original classification
table(hughes.2011$pitch_type, hughes.2011.ward)


# KMeans Clustering
print("K-Means Clustering")
# Mariano Rivera
print("Mariano Rivera")
rivera.2011.kmeans.pNSC  = kmeans(rivera.2011.pNSC, 2, nstart=10)$cluster
rivera.2011.kmeans.pSC   = kmeans(rivera.2011.pSC, 2, nstart=10)$cluster
print("Not Scaled")
table(rivera.2011$pitch_type, rivera.2011.kmeans.pNSC)
print("Scaled")
table(rivera.2011$pitch_type, rivera.2011.kmeans.pSC)
plot_cmethod(rivera.2011.pca$x[,"PC1"], rivera.2011.pca$x[,"PC2"], "Mariano Rivera's_K-means Method (Not Scaled Data)", 
             "Cluster 1", "Cluster 2", as.factor(rivera.2011$pitch_type), as.numeric(as.factor(rivera.2011.kmeans.pNSC)))

plot_cmethod(rivera.2011.pca$x[,"PC1"], rivera.2011.pca$x[,"PC2"], "Mariano Rivera's_K-means Method (Scaled Data)", 
             "Cluster 1", "Cluster 2", as.factor(rivera.2011$pitch_type), as.numeric(as.factor(rivera.2011.kmeans.pSC)))

# Phil Hughes
print("Phil Hughes")
hughes.2011.kmeans.pNSC  = kmeans(hughes.2011.pNSC, 2, nstart=10)$cluster
hughes.2011.kmeans.pSC   = kmeans(hughes.2011.pSC, 2, nstart=10)$cluster
print("Not Scaled")
table(hughes.2011$pitch_type, hughes.2011.kmeans.pNSC)
print("Scaled")
table(hughes.2011$pitch_type, hughes.2011.kmeans.pSC)
plot_cmethod(hughes.2011.pca$x[,"PC1"], hughes.2011.pca$x[,"PC2"], "Phil Hughes's_K-means Method (Not Scaled Data)", 
             "Cluster 1", "Cluster 2", as.factor(hughes.2011$pitch_type), as.numeric(as.factor(hughes.2011.kmeans.pNSC)))

plot_cmethod(hughes.2011.pca$x[,"PC1"], hughes.2011.pca$x[,"PC2"], "Phil Hughes's_K-means Method (Scaled Data)", 
             "Cluster 1", "Cluster 2", as.factor(hughes.2011$pitch_type), as.numeric(as.factor(hughes.2011.kmeans.pSC)))


#scaled is clearly more like MLB's classification so using that
rivera.2011.kmeans = rivera.2011.kmeans.pSC
hughes.2011.kmeans = hughes.2011.kmeans.pSC

# SVM
# http://rischanlab.github.io/SVM.html
print("Support Vector Machines")
print("Classify 2011 Pitches w/2010 Training Data")

print("Mariano Rivera")
#radial kernel
#rivera.2010.svm_tune = tune(svm, train.x=rivera.2010.pSC, train.y=as.factor(rivera.2010$pitch_type), 
#                            kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(1/dim(rivera.2010.pSC)[2],.5,1,2)))
#print(rivera.2010.svm_tune)
rivera.2010.svm_model = svm(rivera.2010.pSC, as.factor(rivera.2010$pitch_type),
                           kernel="radial",gamma=1/dim(rivera.2010.pSC)[2], cost=1)


rivera.2011.svm = predict(rivera.2010.svm_model, rivera.2011.pSC)
table(rivera.2011$pitch_type, rivera.2011.svm)
plot_cmethod(rivera.2011.pca$x[,"PC1"], rivera.2011.pca$x[,"PC2"], "Mariano Rivera's_SVM Method", 
             "SVM FF", "SVM FC", as.factor(rivera.2011$pitch_type), as.numeric(as.factor(rivera.2011.svm)))


print("Phil Hughes")
#hughes.2010.svm_tune = tune(svm, train.x=hughes.2010.pSC, train.y=as.factor(hughes.2010$pitch_type), 
#                            kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(1/dim(hughes.2010.pSC)[2],.5,1,2)))
#print(hughes_svm_tune)
hughes.2010.svm_model = svm(hughes.2010.pSC, as.factor(hughes.2010$pitch_type),
                            kernel="radial",gamma=1/dim(hughes.2010.pSC)[2], cost=1)

hughes.2011.svm = predict(hughes.2010.svm_model, hughes.2011.pSC)
table(hughes.2011$pitch_type, hughes.2011.svm)
plot_cmethod(hughes.2011.pca$x[,"PC1"], hughes.2011.pca$x[,"PC2"], "Phil Hughes's_SVM Method", 
             "SVM FF", "SVM FC", as.factor(hughes.2011$pitch_type), as.numeric(as.factor(hughes.2011.svm)))


# Pairwise and PCA Analysis

# Pair-Wise Analysis
# No-Rotation Pairs
#png("rivera-pairwise-hierarchical.png", width=900, height=900)
#pairs(rivera.2011.pSC, main = "Mariano Rivera - Pairwise Ward Clusters", 
#      pch=as.numeric(as.factor(rivera.2011.ward)), col=as.factor(rivera.2011$pitch_type))
#dev.off()
#png("rivera-pairwise-kmeans.png", width=900, height=900)
#pairs(rivera.2011.pSC, main = "Mariano Rivera - Pairwise K-means Clusters", 
#      pch=as.numeric(as.factor(rivera.2011.kmeans)), col=as.factor(rivera.2011$pitch_type))
#dev.off()
#png("rivera-pairwise-svm.png", width=900, height=900)
#pairs(rivera.2011.pSC, main = "Mariano Rivera - Pairwise SVM Classification", 
#      pch=as.numeric(as.factor(rivera.2011.svm)), col=as.factor(rivera.2011$pitch_type))
#dev.off()

#png("hughes-pairwise-hierarchical.png", width=900, height=900)
#pairs(hughes.2011.pSC, main = "Phil Hughes - Pairwise - Ward Clusters", 
#      pch=as.numeric(as.factor(rivera.2011.ward)), col=as.factor(rivera.2011$pitch_type))
#dev.off()
#png("hughes-pairwise-kmeans.png", width=900, height=900)
#pairs(hughes.2011.pSC, main = "Phil Hughes - Pairwise - K-means Clusters", 
#      pch=as.numeric(as.factor(hughes.2011.kmeans)), col=as.factor(hughes.2011$pitch_type))
#dev.off()
#png("hughes-pairwise-svm.png", width=900, height=900)
#pairs(hughes.2011.pSC, main = "Phil Hughes - Pairwise - SVM Classification", 
#      pch=as.numeric(as.factor(hughes.2011.svm)), col=as.factor(hughes.2011$pitch_type))
#dev.off()

# PCA Pairs
#png("rivera-pca-hierarchical.png", width=900, height=900)
#pairs(rivera.2011.pca$x[,c("PC1", "PC2", "PC3")], main = "Mariano Rivera - PCA - Ward Clusters", 
#      pch=as.numeric(as.factor(rivera.2011.ward)), col=as.factor(rivera.2011$pitch_type))
#dev.off()
#png("rivera-pca-kmeans.png", width=900, height=900)
#pairs(rivera.2011.pca$x[,c("PC1", "PC2", "PC3")], main = "Mariano Rivera - PCA - K-means Clusters", 
#      pch=as.numeric(as.factor(rivera.2011.kmeans)), col=as.factor(rivera.2011$pitch_type))
#dev.off()
#png("rivera-pca-svm.png", width=900, height=900)
#pairs(pcaan.rivera$x[,c("PC1", "PC2", "PC3")], main = "Mariano Rivera - PCA - SVM Classification", 
#      pch=as.numeric(as.factor(rivera.2011.svm)), col=as.factor(rivera.2011$pitch_type))
#dev.off()

# Scree Plot (Not Currently Used)
#screeplot(pcaan.hughes, type="lines")

#png("hughes-pca-hierarchical.png", width=900, height=900)
#pairs(hughes.2011.pca$x[,c("PC1", "PC2", "PC3")], main = "Phil Hughes - PCA - Ward Clusters", 
#      pch=as.numeric(as.factor(hughes.2011.ward)), col=as.factor(hughes.2011$pitch_type))
#dev.off()
#png("hughes-pca-kmeans.png", width=900, height=900)
#pairs(hughes.2011.pca$x[,c("PC1", "PC2", "PC3")], main = "Phil Hughes - PCA - K-means Clusters", 
#      pch=as.numeric(as.factor(hughes.2011.kmeans)), col=as.factor(hughes.2011$pitch_type))
#dev.off()
#png("hughes-pca-svm.png", width=900, height=900)
#pairs(hughes.2011.pca$x[,c("PC1", "PC2", "PC3")], main = "Phil Hughes - PCA - SVM Classification", 
#      pch=as.numeric(as.factor(hughes.2011.svm)), col=as.factor(hughes.2011$pitch_type))
#dev.off()





