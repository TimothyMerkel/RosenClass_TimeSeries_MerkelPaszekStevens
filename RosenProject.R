setwd ("C:/Users/Sean/Documents/David-timeseries/data")
library(plotrix)
library("Rcpp")
library(plyr)
library(ggplot2)
library(quadprog)
library(forecast)

salivaA <- read.csv("david14_otus_metadata_saliva_A.csv")
salivaA <- data.frame(salivaA[,-1],row.names= salivaA[,1])

stoolA <- read.csv("david14_otus_metadata_stool_A.csv")
stoolA <- data.frame(stoolA[,-1],row.names= stoolA[,1])

stoolB <- read.csv("david14_otus_metadata_stool_b.csv")
stoolB <- data.frame(stoolB[,-1],row.names= stoolB[,1])

threshold <- 50

## remove rows that are too low
#silivaA
sumSilivaA <- rowSums(salivaA[1:5431,1:277])/277
salivaA[1:5431,278] = sumSilivaA
salivaA[5432:5443,278] = 0
sA_values <- salivaA[salivaA[,278]>threshold,]
sA_values[119:130,] <- salivaA[5432:5443,]
write.csv(sA_values, file = "SilivaA_Out.csv")

sumStoolA <- rowSums(stoolA[1:5431,1:329])/329
stoolA[1:5431,330] = sumStoolA
stoolA[5432:5443,330] = 0
stA_values <- stoolA[stoolA[,330]>threshold,]
stA_values[191:202,] <- stoolA[5432:5443,]
write.csv(stA_values, file = "StoolA_Out.csv")

sumStoolB <- rowSums(stoolB[1:5431,1:190])/190
stoolB[1:5431,191] = sumStoolB
stoolB[5432:5443,191] = 0
stB_values <- stoolB[stoolB[,191]>threshold,]
stB_values[140:151,] <- stoolB[5432:5443,]
write.csv(stB_values, file = "StoolB_Out.csv")

# siliva A by phylum

sA_MD <- read.csv("SilivaA_metaData.csv")
sA_MD <- data.frame(sA_MD[,-1],row.names= sA_MD[,1])

#saliva data aggregated by phylum

sA_Genus <- aggregate(sA_MD[1:118,1:277],list(sA_MD[1:118,280]),sum)
sA_Genus <- data.frame(sA_Genus[,-1],row.names= sA_Genus[,1])
day <- data.frame(1:277)
colnames(day) <- "day"
sA_Genus_toPlot <- data.frame(t(sA_Genus))
sA_Genus_toPlot[,9] <- day

#Stool A by phylum

stA_MD <- read.csv("StoolA_Metadata.csv")
stA_MD <- data.frame(stA_MD[,-1],row.names= stA_MD[,1])

#saliva data aggregated by phylum

stA_phylum <- aggregate(stA_MD[1:191,1:329],list(stA_MD[1:191,331]),sum)
stA_phylum <- data.frame(stA_phylum[,-1],row.names= stA_phylum[,1])
day <- data.frame(1:329)
colnames(day) <- "day"
stA_phylum_toPlot <- data.frame(t(stA_phylum))
stA_phylum_toPlot[,8] <- day

#stool B by phylum

stB_MD <- read.csv("StoolB_metaData.csv")
stB_MD <- data.frame(stB_MD[,-1],row.names= stB_MD[,1])

#saliva data aggregated by phylum

stB_phylum <- aggregate(stB_MD[1:139,1:190],list(stB_MD[1:139,192]),sum)
stB_phylum <- data.frame(stB_phylum[,-1],row.names= stB_phylum[,1])
day <- data.frame(1:190)
colnames(day) <- "day"
stB_phylum_toPlot <- data.frame(t(stB_phylum))
stB_phylum_toPlot[,7] <- day

###########################################
## Correlations                         ##
###########################################
sA_cor <- cor(sA_Genus_toPlot[,1:8])
stA_cor <- cor(stA_phylum_toPlot[,1:7])
stB_cor <- cor(stB_phylum_toPlot[,1:6])

sA_cov <- cov(t(sA_values[1:118,1:277]))
stA_cov <- cov(t(stA_values[1:190,1:329]))
stB_cov <- cov(t(stB_values[1:139,1:190]))

#generate some corelations

write.csv(sA_cor, file = "cor_SilivaA_Out.csv")
write.csv(stA_cor, file = "cor_StoolA_Out.csv")
write.csv(stB_cor, file = "cor_StoolB_Out.csv")


png("RGBcorSA.png",600,600)
par(mar = c(9, 9, 4, 2) + 0.1)
color2D.matplot(sA_cor,c(0,1),0,0,c(2,3),show.legend = FALSE, show.values = TRUE,  xlab = "", ylab = "", vcex = 1.5,axes = FALSE)

# Set up x axis with tick marks alone
axis(1, labels = FALSE)
axis(2, labels = FALSE)

# Create arbitrary text
labels <- c(colnames(sA_cor))

# plot x axis labels using:
# par("usr")[3] - 0.25 as the vertical placement
# srt = 45 as text rotation angle
# adj = 1 to place right end of text at tick mark
# xpd = TRUE to allow for text outside the plot region
text(par("usr")[1] - 0.25, 8:1, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(1, text = "Phylum", line = 6)

text( 1:8,par("usr")[1] - 0.25, srt = 45, adj = 1,
      labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(2, text = "Phylum", line = 6)
dev.off()

png("RGBcorSTA.png",600,600)
par(mar = c(9, 9, 4, 2) + 0.1)
color2D.matplot(stA_cor,c(0,1),0,0,c(2,3),show.legend = FALSE, show.values = TRUE,  xlab = "", ylab = "", vcex = 1.5,axes = FALSE)

# Set up x axis with tick marks alone
axis(1, labels = FALSE)
axis(2, labels = FALSE)

# Create arbitrary text
labels <- c(colnames(stA_cor))

# plot x axis labels using:
# par("usr")[3] - 0.25 as the vertical placement
# srt = 45 as text rotation angle
# adj = 1 to place right end of text at tick mark
# xpd = TRUE to allow for text outside the plot region
text(par("usr")[1] - 0.25, 7:1, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(1, text = "Phylum", line = 6)

text( 1:7,par("usr")[1] - 0.25, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(2, text = "Phylum", line = 6)
dev.off()

png("RGBcorSTB.png",600,600)
par(mar = c(9, 9, 4, 2) + 0.1)
color2D.matplot(stB_cor,c(0,1),0,0,c(2,3),show.legend = FALSE, show.values = TRUE,  xlab = "", ylab = "", vcex = 1.5,axes = FALSE)

# Set up x axis with tick marks alone
axis(1, labels = FALSE)
axis(2, labels = FALSE)

# Create arbitrary text
labels <- c(colnames(stB_cor))

# plot x axis labels using:
# par("usr")[3] - 0.25 as the vertical placement
# srt = 45 as text rotation angle
# adj = 1 to place right end of text at tick mark
# xpd = TRUE to allow for text outside the plot region
text(par("usr")[1] - 0.25, 6:1, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(1, text = "Phylum", line = 6)

text( 1:6,par("usr")[1] - 0.25, srt = 45, adj = 1,
      labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(2, text = "Phylum", line = 6)
dev.off()

#generate the covariences

write.csv(sA_cov, file = "cov_SilivaA_Out.csv")
write.csv(stA_cov, file = "cov_StoolA_Out.csv")
write.csv(stB_cov, file = "cov_StoolB_Out.csv")

png("RGBcovSA.png",1080,1080)
color2D.matplot(sA_cov,c(0,1),0,0,c(2,3))
dev.off()

ts.data <- ts(t(stA_values[2,1:329]), start = 1,frequency = 1)
#acf(ts.data, type = "correlation", lag.max = 150)

sA_MD <- read.csv("SilivaA_metaData.csv")
sA_MD <- data.frame(sA_MD[,-1],row.names= sA_MD[,1])

#saliva data aggregated by genus

#######################################################
##             Stacked Area Charts                  ##
#######################################################

png("PhylumStackedArea_SA.png",1080,540)
par(mar = c(4, 4, 4, 2) + 0.1)
labels <- c(colnames(sA_Genus_toPlot[,1:8]))
stackpoly(sA_Genus_toPlot, col = c("blue","red","green","purple","orange","yellow","black","gold"),xlab = "Sample Day", ylab = "Count", main = "Siliva A Phylum by Sample Day")
legend(200,149000,labels,fill = c("blue","red","green","purple","orange","yellow","black","gold"))
dev.off()

png("PhylumStackedArea_StA.png",1080,540)
par(mar = c(4, 4, 4, 2) + 0.1)
labels <- c(colnames(stA_phylum_toPlot[,1:7]))
stackpoly(stA_phylum_toPlot, col = c("blue","red","green","purple","orange","yellow","black","gold"),xlab = "Sample Day", ylab = "Count", main = "Stool A Phylum by Sample Day")
legend(200,300000,labels,fill = c("blue","red","green","purple","orange","yellow","black","gold"))
dev.off()

png("PhylumStackedArea_StB.png",1080,540)
par(mar = c(4, 4, 4, 2) + 0.1)
labels <- c(colnames(stB_phylum_toPlot[,1:6]))
stackpoly(stB_phylum_toPlot, col = c("blue","red","green","purple","orange","yellow","black","gold"),xlab = "Sample Day", ylab = "Count", main = "Stool B Phylum by Sample Day")
legend(130,230000,labels,fill = c("blue","red","green","purple","orange","yellow","black","gold"))
dev.off()

##############################################
##       Arima For siliva A                 ##
##############################################
#firmicutes
png("sA_Firmicutes_Arima.png",600,600)
bacteria_Ts <- as.ts(sA_Genus_toPlot[1:40,3])
fit <- arima(bacteria_Ts, order=c(3, 1, 3),seasonal = c(0,1,1))
toPlot <- forecast(fit, 25)
#plot(toPlot, main = "Siliva A Firmicutes Arima Forecast",xlab = "Sample Day", ylab = "Count")
arima <- toPlot$mean
ari <- data.frame(arima,(1:25)+40)
names(ari) <- c('y','x')
upper <- toPlot$upper
up <- data.frame(upper,(1:25)+40)
names(up) <- c('y1','y','x')
lower <- toPlot$lower
low <- data.frame(lower,(1:25)+40)
names(low) <- c('y1','y','x')
#plot(toPlot, main = "Siliva A Bacteriodites Arima Forecast",xlab = "Sample Day", ylab = "Count")
ggplot() +
  geom_line(data = sA_Genus_toPlot[1:120,c(3,9)],aes(x=day, y=Firmicutes, colour = "data"), size = 0.75)+
  geom_line(data = ari,aes(x=x, y=y, colour = "Arima"), size = 0.75)+
  geom_line(data = up,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  geom_line(data = low,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  scale_color_manual(values=c("Red","Black","Blue"))+
  theme(legend.title=element_blank())+
  labs(title = "Firmicute Arima Prediction During Time Abroad")
dev.off()
#bacteriodites
png("sA_Bacteriodites_Arima.png",600,600)
actino_Ts <- as.ts(sA_Genus_toPlot[1:40,2])
fit <- arima(actino_Ts, order=c(3, 1, 3),seasonal = c(0,1,1))
toPlot <- forecast(fit, 25)
arima <- toPlot$mean
ari <- data.frame(arima,(1:25)+40)
names(ari) <- c('y','x')
upper <- toPlot$upper
up <- data.frame(upper,(1:25)+40)
names(up) <- c('y1','y','x')
lower <- toPlot$lower
low <- data.frame(lower,(1:25)+40)
names(low) <- c('y1','y','x')
#plot(toPlot, main = "Siliva A Bacteriodites Arima Forecast",xlab = "Sample Day", ylab = "Count")
ggplot() +
  geom_line(data = sA_Genus_toPlot[1:120,c(2,9)],aes(x=day, y=Bacteroidetes, colour = "data"), size = 0.75)+
  geom_line(data = ari,aes(x=x, y=y, colour = "Arima"), size = 0.75)+
  geom_line(data = up,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  geom_line(data = low,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  scale_color_manual(values=c("Red","Black","Blue"))+
  theme(legend.title=element_blank())+
  labs(title = "Bacteroidetes Arima Prediction During Time Abroad")
dev.off()
#actino
png("sA_Actinobacteria_Arima.png",600,600)
xna_Ts <- as.ts(sA_Genus_toPlot[1:40,1])
fit <- arima(xna_Ts, order=c(3, 1, 3),seasonal = c(0,1,1))
toPlot <- forecast(fit, 25)
arima <- toPlot$mean
ari <- data.frame(arima,(1:25)+40)
names(ari) <- c('y','x')
upper <- toPlot$upper
up <- data.frame(upper,(1:25)+40)
names(up) <- c('y1','y','x')
lower <- toPlot$lower
low <- data.frame(lower,(1:25)+40)
names(low) <- c('y1','y','x')
#plot(toPlot, main = "Siliva A Bacteriodites Arima Forecast",xlab = "Sample Day", ylab = "Count")
ggplot() +
  geom_line(data = sA_Genus_toPlot[1:120,c(1,9)],aes(x=day, y=Actinobacteria..phylum., colour = "data"), size = 0.75)+
  geom_line(data = ari,aes(x=x, y=y, colour = "Arima"), size = 0.75)+
  geom_line(data = up,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  geom_line(data = low,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  scale_color_manual(values=c("Red","Black","Blue"))+
  theme(legend.title=element_blank())+
  labs(title = "Actinobacteria Arima Prediction During Time Abroad")
#plot(toPlot, main = "Siliva A Actinobacteria Arima Forecast",xlab = "Sample Day", ylab = "Count")
dev.off()

######################################
##    Old Arima Plots               ##
######################################
#firmicutes
png("sA_Firmicutes_Arima_full_seq.png",600,600)
bacteria_Ts <- as.ts(sA_Genus_toPlot[,3])
fit <- arima(bacteria_Ts, order=c(3, 1, 3),seasonal = c(0,1,1))
toPlot <- forecast(fit, 25)
#plot(toPlot, main = "Siliva A Firmicutes Arima Forecast",xlab = "Sample Day", ylab = "Count")
arima <- toPlot$mean
ari <- data.frame(arima,(1:25)+277)
names(ari) <- c('y','x')
upper <- toPlot$upper
up <- data.frame(upper,(1:25)+277)
names(up) <- c('y1','y','x')
lower <- toPlot$lower
low <- data.frame(lower,(1:25)+277)
names(low) <- c('y1','y','x')
#plot(toPlot, main = "Siliva A Bacteriodites Arima Forecast",xlab = "Sample Day", ylab = "Count")
ggplot() +
  geom_line(data = sA_Genus_toPlot[1:277,c(3,9)],aes(x=day, y=Firmicutes, colour = "data"), size = 0.75)+
  geom_line(data = ari,aes(x=x, y=y, colour = "Arima"), size = 0.75)+
  geom_line(data = up,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  geom_line(data = low,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  scale_color_manual(values=c("Red","Black","Blue"))+
  theme(legend.title=element_blank())+
  labs(title = "Firmicute Arima Prediction on Full Sequence")
dev.off()
#bacteriodites
png("sA_Bacteriodites_Arima_full_seq.png",600,600)
actino_Ts <- as.ts(sA_Genus_toPlot[,2])
fit <- arima(actino_Ts, order=c(3, 1, 3),seasonal = c(0,1,1))
toPlot <- forecast(fit, 25)
arima <- toPlot$mean
ari <- data.frame(arima,(1:25)+277)
names(ari) <- c('y','x')
upper <- toPlot$upper
up <- data.frame(upper,(1:25)+277)
names(up) <- c('y1','y','x')
lower <- toPlot$lower
low <- data.frame(lower,(1:25)+277)
names(low) <- c('y1','y','x')
#plot(toPlot, main = "Siliva A Bacteriodites Arima Forecast",xlab = "Sample Day", ylab = "Count")
ggplot() +
  geom_line(data = sA_Genus_toPlot[1:277,c(2,9)],aes(x=day, y=Bacteroidetes, colour = "data"), size = 0.75)+
  geom_line(data = ari,aes(x=x, y=y, colour = "Arima"), size = 0.75)+
  geom_line(data = up,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  geom_line(data = low,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  scale_color_manual(values=c("Red","Black","Blue"))+
  theme(legend.title=element_blank())+
  labs(title = "Bacteroidetes Arima Prediction on Full Sequence")
dev.off()
#actino
png("sA_Actinobacteria_Arima_full_seq.png",600,600)
xna_Ts <- as.ts(sA_Genus_toPlot[,1])
fit <- arima(xna_Ts, order=c(3, 1, 3))
toPlot <- forecast(fit, 25)
arima <- toPlot$mean
ari <- data.frame(arima,(1:25)+277)
names(ari) <- c('y','x')
upper <- toPlot$upper
up <- data.frame(upper,(1:25)+277)
names(up) <- c('y1','y','x')
lower <- toPlot$lower
low <- data.frame(lower,(1:25)+277)
names(low) <- c('y1','y','x')
#plot(toPlot, main = "Siliva A Bacteriodites Arima Forecast",xlab = "Sample Day", ylab = "Count")
ggplot() +
  geom_line(data = sA_Genus_toPlot[1:277,c(1,9)],aes(x=day, y=Actinobacteria..phylum., colour = "data"), size = 0.75)+
  geom_line(data = ari,aes(x=x, y=y, colour = "Arima"), size = 0.75)+
  geom_line(data = up,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  geom_line(data = low,aes(x=x, y=y, colour = "Limit"), size = 0.75)+
  scale_color_manual(values=c("Red","Black","Blue"))+
  theme(legend.title=element_blank())+
  labs(title = "Actinobacteria Arima Prediction on Full Sequence")
#plot(toPlot, main = "Siliva A Actinobacteria Arima Forecast",xlab = "Sample Day", ylab = "Count")
dev.off()

#################################################
##        Nutritional Correlations             ##
#################################################

#get the nutrition data
sA_Nut <- data.frame(t(salivaA[5433:5442,]))
sA_Genus_toPlot_temp <- sA_Genus_toPlot
sA_Genus_toPlot_temp[,9:10] <- NA
sA_Nut_Cor <- cor(sA_Genus_toPlot_temp,sA_Nut[1:277,], use="pairwise.complete.obs", method="pearson")

png("RGBcorsANut.png",600,600)
par(mar = c(15, 10, 4, 2) + 0.1)
color2D.matplot(sA_Nut_Cor,c(0,1),0,0,c(2,3),main = 'Correlation between nutrition and Phylum in Siliva A', show.legend = FALSE, show.values = TRUE,  xlab = "", ylab = "", vcex = 1.5,axes = FALSE)

# Set up x axis with tick marks alone
axis(1, labels = FALSE)
axis(2, labels = FALSE)

# Create arbitrary text
labels <- c(colnames(sA_Genus_toPlot[,1:8]))

# plot x axis labels using:
# par("usr")[3] - 0.25 as the vertical placement
# srt = 45 as text rotation angle
# adj = 1 to place right end of text at tick mark
# xpd = TRUE to allow for text outside the plot region
text(par("usr")[1] - 0.25, 10:3, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(1, text = "Nutrition", line = 13)

labels <- c(colnames(sA_Nut))
text( 1:10,par("usr")[1] - 0.25, srt = 45, adj = 1,
      labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(2, text = "Phylum", line = 8)
dev.off()

#get the nutrition data
stA_Nut <- data.frame(t(stoolA[5433:5442,]))
stA_phylum_toPlot_temp <- stA_phylum_toPlot
stA_phylum_toPlot_temp[,8:10] <- NA
stA_Nut_Cor <- cor(stA_phylum_toPlot_temp,stA_Nut[1:329,], use="pairwise.complete.obs", method="pearson")

png("RGBcorstANut.png",600,600)
par(mar = c(15, 10, 4, 2) + 0.1)
color2D.matplot(stA_Nut_Cor,c(0,1),0,0,c(2,3),main = 'Correlation between Nutrition and Phylum in Stool A', show.legend = FALSE, show.values = TRUE,  xlab = "", ylab = "", vcex = 1.5,axes = FALSE)

# Set up x axis with tick marks alone
axis(1, labels = FALSE)
axis(2, labels = FALSE)

# Create arbitrary text
labels <- c(colnames(stA_phylum_toPlot_temp[,1:7]))

# plot x axis labels using:
# par("usr")[3] - 0.25 as the vertical placement
# srt = 45 as text rotation angle
# adj = 1 to place right end of text at tick mark
# xpd = TRUE to allow for text outside the plot region
text(par("usr")[1] - 0.25, 10:3, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(1, text = "Nutrition", line = 13)

labels <- c(colnames(stA_Nut))
text( 1:10,par("usr")[1] - 0.25, srt = 45, adj = 1,
      labels = labels, xpd = TRUE)

# plot x axis label at line 6 (of 7)
mtext(2, text = "Phylum", line = 8)
dev.off()
