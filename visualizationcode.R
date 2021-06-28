#
# File No 2: Visualization
# 

rm(list=ls())

library(wesanderson)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggrepel)
library(bio3d)
library(plotrix)


setwd("C:/Users/HP/OneDrive/UZH/Sommersemester2021/Web Data/Essay/twitter")

# Staenderat ----
twitter <- read.csv("twitter.csv", row.names = 1, header= TRUE) 
twitter <- twitter %>% filter(institution == "sr")


# Measuring Ideology ----

ratiomat <- read.csv("ratiomat_sr.csv", row.names = 1, header= TRUE )
#ratiomat$party_id <- twitter$party
head(ratiomat)
# clean up those who dont have overlaps
ratiomat$Levrat <- NULL
ratiomat$Baume.Schneider <- NULL
ratiomat$Zopfi <- NULL
ratiomat$Häberli.Koller <- NULL
ratiomat["Levrat",] <- NA
ratiomat["Baume-Schneider",] <- NA
ratiomat["Zopfi", ] <- NA
ratiomat["Häberli-Koller", ] <- NA
ratiomat <- ratiomat %>% na.omit()
out <- c("Baume-Schneider", "Zopfi", "Levrat", "Häberli-Koller")
twitter <- subset(twitter, !pages%in%out)

# SVD---- reduce matrix to vector
singular <- svd(ratiomat)
singular$u
# u is left singular vectors
left <- as.data.frame(singular$u)
hist(left$V1)
#left$V1[63] <- NA

se <- vector()
for (i in 1:25){
  s <- std.error(left[,i])
  se <- c(se,s)
  }
min(se)

var(left$V25)
var(left$V24)

sd(left$V1)
mean(left$V1)

#left$nv <- normalize.vector(singular$d)
# normalizing
left$scores <- abs(left$V1-mean(left$V1))/sd(left$V1) 
mean(left$scores)
sd(left$scores)

rownames(left) <- twitter$pages
left$id <- rownames(left)
left$party <- twitter$party
left$likes <- twitter$likes
#anchoring
lp <- c("SP", "Gruene")
rp <- c("SVP")
left$scores <- ifelse(left$party%in%lp, left$scores-1, left$scores)
min(left$scores)
mean(left$scores)
hist(left$scores)
#hist(left$nv)

p <- ggplot(data = left, aes(x = scores, y = 0)) + 
  theme_bw () + 
  annotate("segment",x=-1,xend=4, y=0, yend=0, size=1) +
  geom_point(aes(col = party, size = likes*10), show.legend = F)+
  theme(axis.text.y = element_blank())+
  geom_label_repel(aes(label = id, fill = party), size = 3,
                   max.overlaps = 25, label.padding = 0.2, force = 2, )+
  scale_color_manual(values = (c("blue", "green", "orange", "red", "darkgreen")))+
  scale_fill_manual(values = (c("blue", "green", "orange", "red", "darkgreen")))+
  labs(color = "Partei") + 
  ylab(element_blank())+
  guides(likes = F, edge_width = F, size = F, label = F)+  
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.title = element_blank(), 
        text=element_text(size=14,  family="serif"), legend.position = c(0.7,0.2),
        legend.background = element_blank(), legend.direction = "horizontal", legend.text = element_text(size = 14) )
p

#save plot 
pdf("councillors.pdf", paper = "a4r", width = 9, height = 6)
p
dev.off()

#?geom_label_repel()



dens_p <- ggplot(left) +
  theme_bw()+
  geom_density(aes(x = scores, fill = party), alpha = 0.7, show.legend = T) +
  scale_fill_manual(values = (c("blue", "green", "orange", "red", "darkgreen")))+
  geom_point(inherit.aes = FALSE, data = left[left$party== "SP",], 
             aes(y = 4, x = mean(left[left$party== "SP",]$scores)), 
             color = "red", size = 4, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "Gruene",], 
             aes(y = 4, x = mean(left[left$party== "Gruene",]$scores)),
             color = "green", size = 4, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "FDP",], 
             aes(y = 4, x = mean(left[left$party== "FDP",]$scores)), 
             color = "blue", size = 4, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "Mitte",], 
             aes(y = 4, x = mean(left[left$party== "Mitte",]$scores)), 
             color = "orange", size = 4, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "SVP",], 
             aes(y = 4, x = mean(left[left$party== "SVP",]$scores)), 
             color = "darkgreen", size = 4, shape = 19)+
  labs(x =NULL, y = NULL, color = NULL)+
  theme(legend.title = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.8,0.5), legend.text = element_text(size = 14))+
  theme(text=element_text(size=14,  family="serif"))
dens_p

#save plot 
pdf("density_sr.pdf", paper = "a4r", width = 12, height = 8)
dens_p
dev.off()



# Nationalrat ----

twitter <- read.csv("twitter.csv", row.names = 1, header= TRUE) 
twitter <- twitter %>% filter(institution == "nr", pages != "hurni", likes >= 20)



# Measuring Ideology ----

ratiomat <- read.csv("ratiomat_nr.csv", row.names = 1, header= TRUE ) %>% na.omit()
rownames(ratiomat)
ratiomat$hurni <- NULL
ratiomat$grueter <- NULL
ratiomat[56,] <- NA
ratiomat <- ratiomat %>% na.omit()
#ratiomat$party_id <- twitter$party
head(ratiomat)

# SVD---- reduce matrix to vector
singular <- svd(ratiomat)
table(is.na(ratiomat))
left <- as.data.frame(singular$u)
hist(left$V1)
#left$V1[63] <- NA
sd(left$V1)
mean(left$V1)
var(left$V1)/mean(left$V1)
left$scores <- abs(left$V1-mean(left$V1))/sd(left$V1)
lp <- c("SP", "Gruene")
rp <- c("SVP")
left$party <- twitter$party
left$likes <- twitter$likes
left$scores <- ifelse(left$party%in%lp, left$scores-1, 
                      ifelse(left$party%in%rp ,left$scores+0.5, left$scores))
hist(left$scores)
rownames(left) <- rownames(ratiomat)
left$id <- rownames(left)

hist(left$scores)
left <- left %>% select(scores, likes, party, id)
p <- ggplot(data = left, aes(x = scores, y = 0)) + 
  theme_minimal()+
  geom_point(aes(col = party, size = likes*10))+
  theme(axis.text.y = element_blank())+
  geom_label_repel(aes(label = paste(id, ", ", party)), max.overlaps = 50)+
  labs(title = "Ideologie des Nationalrats", subtitle = "Forschungsseminar DDJ", color = "Partei") + 
  ylab(element_blank())+
  theme(panel.background = element_blank())+
  guides(likes = F, edge_width = F, size = F)
#  geom_text(aes(x = scores, y = scores, label = id), nudge_y = -0.25)
p


# density plot

dens_p <- ggplot() +
  geom_density(data = left[left$party== "SP",],aes(x = scores), fill = "red", alpha = 0.5) +
  geom_density(data = left[left$party== "Gruene",],aes(x = scores), fill = "green", alpha = 0.5) +
  geom_density(data = left[left$party== "GLP",],aes(x = scores), fill = "yellow", alpha = 0.5) +
  geom_density(data = left[left$party== "SVP",],aes(x = scores), fill = "darkgreen", alpha = 0.7) + 
  geom_density(data = left[left$party== "FDP",],aes(x = scores), fill = "blue", alpha = 0.7) +
  geom_density(data = left[left$party== "Mitte",],aes(x = scores), fill = "orange", alpha = 0.7) +
  # annotate("segment",x=-0.5,xend=7.6, y=0, yend=0, size=1) +
  # annotate("segment",x=-0.5,xend=-0.5, y=-0.1,yend=0.1, size=1) +
  # annotate("segment",x=7.6,xend=7.6, y=-0.1,yend=0.1, size=1) +
  geom_point(inherit.aes = FALSE, data = left[left$party== "SP",], 
             aes(y = 0.5, x = mean(left[left$party== "SP",]$scores)), color = "red", size = 3, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "Gruene",], 
             aes(y = 0.5, x = mean(left[left$party== "Gruene",]$scores)), color = "green", size = 3, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "GLP",], 
             aes(y = 0.5, x = mean(left[left$party== "GLP",]$scores)), color = "yellow", size = 3, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "SVP",], 
             aes(y = 0.5, x = mean(left[left$party== "SVP",]$scores)), color = "darkgreen", size = 3, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "FDP",], 
             aes(y = 0.5, x = mean(left[left$party== "FDP",]$scores)), color = "blue", size = 3, shape = 19)+
  geom_point(inherit.aes = FALSE, data = left[left$party== "Mitte",], 
             aes(y = 0.5, x = mean(left[left$party== "Mitte",]$scores)), color = "orange", size = 3, shape = 19)+
  theme(panel.background = element_blank())
dens_p  

  

