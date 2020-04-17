#install.packages('dbscan')
#install.packages('ggrepel')
library(dbscan)
library(ggplot2)
library(ggrepel) # 플롯에서 라벨이 서로 겹쳐서 보이지 않도록 재조정하는 라이브러리

# 각 아이템별 벡터 데이터 불러오기
data <- read.csv('설문지_word2vec_300_1_2_daex_곱x_5.csv', fileEncoding='utf8')
head(data)
rownames(data) <- data[,1]
data <- data[,-1]
no <- which(rownames(data) == '게임(소)')
data <- data[-no,]
no1 <- which(rownames(data)=='슬라임(소)')
data <- data[-no1,]
no2 <- which(rownames(data)=='스트레칭(소)')
data <- data[-no2,]
no3 <- which(rownames(data)=='홈트(소)')
data <- data[-no3,]
no4 <- which(rownames(data)=='이스포츠(소)')
data <- data[-no4,]
no5 <- which(rownames(data)=='액체괴물(소)')
data <- data[-no5,]
no6 <- which(rownames(data)=='그림(소)')
data <- data[-no6,]
no7 <- which(rownames(data)=='DIY(소)')
data <- data[-no7,]
no8 <- which(rownames(data)=='직캠(소)')
data <- data[-no8,]
no9 <- which(rownames(data)=='교차편집(소)')
data <- data[-no9,]
no10 <- which(rownames(data)=='운동영상(소)')
data <- data[-no10,]
no11 <- which(rownames(data)=='야생동물(소)')
data <- data[-no11,]
no12 <- which(rownames(data)=='스트레칭(소)')
data <- data[-no12,]
no13 <- which(rownames(data)=='작문정보(소)')
data <- data[-no13,]
no14 <- which(rownames(data)=='기부및자선활동(소)')
data <- data[-no14,]
no15 <- which(rownames(data)=='사회과학(소)')
data <- data[-no15,]
no16 <- which(rownames(data)=='지역뉴스(소)')
data <- data[-no16,]
no17 <- which(rownames(data)=='라디오(소)')
data <- data[-no17,]



## PCA ##
# 차원 축소를 위해 PCA 이용
vec.pca <- prcomp(data) 
item2vec.pca <- data.frame(item=rownames(data), vec.pca$x[, 1:2])
summary(vec.pca)


## k-means ##
#Elbow Method
set.seed(1500)
wss <- 0

item2vec.kmeans <- data.frame(item=rownames(data), vec.pca$x[, 1:2])

#k를 1~15까지 변화시키면서 각 withinss 값을 wss에 저장
for(i in 1:15) wss[i]<-sum(kmeans(item2vec.kmeans[, c('PC1', 'PC2')],centers = i)$withinss)

#withinss값을 그래프로 그리기, 기울기가 완만해지는 3~4가 Elbow point라는 것을 알수 있다
plot(1:15, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")
clustering = kmeans(x = item2vec.pca[, c('PC1', 'PC2')], centers = 6, nstart=500)
item2vec.kmeans$cluster <- as.factor(clustering$cluster)

table(clustering$cluster)
#clustering$centers

# 클러스터링 결과 시각화
windows()
my.theme <- theme_bw() + theme(text = element_text(size=14, face='bold'))
#my.theme <- theme_bw() + theme(text = element_text(size=14, family='nanumgothic'))
g <- ggplot(item2vec.kmeans) + 
  geom_point(aes(PC1, PC2, col=cluster)) + 
  my.theme + 
  geom_text_repel(aes(PC1, PC2, label=item, col=cluster), size=2.3)

centroids <- data.frame(cluster = factor(seq(1:6)),
                        pc1 = clustering$centers[,'PC1'],
                        pc2 = clustering$centers[,'PC2'])

g +  geom_point(data = centroids, aes(x = pc1, y = pc2, col = cluster), 
                #pch=13,
                size=10,
                alpha = 0.5)

library(tidyverse)
library(showtext)
showtext_auto()

font_add_google("Nanum Gothic", "nanumgothic")


###################################################################
# 각 아이템별 벡터 데이터 불러오기
data1 <- read.csv('설문지_word2vec_300_5_20_daex.csv', fileEncoding='utf8')
head(data1)
rownames(data1) <- data1[,1]
data1 <- data1[,-1]

## PCA ##
# 차원 축소를 위해 PCA 이용
vec.pca1 <- prcomp(data1) 
item2vec.pca1 <- data.frame(item=rownames(data1), vec.pca1$x[, 1:2])
summary(vec.pca1)

## mclust ##
item2vec.clust1 <- data.frame(item=rownames(data1), vec.pca1$x[, 1:2])
xyMclust <- Mclust(item2vec.clust1[, c('PC1', 'PC2')])
plot(xyMclust)

## k-means ##
#Elbow Method
wss <- 0

item2vec.kmeans1 <- data.frame(item=rownames(data1), vec.pca1$x[, 1:2])

#k를 1~15까지 변화시키면서 각 withinss 값을 wss에 저장
for(i in 1:15) wss[i]<-sum(kmeans(item2vec.kmeans1[, c('PC1', 'PC2')],centers = i)$withinss)

#withinss값을 그래프로 그리기, 기울기가 완만해지는 3~4가 Elbow point라는 것을 알수 있다
plot(1:15, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")
clustering1 = kmeans(x = item2vec.pca1[, c('PC1', 'PC2')], centers = 9)
item2vec.kmeans1$cluster <- as.factor(clustering1$cluster)

table(clustering1$cluster)
clustering1$centers

# 클러스터링 결과 시각화
windows()
my.theme <- theme_bw() + theme(text = element_text(size=14, face='bold'))
g <- ggplot(item2vec.kmeans1) + 
  geom_point(aes(PC1, PC2, col=cluster)) + 
  my.theme + 
  geom_text_repel(aes(PC1, PC2, label=item, col=cluster), size=2.5)

g +  geom_point(data = as.data.frame(clustering1$centers), aes(x = PC1, y = PC2), color = 'yellow',
                size=10,
                alpha = 0.5)
g +  geom_point(data = as.data.frame(clustering$centers), aes(x = PC1, y = PC2), pch=8,
                size=10,
                alpha = 0.5)
view(g)






