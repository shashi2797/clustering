########CRIME DATA##############
input1 <- read.csv("crime_data.csv")
View(input1)
mydata <- input1[,(2:5)]
# Load data as mydata
View(mydata)

normalized_data <- scale(mydata)
View(normalized_data)
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete") 
fit
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=4)# cut tree into 4 clusters

?cutree
rect.hclust(fit, k=4, border="red")
?rect.hclust

membership<-as.matrix(groups)
View(membership)

final <- data.frame(input1, membership)
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
ncol(final)
View(final1)
final1 <- final[,c(6,1,2,3,4,5)]
final1
head(final1)


##############AIRLINES############
library(data.table)
library(readxl)
input2 <- read_excel("C:/Users/user/Documents/Airlines.xlsx")
View(input2)
colnames(input2)

mydata <- input2[,(2:12)]
# Load data as mydata
View(mydata)

normalized_data <- scale(mydata)
View(normalized_data)
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete") 
fit

plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=5)# cut tree into 4 clusters
rect.hclust(fit, k=4, border="red")

membership<-as.matrix(groups)
View(membership)
final <- data.frame(input2, membership)
View(final)



x <-  runif(50) # generating 50 random numbers
x

y <-  runif(50) # generating 50 random numbers 
y


input2<- cbind(x,y)
input2
plot(input2)
plot(input2, type = 'n')
text(input2,rownames(input2))
input2_km <- kmeans(input2,5) #kmeans clustering
str(input2_km)
input2_km$cluster
input2_km$centers


# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)

xcl <- clara(normalized_data, 5, sample = 100)
clusplot(xcl)


#Partitioning around medoids
xpm <- pam(normalized_data, 5)
clusplot(xpm)
