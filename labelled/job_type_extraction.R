library(tm)
library(stringr)
library(RTextTools)
library(lsa)


setwd("C:/Users/alluring/Downloads/labelled")
#options(encoding="utf-8")


origsent<-read.table("C:/Users/alluring/Downloads/labelled/sentences_german.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE, quote="", comment.char="", fileEncoding="utf-8")
labels<-read.table("C:/Users/alluring/Downloads/labelled/labels_german.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE, quote="", comment.char="", fileEncoding="utf-8")
predicted<-read.table("C:/Users/alluring/Downloads/labelled/tasklist_nursing.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE, quote="", comment.char="", fileEncoding="utf-8")
explabel<-read.table("C:/Users/alluring/Downloads/labelled/explabel.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE, quote="", comment.char="", fileEncoding="utf-8")

humanlabel<-data.frame(origsent, labels)
names(humanlabel)<-c("sentence", "first")
expertlab<-data.frame(predicted, explabel)
names(expertlab)<-c("sentence", "second")
human_expert_label<-merge(expertlab,humanlabel, by.y="sentence", by.x="sentence", all=TRUE)

newdata<-read.table("humanandExpert2.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE, encoding="utf-8")
write.table(humanexpertlabelcopy, file="humanandExpert2.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
names(origdata)<-c("vacancyid","sentence")

taskdata<-read.table("tasklist_nursing2.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE, fileEncoding="utf-8")
write.table(topic_matrix, file="topicmatrix.csv", sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE)
write.table(topic_matrix_scores, file="topicmatrixscores.csv", sep=",", row.names=FALSE, col.names=paste("t",1:50,sep=""), quote=FALSE)


topic_data<-read.table("topicmatrix.csv", header=TRUE, sep=",")
d<-dist(as.matrix(topic_matrix_scores), method="canberra")


k<-20
SEED<- 2010
topicsmydtm<-list(VEM = LDA(mydtm, k=k, control=list(seed=SEED)),
VEM_fixed = LDA(mydtm, k=k,
control =list(estimate.alpha=FALSE, seed = SEED)),
Gibbs = LDA(mydtm, k =k , method="Gibbs",
control = list(seed=SEED, burnin=1000,
thin=100, iter=1000)),
CTM=CTM(mydtm, k=k, control=list(seed=SEED,
var=list(tol=10^-4), em=list(tol=10^-3))))


topicperdoc<-topicsmydtm[["CTM"]]@gamma
topicperdoc[c(4,47),]
d<-as.dist(1-cosine(t(topicperdoc)))


d<-dist(topicperdoc, method="euclidean")
hc<-hclust(d, method="complete")
plot(hc)

clustmem<-cutreeDynamicTree(hc, maxTreeHeight = .15, minModuleSize=3)
clustmem[571]

nursetask[which(clustmem==80)]
table(cutree(hc, h=.2))
which(cutree(hc, h=0.25)==85)
cutree(hc, h=.25)

nursetask[which(cutree(hc, h=0.2)==27)]

table(cutree(hc, h=40))
which(cutree(hc, h=2)==1)

files<-list.files()
for(i in files){
  thesent<-readLines(i, encoding="utf-8")
  clust<-str_extract(i, pattern="[[:digit:]]+")
  cat(thesent,file="allsentences.txt", sep="\n",append=TRUE)
  
  cat(rep(clust, length(thesent)), file="all_labels.txt", sep="\n", append=TRUE)
  
}


#fuzzy
fres<-fanny(d, k=100, diss=TRUE)
table(fres$clustering)
### Step 1: Import training database

taskdata1<-read.table("./first0_5405_/tasklist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)
taskdata2<-read.table("./second_1216_7_/tasklist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)
taskdata3<-read.table("./third_1615_20/tasklist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)

skilldata1<-read.table("./first0_5405_/skilllist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)
skilldata2<-read.table("./second_1216_7_/skilllist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)
skilldata3<-read.table("./third_1615_20/skilllist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)


otherdata1<-read.table("./first0_5405_/otherlist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)
otherdata2<-read.table("./second_1216_7_/otherlist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)
otherdata3<-read.table("./third_1615_20/otherlist.txt", header=FALSE, sep="\t", stringsAsFactors = FALSE)

# merge

taskdata<-rbind(taskdata1, taskdata2,taskdata3) 
skilldata<-rbind(skilldata1, skilldata2, skilldata3)
otherdata<-rbind(otherdata1, otherdata2, otherdata3)

taskdatau<-taskdata[!duplicated(taskdata$V2),]
skilldatau<-skilldata[!duplicated(skilldata$V2),]
otherdatau<-otherdata[!duplicated(otherdata$V2),]

## 231 tasks; 413 skills; 5532 other ; total: 6176
## 197 tasks; 252 skills; 2051 other ; total: 2500

# merge 
alldatau<-rbind(taskdatau, skilldatau, otherdatau)

# task and skill only
alldatatemp<-alldatau
alldatatemp$jobtype<-c(rep(1,197), rep(2,252),rep(0,2051))
alldatatemp$jobtype<-factor(alldatatemp$jobtype, levels=c(1,2,0), labels=c("task", "skill","other"))
names(alldatatemp)[1:2]<-c("vacancyid","sentence")

#write.table(labeldata, file="test4.txt", sep="\t", row.names=FALSE, col.names=FALSE)

mixdata<-merge(origdata, alldatatemp, by.x="sentence",by.y="sentence", all.x=TRUE, all.y=FALSE)


# split train-test
#idx<-sample(1:644, size=450, replace=FALSE)
#traindata<-alldatatemp[idx,]
#testdata<-alldatatemp[-idx,]

#corpus
#taskcorpus<-VCorpus(VectorSource(mixdata$sentence), readerControl=list(language="de"))
taskcorpus<-VCorpus(VectorSource(taskdata$V1), readerControl=list(language="de"))

# preprocessing
taskcorpus <- tm_map(taskcorpus, content_transformer(tolower))
taskcorpus[[1]]$content
taskcorpus <- tm_map(taskcorpus, removeWords, iconv(stopwords("de")))
taskcorpus <- tm_map(taskcorpus, removeNumbers)
taskcorpus <- tm_map(taskcorpus, removePunctuation)
taskcorpus <- tm_map(taskcorpus, stripWhitespace)
taskcorpus <- tm_map(taskcorpus, content_transformer(str_trim))
taskcorpus<-tm_filter(taskcorpus, FUN=function(x) nchar(content(x))!=0)
#taskcorpus <- tm_map(taskcorpus, stemDocument, language="de")

for(k in 1:1179){
  #write(taskcorpus[[k]]$content,"german_nurses2.txt", append=TRUE)
  write(reskmeans$cluster[k],"cluster_membership.txt", append=TRUE)
}

for(k in 1:835){
  #write(taskcorpus[[k]]$content,"german_nurses2.txt", append=TRUE)
  write(clustmem[k],"cluster_membership835.txt", append=TRUE)
}

task_cluster<-read.table("origpluscluster.csv",header=TRUE, sep=";", stringsAsFactors=FALSE)

for(m in 1:100){
  #write(taskcorpus[[k]]$content,"german_nurses2.txt", append=TRUE)
  write(task_cluster[which(task_cluster$cluster==m),1],paste("./clusteringOfTasks/cluster",m,".txt", sep=""))
}

reskmeans<-kmeans(topic_matrix_scores, centers=100, iter.max=10, algorithm="Hartigan-Wong")
# document term matrix


wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:150) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:150, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


mydtm<-DocumentTermMatrix(taskcorpus)
#jobtypeinfo<-mixdata$jobtype[as.integer(dimnames(mydtm)[[1]])]
findFreqTerms(mydtm, 100)
findAssocs(mydtm, "dokumentation", .1)

term_tfidf<- tapply(mydtm$v/row_sums(mydtm)[mydtm$i], mydtm$j, mean) * log2(nDocs(mydtm)/col_sums(mydtm>0))
summary(term_tfidf)
mydtmnew<-mydtm[,term_tfidf >= 1.54]
mydtmnew<-mydtmnew[row_sums(mydtmnew) > 0,]
summary(col_sums(mydtmnew))

library(topicmodels)
k<-9
SEED<- 2010

topicsmydtm<-list(VEM = LDA(mydtm, k=k, control=list(seed=SEED)),
                  VEM_fixed = LDA(mydtm, k=k,
                    control =list(estimate.alpha=FALSE, seed = SEED)),
                  Gibbs = LDA(mydtm, k =k , method="Gibbs",
                              control = list(seed=SEED, burnin=1000,
                                             thin=100, iter=1000)),
                  CTM=CTM(mydtm, k=k, control=list(seed=SEED,
                                                   var=list(tol=10^-4), em=list(tol=10^-3))))

sapply(topicsmydtm[1:2], slot, "alpha")
sapply(topicsmydtm, function(x)
  mean(apply(posterior(x)$topics, 1, function(z)-sum(z * log(z)))))

Topic<-topics(topicsmydtm[["VEM"]],1)
Terms<-terms(topicsmydtm[["VEM"]],5)

head(topicsmydtm[["CTM"]]@gamma)

dtm_tfxidf<-weightTfIdf(mydtm)

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)


### cluster into 10 clusters
cl <- kmeans(m_norm, 100)
table(cl$cluster)


for(j in 1:17873){
  write(newdata$first[j],"labels_german2.txt", append=TRUE)
  #write(taskcorpus[[j]]$content,"german_nurses.txt", append=TRUE)
}

sent_included<-mixdata$sentence[as.integer(dimnames(mydtm)[[1]])]

for(i in sent_included){
  write(i,"sentences_german.txt", append=TRUE)
}

# perform lsa for dimensionality reduction
td.mat <- as.matrix(t(mydtm))
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa, dims=dimcalc_share(share=0.40))  # create LSA space
newdata<-t(td.mat.lsa) %*% lsaSpace$tk %*% solve(diag(lsaSpace$sk))
newdata2<-newdata[,which(apply(newdata,2,max)>.00004)]
newdata2<-as.data.frame(newdata2)
newdata2$jobinfo<-jobtypeinfo




# SMOTE
data<-newdata2
data$newjobinfo <- factor(ifelse(data$jobinfo == "skill","rare","common")) 

dataSmote<-SMOTE(newjobinfo~.,data, k=5, perc.over=200, perc.under=100)

table(dataSmote$newjobinfo)
table(dataSmote$jobinfo)

taskdataover<-dataSmote
skilldataover<-dataSmote

mydata<-rbind(taskdataover, skilldataover)

dim(mydata)
rownames(mydata)<-make.names(rownames(mydata), unique=TRUE)

container <- create_container(mydata[,1:301],mydata[,302],trainSize=1:3220,virgin=FALSE)
models <- train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
results <- classify_models(container, models)
analytics <- create_analytics(container, results)
analytics@algorithm_summary

dist.mat.lsa <- 1-cosine(as.textmatrix(lsaSpace))  # compute distance matrix
dist.mat.lsa<-dist.mat.lsa[,!(colSums(!is.finite(dist.mat.lsa))==651)]
dist.mat.lsa<-dist.mat.lsa[!(rowSums(!is.finite(dist.mat.lsa))==641),]
dist.mat.lsa  # check distance mantrix





attributes(mydtm)
term_tfidf<-tapply(mydtm$v/row_sums(mydtm)[mydtm$i], mydtm$j,mean)*log2(nDocs(mydtm)/col_sums(mydtm>0))
summary(term_tfidf)
# median = 1.04600
mydtm<-mydtm[, term_tfidf>=1.042]
nonempty<-as.vector(which(row_sums(mydtm)>0))
mydtm<-mydtm[row_sums(mydtm)>0,]
#which(row_sums(mydtm)>0)
summary(col_sums(mydtm))


mydtm_matrix<-as.matrix(mydtm)
dist.mat<-dist(mydtm_matrix, method="binary")

fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y,
                                                                  color = alldatatemp$target[as.integer(rownames(dist.mat.lsa))])) + coord_cartesian(xlim = c(-10, 10), ylim=c(-10,10)) 




#fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
#points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])

write( c("dog", "cat", "mouse"), file=paste(td, "D1", sep="/") )
write( c("ham", "mouse", "sushi"), file=paste(td, "D2", sep="/") )
write( c("dog", "pet", "pet"), file=paste(td, "D3", sep="/") )
data(stopwords_en)
myMatrix = textmatrix(td, stopwords=stopwords_en)

vec1 = c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 = c( 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )
vec3 = c( 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0 )
matrix = cbind(vec1,vec2, vec3)
weighted = lw_logtf(matrix)*gw_idf(matrix)




findFreqTerms(mydtm, lowfreq=40)

if (tgt < ncol(data)) {
  cols <- 1:ncol(data)
  cols[c(tgt, ncol(data))] <- cols[c(ncol(data), tgt)]
  data <- data[, cols]
}

smote.exs <- function(data,tgt,N,k)
  # INPUTS:
  # data are the rare cases (the minority "class" cases)
  # tgt is the name of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours to use for the generation
  # OUTPUTS:
  # The result of the function is a (N/100)*T set of generated
  # examples with rare values on the target
{
  nomatr <- c()
  T <- matrix(nrow=dim(data)[1],ncol=dim(data)[2]-1)
  for(col in seq.int(dim(T)[2]))
    if (class(data[,col]) %in% c('factor','character')) {
    #if (class(data[,col]) %in% c('factor')) {
      T[,col] <- as.integer(data[,col])
      nomatr <- c(nomatr,col)
    } else T[,col] <- data[,col]
  
  if (N < 100) { # only a percentage of the T cases will be SMOTEd
    nT <- NROW(T)
    idx <- sample(1:nT,as.integer((N/100)*nT))
    T <- T[idx,]
    N <- 100
  }
  
  p <- dim(T)[2]
  nT <- dim(T)[1]
  
  #ranges <- apply(T,2,max)-apply(T,2,min)
  
  nexs <-  as.integer(N/100) # this is the number of artificial exs generated
  # for each member of T
  new <- matrix(nrow=nexs*nT,ncol=p)    # the new cases
  
  for(i in 1:nT) {
    
    # the k NNs of case T[i,]
    xd <- scale(T,T[i,],ranges)
    for(a in nomatr) xd[,a] <- xd[,a]==0
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    kNNs <- order(dd)[2:(k+1)]
    
    for(n in 1:nexs) {
      # select randomly one of the k NNs
      neig <- sample(1:k,1)
      
      ex <- vector(length=ncol(T))
      
      # the attribute values of the generated case
      difs <- T[kNNs[neig],]-T[i,]
      new[(i-1)*nexs+n,] <- T[i,]+runif(1)*difs
      for(a in nomatr)
        new[(i-1)*nexs+n,a] <- c(T[kNNs[neig],a],T[i,a])[1+round(runif(1),0)]
      
    }
  }
  newCases <- data.frame(new)
  for(a in nomatr)
    newCases[,a] <- factor(newCases[,a],levels=1:nlevels(data[,a]),labels=levels(data[,a]))
  
  newCases[,tgt] <- factor(rep(data[1,tgt],nrow(newCases)),levels=levels(data[,tgt]))
  colnames(newCases) <- colnames(data)
  newCases
}


convert_text_to_sentences <- function(text, lang = "de") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}


topic_matrix_scores<-matrix(data=rep(0,50*1179), nrow=1179, ncol=50)
topic_scores<-readLines("mycorpus_topics_scores.csv")
topics<-readLines("mycorpus_topics.csv")

for(i in 1:length(topics)){
  print(i)
  l<-unlist(strsplit(topic_scores[i],", "))
  n=1
  for(j in unlist(strsplit(topics[i],", "))){
    print(j)
    k<-as.numeric(j)
    #topic_matrix[i,k+1]<-1
    topic_matrix_scores[i,k+1]<-as.numeric(l[n])
    n=n+1
  }
}
mydata<-topic_matrix_scores
which(!complete.cases(mydata))

res<-biclust(topic_matrix_scores, method=BCCC(), delta = .5, alpha=.5, number=40)


harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

k = 20
burnin = 1000
iter = 1000
keep = 50

fitted <- LDA(mydtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
harmonicMean(logLiks)

# generate numerous topic models with different numbers of topics
sequ <- seq(2, 100, 1) # in this case a sequence of numbers from 1 to 50, by ones.
fitted_many <- lapply(sequ, function(k) LDA(mydtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(sequ, hm_many, type = "l")

# compute optimum number of topics
sequ[which.max(hm_many)]
