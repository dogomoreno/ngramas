library(readr)
data <- read_csv("mananeras texto oraciones.csv", 
                                      locale = locale(encoding = "ISO-8859-1"))

library(knitr); kable(data,align="c")

Nlines <- 600000
d.m <- readLines("mananeras texto oraciones.csv",Nlines)

# Split data into training, validation and test sets
set.seed(100)
idx <- sample(seq(Nlines)); N1 <- round(0.6*Nlines); N2 <- round(0.8*Nlines)
train <- d.m[idx[1:N1]]
valid <- d.m[idx[(N1+1):N2]]
test  <- d.m[idx[(N2+1):Nlines]]


# Uni-gram analysis

M <- 10000
# Compute unigrams
unigrams <- ngram_compute(train[1:M],n=1,mat=1) # Refer appendix for function definition

um <- unigrams %*% matrix(1,M,1) #row sums
names(um) <- rownames(unigrams); um <- sort(um, decreasing = TRUE)

um[1:10]

library(wordcloud)
wordcloud(names(um),um,max.words=200,scale = c(3,0.5),rot.per = 0.2, colors = brewer.pal(6,"Dark2"),random.order = FALSE)

# Statistical Inference

Ms <- 100; Mcol = floor(M/Ms)
mask_mat <- matrix(0,Mcol*Ms,Mcol); for(i in 1:Mcol) mask_mat[(((i-1)*Ms+1):(i*Ms)),i] <- 1
ums <- unigrams[names(um[1:10]),] %*% mask_mat; rownames(ums) <- names(um[1:10])

boxplot(t(ums[10:1,]),xlab = "frequency (Number of unigrams per 100 lines)", main = "Unigram frequencies",horizontal = T,las=1)


# Bi-gram and Tri-gram analysis

sum(um); length(um)

round(c(sum(um[1:820]), sum(um[1:3300]))/sum(um)*100,2)

dict <- names(um[1:3300])


# Compute bigrams and trigrams
bigrams <- ngram_compute(train[1:M],n=2,mat=1,dict)
trigrams <- ngram_compute(train[1:M],n=3,mat=1,dict)


bm <- bigrams %*% matrix(1,ncol(bigrams),1); names(bm) <- rownames(bigrams); bm <- sort(bm,decreasing = TRUE)
tm <- trigrams %*% matrix(1,ncol(trigrams),1); names(tm) <- rownames(trigrams); tm <- sort(tm,decreasing = TRUE)

wordcloud(names(bm),um,max.words=200,scale = c(3,0.5),rot.per = 0.2, colors = brewer.pal(6,"Dark2"),random.order = FALSE)
wordcloud(names(tm),um,max.words=200,scale = c(3,0.5),rot.per = 0.2, colors = brewer.pal(6,"Dark2"),random.order = FALSE)

# Prediction model

# Model Building

M <- 100000
prune <- 2
u.count <- ngram_compute(train[1:M],n=1)   # Unigram model
dict <- names(u.count[u.count>prune])    # Dictionary
b.count <- ngram_compute(train[1:M],n=2,dictionary = dict)   # Bigram model
t.count <- ngram_compute(train[1:M],n=3,dictionary = dict)   # Trigram model


# Unigram, Bigram and Trigram probabilities

# Calculate unigram log likelihood
N <- sum(u.count) # Total number of tokens
V <- length(dict) # Number of words in the dictionary
u.model <- rbind(as.matrix(u.count[dict]),"UNK" = 0) + 1 # Laplacian smoothing
u.model <- cbind(u.model,log(u.model[,1]/(N+V)) ); colnames(u.model) <- c("count","MLE")

# Calculate bigram log likelihood
bc.prune <- b.count[b.count > prune]
PQ <- names(bc.prune)
P <- sapply(strsplit(PQ, split = " "), function(x) x[1])
bm.1 <- as.matrix(bc.prune) + 1 # Laplacian smoothing
bm.1 <- cbind(bm.1,log(bm.1[,1]/u.model[P,1]) ) # "Word1" "Word2"
bm.2 <- as.matrix(cbind(rep(1,V),log(1/u.model[1:V,1]))) # "Word1" "UNK"
rownames(bm.2) <- paste(dict,"UNK")
bm.3 <- as.matrix(cbind(1,u.model["UNK",2])); rownames(bm.3) <- "UNK UNK" # "UNK" "UNK"
b.model <- rbind(bm.1,bm.2,bm.3); colnames(b.model) <- c("count","MLE")

# Calculate trigram log likelihood
tc.prune <- t.count[t.count > prune]
PQR <- rownames(tc.prune)
PQ <- sapply(strsplit(PQR, split = " "), function(x) paste(x[1:2],collapse = " "))
t.model <- cbind(as.matrix(tc.prune+1), log(as.matrix(tc.prune+1)/b.model[PQ,1]) )
colnames(t.model) <- c("count","MLE")

# Save model to disk
u.mle <- sort(u.model[,2], decreasing = TRUE)
b.mle <- sort(b.model[,2], decreasing = TRUE)
t.mle <- sort(t.model[,2], decreasing = TRUE)
save(u.mle,b.mle,t.mle,file="Model.RData")

summary(u.model[,2]); summary(b.model[,2]); summary(t.model[,2])
par(mfrow=c(1,3))
hist(u.model[,2],xlab="MLE", main= "Unigrams", col = "blue")
hist(b.model[,2],xlab="MLE", main="Bigrams", col = "green")
hist(t.model[,2],xlab="MLE", main="Trigrams",col = "salmon")

# Model comparison and selection

u_perp <- ngram_perplexity(valid[1:10000],u.model,b.model,t.model,1) # Unigram model perplexity
b_perp <- ngram_perplexity(valid[1:10000],u.model,b.model,t.model,2) # Bigram model perplexity
t_perp <- ngram_perplexity(valid[1:10000],u.model,b.model,t.model,3) # Trigram model perplexity


# Predicciones

print(ngram_predict("banco de"))

print(ngram_predict("Gobernadora"))
