ngram_tokenize <- function(data) {
  for(i in 1:length(data)) {
    temp <- data[i]
    temp <- gsub("[.-]"," ", temp) # replace . - with space
    temp <- gsub("[[:punct:]]","",temp) # remove punctuation
    temp <- gsub("[0-9]","",temp) # remove numbers
    data[i] <- tolower(temp) # to lower case
  }
  data <- lapply(data, function(x) unlist(strsplit(x,split=" ")) ) # into words
  data <- lapply(data, function(x) grep("^[a-z]+$",x,value=TRUE) ) # select only english
  
  # Remove profane and remove stop words
  stopWords <- readLines("C:/Users/luism/OneDrive/Python/ngramas/stop-words.txt"); stopWords <- gsub("[[:punct:]]","",stopWords)
  remWords <- c(stopWords)
  data <- lapply(data, function(x) { x[!(x %in% remWords)] })
}


ngram_compute <- function(data,n,mat = 0,dictionary = character()) {
  data <- ngram_tokenize(data) # Tokenize the data
  # Create n-grams
  if(n>1) { data <- sapply(data,function(x) x[x %in% dictionary]) } # select words in dictionary
  if(n==2) { # Bigrams
    idx2 <- sapply(data, function(x) ifelse(length(x)>1,TRUE,FALSE)) # rows with atleast 2 words
    data <- lapply(data[idx2],function(x) { paste(x[1:(length(x)-1)],x[2:length(x)]) })
  }
  if(n==3) { # Trigrams
    idx3 <- sapply(data, function(x) ifelse(length(x)>2,TRUE,FALSE)) # rows with atleast 3 words
    data <- lapply(data[idx3],function(x) { paste(x[1:(length(x)-2)],x[2:(length(x)-1)],x[3:length(x)]) })
  }
  # Count unique n-grams
  if(mat==1) { # simulates term-document-matrix
    L <- length(data)
    unique_ngrams <- unique(unlist(data))
    ngram <- matrix(0,length(unique_ngrams),L); rownames(ngram) <- unique_ngrams; colnames(ngram) <- 1:L
    for(i in 1:L) { ns <- table(data[[i]]);  ngram[names(ns),i] <- ns }
  }else { # obtains ngram counts
    ngram <- sort(table(unlist(data)),decreasing = TRUE)
  }
  ngram
}

bigram_mle <- function(D,u.model,b.mle,flag=0) {
  D[!(D %in% rownames(u.model))] <- "UNK"; 
  L <- length(D)
  if(flag == 0) { 
    Y <- D[1:(L-1)]
    Z <- D[2:L]
  } else { # special case for predicted word
    Z <- rownames(u.model); Z <- Z[1:(length(Z)-1)] # Map against dictionary  
    Y <- rep(D[L],length(Z))
  }
  YZ <- paste(Y,Z)
  id1 <- (YZ %in% rownames(b.model))
  id2 <- (!id1) & (Y != "UNK")
  id3 <- (!id1) & (Y == "UNK")
  a <- matrix(NA,length(YZ),1)
  if(sum(id1)>0) { a[id1] <- b.model[YZ[id1],2] }
  if(sum(id2)>0) { a[id2] <- log(1/u.model[Y[id2],1]) }
  if(sum(id3)>0) { a[id3] <- u.model["UNK",2] }
  rownames(a) <- YZ; colnames(a) <- "MLE"
  a
}


trigram_mle <- function(D,u.model,b.model,t.model,flag = 0) {
  D[!(D %in% rownames(u.model))] <- "UNK"; L <- length(D)
  if(flag == 0) {
    P <- D[1:(L-2)]; Q <- D[2:(L-1)]; R <- D[3:L]
  }else { # special case for predicted word
    R <- rownames(u.model); R <- R[1:(length(R)-1)] # Map against dictionary
    P <- rep(D[L-1],length(R)) 
    Q <- rep(D[L],length(R))
  }
  PQR <- paste(P,Q,R)
  id1 <- (PQR %in% rownames(t.model))
  PQ <- paste(P,Q)
  id2 <- (!id1) & (PQ %in% rownames(b.model))
  id3 <- (!id1) & (!(PQ %in% rownames(b.model))) & (P != "UNK")
  id4 <- (!id1) & (!(PQ %in% rownames(b.model))) & (P == "UNK")
  
  a <- matrix(NA,length(PQR),1)
  if(sum(id1)>0) { a[id1] <- t.model[PQR[id1],2] }
  if(sum(id2)>0) { a[id2] <- log(1/b.model[PQ[id2],1]) }
  if(sum(id3)>0) { a[id3] <- log(1/u.model[P[id3],1]) }
  if(sum(id4)>0) { a[id4] <- u.model["UNK",2] }
  
  rownames(a) <- PQR; colnames(a) <- "MLE"
  a
}


ngram_perplexity <- function(data,u.model,b.model,t.model,n) {
  D <- ngram_tokenize(data); D <- unlist(D); 
  if(n==1) { # Unigram
    D[!(D %in% rownames(u.model))] <- "UNK"
    a <- u.model[D,2]
    n_perp <- exp(-sum(a)/length(a))
  }
  if(n==2) { # Bigram model
    a <- bigram_mle(D,u.model,b.model)
    n_perp <- exp(-sum(a)/length(a))
  }
  if(n==3) { # Trigram model
    a <- trigram_mle(D,u.model,b.model,t.model)
    n_perp <- exp(-sum(a)/length(a))
  }
  n_perp
}


ngram_predict <- function(data) {
  
  D <- ngram_tokenize(data) # Tokenize
  D <- unlist(D); D[!(D %in% names(u.mle))] <- "UNK" # mark the ones not in dictionary as UNK
  L <- length(D)
  
  if(L==0) { P <- "UNK"; Q <- "UNK"}
  if(L==1) { P <- "UNK"; Q <- D[L] }
  if(L>1) { P <- D[L-1]; Q <- D[L] }
  R <- names(u.mle); R <- R[-length(R)] # Map against dictionary
  
  PQR <- paste(P,Q,R)
  id1 <- (PQR %in% names(t.mle))
  PQ <- paste(P,Q)
  QR <- paste(Q,R)
  id2 <- (!id1) & (PQ %in% names(b.mle))    &   (QR %in% names(b.mle))
  id3 <- (!id1) & (PQ %in% names(b.mle))    & (!(QR %in% names(b.mle)))
  id4 <- (!id1) & (!(PQ %in% names(b.mle))) &   (QR %in% names(b.mle))
  id5 <- (!id1) & (!(PQ %in% names(b.mle))) & (!(QR %in% names(b.mle)))
  
  a <- matrix(NA,length(PQR),1)
  if(sum(id1)>0) { a[id1] <- t.mle[PQR[id1]] }
  if(sum(id2)>0) { a[id2] <- b.mle[PQ] + b.mle[QR[id2]]            + log(2*0.4) }
  if(sum(id3)>0) { a[id3] <- b.mle[PQ] + u.mle[Q] + u.mle[R[id3]]  + log(3*0.4) }
  if(sum(id4)>0) { a[id4] <- u.mle[P]  + u.mle[Q] + b.mle[QR[id4]] + log(3*0.4) }
  if(sum(id5)>0) { a[id5] <- u.mle[P]  + u.mle[Q] + u.mle[R[id5]]  + log(4*0.4) }
  
  pred_word <- R[which.max(a)]
}


