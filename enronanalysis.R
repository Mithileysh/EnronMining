# ------------------------------------------------
#
#           PART 1 - TOPIC ANALYSIS
#
# ------------------------------------------------

# Load libraries 
library(stringr)
library(plyr)
library(tm)
library(tm.plugin.mail)
library(SnowballC)
library(topicmodels)
library(foreach)
setwd("C:/Users/andry/Desktop/KernixChallenge/maildirsample")

# Load data
email.txts <- list.files('clsdata/')
enron <- Corpus(DirSource("C:/Users/andry/Desktop/KernixChallenge/maildirsample/clsdata"))

extendedstopwords <- c( "a","about","above","across","after","MIME Version","forwarded","again","against","all","almost","alone","along","already","also","although","always","am","among","an","and","another","any","anybody","anyone","anything","anywhere","are","area","areas","aren't","around","as","ask","asked","asking","asks","at","away","b",
                        "back","backed","backing","backs","be","became","because","become","becomes","been","before","began","behind","being","beings","below","best","better","between","big","both","but","by","c","came","can","cannot","can't","case","cases","certain","certainly","clear","clearly","come","could","couldn't","d","did","didn't","differ",
                        "different","differently","do","does","doesn't","doing","done","don't","down","downed","downing","downs","during","e","each","early","either","end","ended","ending","ends","enough","even","evenly","ever","every","everybody","everyone","everything","everywhere","f","face","faces","fact","facts","far","felt","few","find","finds",
                        "first","for","four","from","full","fully","further","furthered","furthering","furthers","g","gave","general","generally","get","gets","give","given","gives","go","going","good","goods","got","great","greater","greatest","group","grouped","grouping","groups","h","had","hadn't","has","hasn't","have","haven't","having","he","he'd",
                        "he'll","her","here","here's","hers","herself","he's","high","higher","highest","him","himself","his","how","however","how's","i","i'd","if","i'll","i'm","important","in","interest","interested","interesting","interests","into","is","isn't","it","its","it's","itself","i've","j","just","k","keep","keeps","kind","knew","know","known",
                        "knows","l","large","largely","last","later","latest","least","less","let","lets","let's","like","likely","long","longer","longest","m","made","make","making","man","many","may","me","member","members","men","might","more","most","mostly","mr","mrs","much","must","mustn't","my","myself","n","necessary","need","needed","needing","needs",
                        "never","new","newer","newest","next","no","nobody","non","noone","nor","not","nothing","now","nowhere","number","numbers","o","of","off","often","old","older","oldest","on","once","one","only","open","opened","opening","opens","or","order","ordered","ordering","orders","other","others","ought","our","ours","ourselves","out","over","own",
                        "p","part","parted","parting","parts","per","perhaps","place","places","point","pointed","pointing","points","possible","present","presented","presenting","presents","problem","problems","put","puts","q","quite","r","rather","really","right","room","rooms","s","said","same","saw","say","says","second","seconds","see","seem","seemed","seeming",
                        "seems","sees","several","shall","shan't","she","she'd","she'll","she's","should","shouldn't","show","showed","showing","shows","side","sides","since","small","smaller","smallest","so","some","somebody","someone","something","somewhere","state","states","still","such","sure","t","take","taken","than","that","that's","the","their","theirs","them",
                        "themselves","then","there","therefore","there's","these","they","they'd","they'll","they're","they've","thing","things","think","thinks","this","those","though","thought","thoughts","three","through","thus","to","today","together","too","took","toward","turn","turned","turning","turns","two","u","under","until","up","upon","us","use","used","uses",
                        "v","very","w","want","wanted","wanting","wants","was","wasn't","way","ways","we","we'd","well","we'll","wells","went","were","we're","weren't","we've","what","what's","when","when's","where","where's","whether","which","while","who","whole","whom","who's","whose","why","why's","will","with","within","without","won't","work","worked","working","works",
                        "would","wouldn't","x","y","year","years","yes","yet","you","you'd","you'll","young","younger","youngest","your","you're","yours","yourself","yourselves","you've","z")

dtm.control <- list( tolower           = T, 
                    removePunctuation = T,
                    removeNumbers     = T,
                    stopwords         = c(stopwords("english"),extendedstopwords),
                    stemming          = T,
                    wordLengths       = c(3,Inf),
                    weighting         = weightTf)

dtm = DocumentTermMatrix(enron, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
dtm = dtm[rowSums(as.matrix(dtm))>0,]

# Make test and train set
idtest <- sample(nrow(dtm), floor(0.30*nrow(dtm)))
TEST   <- dtm[idtest, ]
TRAIN  <- dtm[setdiff(1:nrow(dtm), idtest), ]

# Launch Latent Dirichlet Allocation with mulitple k
control <- list(verbose = 1,initialize = "seeded",nstart = 1,best = TRUE)
VLDA    <- foreach (K=2:5, .combine=list, .multicombine=T)  %dopar% {
  model = LDA(dtm, K, 'VEM', control)
}

# Compute perplexity and log-likelihood
loglik <- 0
perpl  <- 0
for (K in 2:5){
  loglik <- c(loglik,as.numeric(logLik(VLDA[[K-1]])))
  perpl  <- c(perpl,perplexity(VLDA[[K-1]],TEST))
}


# Plot Log-likelihood and perplexity curve
plot(loglik, xlim = c(2,5), ylim = c(min(as.numeric(loglik[2:5])),max(as.numeric(loglik[2:5]))), 
     xlab="Clusnum", ylab = "Loglikelihood", type = "l")

plot(perpl, xlim = c(2,5), ylim = c(min(perpl[2:5]),max(perpl[2:5])), 
     xlab = "Clusnum", ylab = "Perplexity", type = "l")


  
# Examine the most representative words for the optimized and fixed Kk 
kk        <- 2
lda.model <- VLDA[[kk]]
terms(lda.model,30)

emails.topics    <- posterior(lda.model, dtm)$topics
df.emails.topics <- as.data.frame(emails.topics)
df.emails.topics <- cbind(email = as.character(rownames(df.emails.topics)), 
                          df.emails.topics, stringsAsFactors=F)










# ------------------------------------------------
#
#           PART 2 - SENTIMENT SCORING
#
# ------------------------------------------------

library(sentiment)
setwd("C:/Users/andry/Desktop/KernixChallenge/maildirsample/clsdata")

# Read the txt namefile in clsdata
txt.files <- list.files("C:/Users/andry/Desktop/KernixChallenge/maildirsample/clsdata");

# Load all the strings in a list
txt.list  <- lapply(txt.files, readChar, nchars = 5e3)

# classify_emotion function accepts a list of strings
sentiments = classify_emotion(txt.list, algorithm = "bayes", prior = 1.0)



