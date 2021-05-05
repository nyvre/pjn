
# change working directory
library("tm")
library("hunspell")

workDir <- "G:\\Informatyka_zaoczna\\PJN\\projekt"
setwd(workDir)

# functional catalogs localization definition
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"

# 
dir.create(outputDir)

corpusDir <- paste(
  inputDir,
  "Artykuly_original",
  sep = "\\"
)

corpus <- VCorpus(
  DirSource(
    corpusDir,
    encoding = "UTF-8",
    pattern = "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

# initial transformation
pasteParagraphs <- function (text) {
  paste(text, collapse = " ")
}

corpus <- tm_map(corpus, content_transformer(pasteParagraphs))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))

polish <- dictionary("G:\\Informatyka_zaoczna\\PJN\\pl_PL.dic")
lematize <- function (text) {
  text <-corpus[[1]]$content
  tokenizedText <- unlist(hunspell_parse(text, dict = polish))
  lematizedVec <- hunspell_stem(tokenizedText, dict = polish)
  for (t in 1:length(lematizedVec)) {
    if (length(lematizedVec[[t]]) == 0) lematizedVec[[t]] <- tokenizedText[[t]]
    if (length(lematizedVec[[t]]) > 1) lematizedVec[[t]] <- lematizedVec[[t]][1]
  }
  lematizedText <- paste(lematizedVec, collapse = " ")
  return(lematizedText)
}
corpus <- tm_map(corpus, content_transformer(lematize))

stoplistFile <-paste(
  inputDir, 
  "stopwords_pl.txt", 
  sep = "\\"
)

stoplist <- readLines(stoplistFile)
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

removeChar <- content_transformer(
  function (text, char) {
    gsub(char, "", text)
  }
)
corpus <- tm_map(corpus, removeChar, intToUtf8(8722))
corpus <- tm_map(corpus, removeChar, intToUtf8(190))


cutExtension <- function (document) {
  meta(document, "id") <- gsub(
    "\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}

corpus <- tm_map(corpus, cutExtension)



#save corpus

preprocessedDir <- paste(
  inputDir,
  "Literatura - streszczenia - przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir)
writeCorpus(corpus, preprocessedDir)

