DEFAULT_WORD_COUNT_FILE = "wordlist.csv";
DEFAULT_WORD_GRAM_FILE = "wordgrams.csv";

ngram = function(x, n) {
  if(!(class(x) == "character") & !(class(x) == "numeric")) {
    return(-1);
  }
  if(!(class(n) == "numeric") | n < 1) {
    return(-1);
  }
  if(n > nchar(x)) {
    print('n too big');
    return(-1);
  }
  ngrams <- vector();
  counts <- vector();
  for(i in 1:(nchar(x) - n + 1)) {
    s <- substr(x, i, i + n - 1);
    if(sum(ngrams ==s) == 0) {
      ngrams <- c(ngrams, s);
      counts <- c(counts, 1);
    } else {
      index <- which(ngrams == s);
      counts[index] <- counts[index] + 1;
    }
  }
  ngrams <- ngrams[order(counts, decreasing = T)];
  counts <- counts[order(counts, decreasing = T)];
  total <- sum(counts);
  for(j in 1:length(counts)) {
    print(paste(ngrams[j], counts[j], counts[j] / total * 100, sep=" "))
  }
}

wordCountFile = function(filename, wordCountFile = DEFAULT_WORD_COUNT_FILE) {
  wordVec <- scan(filename, quote='"', what="");
  return(wordCountVector(wordVec, wordCountFile));
}

wordCountRawText = function(text) {
  wordVec <- strsplit(text, ' ')[[1]];
  return(wordCountVector(wordVec));
}

wordCountVector <- function(wordVec, wordCountFile = DEFAULT_WORD_COUNT_FILE) {
  wordVec <- sub("[^[:alnum:]]", "", wordVec);
  wordVec <- tolower(wordVec);
  wordList <- vector();
  counts <- vector();
  for(i in 1:length(wordVec)) {
    if(sum(wordList == wordVec[i]) ==0) {
      wordList <- c(wordList, wordVec[i]);
      counts <- c(counts, 1);
    } else {
      index <- which(wordList == wordVec[i]);
      counts[index] <- counts[index] + 1;
    }
  }
  wordList <- wordList[order(counts, decreasing = T)];
  counts <- counts[order(counts, decreasing = T)];
  percentages <- counts / length(wordVec) * 100;
  output = data.frame(wordList, counts, percentages);
  write.csv(output, wordCountFile);
}

wordGrams <- function(words, n, outputFile = DEFAULT_WORD_GRAM_FILE, toLower = TRUE, removePunctuation = TRUE) {
  print(date());
  if(removePunctuation == TRUE) {
    words = sub("[^[:alnum:]]", "", words);
  }
  if(toLower == TRUE) {
    words = tolower(words);
  }
  wordGrams = vector();
  counts = vector();
  for(i in 1:(length(words) - n + 1)) {
    current = words[i];
    #if 'grams' of more than one, construct the strings of words
    if(n > 1) {
      for(j in (i + 1):(i + n - 1)) {
        current = paste(current, words[j], sep=" ");
      }
    }
    
    #if the n words are in data, increment count
    if(sum(wordGrams == current) > 0) {
      index = which(wordGrams == current);
      counts[index] = counts[index] + 1;
    }
    #otherwise, add it to the bottom with count 1
    else {
      wordGrams = c(wordGrams, current);
      counts = c(counts, 1);
    }
  }
  data = data.frame(wordGrams, counts, counts / length(wordGrams) * 100);
  names(data) = c("words", "count", "percentage");
  write.csv(data[order(data$count, decreasing = TRUE),], outputFile);
  print(date());
}

wordGramsFile = function(filename, n, outputFile = DEFAULT_WORD_GRAM_FILE, toLower = TRUE, removePunctuation = TRUE) {
  wordVector = scan(filename, quote = '"', what = '');
  return(wordGrams(wordVector, n, outputFile, toLower, removePunctuation));
}

#words must be a vector of words
wordGram1 = function(words, n) {
  vecs = vector();
  for(i in 1:(length(words) - n + 1)) {
    vecs = c(vecs, c(i));
  }
  print(vecs);
  wordGrams = data.frame(vecs);
  names(wordGrams) = 1:n;
  print(names(wordGrams));
  counts = vector();
  for(i in 1:(length(words) - 1 + 1)) {
    current = words[i:(i + n - 1)];
    if(sum(wordGrams == current) == 0) {
      print(current);
      wordGrams = rbind(wordGrams, current);
    }
  }
  print(wordGrams);
}

#does whichever one is last
overloaded = function(x) {
  print("character");
}

overloaded = function(x) {
  print('numeric');
}

optional = function(x, y="default") {
  print(x);
  print(y);
}