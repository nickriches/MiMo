
is_past <- function(x){ # identifies past tense in "feats" variable
  result <- as.numeric(grepl("Tense=Past", x))
  return(result)
}

ends_reg_past <- function(x){ # identifies regular past tense ending in "token" variable
  result <- as.numeric(substr(x, nchar(x)-1, nchar(x)) == "ed")
  return(result)
}

is_plural <- function(x){ # identifies plural in "feats" variable
  result <- as.numeric(grepl("Number=Plur", x))
  return(result)
}

ends_plural <- function(x){ # identifies plural ending of "token" variable
  result <- as.numeric(substr(x, nchar(x), nchar(x)) == "s")
  return(result)
}

is_pronoun <- function(x){ # identifies whether "token" corresponds to personal pronoun
  x <- tolower(x)
  return(x %in% c("i", "you", "he", "she", "it", "me", "him", "her", "we", "you", "they", "us", "them"))
}

text$upos[which(is_pronoun(text$token))] <- "PRON" # ensures pronouns correctly labelled

text$morpheme <- text$morpheme + as.numeric(is_past(text$feats) & ends_reg_past(text$token)) # ensures that regular past tense morphemes are counted in MLU

text$morpheme <- text$morpheme + as.numeric(is_plural(text$feats) & text$upos == "NOUN" & ends_plural(text$token)) # ensures regular plural is counted in MLU

# Change "AUX" to "COPULA" for copula

text$upos[which(text$dep_rel == "cop")] <- "COPULA"

text$upos[which(text$xpos == "PRP$")] <- "DET.poss"


