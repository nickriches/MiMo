
extract_class <- functionx(string){
  string <- "<div class = \"tooltiptext\">poo</div>"
  string <- str_extract(string, "<div class = \"mytooltip\">.+</div>")
  string <- substr(string, 26, length(string) - x)
  string
}

<div class = “mytooltip”>poo</div>
<span class = “tooltiptext”></span>


word_counts <- function(sentence){
  sentence <- "she is a she is a big baby"
  words <- as.data.frame(unlist(str_extract_all(sentence, "<div class = \"mytooltip\">.+</div>")))
  names(words) <- "words"
  words %>% group_by(words) %>% summarise(n = n()) %>% arrange(-n) -> words
  words
}