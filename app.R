
library(shiny)
library(knitr) # To prepare Rmarkdown instructions
library(tidyverse) # For data manipulation
library(readtext) # Read in .doc and .docx files
library(udpipe) # Part-of-speech-tagger
library(tools) # To get file extension
library(DT) # To create a datatable
library(textcat) # To detect the language of a text
library(eeptools) # Calculate ages
library(childesr) # Look up CHILDES norms
library(zoo) # For the time series analysis.
library(koRpus) # To obtain lexical diversity measures
library(koRpus.lang.en) # Corpus English Language
library(colourpicker) 



shinyApp(
  
  ui <- fluidPage(#theme = "flatly.css",
    # Instructions page ----
    navbarPage("MiMo",
               tabPanel("Instructions",
                        uiOutput('Rmarkdown_instructions')
               ),
               # Let's get started navbar ----
               navbarMenu("Let's get started!",
                          #(1) Enter text tab panel ----
                          tabPanel("(1) Enter text",
                                   radioButtons("radio", label = h3("How do you wish to enter your data?"),
                                                choices = list("Upload file (.doc, .docx, or .txt)" = 1, "Enter text in textbox" = 2), 
                                                width = '100%', selected = 1),
                                   conditionalPanel(condition = "input.radio == 1",
                                                    fileInput("text_file", "Select file",
                                                              multiple = FALSE,
                                                              accept = c("text/plain",
                                                                         "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                                                                         "application/msword")
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.radio == 2",
                                                    textAreaInput("text_file_TA", "Enter text here...",
                                                                  placeholder = "Enter text here...",
                                                                  width = "100%", height = "100%", resize = "both")
                                                    # verbatimTextOutput("value")
                                   )
                          ),
                          #(2) Check language tab panel ----
                          tabPanel("(2) Check language",
                                   htmlOutput("text_example"),
                                   radioButtons("proceed", label = h3("How do you wish to proceed?"),
                                                choices = list("Continue" = 1, "Select another language" = 2), 
                                                width = '100%', selected = 1),
                                   conditionalPanel(condition = "input.proceed == 2",
                                                    textAreaInput("manual_language", "Enter a language...",
                                                                  placeholder = "Enter language name here...",
                                                                  width = "1000px", resize = "both"),
                                                    textAreaInput("manual_url", "(OPTIONAL) Enter a model repo...",
                                                                  placeholder = "Enter location of repo here...",
                                                                  width = "1000px", resize = "both")
                                   )
                          )
               ),
               # Let's explore nav bar ----
               navbarMenu("Let's explore!",
                          #(1) coloured output tab panel ----
                          tabPanel("(1) Coloured output",
                                   
                                   tags$head(
                                     tags$style(HTML({"
                                       .mytooltip {
                                       position: relative;
                                       display: inline-block;
                                       }
                                       
                                       .mytooltip .tooltiptext {
                                       visibility: hidden;
                                       width: 120px;
                                       background-color: #4d0026;
                                       color: #fff;
                                       text-align: center;
                                       border: 6px solid #ff80ff;
                                       padding: 5px 0;

                                       
                                       /* Position the tooltip */
                                       position: absolute;
                                       z-index: 1;
                                       bottom: 100%;
                                       left: 50%;
                                       margin-left: -60px;
                                       }
                                       
                                       .mytooltip:hover .tooltiptext {
                                       visibility: visible;
                                       }

                                       "}))
                                   ),
                                   
                                   h3("Table will take a few seconds to appear/refresh..."),
                                   DT::dataTableOutput("table_coloured")
                          ),
                          
                          
                          
                          #(2) Syntactic measures tab panel-----
                          tabPanel("(2) Syntactic measures",
                                     h3("Table will take a few seconds to appear/refresh..."),
                                     DT::dataTableOutput("table_summaries"),
                                     br()
                                  ),  # end of tab panel ----

                          
                          
                          
                          
                          #(3) Lexical measures tab panel-----
                          tabPanel("(3) Lexical measures",
                                     h3("Table will take a few seconds to appear/refresh..."),
                                     DT::dataTableOutput("table_summaries2")
                          ), # end of tab panel
                          
                          
                          #(4) Tags -----
                          tabPanel("(4) Tags",
                                   
                                   mainPanel(
                                     DT::dataTableOutput("tag_table")
                                   )
                                   
                          ) # end of tags tabpanel
                          
                          
                          
               ), # end of nav bar menu
               
               # Cheat sheet tab panel----
               tabPanel("Cheat sheet",
                        uiOutput('cheatsheet')
               ),
               
               # Colour tab panel----
               tabPanel("Colours",
                        
                        h3("Widgets contain hexadecimal colour codes.
                                   Colours may be conveniently copied and pasted by copying and pasting these codes."),
                        
                        br(),
                        
                        h3("Verb Complex / Verb Phrase"), 
                        
                        colourInput(
                          inputId = "VERB_colour",
                          label = "Main Verb (label = VERB)",
                          value = "#FFAB94"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "COPULA_colour",
                          label = "Copula (label = COP.)",
                          value = "#FFAB94"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "AUXILIARY_colour",
                          label = "Auxiliary Verb (label = AUX.)",
                          value = "#FAD4CB"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "PARTICLE_colour",
                          label = "Particle e.g. \"to\" in \"to go\" (label = PART.)",
                          value = "#FAD4CB"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "ADV_colour",
                          label = "Adverb (label = ADV.)",
                          value = "#FAD4CB"
                          # showColour = "background"
                        ),
                        
                        hr(),
                        
                        h3("Noun Phrase"), 
                        
                        colourInput(
                          inputId = "NOUN_colour",
                          label = "Noun (label = NOUN)",
                          value = "#B6B6F5"
                          # showColour = "background"
                        ),
                        
                        
                        colourInput(
                          inputId = "DET_colour",
                          label = "Determiner (label = DET., or DET.poss if possessive)",
                          value = "#ADFFFF"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "ADJ_colour",
                          label = "Adjective (label = ADJ.)",
                          value = "#ADFFFF"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "PRON_colour",
                          label = "Pronoun (label = PRON.)",
                          value = "#99FF69"
                          # showColour = "background"
                        ),
                        
                        hr(),
                        
                        h3("Prepositions"),
                        
                        colourInput(
                          inputId = "PREP_colour",
                          label = "Prepositions (label = PREP.)",
                          value = "#FFFF52"
                          # showColour = "background"
                        ),
                        
                        hr(),
                        
                        h3("Linking Words"),
                        
                        colourInput(
                          inputId = "SUB_colour",
                          label = "Subordinating Conjunction (label = SCONJ.)",
                          value = "#FCAD46"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "COORD_colour",
                          label = "Coordinating Conjunction (label = CCONJ.)",
                          value = "#FFCD7D"
                          # showColour = "background"
                        ),
                        
                        hr(),
                        
                        h3("Others"),
                        
                        colourInput(
                          inputId = "PUNCT_colour",
                          label = "Punctuation Character (label = PUNCT.)",
                          value = "#eeeedd"
                          # showColour = "background"
                        ),
                        
                        colourInput(
                          inputId = "INTERJECTION_colour",
                          label = "Interjection (label = INTJ.)",
                          value = "#C29A72"
                          # showColour = "background"
                        )
                        
                        
                        
               ),
               
               
               # Extra... nav bar ----
               navbarMenu("Extra...",
                          # Coloured output CnP ----
                          tabPanel("(1) Coloured output CnP",
                                   h3("This table can be copied and pasted into a Word Processor Document, e.g. Microsoft Word"),
                                   
                                   h3("Table will take a few seconds to appear/refresh..."),
                                   DT::dataTableOutput("table_coloured_reduced")
                          ), # end of Tab Panel
                          
                          tabPanel("(2) Plot data", # Plot data ----
                                   
                                   sidebarPanel(
                                     selectInput(inputId = "collection3",
                                                 label = "Collection",
                                                 choices = c("PLEASE CHOOSE...",
                                                             "Eng-NA", "Eng-UK", "KIDEVAL",
                                                             "Spanish", "French", "German", "Japanese", "EastAsian",
                                                             "Clinical-MOR", "Biling"),
                                                 selected = NULL
                                     ),
                                     
                                     selectInput(inputId = "variable3",
                                                 label = "Variable",
                                                 choices =  c(
                                                   "MLU in morphemes" = "mlu_m",
                                                   "MLU in words" = "mlu_w",
                                                   "HDD" = "hdd",
                                                   "TTR" = "ttr"),
                                                 selected = "mlu_m"
                                     ),
                                     
                                     numericInput(inputId = "num_utts3",
                                                  label = "Min. utterances",
                                                  value = 100
                                     ),
                                     
                                     numericInput(inputId = "bin_width3",
                                                  label = "Bin width",
                                                  value = 10
                                     ),
                                     
                                     sliderInput(inputId = "shading3",
                                                 label = "Shading",
                                                 min = 0,
                                                 max = 0.3,
                                                 value = 0.1
                                     ),
                                     
                                     sliderInput(inputId = "trim_data3",
                                                 label = "Trim data?",
                                                 min = 0,
                                                 max = 100,
                                                 value = c(1,100)
                                     ),
                                     
                                     hr(),
                                     
                                     h3("Show speaker"),
                                     textInput(inputId = "child_name",
                                               label = "Name of Child",
                                               placeholder = "Child Name"),
                                     # Copy the line below to make a date selector
                                     textInput(inputId = "value",
                                               label = "Value for MLU/HDD etc",
                                               placeholder = "Value"),
                                     dateInput("dob3", label = h4("Date of Birth")),
                                     dateInput("dot3", label = h4("Date of Test")),
                                     htmlOutput("age")
                                   ), # sidebarpanel
                                   
                                   mainPanel(
                                     h3("Table will take a few seconds to appear/refresh..."),
                                     plotOutput("DIY_plot",
                                                dblclick = "plot_dblclick3",
                                                brush = brushOpts(
                                                  id = "plot_brush3",
                                                  resetOnNew = TRUE
                                                ))
                                   )
                                   
                                   
                                   
                          ) # end of tabPanel
                          
               ) # end of navbar menu
               
    ) # end of nav bar page
  ),
  
  
  # server statement----
  server <- function(input, output, session){
    
    
    # ***REACTIVE STATEMENTS*** ----
    # text (read in text file) ----
    
    text <- reactive({
      
      
      if(is.null(input$text_file) & input$text_file_TA=="") return(NULL)
      
      # browser()
      
      if(is.null(input$text_file)==FALSE){
        text <- readtext(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      # 
      
      return(text)
      
    })
    
    
    
    
    # lang (obtaining language) ----
    
    lang <- reactive({
      
      if(is.null(input$text_file) & input$text_file_TA=="") return(NULL)
      
      if(is.null(input$text_file)==FALSE){
        text <- readtext(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      
      
      lang <- textcat(text)
      
      return(lang)
      
    })
    
    
    
    # table (showing transcript)----
    
    table <- reactive({
      
      if(is.null(input$text_file) & input$text_file_TA=="") return(NULL)
      
      if(is.null(input$text_file)==FALSE){
        text <- readtext(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      
      
      
      if(input$manual_language==""){
        lang <- textcat(text)} else{
          lang <- input$manual_language
        }
      
      lang <- tolower(lang)
      
      
      
      str_split_keep_delimiter <- function(string, delV){ #string and delimiter vector
        for(i in 1:length(delV)){
          search_string <- paste0("(", delV[i], ")")
          replace_string <- "\\1***"
          string <- str_replace_all(string, search_string, replace_string)
        }
        string <- str_split(string, "[\x2a][\x2a][\x2a]")
        string <- unlist(string)
        string <- string[which(string != "")] # removes any blanks which may have been created
        return(string)
      }
      
      # Convert text object to vector
      
      alphanumeric <- function(x){
        return(grepl("[a-zA-Z0-9]",x))
      }
      
      
      text <- str_split_keep_delimiter(text, c("[\x2e]+[\x22|\x27]*[\x20]*([\\[|\x28][^\\[|\x28]*[\\]|\x29][\x20]*)*",
                                               "[\x21]+[\x22|\x27]*[\x20]*([\\[|\x28][^\\[|\x28]*[\\]|\x29][\x20]*)*",
                                               "[\x3f]+[\x22|\x27]*[\x20]*([\\[|\x28][^\\[|\x28]*[\\]|\x29][\x20]*)*")
      )
      
      
      
      text <- as.data.frame(text)
      
      text <- text %>% filter(alphanumeric(text) == TRUE) # Gets rid of blank lines / lines with only punctuation
      text$text <- str_trim(text$text) # Trims lead/trailing spaces
      
      extract_speaker <- function(x){ 
        x <- str_trim(x) # trim leading and trailing spaces
        x <- strsplit(x, " ")[[1]][1] # split by space and identify first word
        x <- stringr::str_match(x, "[a-zA-Z]+[\x3a]$") # identify whether first word could be speaker
        return(x)
      }
      
      remove_speaker <- function(x){ 
        x <- gsub("[a-zA-Z]+[\x3a]", "", x) # replace speaker with ""
        return(x)
      }
      
      text$speaker <- sapply(text$text, extract_speaker)
      
      text %>%
        fill(speaker, .direction = "down") -> text
      
      text$speaker[which(is.na(text$speaker))] <- "xxx:"
      
      speakers <- text$speaker
      
      text$text <- sapply(text$text, remove_speaker)
      
      #Remove standalone punctation
      
      text$text <- gsub("[\x20][:punct:][\x20]", "", text$text) # needs to be changed
      
      # Calculate Num Words - using spaces to delimit words
      
      count_words_using_spaces <- function(x){
        return(str_count(x, "[^\x20]+"))
      }
      
      remove_non_alphanumeric <- function(x){
        return(gsub("[^\x20a-zA-Z0-9]", "", x))
      }
      
      extract_comments_as_vector <- function(x){
        result <- unlist(str_extract_all(x, "[\x28][^\x28|\x29]*[\x29]"))
        result <- paste0("", result)
        result <- as.vector(result)
        return(result)
      }
      
      extract_tags_as_vector <- function(x){
        result <- unlist(str_extract_all(x, "\\[[^\\[]*\\]"))
        result <- paste0("", result)
        result <- as.vector(result)
        return(result)
      }
      
      extract_comments_as_string <- function(x){
        result <- unlist(str_extract_all(x, "[\x28][^\x28|\x29]*[\x29]"))
        result <- paste0("", result, collapse = "")
        return(result)
      }
      
      extract_tags_as_string <- function(x){
        result <- unlist(str_extract_all(x, "\\[[^\\[]*\\]"))
        result <- paste0("", result, collapse = "")
        return(result)
      }
      
      replace_comments <- function(x){
        result <- str_replace_all(x, "[\x28][^\x28|\x29]*[\x29]", "\x28")
        return(result)
      }
      
      replace_tags <- function(x){
        result <- str_replace_all(x, "\\[[^\\[]*\\]", "\\[")
        return(result)
      }
      
      insert_comments <- function(x, y){ ## Doesn't work
        y <- str_replace_all(y, "[\x28]", "OPENBRACKETHERE")
        y <- str_replace_all(y, "[\x29]", "CLOSEDBRACKETHERE")
        for(i in 1:length(y)){
          x <- str_replace(x, "[\x28]", y[i])
        }
        x <- str_replace_all(x, "OPENBRACKETHERE","\x28")
        x <- str_replace_all(x, "CLOSEDBRACKETHERE", "\x29")
        return(x)
      }
      
      insert_tags <- function(x, y){ ## x = string, y = vector
        y <- str_replace_all(y, "\\[", "OPENBRACKETHERE")
        y <- str_replace_all(y, "\\]", "CLOSEDBRACKETHERE")
        for(i in 1:length(y)){
          x <- str_replace(x, "\\[", y[i])
        }
        x <- str_replace_all(x, "OPENBRACKETHERE","\\[")
        x <- str_replace_all(x, "CLOSEDBRACKETHERE", "\\]")
        return(x)
      }
      
      
      
      Num_Words <- count_words_using_spaces(remove_non_alphanumeric(text$text))
      
      text$comments <- sapply(text$text, extract_comments_as_string)
      
      text$tags <- sapply(text$text, extract_tags_as_string)
      
      comments <- text$comments
      tags <- text$tags
      
      text$text <- replace_comments(text$text)
      text$text <- replace_tags(text$text)
      
      text_comments_extracted <- gsub("\\[", "", text$text)
      text_comments_extracted <- gsub("\\(", "", text_comments_extracted)
      
      has_period <- function(x){
        return(grepl("[\x2e]", x))
      }
      
      has_question_mark <- function(x){
        return(grepl("[\x3f]", x))
      }
      
      has_single_exclamation_mark <- function(x){
        return(str_count(x, "[\x21]") == 1)
      }
      
      has_multiple_exclamation_marks <- function(x){
        return(str_count(x, "[\x21]") >= 2)
      }
      
      text$mood <- ""
      
      text$mood[has_period(text$text)] <- "isdeclarative"
      text$mood[has_question_mark(text$text)] <- "isinterrogative isquestion"
      text$mood[has_single_exclamation_mark(text$text)] <- "isimperative"
      text$mood[has_multiple_exclamation_marks(text$text)] <- "isexclamative"
      
      mood <- text$mood
      
      # Download language model and parse text
      
      if(input$manual_url==""){
        model <- udpipe_download_model(lang, model_dir = tempdir()) # NB can add "model_dir = tempdir()"
      }
      
      if(input$manual_url!=""){
        model <- udpipe_download_model(lang, model_dir = tempdir(), udpipe_model_repo = input$manual_url)
      }
      
      
      model <- udpipe_load_model(model$file_model)
      
      # browser(); one <- 1; one <- 1; one <- 1; one <- 1; one< -1
      
      text <- udpipe_annotate(model, text$text)
      
      text <- as.data.frame(text)
      
      text$morpheme <- 1
      text$morpheme[which(text$upos == "PUNCT")] <- 0 # So we don't count punctuation as a morpheme
      
      if(grepl("english", lang, ignore.case=TRUE)){source("english_labelling_rules.R", local = TRUE)}
      
      
      text$upos[which(text$dep_rel == "cop")] <- "COPULA"
      
      # Identify number of clauses
      
      # browser(); one <- 1; one <- 1; one <- 1; one <- 1; one <- 1; one <- 1; one <-1
      
      text$num_clause <- as.numeric(text$upos == "VERB" | text$upos == "COPULA")
      
      text$num_fin_clause <- as.numeric(grepl("VerbForm=Fin", text$feats))
      
      verb_form <- function(x){
        return(
          case_when(
            grepl("Tense=Past[\x7c]VerbForm=Fin",x) == TRUE ~ "hasPastTense",
            grepl("Tense=Pres[\x7c]VerbForm=Fin", x) == TRUE ~ "hasPresTense hasPresentTense",
            grepl("Tense=Past[\x7c]VerbForm=Part", x) == TRUE ~ "hasPastParticiple",
            grepl("Tense=Pres[\x7c]VerbForm=Part", x) == TRUE ~ "hasPresentParticiple hasPresParticiple",
            grepl("VerbForm=Inf", x) == TRUE ~ "hasInfinitive",
            TRUE ~ ""
          )
        )
      }
      
      
      text$verb_form <- sapply(text$feats, verb_form)
      
      rel_clause <- function(x){
        return(
          case_when(
            grepl("relcl", x) == TRUE ~ "hasRelativeClause",
            TRUE ~ ""
          )
        )
      }
      
      text$rel_clause <- sapply(text$dep_rel, rel_clause)
      
      
      highlight <- function(text, colour){ # highlights text in a particular colour
        result <- paste0("<span style=\"background-color:", colour, ";\">",
                         "&thinsp;", text, "&thinsp;",
                         "</span>")
        return(result)
      }
      
      text$features_coloured <- paste0(highlight(text$dep_rel, "#cc99ff"), # Violet 
                                       highlight(text$token, "#ff6666"), # Orange
                                       highlight(text$upos, "#ffc299"), # Green
                                       highlight(text$xpos, "#9999ff"), # Dark blue
                                       highlight(text$feats, "#c68c53"), # Brown
                                       " "
      ) 
      
      add_tool_tip <- function(text, label){
        result <- paste0("<div class=\"mytooltip\">",
                         text,
                         "<span class=\"tooltiptext\">",
                         label,
                         "</span>",
                         "</div>")
        return(result)
      }
      
      
      highlight_wc <- function(string, wc){
        # red
        if(wc == "VERB"){result <- add_tool_tip(highlight(paste0("<b>",string,"</b>"), input$VERB_colour), "VERB")}
        else if(wc == "COPULA"){result <- add_tool_tip(highlight(paste0("<b>", string, "</b>"), input$COPULA_colour), "COPULA")}
        # orange
        else if(wc == "SCONJ"){result <- add_tool_tip(highlight(string, input$SUB_colour), "SCONJ.")}
        # light orange
        else if(wc == "CCONJ"){result <- add_tool_tip(highlight(string, input$COORD_colour), "CCONJ.")}
        # green
        else if(wc == "PRON"){result <- add_tool_tip(highlight(string, input$PRON_colour), "PRON.")}
        # pink
        else if(wc == "AUX"){result <- add_tool_tip(highlight(string, input$AUXILIARY_colour), "AUX.")}
        else if(wc == "ADV"){result <- add_tool_tip(highlight(string, input$ADV_colour), "ADV.")}
        else if(wc == "PART"){result <- add_tool_tip(highlight(string, input$PARTICLE_colour), "PARTICLE")}
        # dark blue
        else if(wc == "NOUN"){result <- add_tool_tip(highlight(string, input$NOUN_colour), "NOUN")}
        else if(wc == "PROPN"){result <- add_tool_tip(highlight(string, input$NOUN_colour), "PROPN")}
        # cyan
        else if(wc == "DET"){result <- add_tool_tip(highlight(string, input$DET_colour), "DET.")}
        else if(wc == "DET.poss"){result <- add_tool_tip(highlight(string, input$DET_colour), "DET.poss")}
        else if(wc == "ADJ"){result <- add_tool_tip(highlight(string, input$ADJ_colour), "ADJ.")}
        else if(wc == "NUM"){result <- add_tool_tip(highlight(string, input$DET_colour), "NUM.")}
        # brown
        else if(wc == "INTJ"){result <- add_tool_tip(highlight(string, input$INTERJECTION_colour), "INTJ")}
        # yellow
        else if(wc == "ADP"){result <- add_tool_tip(highlight(string, input$PREP_colour), "PREP.")}
        # grey
        else if(wc == "PUNCT"){result <- add_tool_tip(highlight(string, input$PUNCT_colour), "PUNCT.")}
        else if(wc == "X"){result <- add_tool_tip(highlight(string, "#b8b894"), "X")}
        else if(wc == "SYM"){result <- add_tool_tip(highlight(string, "#b8b894"), "SYM")}
        else{result <- string}
        return(result)
      }
      
      text %>% filter(!is.na(upos)) -> text
      
      text$coloured <- mapply(highlight_wc, text$token, text$upos) # Applies html formatting to tokens
      
      # Create placemarkers used for inserting comments and tags back into text
      text$coloured[which(text$token == "(")] <- "("
      text$coloured[which(text$token == "[")] <- "["
      
      # create variable showing line of text
      text$line <- NULL
      get_doc_number <- function(x){
        return(as.numeric(substr(x, 4, nchar(x))))
      }
      text$line <- sapply(text$doc, get_doc_number)
      
      # create variable which will allow user to search for word by class
      
      text$upos[which(text$upos == "ADP")] <- "PREP"
      text$hasclass <- paste0("has", tolower(text$upos))
      
      text$neg <- ""
      text$neg[which(grepl("not", text$token))] <- "hasneg"
      text$neg[which(grepl("n't", text$token))] <- "hasneg"
      
      # Reshape data
      text %>%
        group_by(line) %>%
        summarise(sentence_coloured = paste(coloured, collapse = " "),
                  sentence = paste(token, collapse = " "),
                  features_coloured = paste(features_coloured, collapse = " "),
                  features = paste(feats, collapse = " "),
                  has_class = paste(hasclass, collapse = " "),
                  pos_tags = paste(upos, collapse = " "),
                  neg = paste(neg, collapse = " "),
                  `Num Morphs` = sum(morpheme),
                  num_clause = sum(num_clause),
                  num_fin_clause = sum(num_fin_clause),
                  verb_form = paste(verb_form, collapse = " "),
                  rel_clause = paste(rel_clause, collapse = " ")
        ) -> text
      
      text$NPexpansion <- ""
      
      text$NPexpansion[grepl("((DET|DET\x2eposs|ADJ|NUM)\x20)+(NOUN|PROPN)", text$pos_tags)] <- "hasNPexpansion"
      
      text$VCexpansion <- ""
      
      text$VCexpansion[grepl("((AUX|ADV|VERB|PART)\x20)+(VERB)", text$pos_tags)] <- "hasVCexpansion hasVPexpansion"
      
      text$clause2 <- ""
      text$multipleclauses <- ""
      text$clause3 <- ""
      text$clause4 <- ""
      text$clause5 <- ""
      
      text$clause2[which(text$num_fin_clause == 2)] <- "has2clauses"
      text$multipleclauses[which(text$num_fin_clause >= 2)] <- "hasmultipleclauses, iscomplex"
      text$clause3[which(text$num_fin_clause == 3)] <- "has3clauses"
      text$clause4[which(text$num_fin_clause == 4)] <- "has4clauses"
      text$clause5[which(text$num_fin_clause == 5)] <- "has5clauses"
      
      
      
      # This section has functions to colour comments and tags. The comments and tags are then
      # inserted back into the sentence_coloured variable, and coloured accordingly
      
      colour_comments <- function(x){
        result <- str_replace_all(x, "[\x28]", "<span style=\"color:#333399;\">(")
        result <- str_replace_all(result, "[\x29]", ")</span>")
        return(result)
      }
      
      colour_tags <- function(x){
        result <- str_replace_all(x, "\\[", "<span style=\"color:#992600;\">\\[")
        result <- str_replace_all(result, "\\]", "\\]</span>")
        return(result)
      }
      
      for(i in 1:nrow(text)){
        
        text$sentence_coloured[i] <- colour_comments(insert_comments(text$sentence_coloured[i],
                                                                     extract_comments_as_vector(comments[i])))
        
        text$sentence_coloured[i] <- colour_tags(insert_tags(text$sentence_coloured[i],
                                                             extract_tags_as_vector(tags[i])))
      }
      
      text$speaker <- speakers
      text$speaker_no_html <- speakers
      
      unique_speakers <- unique(speakers)
      unique_speakers <- unique_speakers[which(unique_speakers != "")]
      
      speaker_colours <- c("#f2ffe6", "#ffddcc", "#e6f7ff", "#ffe6ff",
                           "#ffffcc", "#ffe6ff", "#ccddff", "#ffcce0",
                           "#ccccff", "#ff0000", "#81a375", "	#ccffcc")
      
      speaker_colours <- c(speaker_colours, rep("#ffffff", 100)) # just in case there are lots of speakers!
      
      for(i in 1:nrow(text)){
        text$speaker[i] <- highlight(text$speaker[i],
                                     speaker_colours[which(unique_speakers == text$speaker[i])])
        
      }
      
      text$mood <- mood
      
      text$passive <- ""
      text$passive[which(grepl("Voice=Pass", text$features))] <- "haspassive"
      
      text$relativepronoun <- ""
      text$relativepronoun[which(grepl("PronType=Rel", text$features))] <- "hasrelativepronoun"
      
      text$modal <- ""
      text$modal[which(grepl("MD", text$features))] <- "hasmodalverb"
      
      text$speaker[which(text$`Num Morphs`==0)] <- ""
      text$speaker_no_html[which(text$`Num Morphs`==0)] <- ""
      
      text$`Num Words` <- Num_Words
      
      text$text_comments_extracted <- text_comments_extracted
      
      # Create variable to allow user to identify lines with comments or tags
      hascomment <- rep("", length(comments))
      hascomment[which(comments!="")] <- "hascomment"
      text$hascomment <- hascomment
      
      hastag <- rep("", length(tags))
      hastag[which(tags!="")] <- "hastag"
      text$hastag <- hastag
      
      # Create a column to allow user to search for specific tags
      tags_plus_content <- str_replace_all(tags, "\x20", "") #remove gaps
      tags_plus_content <- str_replace_all(tags_plus_content, "\\[", "hastag") #start with "hastag"
      tags_plus_content <- str_trim(str_replace_all(tags_plus_content, "\\]", "\x20")) #remove final brackets
      text$tags_plus_content <- tags_plus_content # create variable
      text$tags <- tags
      
      # Code produces position_within_turn and turn_length variables.
      
      text$new_turn <- 0
      speaker <- ""
      
      for(i in 1:nrow(text)){
        if(text$speaker[i] != "" & text$speaker[i] != speaker){text$new_turn[i] <- 1}
        if(text$speaker[i] != "" & text$speaker[i] != speaker){speaker <- text$speaker[i]}
      }
      
      text$position_within_turn <- NA
      position_within_turn <- 1
      
      for(i in 1:nrow(text)){
        if(text$new_turn[i] == 1){position_within_turn <- 0}
        if(text$speaker[i] != ""){position_within_turn <- position_within_turn + 1}
        text$position_within_turn[i] <- position_within_turn
      }
      
      text$position_within_turn[which(text$speaker == "")] <- 0
      
      text$turn_length <- NA
      top <- 0
      
      for(i in nrow(text):1){
        
        top <- max(top, text$position_within_turn[i])
        text$turn_length[i] <- top
        if(text$position_within_turn[i] == 1){top <- 0}
      }
      
      text$turn_length_string <- paste0("turn", text$turn_length)
      text$turn_length_string[which(text$turn_length == 0)] <- ""
      text$turn_length_string[which(text$turn_length == 5)] <- "turn5, turn5plus"
      text$turn_length_string[which(text$turn_length >= 6)] <- "turn5plus"
      
      text$turn_length_first_turn <- NA
      
      text$turn_length_first_turn[which(text$new_turn == 1)] <- text$turn_length[which(text$new_turn == 1)]
      
      text %>% select(line, speaker, sentence_coloured,
                      `Num Morphs`, `Num Words`,
                      num_clause, num_fin_clause, turn_length,
                      hascomment, hastag, tags_plus_content, tags,
                      mood, neg, verb_form,
                      NPexpansion, VCexpansion,
                      has_class,
                      clause2, clause3, clause4, clause4, multipleclauses,
                      passive, relativepronoun, rel_clause, modal, text_comments_extracted,
                      new_turn, position_within_turn, turn_length, turn_length_string, turn_length_first_turn,
                      features_coloured, speaker_no_html) -> text
      
      
      
      tag_list <- paste0(text$tags, collapse = "")
      tag_list <- unique(extract_tags_as_vector(tag_list))
      
      for(i in 1:length(tag_list)){
        
        text[[tag_list[i]]] <- str_count(text$sentence_coloured,
                                         str_replace_all(str_replace_all(tag_list[i],"\\[", "\\\\["), "\\]", "\\\\]"))
        
      }
      
      return(text)
    })
    
    
    
    
    # table_lex (HDD etc) ----
    table_lex <- reactive({
      
      df <- table()
      df %>% group_by(speaker, speaker_no_html) %>% filter(speaker != "") %>%
        summarise(lex = paste(text_comments_extracted, collapse = " ")) -> df
      
      df$hdd <- as.numeric(NA)
      df$ttr <- as.numeric(NA)
      
      for(i in 1:nrow(df)){
        corpus <- koRpus::tokenize(df$lex[i], lang = "en", format = "obj")
        hdd <- HDD(corpus)
        ttr <- TTR(corpus)
        df$types[i] <- length(types(corpus))
        df$tokens[i] <- length(tokens(corpus))
        df$hdd[i] <- as.numeric(koRpus::summary(hdd)[2])
        df$ttr[i] <- as.numeric(koRpus::summary(ttr)[2])
      }
      
      return(df)
    })
    

    
    # df_childes_DIY ----
    
    df_childes_DIY <- reactive({
      
      if(input$collection3 == "PLEASE CHOOSE...")return(NULL)
      
      if(input$collection3 == "KIDEVAL"){
        
        kideval_corpora_id <- c(65, #Bates
                                60, #Bernstein
                                71, #Bliss
                                76, #Bloom70
                                41, #Bloom73
                                73, #Braunwald
                                36, #Brown
                                29, #Clark
                                39, #Demetras1
                                46, #Demetras2
                                50, #Feldman
                                43, #Gathercole
                                64, #Gleason
                                57, #Hall
                                31, #Higginson
                                48, #HSLLD
                                54, #MacWhinney
                                47, #McCune
                                30, #NewEngland
                                63, #Post
                                49, #Providence
                                55, #Sachs
                                61, #Snow
                                67, #Supes
                                66, #Tardif
                                62, #Valian
                                52, #VanHouten
                                32, #VanKleeck
                                56, #Warren
                                69) #Weist
        
        # df <- get_speaker_statistics(role = "Target_Child")
        
        df <- read.csv("speaker_statistics.csv")
        
        df <- df[which(df$corpus_id %in% kideval_corpora_id), ]
        
      }
      
      if(input$collection3 != "KIDEVAL"){
        
        # df <- get_speaker_statistics(collection = input$collection3, role = "Target_Child")
        
        df <- read.csv("speaker_statistics.csv")
        
        df %>% filter(collection_name == input$collection3) -> df
        
      }
      
      df <- as.data.frame(df)
      
      
      
      df %>% filter(num_utterances >= input$num_utts3) -> df
      
      df %>% arrange(target_child_age) %>% filter(is.na(target_child_age) == FALSE) -> df
      
      age_range <- max(df$target_child_age) - min(df$target_child_age)
      
      upper_age_bound <- min(df$target_child_age) + age_range*(input$trim_data3[2]/100)
      lower_age_bound <- min(df$target_child_age) + age_range*(input$trim_data3[1]/100)
      
      df %>%
        filter(target_child_age >= lower_age_bound) %>%
        filter(target_child_age <= upper_age_bound) ->
        df
      
      
      if(input$variable3 == "mlu_m"){
        df$dv <- df$mlu_m
      }
      
      if(input$variable3 == "mlu_w"){
        df$dv <- df$mlu_w
      }
      
      if(input$variable3 == "hdd"){
        df$hdd <- df$hdd*42 # to obtain ACTUAL HDD
        df$dv <- df$hdd
      }
      
      if(input$variable3 == "ttr"){
        df$ttr <- df$num_types/df$num_tokens
        df$dv <- df$ttr
      }
      
      df %>% filter(dv != 0) -> df
      
      df %>% filter(is.na(dv) == FALSE) -> df
      df$mean <- rollapply(df$dv, mean, width = input$bin_width3, partial = TRUE)
      df$sd <- rollapply(df$dv, sd, width = input$bin_width3, partial = TRUE)
      df$plus_one <- df$mean + df$sd
      df$plus_one_point_five <- df$mean + 1.5*df$sd
      df$minus_one <- df$mean - df$sd
      df$minus_one_point_five <- df$mean - 1.5*df$sd
      
      return(df) 
      
    })
    
    
    # xmax_hdd (obtain highest value on x axis)----
    xmax_DIY <- reactive({
      return(max(df_childes_DIY()$target_child_age, na.rm = TRUE))
    })
    

    # Speaker_age----
    speaker_age <- reactive({
      age <- age_calc(input$dob3, input$dot3, units = "months", precise = TRUE)
      return(age)
    })
    
    
    
    # ***RENDERING STATEMENTS*** ----
    # Rmarkdown_instructions ----
    
    output$Rmarkdown_instructions <- renderUI({
      # HTML(rmarkdown::render('Rmarkdown_instructions.Rmd'))
      HTML(markdown::markdownToHTML(knit('Rmarkdown_instructions.Rmd', quiet = TRUE)))
      # includeHTML("Rmarkdown_instructions.html")
    })
    
    # (2) Check language tab panel ----
    output$text_example <- renderUI({
      text <- substr(text(), 1, 1000)
      HTML(paste0("<p><h1>Text</h1><h3>(up to 1000th character)</h3>", text,"</p> <p><h2>Language detected: ",
                  "<strong><span style=\"background-color:#00ffff;\">",
                  lang(),
                  "</style></h2></p>"))
    })
    
    # table_coloured ----
    output$table_coloured = DT::renderDataTable({
      datatable(table(),
                filter = c("top"),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = TRUE,
                               search = list(regex = TRUE, scrollX = TRUE)
                              )
      ) %>% formatStyle(columns = c(2), width='100px') %>% 
        formatStyle("features_coloured","white-space"="nowrap") %>%
        formatStyle("sentence_coloured","white-space"="nowrap") %>%
        formatStyle("verb_form", "white-space"="nowrap") %>%
        formatStyle("has_class", "white-space"="nowrap") %>%
        formatStyle("neg", "white-space"="nowrap") %>%
        formatStyle("text_comments_extracted", "white-space"="nowrap") %>%
        formatStyle("rel_clause", "white-space" = "nowrap") %>%
        formatStyle("tags", "white-space"="nowrap") %>%
        formatStyle("mood", "white-space"="nowrap") %>%
        formatStyle("VCexpansion", "white-space"="nowrap") %>%
        formatStyle("multipleclauses", "white-space"="nowrap") %>%
        formatStyle(8, `border-right` = "solid 2px")
    })
    
    
    
    
    # table_coloured_reduced ----
    output$table_coloured_reduced = DT::renderDataTable({
      datatable(table() %>%
                  select(speaker, sentence_coloured) %>%
                  mutate(sentence_coloured = stringr::str_replace_all(string = sentence_coloured,
                                                                      pattern = "<div class=\"mytooltip\">",
                                                                      replacement = "")) %>%
                  mutate(sentence_coloured = stringr::str_replace_all(string = sentence_coloured,
                                                                      pattern = "</div>",
                                                                      replacement = "")) %>%
                  mutate(sentence_coloured = stringr::str_replace_all(string = sentence_coloured,
                                                                      pattern = "<span class=\"tooltiptext\">[^\x20]+</span>",
                                                                      replacement = ""))
                ,
                filter = c("top"),
                selection = "none",
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = TRUE,
                               search = list(regex = TRUE, scrollX = TRUE)
                )
      ) %>% 
        formatStyle("sentence_coloured","white-space"="nowrap")
    }) 
    
    
    
    # table_summaries (MLU etc)----
    output$table_summaries = DT::renderDataTable({
      
      
      datatable(table() %>% filter(speaker!="") %>% group_by(speaker) %>%
                  summarise(`NUtts` = n(),
                            `MLU-w` = round(mean(`Num Words`), digits = 2),
                            `MLU-m` = round(mean(`Num Morphs`), digits = 2),
                            `MNumCl` = round(mean(num_clause), digits = 2),
                            `MLT` = round(mean(turn_length_first_turn, na.rm = TRUE), digits = 2)),
                # filter = c("top"),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE,
                               search = list(regex = TRUE, scrollX = TRUE)
                ),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'NUtts: Number of Utterances, 
                  MLU-w: Mean Length of Utterance in Words, 
                  MLU-m: Mean Length of Utterance in Morphemes, 
                  MNumCl: Mean Number of (finite) Clauses per utterance, 
                  MLT: Mean Length of Turn')
      )
      
    })
    
    
    
    # table_summaries2 (diversity) ----
    output$table_summaries2 = DT::renderDataTable({
      datatable(table_lex() %>% filter(speaker!="") %>% group_by(speaker) %>%
                  summarise(`HDD` = round(mean(hdd), digits = 2),
                            `TTR` = round(mean(ttr), digits = 2)),
                # filter = c("top"),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE,
                               search = list(regex = TRUE, scrollX = TRUE)
                ),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'HDD: Hypergeometric Distribution density (virtually identical to VocD), 
                  TTR: Type-Token Ratio')
      ) 
    })
    
    # (4) tag_table  ----
    output$tag_table = DT::renderDataTable({
      
      datatable(table() %>% filter(speaker!="") %>%
                  select(matches("speaker|[\x5b][^\x5b]+")) %>%
                  select(-contains("no_html")) %>%
                  group_by(speaker) %>%
                  summarise_all(mean),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE,
                               search = list(regex = TRUE, scrollX = TRUE)
                )
      ) 
      
      # https://stackoverflow.com/questions/42125049/using-starts-with-in-dplyr-with-a-vector-of-partial-column-names
      
    })
    
    
    # Universal Search Term Cheat Sheet ----
    
    output$cheatsheet <- renderUI({
      # HTML(rmarkdown::render('Rmarkdown_instructions.Rmd'))
      HTML(markdown::markdownToHTML(knit('cheat_sheet.Rmd', quiet = TRUE)))
      # includeHTML("Rmarkdown_instructions.html")
    })
    
    
    # ranges and observeEvent for interactive plots ----
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    
    observeEvent(input$plot_dblclick3, {
      brush <- input$plot_brush3
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
    

    
    # DIY_plot -----
    output$DIY_plot <- renderPlot({
      
      # browser(); one <- 1; one <- 1; one <- 1; one <- 1
      
      
      req(df_childes_DIY())
      
      m2ym <- function(age_m){
        year <- floor(age_m/12)
        month <- floor(age_m - (year*12))
        return(paste0(year, ";", month))
      }
      
      breakpoints <- function(min,max){
        seq <- seq(min, max, 1)
        seq <- unique(floor(seq/3))
        seq <- seq*3
        return(seq)
      }
      
      g <- ggplot()
      
      g <- g + theme_bw()
      
      g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      
      g <- g + geom_point(data = df_childes_DIY(), alpha = input$shading3, aes(x = target_child_age, y = dv, size = num_utterances))
      
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = mean), linetype = "solid", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = plus_one), linetype = "dashed", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = plus_one_point_five), linetype = "dotted", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = minus_one), linetype = "dashed", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = minus_one_point_five), linetype = "dotted", lwd = 1, se = FALSE, method = "loess")
      
      mean_model <- loess(mean ~ target_child_age, data = df_childes_DIY())
      plus_one_model <- loess(plus_one ~ target_child_age, data = df_childes_DIY())
      minus_one_model <- loess(minus_one ~ target_child_age, data = df_childes_DIY())
      
      g <- g + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
      
      g <- g + scale_x_continuous(breaks = breakpoints(0, xmax_DIY()),         # use these breaks...
                                  labels = m2ym(breakpoints(0, xmax_DIY()))    # ...with these labels
      ) 
      
      dv_name <- case_when(input$variable3 == "mlu_m" ~ "MLU in morphemes",
                           input$variable3 == "mlu_w" ~ "MLU in words",
                           input$variable3 == "ttr" ~ "Type Token Ratio",
                           input$variable3 == "hdd" ~ "HDD")
      
      g <- g + labs(title = paste("MLU in morphemes for CHILDES collection", input$collection3),
                    x = "Age (Months;Years)", y = dv_name)
      
      # Activate this routine if speaker has been selected, and mlum has been chosen as input$variable
      
      
      g_subtitle <- "Blue lines show mean, 1 st.dev, and 1.5 st.dev"
      
      if(input$child_name != "" &
         input$value != "" &
         is.na(predict(mean_model, speaker_age())) == FALSE)
        
      {
        
        mean_for_speaker_age <- predict(mean_model, speaker_age())
        sd_for_speaker_age <- predict(plus_one_model, speaker_age()) - mean_for_speaker_age
        z_score <- round((as.numeric(input$value) - mean_for_speaker_age)/sd_for_speaker_age, 2)
        perc <- round(pnorm(z_score)*100,0)
        g <- g + geom_point(data = df_childes_DIY(), aes(x = speaker_age(), y = as.numeric(input$value), pch = 9, size = 500, colour = "red")) + scale_shape_identity()
        zp_label <- paste0("z = ", as.character(z_score), ", perc =", perc)
        g_subtitle <- paste0(g_subtitle, "\nMarker shows participant ", input$child_name, ",", zp_label)
      }
      
      g <- g + labs(subtitle = g_subtitle)
      
      g
      
    }) # end of output$all plot <- renderPlot...
    

    
    output$age = renderUI({
      
      m2ym <- function(age_m){
        year <- floor(age_m/12)
        month <- floor(age_m - (year*12))
        return(paste0(year, ";", month))
      }
      
      age <- paste("Age = ", as.character(m2ym(speaker_age())))
      
    })
    
  }
  
)




