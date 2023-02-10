# functions for the shiny app


get_journals <- function(){
  D <- fromJSON("data/abstracts_yes.json", flatten=TRUE) %>%
    select(`container-title`) %>%
    unique() %>%
    unlist(use.names = F) %>%
    sort() 
  
  return(D) 
}

read_upload <- function(fname){
      misqBoilerplate <- "Copyright of MIS Quarterly is the property of MIS Quarterly and its content may not be copied or emailed to multiple sites or posted to a listserv without the copyright holder's express written permission. However, users may print, download, or email articles for individual use. This abstract may be abridged. No warranty is given about the accuracy of the copy. Users should refer to the original published version of the material for the full abstract."

  #print("TEST 1############")
  D <- read_file(
    fname,
    locale = locale(encoding = "UTF-8")
  ) 
  Encoding(D) <- "utf-8"
  D = stri_trans_general(str = D, id = "Latin-ASCII")
  #D <- iconv(D, "utf-8", "ascii",sub='') 
  
   
  #print("TEST 2 ###############")
  if("nbib" %in% fname){
  D %>%
    write_file(
      "temp.nbib"
    )
  }else if ("bib" %in% fname) {
      D %>%
          write_file(
              "temp.bib"
          )
  } else{
    D %>%
      write_file(
        "temp.ris"
      )
  }
  #print("TEST 3 ###############")  
  if("nbib" %in% fname){
  D <- read_bibliography(
    "temp.nbib",
    return_df = TRUE
  )} else if("bib" %in% fname) {
       D <- read_bibliography(
           "temp.bib",
           return_df=TRUE
           )
   } else{
    D <- read_bibliography(
      "temp.ris",
      return_df = TRUE
    ) 
  }
#  fwrite(D,"temp.csv")
#  D <- fread("temp.csv",encoding = "UTF-8")
  
  D <- D %>%
    select(
      abstract,
      label,
      title,
      journal,
      year,
      author,
      doi,
      keywords
    ) %>%
      mutate(author = stri_trans_general(author, id = "Any-Any")) %>%
    rename(id = label) %>%
    rename(yr = year) %>%
    rename(Url = doi) %>%
    rename(Author = author) %>%
    mutate(yr = str_extract(yr, "\\d{4}")) %>%
    mutate(yr = as.numeric(yr)) %>%
    mutate(Url = str_glue(
      "https://doi.org/{Url}"
    )) %>%
    mutate(tags = keywords) %>%
    mutate(tags = str_replace_all(tags, " ", "-")) %>%
    mutate(tags = str_replace_all(tags, "-and-", " and ")) %>%
    mutate(abstract = str_replace_all(abstract, misqBoilerplate, "")) %>%
    drop_na(abstract) %>%
    select(
      title,
      journal,
      id,
      abstract,
      Author,
      Url,
      tags,
      yr
    )
    #print("TEST 4 ###############")
  
  #print("TEST 5 ###############")
  return(D)
}


read_specialties <- function(fnames){
  D <- read_bibliography(
    fnames,
    return_df = TRUE
  )
  
  Encoding(D$author) <- "utf-8"
  #D <- iconv(D, "utf-8", "ascii",sub='') 
    
  D <- D %>%
    select(
      abstract,
      label,
      title,
      journal,
      year,
      author,
      doi,
      keywords
    ) %>%
      mutate(author = stri_trans_general(author, id = "Any-Any")) %>%
    rename(id = label) %>%
    rename(yr = year) %>%
    rename(Url = doi) %>%
    rename(Author = author) %>%
    mutate(yr = str_extract(yr, "\\d{4}")) %>%
    mutate(yr = as.numeric(yr)) %>%
    mutate(Url = str_glue(
      "https://doi.org/{Url}"
    )) %>%
    mutate(tags = keywords) %>%
    mutate(tags = str_replace_all(tags, " ", "-")) %>%
    mutate(tags = str_replace_all(tags, "-and-", " and ")) %>%
    drop_na(abstract) %>%
    select(
      title,
      journal,
      id,
      abstract,
      Author,
      Url,
      tags,
      yr
    )
  return(D)
    }


get_abstracts <- function(){
  # load from one file for now, will need to modify
  
  misqBoilerplate <- "Copyright of MIS Quarterly is the property of MIS Quarterly and its content may not be copied or emailed to multiple sites or posted to a listserv without the copyright holder's express written permission. However, users may print, download, or email articles for individual use. This abstract may be abridged. No warranty is given about the accuracy of the copy. Users should refer to the original published version of the material for the full abstract."
  
  D <- fromJSON("data/abstracts_yes.json", flatten=TRUE) %>%
    select(DOI,abstract,id,title,`container-title`) %>%
    rename(journal = `container-title`) %>%
    mutate(yr = str_extract(id, "\\d{4}")) %>%
    mutate(yr = as.numeric(yr)) %>%
    mutate(abstract = str_replace_all(abstract, misqBoilerplate, "")) 
  
  return(D) 
}


get_tag_data <- function(){
  fname <- "data/tag_export.csv"
  D <- read_csv(fname) %>%
    unite("tags",`Manual Tags`, `Automatic Tags`, sep=';', na.rm=T) %>%
    filter(!is.na(tags)) %>%
    mutate(tags = str_replace_all(tags,"; ", ";")) %>%
    mutate(tags = str_replace_all(tags, "-", "")) %>%
    mutate(tags = str_replace_all(tags, "&", "and")) %>%
    mutate(tags = str_replace_all(tags, "\\(", "")) %>%
    mutate(tags = str_replace_all(tags, "\\)", "")) %>%
    mutate(tags = str_replace_all(tags, " ", "_")) %>%
    mutate(tags = str_remove_all(tags, "POMSIYes")) %>%
    mutate(tags = str_remove_all(tags, "POMSIMaybe")) %>%
    mutate(tags = str_remove_all(tags, "POMSIMethods")) %>%
    mutate(tags = str_remove_all(tags, "Yes__KW")) %>%
    mutate(tags = str_remove_all(tags, "Maybe__KW")) %>%
    mutate(tags = str_remove_all(tags, "Case_Study__KW")) %>%
    mutate(tags = str_remove_all(tags, "JMIS_Maybe")) %>%
    mutate(tags = str_remove_all(tags, "JMIS_Yes"))
  
  return(D)
}


get_merged_data <- function(){
  D.abstracts <- get_abstracts()
  D.tags <- get_tag_data() 
  
  D <- D.abstracts %>%
    inner_join(
      D.tags,
      by = c("title" = "Title")
    ) %>%
    select(
      title, 
      journal,
      id,
      abstract,
      Author,
      Url,
      tags,
      yr
    ) %>%
    mutate(title = str_to_title(title))
  
  return(D)
}


create_cooccurance_matrix <- function(
  topicData, 
  start, 
  end, 
  selectedJournals, 
  percentInclude, 
  sourcetag=NA, 
  targettag=NA
){
  D <- topicData %>%
    drop_na(networkKeywords)

  if(!is.na(sourcetag)){
    D <- D %>%
      filter(
        grepl(
          sourcetag, 
          networkKeywords,
          ignore.case = TRUE
        )
      )
  }
  if(!is.na(targettag)){
    D <- D %>%
      filter(
        grepl(
          targettag, 
          networkKeywords,
          ignore.case=TRUE
        )
      )
  }
  corpus <- corpus(D$networkKeywords) %>%
    tokens(remove_punct = TRUE,
           remove_numbers = TRUE,
           remove_symbols = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(pattern=stopwords())
  
  
  fcmat <- fcm(
    corpus, 
    context = "document", 
    tri = FALSE
  )
  
  num_feat = max(percentInclude, 5) 
  print(num_feat)
  feat <- names(topfeatures(fcmat, num_feat, scheme = "docfreq"))
  print(feat)
  
  nodes <- corpus %>%
    dfm(
      remove=stopwords("english")
    ) %>%
    #dfm_trim(
    #  max_docfreq = 0.5, 
    #  docfreq_type = "prop"
    #) %>% 
    textstat_frequency() %>%
    as.data.frame()%>%
    select(feature, docfreq) %>%
    filter(feature %in% feat)
  
  fcmat <- fcm_select(fcmat, pattern = nodes$feature) %>%
    as.matrix()
  
  
  print(nodes)
  print(feat)
  
  ig <- graph_from_adjacency_matrix(
    fcmat,
    mode="undirected",
    weighted="weights",
    diag=FALSE
  )
  
  plot_d3 <- igraph_to_networkD3(ig)
  plot_d3$nodes$group <- 1
  plot_d3$nodes$weights <- nodes[order(nodes$feature),]$docfreq * 10
  plot_d3$links$value <- (plot_d3$links$value + 1) #^ (1/2)
  myClickAction <- 'Shiny.setInputValue("graphLinks", {source: d.name, target: d.name});'
  #'Shiny.setInputValue("obs", 1);'
  
  op <- forceNetwork(Links = plot_d3$links, 
                     Nodes = plot_d3$nodes, 
                     Source = 'source', 
                     Target = 'target', 
                     Value="value",
                     NodeID = 'name', 
                     Group = 'group', 
                     Nodesize="weights",
                     fontSize = 25,
                     #linkDistance = 300,
                     linkDistance = networkD3::JS("function(d) { return 800/(d.value); }"), # mess with the 5 to mess with the distances
                     height = 600, 
                     charge = -30,  # default
                     zoom = TRUE,
                     clickAction = myClickAction,
                     opacityNoHover = 0.45,
                     opacity = 0.65,
                     bounded=FALSE
                     )
  
  # fnrender for updating R input on edge click
  # Get the target variable in fn$x$links (an integer id) to show up as a tooltip when user hovers over a link (i.e. edge) in the graph
  fnrender <- htmlwidgets::onRender(
    op,
    '
  function(el, x) {
    
    d3.selectAll(".link").append("svg:title")
      .text(function(d) { return d.source.name + " -> " + d.target.name; });
      d3.selectAll(".link").on(
      "click", function(d){
        Shiny.setInputValue("graphLinks", {source: d.source.name, target: d.target.name});
      }
      );
  }
  '
  )
  return(fnrender)
}


