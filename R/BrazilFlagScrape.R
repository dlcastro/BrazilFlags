# Download Bandeiras municipais
library(rvest)
library(tidyverse)
library(purrr)
library(stringi)
library(furrr)
library(renv)


# Utilizar 3 cores --------------------------------------------------------
# Mude aqui para aumentar ou diminuir os Cores do processador em uso
plan(multisession, workers = 3)
set.seed(123)




url1 <- "https://www.crwflags.com/fotw/flags/br-mun"
url2 <- ".html"
fg_url <- "https://www.crwflags.com/fotw/images/b/"




# Função apra coletar URL e baixar bandeira -------------------------------
flag_collect <- function(x) {
  
  print(x)
  
  cidades <- rvest::read_html(paste0(url1,x,url2))
  
  
  municipios <- cidades %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    as.data.frame()
  
  
  links <- cidades %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    map(., html_attrs) %>% 
    map_df(~as.list(.)) %>% 
    bind_cols(municipios) %>% 
    mutate(order = x) %>% 
    select(-name) %>% 
    rename(municipio = ".") %>% 
    na.omit %>% 
    mutate(site = paste0("https://www.crwflags.com/fotw/flags/",href),
           cidade_tratado = stringi::stri_trans_general(municipio, 'latin-ascii; upper'),
           cidade_tratado = gsub("\\s","_", cidade_tratado),
           cidade_tratado = gsub("\\(","", cidade_tratado),
           cidade_tratado = gsub("\\)","", cidade_tratado))
  
  

 
}


# Comando para encontrar url da bandeira em formato gif -------------------
# Pelo que notei, a url que salvei em site não é a mesma da bandeira
# Parece que há apenas um "-" a menos. Contudo, prefiro fazer o trabalho que,
# apesar de parecer mais longo, é garantido que vou pegar todas as bandeiras.
# Ou seja, irei buscar página por página de cada cidade a bandeira
# Ao invés de fazer o caminho curto através do flag_collect.
# Depois testo para ver se adicionar "-" ao site do flag_collect resolve ou não.
flag_url <- function(x){
  
  print(x)
  url_bandeira <- rvest::read_html(x)
  
  
  indicacao <- url_bandeira %>%
    html_nodes("a") %>% 
    html_text() %>% 
    map(.,print)%>%
    map_df(., data.frame)
  
  
  
  fonte <- url_bandeira  %>% 
    html_nodes("a") %>%  
    html_attrs() %>%
    map(.,print)%>% 
    map_df(., data.frame) %>% 
    bind_cols(indicacao) %>% 
    mutate(order = row_number())%>%
    rename(fonte_a = .x..i.....1,
           fonte_b = .x..i.....2)%>%
    filter(grepl("../images/b/",fonte_a, fixed = TRUE)) %>%
    mutate(fonte_b = ifelse(grepl("../images/b/",fonte_a, fixed = TRUE),
                            gsub("../images/b/",
                                 "", 
                                 fonte_a,
                                 fixed = T),
                            fonte_b)) %>% 
    # Algumas vezes colocam o brasão e em outros a bandeira por último.
    # Fazer slice para proteger
    slice(nrow(.))%>% 
    bind_cols(data.frame("fonte_original"= "url_bandeira")) %>% 
    select(fonte_b,fonte_original) %>% 
    pivot_wider(names_from = "fonte_original",
                values_from = "fonte_b") %>% 
    mutate(site = {{x}}) %>% 
    mutate(url_bandeira = as.character(unlist(url_bandeira)))
  
  
  # 
  # # Download da bandeira por cada loop do map_df ----------------------------
  
  
  
  
  
}


# Função para baixar a imagem da bandeira ------------------------------------
download.image <- function(image.url, 
                           image.name){
  
  download.file(paste0(fg_url,image.url),
                destfile = paste0("Result/",image.name,".gif"),
                mode="wb")  
  
  log <- data.frame("flag" = image.name,
                    "situacao" = "Ok")
  
} 



# Baixar url de cada cidade disponível no site ----------------------------
flags <- furrr::future_map_dfr(letters, 
                possibly(flag_collect,
                         otherwise = data.frame(href = "NA - ERROR"))) %>% 
  filter(href != "NA - ERROR")




# encontrar url da imagem de cada bandeira, contendo fonte e site  --------
link_bandeiras <- furrr::future_map_dfr(flags$site,
                         possibly(flag_url,
                                  otherwise = data.frame(url_bandeira = "NA - ERROR"))) %>% 
  left_join(flags, by = "site")



# Iniciar download bandeiras ------------------------------------------------
log <- furrr::future_map2_dfr(
  link_bandeiras$url_bandeira,
  link_bandeiras$cidade_tratado,
     ~tryCatch(download.image(.x,.y),
               error = function(e) data.frame("flag" = .y,
                                              "situacao" = "Problema")
               ))


write.csv(log, file = "Result/0-Situacao_Download_Flag.csv")









