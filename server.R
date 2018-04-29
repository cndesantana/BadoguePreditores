library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(readxl)
library(maptools)
library(brmap)
library(maptools)
library(rgdal) # ensure rgdal is loaded
library(readxl)
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)

options(shiny.maxRequestSize=30*1024^2)
corpositivo <- "#20B2AA";
cornegativo <- "#c00000";
corneutro <- "#FFA500";

minposts <- 2; #número mínimo de posts por semana

function(input, output, session) {
   read_allxlsx <- function(reladir){
      
      allfiles <- system(paste("ls ",reladir,"*/*.xlsx",sep=""),intern=TRUE);
      for(i in 1:length(allfiles)){
         df <- read_xlsx(allfiles[i]);
         if(i == 1){
            NI <- df
         }
         else{
            NI <- rbind(NI,df)
         } 
      } 
      
      return(NI)
   }
   
   read_allxlsx_mes <- function(reladir){
      
      allfiles <- system(paste("ls ",reladir,"*.xlsx",sep=""),intern=TRUE);
      for(i in 1:length(allfiles)){
         df <- read_xlsx(allfiles[i]);
         if(i == 1){
            NI <- df
         }
         else{
            NI <- rbind(NI,df)
         } 
      } 
      
      return(NI)
   }
   
   lm_eqn <- function(df){
      m <- lm(y ~ x, df);
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                       list(a = format(coef(m)[1], digits = 2), 
                            b = format(coef(m)[2], digits = 2), 
                            r2 = format(summary(m)$r.squared, digits = 3)))
      cat(paste(as.character(df$regiao)[1], format(coef(m)[2], digits = 2), sep=","),file=as.character(df$filename)[1],sep="\n",append=TRUE)
      as.character(as.expression(eq));                 
   }
   
   cleaningRelNI1Temas = function(relNI){
      
      relNI$Temas <- toupper(relNI$Temas)
      relNI$Temas <- str_replace_all(relNI$Temas, "DITRIBUIÇÃO DE ÁGUA","DISTRIBUIÇÃO DE ÁGUA")
      
      relNI$Temas <- str_replace_all(relNI$Temas, "\'","\"")
      relNI$Temas <- str_replace_all(relNI$Temas, "\\] \\[",",")
      relNI$Temas <- str_replace_all(relNI$Temas, "\\]  \\[",",")
      relNI$Temas <- str_replace_all(relNI$Temas, " , ",",")
      relNI$Temas <- str_replace_all(relNI$Temas, ", ",",")
      
      relNI$Temas <- str_replace_all(relNI$Temas, "\"]","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\"] ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \"]","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \"] ","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" ]","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" ] ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \" ]","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \" ] ","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\\]","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\\] ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \\]","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \\] ","")
      
      
      
      relNI$Temas <- str_replace_all(relNI$Temas, " \"\\[ ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \"\\[","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\"\\[ ","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\"\\[","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" \\[ ","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" \\[","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \" \\[ ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \" \\[","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\\[ ","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\\[","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \\[ ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \\[","")
      
      
      relNI$Temas <- str_replace_all(relNI$Temas, "[\\[\\]]","")
      
      
      relNI$Temas <- str_replace_all(relNI$Temas, "\",",",")
      relNI$Temas <- str_replace_all(relNI$Temas, " \",",",")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" ,",",")
      relNI$Temas <- str_replace_all(relNI$Temas, " \" ,",",")
      
      relNI$Temas <- str_replace_all(relNI$Temas, "\"","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \"","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" ","")
      relNI$Temas <- str_replace_all(relNI$Temas, " \" ","")
      relNI$Temas <- str_replace_all(relNI$Temas, "\" ","\"")
      
      
      
      
      relNI <- relNI %>%
         mutate(Temas = strsplit(as.character(Temas), ", ")) %>% 
         unnest(Temas) 
      relNI <- relNI %>%
         mutate(Temas = strsplit(as.character(Temas), ",")) %>% 
         unnest(Temas)
      
      relNI$Temas <- str_replace_all(relNI$Temas, "SAÚDE ","SAÚDE")
      relNI$Temas <- str_replace_all(relNI$Temas, "SEGURANÇA ","SEGURANÇA")
      relNI$Temas <- str_replace_all(relNI$Temas, "OBRAS DA INFRAESTRUTURA ","OBRAS DA INFRAESTRUTURA")
      relNI$Temas <- str_replace_all(relNI$Temas, "INCENTIVO AO ESPORTE E LAZER ","INCENTIVO AO ESPORTE E LAZER")
      relNI$Temas <- str_replace_all(relNI$Temas, "TRANSPORTE PÚBLICO/MOBILIDADE ","TRANSPORTE PÚBLICO/MOBILIDADE")
      relNI$Temas <- str_replace_all(relNI$Temas, "HABITAÇÃO/MORADIA ","HABITAÇÃO/MORADIA")
      relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGOS ","GERAÇÃO DE EMPREGO")
      relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGO E RENDA ","GERAÇÃO DE EMPREGO")
      relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGO E RENDA","GERAÇÃO DE EMPREGO")
      relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGOS","GERAÇÃO DE EMPREGO")
      relNI$Temas <- str_replace_all(relNI$Temas, "APOIO À AGRICULTURA ","APOIO À AGRICULTURA")
      relNI$Temas <- str_replace_all(relNI$Temas, "ATRAÇÃO DE INDUSTRIAS","ATRAÇÃO DE INDÚSTRIAS")
      
      relNI$Temas <- str_replace_all(relNI$Temas, "APOIO À CULTURA ","APOIO À CULTURA")
      relNI$Temas <- str_replace_all(relNI$Temas, "APOIO AO TURISMO ","APOIO AO TURISMO")
      relNI$Temas <- str_replace_all(relNI$Temas, "DISTRIBUIÇÃO DE ÁGUA ","DISTRIBUIÇÃO DE ÁGUA")
      relNI$Temas <- str_replace_all(relNI$Temas, "ESTRADAS E RODOVIAS ","ESTRADAS E RODOVIAS")
      relNI <- relNI %>% filter(!is.na(Temas))
      relNI <- relNI[which(relNI$Temas != " "),]
      return(relNI)
   }
   
   cleaningRelNI1Grupos = function(relNI1){
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, " \"","")
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\"]","")
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\\[","")
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\" ,",",")
      
      relNI1 <-  relNI1 %>%
         mutate(Grupos = strsplit(as.character(Grupos), ", ")) %>% 
         unnest(Grupos)
      relNI1 <-  relNI1 %>%
         mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
         unnest(Grupos)
      
      relNI1 <- relNI1 %>% 
         mutate(Grupos = stringr::str_replace_all(Grupos, "[\\[\\]]","")) %>% 
         mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
         unnest(Grupos) %>% 
         filter(Grupos != "CPBA_3629944") %>%
         filter(Grupos != "Rui Correria_9356576")  %>%
         filter(Grupos != "Carnaval_974981")%>%
         filter(Grupos != "Notícias Carnaval") %>%
         filter(Grupos != "teste_6503868") %>%
         filter(Grupos != "Neutro")%>%
         filter(Grupos != "Rui Correria") %>%
         filter(Grupos != "Carnaval") %>%
         filter(Grupos != "TESTE Região 11 Caetité") %>%
         filter(Grupos != "TESTE 11 Caetité") %>%
         filter(Grupos != " Correria") %>%
         filter(Grupos != "TESTE11 Caetité")
      
      relNI1$Grupos[which(relNI1$Grupos == "Região 8 - Feira + ParÁguaçu")] <- "Região 8 - Feira + Paraguaçu"
      relNI1$Grupos[which(relNI1$Grupos == "Região 8 - Feira + Paráguaçu")] <- "Região 8 - Feira + Paraguaçu"
      relNI1$Grupos[which(relNI1$Grupos == "Região 10 - Irecê/ChapadaDiamantina")] <- "Região 10 - Irecê/Chapada Diamantina"
      #   relNI1 <- relNI1[which(relNI1$Grupos != "TESTE11 Caetité_5992896"),]
      
      return(relNI1)
   }
   
   limpaPlanilha = function(relNI1){
      
      ################### CLEANING GRUPOS
      relNI1 <- cleaningRelNI1Grupos(relNI1)
      
      ################### CLEANING 'TEMAS'
      relNI1 <- cleaningRelNI1Temas(relNI1)
      relNI1 <- relNI1 %>% 
         group_by(Polaridade, Grupos, Temas) %>%
         summarise(npositivo = length(which(Polaridade == "Positivo")),
                   nnegativo = length(which(Polaridade == "Negativo")),
                   nneutro = length(which(Polaridade == "Neutro")),
                   is = ifelse(((npositivo + nnegativo)==0),
                               0,
                               ifelse((nnegativo + npositivo > 2),
                                      (npositivo - nnegativo) / (npositivo + nnegativo),
                                      0))
         )%>%
         ungroup()%>%
         select(Grupos, Temas, npositivo, nnegativo, is)
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, '_.*', '')
      relNI1$Temas <- str_replace_all(relNI1$Temas, '_.*', '')
      
      return(relNI1)
   }

   plotPreditores = function(){
      filepath <- input$file$datapath
      if(length(filepath) > 0){
            relNI1 <- read_xlsx(filepath)
            dfGrupos1 <- limpaPlanilha(relNI1)
            dfGrupos1 <- dfGrupos1[which(dfGrupos1$Grupos != "TESTE Região 11 Caetité"),]
            dfGrupos1$grpnumber <- unlist(strsplit(unlist(strsplit(dfGrupos1$Grupos,"-"))[seq(from=1,to=2*length(dfGrupos1$Grupos),by=2)]," "))[seq(from=2,to=2*length(dfGrupos1$Grupos),by=2)]
            
            
            grupos_ <- input$grupo
            positivos <- dfGrupos1  %>% 
               filter(grpnumber == grupos_, npositivo > 0)%>%
               select(Grupos, Temas, npositivo)
            
            negativos <- dfGrupos1 %>% 
               filter(grpnumber == grupos_, nnegativo > 0) %>% 
               select(Grupos, Temas, nnegativo) %>%
               mutate(nnegativo = nnegativo * (-1)) %>%
               select(Grupos, Temas, nnegativo)
            
            names(positivos) <- c("Grupos","Temas","Comentarios")
            names(negativos) <- c("Grupos","Temas","Comentarios")
            todos <- rbind(positivos,negativos)
            todos <- todos %>% mutate(Sentimento = ifelse(Comentarios > 0, "Positivo","Negativo"))
            
            todos <- todos %>% 
               group_by(Temas, Sentimento) %>% 
               summarise(count = sum(abs(Comentarios))) %>%
               group_by(Temas) %>%
               mutate(Porcentagem = 100*count / sum(count) ) %>%
               arrange(Temas)
            
            todos %>%
               ggplot(aes(x = Temas, y = Porcentagem, fill=Sentimento)) + 
               geom_bar(stat="identity") +          
               scale_fill_manual("Sentimento", values = c("Positivo" = corpositivo, "Negativo" = cornegativo))+
               geom_text(size = 3, col = "white", aes(x = Temas, y = if_else(Sentimento == "Negativo", 10, 90), label = paste(as.character(round(Porcentagem,2)),"%",sep=""), hjust = 0)) +
               ylab("% de Comentários") + xlab("Temas") + 
               coord_flip() + 
               facet_grid(. ~ paste(input$mes,grupos_,sep=" - "))
        }
   }
   
   output$preditoresTS = downloadHandler(
      filename = function() {
         paste("preditores_",input$mes,".png", sep = "")
      },
      content = function(file) {
         device <- function(..., width, height) {
            grDevices::png(..., width = 3200, height = 1800,
                           res = 300)
         }
         ggsave(file, plot = plotPreditores(), device = device)
      }   
      )    

  output$preditores <- renderPlot({
     filepath <- input$file$datapath
     if(length(filepath) > 0){
        relNI1 <- read_xlsx(filepath)
        dfGrupos1 <- limpaPlanilha(relNI1)
        dfGrupos1 <- dfGrupos1[which(dfGrupos1$Grupos != "TESTE Região 11 Caetité"),]
        dfGrupos1$grpnumber <- unlist(strsplit(unlist(strsplit(dfGrupos1$Grupos,"-"))[seq(from=1,to=2*length(dfGrupos1$Grupos),by=2)]," "))[seq(from=2,to=2*length(dfGrupos1$Grupos),by=2)]
        
        
        grupos_ <- input$grupo
        positivos <- dfGrupos1  %>% 
           filter(grpnumber == grupos_, npositivo > 0)%>%
           select(Grupos, Temas, npositivo)
        
        negativos <- dfGrupos1 %>% 
           filter(grpnumber == grupos_, nnegativo > 0) %>% 
           select(Grupos, Temas, nnegativo) %>%
           mutate(nnegativo = nnegativo * (-1)) %>%
           select(Grupos, Temas, nnegativo)
        
        names(positivos) <- c("Grupos","Temas","Comentarios")
        names(negativos) <- c("Grupos","Temas","Comentarios")
        todos <- rbind(positivos,negativos)
        todos <- todos %>% mutate(Sentimento = ifelse(Comentarios > 0, "Positivo","Negativo"))
        
        todos <- todos %>% 
           group_by(Temas, Sentimento) %>% 
           summarise(count = sum(abs(Comentarios))) %>%
           group_by(Temas) %>%
           mutate(Porcentagem = 100*count / sum(count) ) %>%
           arrange(Temas)
        
        todos %>%
           ggplot(aes(x = Temas, y = Porcentagem, fill=Sentimento)) + 
           geom_bar(stat="identity") +          
           scale_fill_manual("Sentimento", values = c("Positivo" = corpositivo, "Negativo" = cornegativo))+
           geom_text(size = 3, col = "white", aes(x = Temas, y = if_else(Sentimento == "Negativo", 10, 90), label = paste(as.character(round(Porcentagem,2)),"%",sep=""), hjust = 0)) +
           ylab("% de Comentários") + xlab("Temas") + 
           coord_flip() + 
           facet_grid(. ~ paste(input$mes,grupos_,sep=" - "))
     }
  })

}
