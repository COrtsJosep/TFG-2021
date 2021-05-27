### Import and preliminar cleaning ###
library(readxl)
library(tidyverse)
library(gtools)
library(stringdist)

output_h <- read_excel("output_excel.xlsx", 
                       sheet = "2021-05-03_h", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "text", "text", "text", 
                                                             "text", "text", "text", "numeric", 
                                                             "numeric", "numeric", "numeric"))
output_h <- output_h %>% mutate(RentM = RentPrice/Surface) %>% mutate(Date = as.Date(LastUpdate, format = '%d/%m/%Y'), .keep = "unused")
output_h <- output_h %>% distinct(Latitude, Longitude, Description, RentM, .keep_all = TRUE) %>% 
  filter(is.na(RentM) == FALSE, Date >= "2021-04-03", Surface >= 10, RentM >= 5, RentPrice < 50000, DistanceToCenter <= 9)
keep <- subset(1:dim(output_h)[1], !(1:dim(output_h)[1] %in% which(output_h$Bathrooms > 20 | output_h$Bedrooms > 30)))
output_h <- output_h %>% slice(keep)

output_f <- read_excel("output_excel.xlsx", 
                       sheet = "2021-05-03_f", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "text", "text", 
                                                             "text", "text", "numeric", "numeric", 
                                                             "numeric"))
output_f <- output_f %>% mutate(RentM = RentPrice/Surface) %>% mutate(Date = as.Date("2021-05-03")-LastUpdate, .keep = "unused")
output_f <- output_f %>% distinct(Latitude, Longitude, Description, RentM, .keep_all = TRUE) %>% 
  filter(is.na(RentM) == FALSE, Date >= "2021-04-03", Surface >= 10, RentM >= 5, RentPrice < 50000, DistanceToCenter <= 9)
keep <- subset(1:dim(output_f)[1], !(1:dim(output_f)[1] %in% which(output_f$Bathrooms > 20 | output_f$Bedrooms > 30)))
output_f <- output_f %>% slice(keep)

output_p <- read_excel("output_excel.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "numeric", "numeric", "numeric"))
output_p <- output_p %>% mutate(RentM = RentPrice/Surface) %>% mutate(Date = as.Date(LastUpdate, format = '%d/%m/%Y'), .keep = "unused")
output_p <- output_p %>% distinct(Latitude, Longitude, Description, RentM, .keep_all = TRUE) %>% 
  filter(is.na(RentM) == FALSE, Date >= "2021-04-03", Surface >= 10, RentM >= 5, RentPrice < 50000, DistanceToCenter <= 9)
keep <- subset(1:dim(output_p)[1], !(1:dim(output_p)[1] %in% which(output_p$Bathrooms > 20 | output_p$Bedrooms > 30)))
output_p <- output_p %>% slice(keep)

output_i <- read_excel("output_excel.xlsx", 
                       sheet = "2021-05-03_i", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "text", "text", "text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric"))
output_i <- output_i %>% mutate(RentM = RentPrice/Surface)
output_i <- output_i %>% distinct(RentPrice, Surface, Bedrooms, Bathrooms, PropertySubtype,
                                  Municipality, Neighborhood, Latitude, Longitude,
                                  Floor, .keep_all = TRUE) %>% 
  filter(is.na(RentM) == FALSE, Municipality == "Barcelona", Surface >= 10, RentM >= 5, RentPrice < 50000, DistanceToCenter <= 9)
keep <- subset(1:dim(output_i)[1], !(1:dim(output_i)[1] %in% which(output_i$Bathrooms > 20 | output_i$Bedrooms > 30)))
output_i <- output_i %>% slice(keep)



### Duplicate deletion ###
max_sim_tolerated <- 0.6

# Habitaclia
output_h <- output_h %>%  add_count(Latitude, Longitude, RentM, name = "Suspects") 
output_h_uniques <- output_h %>% filter(Suspects == 1)
output_h_suspects <- output_h %>% filter(Suspects != 1)
output_h_suspects <- output_h_suspects %>% select(names(output_h_suspects)[-length(output_h_suspects)]) %>%
  add_count(Latitude, Longitude, RentM, name = "Suspects") %>% add_column(Cluster = NA)
uniques <- unique(output_h_suspects[,c(11,12,15)])

for (i in 1:dim(uniques)[1]){
  for (j in 1:dim(output_h_suspects)[1]){
    if (all(uniques[i,]==output_h_suspects[j,c(11,12,15)])){
      output_h_suspects$Cluster[j] <- i
    }
  }
}

confirmed <- c()
for (i in 1:max(output_h_suspects$Cluster)){
  values <- which(output_h_suspects$Cluster == i)
  perm <- permutations(n=length(values),r=2,v=values)
  for (j in 1:dim(perm)[1]){
    if (!(perm[j, 1] %in% confirmed | perm[j,2] %in% confirmed)){
      string1 <- na_to_text(as.character(output_h_suspects$Description[perm[j, 1]]))
      string2 <- na_to_text(as.character(output_h_suspects$Description[perm[j, 2]]))
      similarity <- stringsim(string1, string2)
      if (similarity > max_sim_tolerated){
        confirmed <- append(confirmed, perm[j, 1])
      }
    }
  }
}

keep <- subset(1:dim(output_h_suspects)[1], !(1:dim(output_h_suspects)[1] %in% confirmed))
output_h_suspects <- output_h_suspects %>% slice(keep) %>% select(head(names(output_h_suspects), -2))
output_h_uniques <- output_h_uniques %>% select(head(names(output_h_uniques), -1))
output_h <-  bind_rows(output_h_suspects, output_h_uniques)

# Pisos
output_p <- output_p %>%  add_count(Latitude, Longitude, RentM, name = "Suspects") 
output_p_uniques <- output_p %>% filter(Suspects == 1)
output_p_suspects <- output_p %>% filter(Suspects != 1)
output_p_suspects <- output_p_suspects %>% select(names(output_p_suspects)[-length(output_p_suspects)]) %>%
  add_count(Latitude, Longitude, RentM, name = "Suspects") %>% add_column(Cluster = NA)
uniques <- unique(output_p_suspects[,c(12,13,15)])

for (i in 1:dim(uniques)[1]){
  for (j in 1:dim(output_p_suspects)[1]){
    if (all(uniques[i,]==output_p_suspects[j,c(12,13,15)])){
      output_p_suspects$Cluster[j] <- i
    }
  }
}

confirmed <- c()
for (i in 1:max(output_p_suspects$Cluster)){
  values <- which(output_p_suspects$Cluster == i)
  perm <- permutations(n=length(values),r=2,v=values)
  for (j in 1:dim(perm)[1]){
    if (!(perm[j, 1] %in% confirmed | perm[j,2] %in% confirmed)){
      string1 <- na_to_text(as.character(output_p_suspects$Description[perm[j, 1]]))
      string2 <- na_to_text(as.character(output_p_suspects$Description[perm[j, 2]]))
      similarity <- stringsim(string1, string2)
      if (similarity > max_sim_tolerated){
        confirmed <- append(confirmed, perm[j, 1])
      }
    }
  }
}

keep <- subset(1:dim(output_p_suspects)[1], !(1:dim(output_p_suspects)[1] %in% confirmed))
output_p_suspects <- output_p_suspects %>% slice(keep) %>% select(head(names(output_p_suspects), -2))
output_p <- output_p_uniques %>% select(head(names(output_p_uniques), -1)) %>% bind_rows(output_p_suspects)

# Fotocasa
output_f <- output_f %>%  add_count(Latitude, Longitude, RentM, name = "Suspects") 
output_f_uniques <- output_f %>% filter(Suspects == 1)
output_f_suspects <- output_f %>% filter(Suspects != 1)
output_f_suspects <- output_f_suspects %>% select(names(output_f_suspects)[-length(output_f_suspects)]) %>%
  add_count(Latitude, Longitude, RentM, name = "Suspects") %>% add_column(Cluster = NA)
uniques <- unique(output_f_suspects[,c(10,11,13)])

for (i in 1:dim(uniques)[1]){
  for (j in 1:dim(output_f_suspects)[1]){
    if (all(uniques[i,]==output_f_suspects[j,c(10,11,13)])){
      output_f_suspects$Cluster[j] <- i
    }
  }
}

confirmed <- c()
for (i in 1:max(output_f_suspects$Cluster)){
  values <- which(output_f_suspects$Cluster == i)
  perm <- permutations(n=length(values),r=2,v=values)
  for (j in 1:dim(perm)[1]){
    if (!(perm[j, 1] %in% confirmed | perm[j,2] %in% confirmed)){
      string1 <- na_to_text(as.character(output_f_suspects$Description[perm[j, 1]]))
      string2 <- na_to_text(as.character(output_f_suspects$Description[perm[j, 2]]))
      similarity <- stringsim(string1, string2)
      if (similarity > max_sim_tolerated){
        confirmed <- append(confirmed, perm[j, 1])
      }
    }
  }
}

keep <- subset(1:dim(output_f_suspects)[1], !(1:dim(output_f_suspects)[1] %in% confirmed))
output_f_suspects <- output_f_suspects %>% slice(keep) %>% select(head(names(output_f_suspects), -2))
output_f <- output_f_uniques %>% select(head(names(output_f_uniques), -1)) %>% bind_rows(output_f_suspects)

seller <- function(x){
  x <- tolower(x)
  if (is.na(x)){
    return(NA)
  } else if (x == "professional"){
    return(1)
  } else{return(0)}
}
seller <- Vectorize(seller)

output_h <- output_h %>% mutate(Professional = seller(Seller), .keep = "unused")
output_f <- output_f %>% mutate(Professional = seller(Seller), .keep = "unused")
output_p <- output_p %>% mutate(Professional = seller(Seller), .keep = "unused")



### Remove all intermediary elements created in the process ###
to_remove <- ls()
data <- list(output_h = output_h, output_f = output_f, 
                   output_p = output_p, output_i = output_i)

rm(list = to_remove)
rm(to_remove)

save(data, file ="data.RData")
