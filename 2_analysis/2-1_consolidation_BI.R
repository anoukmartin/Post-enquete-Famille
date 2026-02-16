dir.create(path= "1_data/cleaned")

# Idem pour BI 
BI <- readRDS(file = "1_data/processed/BI.rds")
names(BI)
BI <- BI %>%
  select(-ends_with((".y")))
names(BI) <- str_remove_all(names(BI), "_x|\\.x|\\.y|_merge")

# Unifromisation des identifiant invifiduels 
BI <- BI  %>%
  mutate(interrogationId = str_sub(identifiant, 1, 9),
         identifiant = str_sub(identifiant, 10, -1))


## Brigitte et sandra qui n'ont pas répondu à l'EAR : 


tab <- BI[is.na(BI$sexe), ] # Brigitte et Sandra, on peut supposer que ce sont des femmes 

## Sandra et Brigitte
BI[BI$identifiant == "BRIGITTE 1959-06-15", c("sexe", "PRENOM.x", "NOMNAISSANCE")] <-c("Féminin", "Brigitte", "HANSER")
BI[BI$identifiant == "SANDRA 1972-03-26", c("sexe", "PRENOM.x", "NOMNAISSANCE")] <-c("Féminin", "Sandra", "CREBOIS")

#### Idem pour tempBI ###

namesBI_tech <- names(BI)[(str_detect(names(BI), "[[:upper:]]")) | str_detect(names(BI), "id|Id|cabfl|tableau")]
namesBI_tech
namesBI <- names(BI)[!(names(BI) %in% namesBI_tech)]

vars_BI <- readRDS(file = "1_data/processed/vars_EAR.rds")
head(vars_BI)
temp <- vars_BI %>% filter(Question == "Date de naissance" )

temp <- temp[rep(1, 3), ]
temp

temp$Variable <- c("jnai", "mnai", "anai")
temp$Question <- paste0(c("Jour", "Mois", "Année"), " de naissance")

vars_BI <- rbind(vars_BI, temp)
rm(temp)


vars_BI$Variable
temp <- vars_BI %>%
  filter(Variable == "nate_1\nnate_2")
temp <- temp[rep(1, 2), ]
temp$Variable <- c("nate_1", "nate_2")
temp$Question <- temp$Question %>% 
  str_remove_all('\"if (INAT1 = true and INAT2 = true) then \"') %>%
  str_remove_all('\" else \"')

vars_BI <- vars_BI %>%
  filter(Variable != "nate_1\nnate_2") %>%
  bind_rows(temp)

correspondances <-  trouver_correspondances(namesBI, 
                                            vars_BI$Variable)


saveRDS(vars_BI, "1_data/cleaned/vars_BI.rds")
saveRDS(BI, "1_data/cleaned/BI.rds")            
