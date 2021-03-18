


########################## Chargmeent des packages ###########################
library(shiny)
library(shinydashboard)

library(tidyverse)
library(gridExtra)
library(ggstance)
library(ggmap)
library(sf)
library(lwgeom)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(DT)
library(rAmCharts)
library(plotly)
library(crayon)
library(data.table)


######################### Importation des données ############################

movies_platforms = fread("data/movies_platforms.csv", encoding = "UTF-8")
tvshows_platforms = fread("data/tvshows_platforms.csv", encoding = "UTF-8")
netflix_titles = fread("data/netflix_titles.csv", encoding = "UTF-8")

########################## Nettoyage des data frames #########################

##### movies_platforms #####
movies_platforms = movies_platforms %>% select(-c(V1, ID))

##### tvshows_platforms #####
tvshows_platforms = tvshows_platforms %>%  select(-V1) %>% rename(Type = type)

##### netflix_titles #####
netflix_titles = netflix_titles %>% select(-show_id)

# Remplacement mois anglais par num puis format date
mois_num = c("01","02","03","04","05","06","07","08","09","10","11","12")
for (i in 1:12) {
  netflix_titles$date_added = gsub(month.name[i], mois_num[i], netflix_titles$date_added)
}

netflix_titles = netflix_titles %>% mutate(date_added = as.Date(date_added, format =
                                                                  "%m %d, %Y"))
# Conversion de date_added en objet date décomposition de cette date en 2 variables (year et month)
netflix_titles = netflix_titles %>% mutate(date_added = as.Date(date_added, format =
                                                                  "%B %d, %Y"))
netflix_titles = netflix_titles %>% mutate(year_added = strftime(date_added, "%Y"),
                                           month_added = strftime(date_added, "%m"))

# Décomposition de la variable duration en 2 variables (nombre saison et durée film en minute)
season_count = rep(NA, dim(netflix_titles)[1])
season_count[grepl("Season", netflix_titles$duration)] = str_split(netflix_titles$duration, " ", simplify = T)[grepl("Season", netflix_titles$duration), 1]
netflix_titles = netflix_titles %>% mutate(season_count = season_count)

duree = rep(NA, dim(netflix_titles)[1])
duree[!(grepl("Season", netflix_titles$duration))] = str_split(netflix_titles$duration, " ", simplify = T)[!(grepl("Season", netflix_titles$duration)), 1]
netflix_titles = netflix_titles %>% mutate(duration = duree)
rm(season_count)
rm(duree)

# Homogénisation des noms de pays (servira notament pour la partie cartographie)

netflix_titles$country = gsub("West Germany", "Germany", netflix_titles$country)
netflix_titles$country = gsub("East Germany", "Germany", netflix_titles$country)
netflix_titles$country = gsub("Russia", "Russian Federation", netflix_titles$country)
netflix_titles$country = gsub("Soviet Union", "Russian Federation", netflix_titles$country)
netflix_titles$country = gsub("South Korea", "Republic of Korea", netflix_titles$country)
netflix_titles$country = gsub("Vatican City", "Vatican", netflix_titles$country)


######################################################################################
##################### Traitement de la base netflix_titles ###########################
######################################################################################

##########################  Affichage de la table  #################################

DT_netflix_titles = DT::datatable(netflix_titles,options = list(scrollX = TRUE,scrollY = TRUE,pageLength = 10))

################## Nombre de films / séries en fonction de temps ###################
g1 = amPie(data.frame(label = as.factor(names(table(netflix_titles$type))), value = as.vector(table(netflix_titles$type))), main = "Breakdown of Netflix content by type",mainColor="white",theme="dark")

################## Nombre de films / séries en fonction de temps ###################
g2 <- function() {
  df_date = netflix_titles %>%
    filter(!is.na(year_added)) %>%
    group_by(year_added, type) %>%
    summarise(Count = n())
  g2 = ggplot(df_date) + 
    aes(x = year_added, y = Count, colour = type) + 
    geom_line(aes(group = type), size =1.5) + 
    geom_point(color = "black") + 
    xlab("Date") + 
    ylab("Number of Content") + 
    ggtitle("Amount of Netflix content") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="darkred", size=14, face="bold"),
      axis.title.y = element_text(color="darkred", size=14, face="bold"))
  
  g2 = ggplotly(g2,
                dynamicTicks = T,
                tooltip = c("Count", "year_added"))
  return(g2)
}

#################### Contenu ajouté par mois avec choix du genre ######################
liste_genre = sort(unique(unlist(
  str_split(netflix_titles$listed_in, ", ")
)))

g3 = function(chose_genre) {
  df_mois = netflix_titles %>% filter(!is.na(month_added) &
                                        grepl(chose_genre, listed_in)) %>% group_by(month_added) %>%  summarise(added_content_month = n())
  df_mois$month_added = factor(df_mois$month_added, labels = month.abb)
  return(
    amBarplot(
      x = "month_added",
      y = "added_content_month",
      data = df_mois,
      main = sprintf("Amount of content (%s) added on Netflix per month", chose_genre),
      mainColor="white",
      theme="dark"
    )
  )
}


####################### Distribution des écarts ajout sur Netflix - date de sortie #############################
g7 <- function() {
  df_ecart = netflix_titles %>% filter(!is.na(year_added)) %>% mutate(ecart = as.numeric(year_added) - release_year) %>%  select(ecart)
  g7 = ggplot(df_ecart) + 
    aes(x = ecart) + 
    geom_bar(colour = "coral2",fill = "coral1",alpha = 0.5) + 
    ggtitle("Distribution of the difference between added and release year") + 
    xlim(-1, 30) + 
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="darkred", size=14, face="bold"),
      axis.title.y = element_text(color="darkred", size=14, face="bold"))
  
  g7 = ggplotly(g7)
  return(g7)
}



############################### Contenu par pays #######################################

# Création du df contenant une ou plusieurs ligne pour chaque film (en fonction de combien de pays il y a pour le film)
countries = str_split(netflix_titles$country, ",")
for (i in 1:length(countries)) {
  countries[[i]] = trimws(countries[[i]])
}
df_countries = data.frame(
  type = rep(netflix_titles$type, sapply(countries, length)),
  genre = rep(netflix_titles$listed_in, sapply(countries, length)) ,
  country = unlist(countries)
)


# Création de la df aggregé en fonction des critères de choix : 2 variables : pays et count
df_countries_agg <- function(chose_type, chose_genre) {
  df_countries_agg = na.omit(df_countries) %>% filter((type == chose_type) &
                                                        (grepl(chose_genre, genre))) %>%  group_by(country) %>% summarise(count = n()) %>%  filter(!(country ==
                                                                                                                                                       ""))
  names(df_countries_agg) = c("pays", "n")
  return(df_countries_agg)
}


### Visualisation par les cartes de cette df aggrégée ###
# Création de la df_carte
world = ne_countries(scale = "medium", returnclass = "sf")
world = rename(world, pays = name_long)

# Carte
# Visualisation échelle classique
c1 <- function(chose_type, chose_genre) {
  # Création de la df_carte
  df_carte <- df_countries_agg(chose_type, chose_genre)
  # Spécification de la geometry de la df pour passer à la cartographie
  st_geometry(df_carte) = inner_join(df_carte, world, by = "pays")$geometry
  c1 = ggplot(world) + 
    geom_sf() +
    geom_sf(data = df_carte, aes(color = n, fill = n)) + 
    scale_fill_viridis_c(option = "viridis") + 
    scale_color_viridis_c(option = "viridis") + 
    theme_void()  # + 
    # ggtitle(sprintf("Content from different countries (%s, %s) : ", chose_type,chose_genre)) +
    # theme(
    #   plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    #   #plot.background = element_rect(fill = "black"),
    # )
  
  return(c1)
}

# Visualisation échelle sqrt
c2 <- function(chose_type, chose_genre) {
  df_carte = df_countries_agg(chose_type, chose_genre)
  st_geometry(df_carte) = inner_join(df_carte, world, by = "pays")$geometry
  c2 = ggplot(world) + 
    geom_sf() + 
    geom_sf(data = df_carte, aes(color = n, fill = n)) + 
    scale_fill_viridis_c(option = "viridis", trans = "sqrt") + 
    scale_color_viridis_c(option = "viridis", trans = "sqrt") + 
    theme_void() # +
    # ggtitle(sprintf("Content from different countries (%s, %s) - sqrt scale :",chose_type,chose_genre)) +
    # theme(
    #   plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    #   #plot.background = element_rect(fill = "black"),
    # )
  return(c2)
}



# Carte intéractive
c3 <- function(chose_type, chose_genre) {
  df_carte = df_countries_agg(chose_type, chose_genre)
  st_geometry(df_carte) = inner_join(df_carte, world, by = "pays")$geometry
  df_carte_interactive = st_transform(df_carte, crs = 4326)
  
  pal = colorNumeric(scales::seq_gradient_pal(
    low = "blue",
    high = "red",
    space = "Lab"
  ),
  domain = df_carte_interactive$n)
  c3 = leaflet() %>% addTiles() %>%
    addPolygons(
      data = df_carte_interactive,
      color =  ~ pal(n),
      fillOpacity = 0.6,
      stroke = TRUE,
      weight = 1,
      popup =  ~ paste(as.character(pays), ":", as.character(n), sep = " ")
    ) %>%
    addLayersControl(options = layersControlOptions(collapsed = FALSE))
  return(c3)
}




### Visualisation autre que par les cartes de cette df aggrégée ###
# Contenu des 10 pays ayant produit le plus de contenu présent dans Netflix en fonction du genre et du type choisi
g4 <- function(chose_type, chose_genre) {
  df_countries_agg = df_countries_agg(chose_type, chose_genre)
  g4 = amBarplot(
    x = "pays",
    y = "n",
    data = (df_countries_agg %>% arrange(desc(n)))[1:10, ],
    main = sprintf(
      "Amount of Netflix content (%s, %s) by countries (top 10)",
      chose_genre,
      chose_type
    ),
    horiz = T,
    mainColor="white",
    theme="dark"
  )
  return(g4)
}


# Graphe bulle du top 10 des pays ayant le plus de contenu sur Netflix
g14 <- function() {
  df_countries_agg = na.omit(df_countries) %>% group_by(country, type) %>% summarise(count = n()) %>%  filter(!(country ==
                                                                                                                  ""))
  df_countries_agg_long = spread(df_countries_agg, type, count)
  names(df_countries_agg_long)[2] = "Nb_Movies"
  names(df_countries_agg_long)[3] = "Nb_TVShows"
  df_countries_agg_long = df_countries_agg_long[order(desc(
    df_countries_agg_long$Nb_Movies + df_countries_agg_long$Nb_TVShows
  )), ]
  df_top_countries = df_countries_agg_long[1:10, ]
  rm(df_countries_agg_long)
  
  g14 = ggplot(df_top_countries,aes(Nb_Movies, Nb_TVShows, colour = country)) + 
    geom_point(size = 4) +
    xlab("Number of Movies") + 
    ylab("Number of TV Shows") +
    ggtitle("Amount of Netflix content by countries (top 10)") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="darkred", size=14, face="bold"),
      axis.title.y = element_text(color="darkred", size=14, face="bold"))
  
  g14 = ggplotly(g14, dynamicTicks = T)
  return(g14)
}


########################## Distribution de la durée des films #######################################
df_duree = netflix_titles %>% filter(!is.na(duration))
df_duree$duration = as.numeric(df_duree$duration)

min_realise_year = min(netflix_titles$release_year)
max_realise_year = max(netflix_titles$release_year)

# Histogramme avec densité avec possibilité de mettre un filtre en fonction de l'année qu'on veut, et du thème choisi
g5 <- function(chose_genre,chose_year){
  df_duree_filtre = df_duree %>% filter(release_year == chose_year & grepl(chose_genre,listed_in))
  g5 = ggplot(df_duree_filtre) + 
    aes(x = duration, y=..density..) + 
    geom_histogram(colour="coral2", fill="coral1", alpha=0.5, bins = 20) + 
    ggtitle(sprintf("Distribution of duration (%s, %i)",chose_genre, chose_year)) + 
    geom_density(colour="darkred",alpha=0.8) + 
    geom_vline(xintercept = mean(df_duree_filtre$duration),color="darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="darkred", size=14, face="bold"),
      axis.title.y = element_text(color="darkred", size=14, face="bold"))
  
  g5 = ggplotly(g5)
  return(g5)
}


# Histogramme sans densité (count) avec possibilité de mettre un filtre en fonction de l'année qu'on veut, et du thème choisi
g6 <- function(chose_genre,chose_year) {
  df_duree_filtre <-df_duree %>% filter(release_year == chose_year & grepl(chose_genre, listed_in))
  g6 = ggplot(df_duree_filtre) + 
    aes(x = duration) +
    geom_histogram( colour = "coral2", fill = "coral1", alpha = 0.5, bins = 20) + 
    ggtitle(sprintf("Distribution of duration (%s, %i)", chose_genre, chose_year)) +
    geom_vline(xintercept = mean(df_duree_filtre$duration), color = "darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="darkred", size=14, face="bold"),
      axis.title.y = element_text(color="darkred", size=14, face="bold"))
  
  g6 = ggplotly(g6)
  return(g6)
}

############################ Evolution de la durée moyenne en fonction des années ################################
df_duree_agg = df_duree %>% group_by(year_added) %>% summarise(mean_duration = mean(duration))
df_duree_agg$year_added = as.numeric(as.character(df_duree_agg$year_added))

g8 <- function() {
  g8 = ggplot(df_duree_agg)  + 
    aes(x = year_added, y = mean_duration) + 
    geom_line(size =1.5, colour = "coral1") + 
    geom_point() +
    xlab("Date") + 
    ylab("Duration means") + 
    ggtitle("Duration means evolution") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14,),
      axis.title.x = element_text(color="darkred", size=14, face="bold"),
      axis.title.y = element_text(color="darkred", size=14, face="bold"))
  
  g8 = ggplotly(g8, dynamicTicks = T)
  return(g8)
}

######################### Nombre de saisons pour les séries ###################################################
df_saisons = netflix_titles %>% filter(!is.na(season_count))
df_saisons$season_count = as.numeric(df_saisons$season_count)

g9 <- function() {
  g9 = ggplot(df_saisons) + 
    aes(x = season_count) + 
    geom_bar(colour="coral2", fill="coral1", alpha=0.5) + 
    ggtitle("Distribution of the number of TV shows seasons") + 
    ylab("") + 
    xlab("Number of seasons") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14,),
      axis.title.x = element_text(color="darkred", size=14, face="bold"))
  
  g9 = ggplotly(g9)
  return(g9)
}

#################### Top N des genres sur Netflix #############################################################
g11 <- function(length_g11) {
  N <- length_g11
  top = str_split(netflix_titles$listed_in, ", ")
  count_listed_in =  data.frame(type = rep(netflix_titles$type, sapply(top, length)),
                                listed_in = unlist(top))
  count_listed_in$listed_in = as.character(gsub(",", "", count_listed_in$listed_in))
  df_count_listed_in = count_listed_in %>% group_by(listed_in) %>% summarise(count = n()) %>% top_n(N)
  
  g11 = plot_ly(
    df_count_listed_in,
    x =  ~ listed_in,
    y =  ~ df_count_listed_in$count,
    type = "bar",
    color = "coral1",
    alpha = 0.8
  )
  g11 = g11 %>% layout(
    xaxis = list(
      categoryorder = "array",
      categoryarray = df_count_listed_in$listed_in,
      title = "Genre"
    ),
    yaxis = list(title = 'Count'),
    title = sprintf("Top %i genres on netflix", N),
    plot_bgcolor="grey",
    paper_bgcolor = "grey",
    font = list(color="white")
  )
  return(g11)
}


################## Top N directeurs sur Netflix ##################################################################
directors = str_split(netflix_titles$director, ", ")
df_directors = data.frame(
  type = rep(netflix_titles$type, sapply(directors, length)),
  country = rep(netflix_titles$country, sapply(directors, length)),
  director = unlist(directors)
)
df_directors$director = as.character(gsub(",", " ", df_directors$director))
df_directors = df_directors %>% na.omit() %>% filter(!(director == ""))

g12 <- function(length_g12,chose_type,chose_country) {
  N = length_g12
  df_directors_visu = df_directors %>% filter(type == chose_type &
                                                country == chose_country) %>%  group_by(director) %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(N)
  g12 = plot_ly(
    df_directors_visu,
    x =  ~ director,
    y =  ~ count,
    type = "bar",
    color = "coral1",
    alpha = 0.8
  )
  g12 = g12 %>% layout(
    xaxis = list(
      categoryorder = "array",
      categoryarray = df_directors_visu$director,
      title = "Director",
      color = "white"
    ),
    yaxis = list(title = 'Count',color = "white"),
    title =  sprintf( "Top %i directors on Netflix (%s, %s)", N, chose_type, chose_country),
    plot_bgcolor="black",
    paper_bgcolor = "black",
    font = list(color="white")
  )
  return(g12)
}
#g13(5,chose_type,chose_country)
# RShiny : possibilité de choisir N (5 à 20), type (Movie / TV Show) et pays (liste_countries)
liste_countries = sort(unique(unlist(countries)))

################## Top N acteurs sur Netflix ##################################################################

actors = str_split(netflix_titles$cast, ", ")
df_actors = data.frame(
  type = rep(netflix_titles$type, sapply(actors, length)),
  country = rep(netflix_titles$country, sapply(actors, length)),
  actor = unlist(actors)
)
df_actors$actor = as.character(gsub(",", " ", df_actors$actor))
df_actors = df_actors %>% na.omit() %>% filter(!(actor == ""))

#chose_type = "Movie"
#chose_country = "Brazil"


g13 <- function(length_g13, chose_type, chose_country) {
  N = length_g13
  df_actors_visu = df_actors %>% filter(type == chose_type &
                                          grepl(chose_country, country)) %>%  group_by(actor) %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(N)
  
  g13 = plot_ly(
    df_actors_visu,
    x =  ~ actor,
    y =  ~ count,
    type = "bar",
    color = "coral1",
    alpha = 0.8
  )
  g13 = g13 %>% layout(
    xaxis = list(
      categoryorder = "array",
      categoryarray = df_actors_visu$actor,
      title = "Actor"
    ),
    yaxis = list(title = 'Count'),
    title = sprintf("Top %i actors on Netflix (%s, %s)", N, chose_type, chose_country),
    plot_bgcolor="grey",
    paper_bgcolor = "grey",
    font = list(color="white")
  )
  return(g13)
}
# RShiny : possibilité de choisir N (5 à 20), type (Movie / TV Show) et pays (liste_countries)


############################################################################################################
##################### Traitement des tables tvshows_platforms et movies_platforms ##########################
############################################################################################################

# Création d'une df réunissant les tables movies_platforms et tvshows_platforms
vect_nom_var = colnames(movies_platforms)[colnames(movies_platforms) %in% colnames(tvshows_platforms)]
platforms = rbind(select(movies_platforms, vect_nom_var),
                  select(tvshows_platforms, vect_nom_var))

################################# Part de marché Netflix ###################################################

# Création de la df de travail

df_total_pf = platforms %>% summarise(
  Netlifx = sum(Netflix),
  Hulu = sum(Hulu),
  `Prime Video` = sum(`Prime Video`),
  `Disney+` = sum(`Disney+`)
)
df_movies_pf = movies_platforms %>% summarise(
  Netlifx = sum(Netflix),
  Hulu = sum(Hulu),
  `Prime Video` = sum(`Prime Video`),
  `Disney+` = sum(`Disney+`)
)
df_tvshows_pf = tvshows_platforms %>% summarise(
  Netlifx = sum(Netflix),
  Hulu = sum(Hulu),
  `Prime Video` = sum(`Prime Video`),
  `Disney+` = sum(`Disney+`)
)

df_pf = rbind(df_total_pf, df_movies_pf, df_tvshows_pf)
rownames(df_pf) = c("Total", "Movies", "TVShows")

# Total / TV Show / Movie
g15 <- function(type_g15) {
  title <- if(type_g15=="Total"){"Amount of content by platforms"
  }else if(type_g15 == "Movies"){"Amount of movies by platforms"
  }else if (type_g15 == "TVShows"){"Amount of TV shows by platforms"}
  
  g15 = ggplot(data.frame(Platform = colnames(df_pf), Nb_content = as.numeric(df_pf[type_g15, ]))) + 
    aes(x = Platform, y = Nb_content, color = Platform, fill = Platform) + 
    geom_bar(stat = "identity", alpha = 0.5) + 
    ggtitle(title) + 
    ylab("") + 
    xlab("Platforms") + 
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="darkred", size=14, face="bold"))
  
  g15 = ggplotly(g15, tooltip = c("Nb_content"))
  return(g15)
}



################ Barplot des années de sortie en fonction des platforms #####################################

df_sorties = platforms %>% group_by(Year) %>% summarise(
  Netflix = sum(Netflix),
  Hulu = sum(Hulu),
  `Prime Video` = sum(`Prime Video`),
  `Disney+` = sum(`Disney+`)
)

date_deb_aoc = min(df_sorties$Year)
date_fin_aoc = max(df_sorties$Year)+1


g18 <- function(date_deb,date_fin) {
  g18 = ggplot(df_sorties) + 
    aes(x = Year, y = Netflix) + 
    geom_bar(stat = "identity", alpha = 0.5, color = "coral1", fill = "coral3") + 
    ggtitle("Amount of Netflix content by released year") + 
    ylab("") + 
    xlab("Years") + 
    xlim(date_deb, date_fin) + 
    geom_vline(xintercept =  weighted.mean(df_sorties$Year, df_sorties$Netflix),color = "darkred",linetype = "dash")+
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", size=12, face="bold"),
      axis.title.y = element_text(color="darkred", size=12, face="bold"))
  
  g18 = ggplotly(g18)
  return(g18)
}


g19 <- function(date_deb,date_fin) {
  g19 = ggplot(df_sorties) + 
    aes(x = Year, y = Hulu) + 
    geom_bar(stat = "identity",alpha = 0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Amount of Hulu content by released year") + 
    ylab("") + 
    xlab("Years") +
    xlim(date_deb, date_fin) + 
    geom_vline(xintercept =  weighted.mean(df_sorties$Year, df_sorties$Hulu), color = "darkred",linetype = "dash")+
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", size=12, face="bold"),
      axis.title.y = element_text(color="darkred", size=12, face="bold"))
  
  g19 = ggplotly(g19)
  return(g19)
}

g20 <- function(date_deb,date_fin) {
  g20 = ggplot(df_sorties) + 
    aes(x = Year, y = `Prime Video`) + 
    geom_bar(stat = "identity", alpha = 0.5, color = "coral1", fill = "coral3") + 
    ggtitle("Amount of Prime Video content by released year") + 
    ylab("") + 
    xlab("Years") + 
    xlim(date_deb, date_fin) + 
    geom_vline(xintercept =  weighted.mean(df_sorties$Year, df_sorties$`Prime Video`),color = "darkred", linetype = "dash")+
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", size=12, face="bold"),
      axis.title.y = element_text(color="darkred", size=12, face="bold"))
  
  g20 = ggplotly(g20)
  return(g20)
}

g21 <- function(date_deb,date_fin) {
  g21 = ggplot(df_sorties) + 
    aes(x = Year, y = `Disney+`) + 
    geom_bar(stat = "identity",alpha = 0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Amount of Disney+ content by released year") + 
    ylab("") + 
    xlab("Years") + 
    xlim(date_deb, date_fin) + 
    geom_vline( xintercept =  weighted.mean(df_sorties$Year, df_sorties$`Disney+`),color = "darkred",linetype = "dash") + 
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", size=12, face="bold"),
      axis.title.y = element_text(color="darkred", size=12, face="bold"))
  
  g21 = ggplotly(g21)
  return(g21)
}




################ Histogramme des Rotten Tomatoes en fonction des platforms #####################################

rating = platforms %>%  filter(!is.na(`Rotten Tomatoes`) & !is.na(IMDb) & `Rotten Tomatoes` != "" & IMDb != "") %>% 
  mutate(`Rotten Tomatoes` = as.numeric(unlist(str_split(`Rotten Tomatoes` ,"%"))[unlist(str_split(`Rotten Tomatoes` ,"%")) != ""]))

df_netflix = rating %>% filter(Netflix==1)
df_hulu = rating %>% filter(Hulu==1)
df_pv = rating %>% filter(`Prime Video`==1)
df_disney = rating %>% filter(`Disney+`==1)

g22 <- function() {
  df_netflix = rating %>% filter(Netflix==1)
  g22 = ggplot(df_netflix) + 
    aes(x = `Rotten Tomatoes`) + 
    geom_histogram(alpha =0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Distribution of Rotten Tomatoes of Netflix content") + 
    ylab("") + 
    xlab("Rotten Tomatoes") + 
    geom_vline(xintercept =  mean(df_netflix$`Rotten Tomatoes`),color = "darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g22 = ggplotly(g22)
  return(g22)
}



g23 <- function() {
  df_hulu = rating %>% filter(Hulu==1)
  g23 = ggplot(df_hulu) + 
    aes(x = `Rotten Tomatoes`) + 
    geom_histogram(alpha =0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Distribution of Rotten Tomatoes of Hulu content") + 
    ylab("") + 
    xlab("Rotten Tomatoes") + 
    geom_vline(xintercept =  mean(df_hulu$`Rotten Tomatoes`),color = "darkred",linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g23 = ggplotly(g23)
  return(g23)
}


g24 <- function() {
  df_pv = rating %>% filter(`Prime Video`==1)
  g24 = ggplot(df_pv) + 
    aes(x = `Rotten Tomatoes`) + 
    geom_histogram(alpha = 0.5, color = "coral1", fill = "coral3") + 
    ggtitle("Distribution of Rotten Tomatoes of Prime Video content") + 
    ylab("") + 
    xlab("Rotten Tomatoes") + 
    geom_vline(xintercept =  mean(df_pv$`Rotten Tomatoes`), color = "darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g24 = ggplotly(g24)
  return(g24)
}

g25 <- function() {
  df_disney = rating %>% filter(`Disney+`==1)
  g25 = ggplot(df_disney) + 
    aes(x = `Rotten Tomatoes`) + 
    geom_histogram(alpha =0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Distribution of Rotten Tomatoes of Disney+ content") + 
    ylab("") + xlab("Rotten Tomatoes") + 
    geom_vline(xintercept =  mean(df_disney$`Rotten Tomatoes`),color = "darkred",linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g25 = ggplotly(g25)
  return(g25)
}


################ Histogramme des note IMDb en fonction des platforms #####################################

g26 <- function() {
  df_netflix = rating %>% filter(Netflix==1)
  g26 = ggplot(df_netflix) +
    aes(x = IMDb) + 
    geom_histogram(alpha = 0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Distribution of IMDb of Netflix content") + 
    ylab("") + 
    xlab("IMDb") + 
    geom_vline(xintercept =  mean(df_netflix$IMDb),color = "darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g26 = ggplotly(g26)
  return(g26)
}


g27 <- function() {
  df_hulu = rating %>% filter(Hulu==1)
  g27 = ggplot(df_hulu) + 
    aes(x = IMDb) + 
    geom_histogram(alpha = 0.5, color = "coral1", fill = "coral3") + 
    ggtitle("Distribution of IMDb of Hulu content") + 
    ylab("") + 
    xlab("IMDb") + 
    geom_vline(xintercept =  mean(df_hulu$IMDb),color = "darkred",linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g27 = ggplotly(g27)
  return(g27)
}

g28 <- function() {
  df_pv = rating %>% filter(`Prime Video`==1)
  g28 = ggplot(df_pv) + 
    aes(x = IMDb) + 
    geom_histogram(alpha = 0.5, color = "coral1",fill = "coral3") + 
    ggtitle("Distribution of IMDb of Prime Video content") + 
    ylab("") + 
    xlab("IMDb") + 
    geom_vline(xintercept =  mean(df_pv$IMDb),color = "darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g28 = ggplotly(g28)
  return(g28)
}

g29 <- function() {
  df_disney = rating %>% filter(`Disney+`==1)
  g29 = ggplot(df_disney) + 
    aes(x = IMDb) + 
    geom_histogram(alpha = 0.5,color = "coral1",fill = "coral3") + 
    ggtitle("Distribution of IMDb of Disney+ content") + ylab("") + 
    xlab("IMDb") + 
    geom_vline(xintercept =  mean(df_disney$IMDb), color = "darkred", linetype = "dash") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", face="bold"),
      axis.title.x = element_text(color="darkred", face="bold"),
      axis.title.y = element_text(color="darkred", face="bold"))
  
  g29 = ggplotly(g29)
  return(g29)
}

################################ Régression tomatoes / imbd ##################################################

r1 <- function() {
  r1 = ggplot(rating) + 
    aes(x = `Rotten Tomatoes`, y = IMDb) + 
    geom_point() +
    geom_smooth(method = "lm") + 
    ggtitle("Linear regression between Rotten Tomatoes and IMDb rating") +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=12, face="bold"),
      axis.title.x = element_text(color="darkred", size=12, face="bold"),
      axis.title.y = element_text(color="darkred", size=12, face="bold"))
  
  
  r1 = ggplotly(r1)
  return(r1)
}
#summary(lm(IMDb~`Rotten Tomatoes`,data=rating))

################################ Régression tomatoes / year ##################################################
r2 <- function(rating_method) {
  if(rating_method == "IMDb"){
    r2 = ggplot(rating) + 
      aes(x = Year, y = IMDb) +
      geom_point() + 
      geom_smooth(method ="lm") + 
      ggtitle("Linear regression between IMDb rating and year released")
  }
  else if (rating_method == "Rotten Tomatoes"){
    r2 = ggplot(rating) + 
      aes(x = Year, y = `Rotten Tomatoes`) + 
      geom_point() +
      geom_smooth(method = "lm") + 
      ggtitle("Linear regression between RT rating and year released")
  }
  r2 = r2 +     
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color="red", size=12, face="bold"),
      axis.title.x = element_text(color="darkred", size=12, face="bold"),
      axis.title.y = element_text(color="darkred", size=12, face="bold"))
  
  r2 = ggplotly(r2)
  return(r2)
}



################### Search Engine ################
title_list = platforms$Title
min_year_SE = min(platforms$Year)
max_year_SE = max(platforms$Year)

search_engine <- function(chose_title,chose_type,chose_year_min,chose_year_max){
  search_engine = platforms[grepl(tolower(chose_title),tolower(platforms$Title)),]
  search_engine = search_engine %>% filter(Year >= chose_year_min & Year <= chose_year_max & Type==chose_type)
  return(DT::datatable(search_engine,options=list(scrollX= TRUE,scrollY=TRUE,pageLength=10)))
}

#search_engine("Inception","Movie",1910,2021)


######################## Top Directors / Genres / Countries en fonction des platforms #####################

# Netflix
df_netflix = movies_platforms %>% filter(Netflix == 1)
top = str_split(df_netflix$Directors, ",")
count =  data.frame(type = rep(df_netflix$Title, sapply(top, length)),
                    Directors = unlist(top))
top_directors = (
  count %>% group_by(Directors) %>% summarise(Number = n()) %>% filter(Directors != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Directors)
)[[1]]

top = str_split(df_netflix$Country, ",")
count =  data.frame(type = rep(df_netflix$Title, sapply(top, length)),
                    Countries = unlist(top))
top_countries = (
  count %>% group_by(Countries) %>% summarise(Number = n()) %>% filter(Countries != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Countries)
)[[1]]

top = str_split(df_netflix$Genres, ",")
count =  data.frame(type = rep(df_netflix$Title, sapply(top, length)),
                    Genres = unlist(top))
top_genres = (
  count %>% group_by(Genres) %>% summarise(Number = n()) %>% filter(Genres != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Genres)
)[[1]]

# Table des TOP 20
df_top_netflix = data.frame(Directors = top_directors,
                            Countries = top_countries,
                            Genres = top_genres)
df_top_netflix = DT::datatable(df_top_netflix, options = list(scrollX = TRUE, scrollY =
                                                                TRUE))

# Hulu

df_hulu = movies_platforms %>% filter(Hulu == 1)
top = str_split(df_hulu$Directors, ",")
count =  data.frame(type = rep(df_hulu$Title, sapply(top, length)),
                    Directors = unlist(top))
top_directors = (
  count %>% group_by(Directors) %>% summarise(Number = n()) %>% filter(Directors != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Directors)
)[[1]]

top = str_split(df_hulu$Country, ",")
count =  data.frame(type = rep(df_hulu$Title, sapply(top, length)),
                    Countries = unlist(top))
top_countries = (
  count %>% group_by(Countries) %>% summarise(Number = n()) %>% filter(Countries != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Countries)
)[[1]]

top = str_split(df_hulu$Genres, ",")
count =  data.frame(type = rep(df_hulu$Title, sapply(top, length)),
                    Genres = unlist(top))
top_genres = (
  count %>% group_by(Genres) %>% summarise(Number = n()) %>% filter(Genres != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Genres)
)[[1]]

# Table des TOP 20
df_top_hulu = data.frame(Directors = top_directors,
                         Countries = top_countries,
                         Genres = top_genres)
df_top_hulu = DT::datatable(df_top_hulu, options = list(scrollX = TRUE, scrollY =
                                                          TRUE))

# PV

df_pv = movies_platforms %>% filter(`Prime Video` == 1)
top = str_split(df_pv$Directors, ",")
count =  data.frame(type = rep(df_pv$Title, sapply(top, length)),
                    Directors = unlist(top))
top_directors = (
  count %>% group_by(Directors) %>% summarise(Number = n()) %>% filter(Directors != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Directors)
)[[1]]

top = str_split(df_pv$Country, ",")
count =  data.frame(type = rep(df_pv$Title, sapply(top, length)),
                    Countries = unlist(top))
top_countries = (
  count %>% group_by(Countries) %>% summarise(Number = n()) %>% filter(Countries != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Countries)
)[[1]]

top = str_split(df_pv$Genres, ",")
count =  data.frame(type = rep(df_pv$Title, sapply(top, length)),
                    Genres = unlist(top))
top_genres = (
  count %>% group_by(Genres) %>% summarise(Number = n()) %>% filter(Genres != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Genres)
)[[1]]

# Table des TOP 20
df_top_pv = data.frame(Directors = top_directors,
                       Countries = top_countries,
                       Genres = top_genres)
df_top_pv = DT::datatable(df_top_pv, options = list(scrollX = TRUE, scrollY = TRUE))


# Disney

df_dis = movies_platforms %>% filter(`Disney+` == 1)
top = str_split(df_dis$Directors, ",")
count =  data.frame(type = rep(df_dis$Title, sapply(top, length)),
                    Directors = unlist(top))
top_directors = (
  count %>% group_by(Directors) %>% summarise(Number = n()) %>% filter(Directors != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Directors)
)[[1]]

top = str_split(df_dis$Country, ",")
count =  data.frame(type = rep(df_dis$Title, sapply(top, length)),
                    Countries = unlist(top))
top_countries = (
  count %>% group_by(Countries) %>% summarise(Number = n()) %>% filter(Countries != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Countries)
)[[1]]

top = str_split(df_dis$Genres, ",")
count =  data.frame(type = rep(df_dis$Title, sapply(top, length)),
                    Genres = unlist(top))
top_genres = (
  count %>% group_by(Genres) %>% summarise(Number = n()) %>% filter(Genres != "") %>% arrange(desc(Number)) %>% slice(1:20) %>% select(Genres)
)[[1]]

# Table des TOP 20
df_top_dis = data.frame(Directors = top_directors,
                        Countries = top_countries,
                        Genres = top_genres)
df_top_dis = DT::datatable(df_top_dis, options = list(scrollX = TRUE, scrollY = TRUE))

# RShiny : afficher les 4 tables df_top_pv, df_top_dis, df_top_hulu, df_top_netflix soit les uns sous les autres, soit des sous onglets




