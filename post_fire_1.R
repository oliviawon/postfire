library(tidyverse)
library(readxl)

transect_1A <- as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "1A", range = "B5:V25"))
#load data
transect_1A[is.na(transect_1A)] = 0 
view(transect_1A)
#replace NA with 0
transect_1A_long <- transect_1A %>%
  pivot_longer(c("*Festuca bromoides",	"*Geranium dissectum",	
                 "*Lysimachia arvensis",	"*Bromus hordaceus",
                 "*Avena barbata",	"*Vicia sativa",	"*Sherardia arvensis",
                 "*Dipsacus sativus",	"Melica californica",	"Stipa pulchra",
                 "*Erodium cicutarium",	"*Cardamine hirsuta",	"*Festuca perennis",
                 "Baccharis pilularis",	"Stipa lepida",	"*Bromus diandrus",
                 "Epilobium brachycarpum",	"*Bromus madritensis",
                 "*Phalaris aquatica",	"Bare ground"), names_to = "Species", values_to = "Cover_Class")
#assign new mean percent cover values for analysis

transect_1A_long <- mutate(transect_1A_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                ,Cover_Class == 2 ~ 8.75
                                ,Cover_Class == 3 ~ 18.75
                                ,Cover_Class == 4 ~ 37.5
                                ,Cover_Class == 5 ~ 62.5
                                ,Cover_Class == 6 ~ 87.5
                                ,TRUE ~ 0), Transect= "1A") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
#add cover class value, new transect column, rename column

transect_1A_long$Species=gsub("\\*","",transect_1A_long$Species)
#remove asterisk from Species names

transect_dat <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects_avgperc.xlsx", sheet= "Transect_Information", range = "A1:B15"))
#read in species data

species_dat <- as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects_avgperc.xlsx", sheet= "Species_List", range = "A1:C77"))
#read in species data

view(species_dat)
transect1A_species_dat <- left_join(transect_1A_long, species_dat, by= c("Species"))
#join with data about species class

t1A_perc_cover_mean <- transect1A_species_dat %>%
  group_by(Quadrat, Class)%>%
  summarise(percent_cover_mean = mean(Percent_Cover)) %>%
  ungroup() %>%
  replace_na(list(Class= "Bare Ground"))
#create dataframe with mean % cover for each quadrat

ggplot(t1A_perc_cover_mean, aes(Quadrat, percent_cover_mean, fill=Class)) +
  geom_area()+
  scale_fill_viridis(discrete = TRUE)
#plot how average % cover changes across transect (for transition transects)

t1A_species_tot <- transect1A_species_dat %>%
  filter(Percent_Cover > 0) %>%
  count(Quadrat, Class) %>%
  replace_na(list(Class= "Bare Ground"))
#get tally of native vs. non-native species per quadrat

t1Asumm <- left_join(t1A_perc_cover_mean, t1A_species_tot, by= c("Quadrat", "Class"))%>%
  replace_na(list(n= 0))
#join dfs

ggplot(t1Asumm, aes(fill=Class, y=n, x=Quadrat))+
  geom_bar(position="stack", stat="identity")

t1Asumm %>%
  ggplot(aes(x= Quadrat, y=n, fill=Class))+
  geom_area()+
  ylab("Observance Count")
#graph showing changing proportions of native vs non-native species over transect

# READING IN REST OF DATA
#3E Burned Grassland
transect_3e <- as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "3E", range = "B4:AB24"))
transect_3e[is.na(transect_3e)] = 0 
transect_3e_long <- transect_3e %>%
  pivot_longer(c("Acmispon wrangelianus",	"Dipterostemon capitatus",	"*Avena barbata",
                 "Eschscholzia californica",	"Eriogonum nudum",	"*Festuca perennis",
                 "Chlorogalum pomeridianum",	"Melica californica",	"Calystegia subacaulis",	
                 "Koeleria macrantha",	"Corethrogyne filaginifolia",	"Crassula connata",	
                 "Claytonia perfoliata",	"Polypodium californicum",	"Astragalus gambelianus",
                 "Stipa pulchra",	"Achillea millefolium",	"Calindrinia menziesii",	"Gilia clivorum",
                 "*Euphorbia peplus",	"*Lactuca serriola",	"*Avena fatua",	"Stipa lepida",	"*Centaurea solstitialis",
                 "Elymus glaucus",	"Elymus multisetus"), names_to = "Species", values_to = "Cover_Class")
transect_3e_long <- mutate(transect_3e_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "3E") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") %>%
  filter(Percent_Cover>0)
#add cover class value, new transect column, rename column

transect_3e_long$Species=gsub("\\*","",transect_3e_long$Species)

#READ in 5A
transect_5a <- as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "5A", range = "B4:Z24"))
transect_5a[is.na(transect_5a)] = 0 
transect_5a_long <- transect_5a %>%
  pivot_longer(c("Stipa pulchra",	"Layia platyglossa",	"*Festuca perennis",
                 "Muilla maritima",	"*Avena barbata",	"*Euphorbia peplus",
                 "Dipterostemon capitatus",	"Delphinium variegatum",	"Eschscholzia californica",
                 "Plantago erecta",	"Eriogonum nudum",	"Cirsium quercetorum",	"Astragalus gambelianus",
                 "*Bromus hordaceus",	"Clarkia purpurea ssp. quadrivulnera",	"*Festuca bromoides",
                 "Acmispon wrangelianus",	"Gilia clivorum",	"Sanicula bipinnatifida",	
                 "Epilobium brachycarpum",	"Lessingia arachnoidea",	"*Lactuca serriola",	"Stipa lepida",
                 "*Centaurea solstitialis"), names_to = "Species", values_to = "Cover_Class")
transect_5a_long <- mutate(transect_5a_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "5A") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") %>%
  filter(Percent_Cover>0)
#add cover class value, new transect column, rename column

transect_5a_long$Species=gsub("\\*","",transect_5a_long$Species)

#Bind two transect datasets
transect_3e5a <- bind_rows(transect_3e_long, transect_5a_long, .id = "Dataset")
transect3e5a_species_dat <- left_join(transect_3e5a, species_dat, by= c("Species")) 
  
#join with data about species class
t3e5a_perc_cover_mean <- transect3e5a_species_dat %>%
  group_by(Transect, Class)%>%
  summarise(percent_cover_mean = mean(Percent_Cover)) %>%
  ungroup() %>%
  replace_na(list(Class= "Bare Ground"))
#create dataframe with mean % cover for each trasect

ggplot(t3e5a_perc_cover_mean, aes(fill=Class, y=percent_cover_mean, x=Transect))+
  geom_bar(position="dodge", stat="identity")
#bar graph showing diff proportions of native vs non native across burn/not burn transects

#TO DO: load rest of data, calculate mean % cover for each transect with species richness

summary_3e5a <- group_by(transect3e5a_species_dat, Transect) %>%
  summarise(
    count = n(),
    mean_perc = mean(Percent_Cover, na.rm = TRUE),
    sd_perc = sd(Percent_Cover, na.rm = TRUE))

res <- t.test(Percent_Cover ~ Transect, data = transect3e5a_species_dat, var.equal = TRUE)

#read in other data sheets
#1B
transect_1b <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "1B", range = "B4:X24"))
#load data
transect_1b[is.na(transect_1b)] = 0 
view(transect_1b)
#replace NA with 0
transect_1b_long <- transect_1b %>%
  pivot_longer(c("*Avena barbata", "*Bromus hordaceus",	"Stipa pulchra",	
  "Melica californica",	"*Lysimachia arvensis",	"*Geranium dissectum",	"*Erodium cicutarium",
  "*Vicia sativa",	"*Festuca bromoides",	"*Sonchus oleraceus",	"*Hypochaeris glabra",
  "*Logfia gallica",	"Baccharis pilularis",	"Toxicodendron diversilobum",	"Sisyrinchium bellum",
  "Chlorogalum pomeridianum",	"*Dipsacus sativus",	"Stipa lepida",	"Brodiaea elegans",	"*Avena fatua",
  "Epilobium brachycarpum",	"Bare ground"), names_to = "Species", values_to = "Cover_Class")
#assign new mean percent cover values for analysis

transect_1b_long <- mutate(transect_1b_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "1B") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
#add cover class value, new transect column, rename column

transect_1b_long$Species=gsub("\\*","",transect_1b_long$Species)

#2A
transect_2a <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "2A", range = "B4:W24"))
#load data
transect_2a[is.na(transect_2a)] = 0 
#replace NA with 0
transect_2a_long <- transect_2a %>%
  pivot_longer(c("*Dipsacus sativus",	"*Sherardia arvensis",	"Baccharis pilularis",
                 "*Trifolium campestre",	"*Lysimachia arvensis",	"*Geranium dissectum",
                 "Dipterostemon capitatus",	"*Erodium cicutarium",	"*Logfia gallica",	
                 "*Avena barbata",	"*Bromus hordaceus",	"*Hypochaeris glabra",	"*Sonchus oleraceus",
                 "Toxicodendron diversilobum",	"Rubus ursinus",	"*Briza minor",	"Epilobium brachycarpum",
                 "Stipa lepida",	"*Centaurea solstitialis",	"*Bellardia trixago", "Bare ground"), names_to = "Species", values_to = "Cover_Class")
#assign new mean percent cover values for analysis

transect_2a_long <- mutate(transect_2a_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "2A") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
#add cover class value, new transect column, rename column

transect_2a_long$Species=gsub("\\*","",transect_2a_long$Species)

#2B
transect_2b <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "2B", range = "B4:X24"))
#load data
transect_2b[is.na(transect_2b)] = 0 
#replace NA with 0
transect_2b_long <- transect_2b %>%
  pivot_longer(c("*Vicia sativa",	"*Dipsacus sativus",	"*Geranium dissectum",	
                 "*Avena barbata",	"*Sherardia arvensis",	"*Festuca perennis",
                 "Dipterostemon capitatus",	"Stipa pulchra",	"Rubus ursinus",
                 "Toxicodendron diversilobum",	"Chlorogalum pomeridianum",	
                 "Quercus agrifolia",	"*Lysimachia arvensis",	"Acmispon wrangelianus",
                 "*Briza minor",	"*Erodium cicutarium",	"*Hypochaeris glabra",
                 "*Bromus diandrus",	"*Festuca bromoides",	"Stipa lepida",	"*Centaurea solstitialis",
                 "Melica californica"), names_to = "Species", values_to = "Cover_Class")
#assign new mean percent cover values for analysis

transect_2b_long <- mutate(transect_2b_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "2B") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
#add cover class value, new transect column, rename column

transect_2b_long$Species=gsub("\\*","",transect_2b_long$Species)

#3A
transect_3a <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "3A", range = "B4:S24"))
#load data
transect_3a[is.na(transect_3a)] = 0 
#replace NA with 0
transect_3a_long <- transect_3a %>%
  pivot_longer(c("Baccharis pilularis",	"*Lysimachia arvensis",	"Rubus ursinus",
                 "Toxicodendron diversilobum",	"*Dipsacus sativus",	"*Centaurea solstitialis",
                 "*Avena barbata",	"*Briza minor",	"Stipa lepida",	"*Brachypodium distachyon",
                 "*Gastridium phleoides",	"*Bellardia trixago",	"*Bromus hordaceus",	"*Avena fatua",
                 "*Bromus madritensis",	"*Sonchus oleraceus",	"*Logfia gallica"), names_to = "Species", values_to = "Cover_Class")
#assign new mean percent cover values for analysis

transect_3a_long <- mutate(transect_3a_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "3A") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
#add cover class value, new transect column, rename column

transect_3a_long$Species=gsub("\\*","",transect_3a_long$Species)

#3B
transect_3b <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "3B", range = "B4:X24"))
#load data
transect_3b[is.na(transect_3b)] = 0 
#replace NA with 0
transect_3b_long <- transect_3b %>%
  pivot_longer(c("Toxicodendron diversilobum",	"Rubus ursinus",	"*Lysimachia arvensis",	
                 "*Dipsacus sativus",	"Sanicula crassicaulis", "*Medicago lupulina",	"*Cirsium vulgare",
                 "Quercus agrifolia",	"Chlorogalum pomeridianum",	"Baccharis pilularis",	"Symphyotrichum chilense",	
                 "Stipa lepida", "Brachypodium distachyon", "Stachys bullata",	"*Briza minor", "*Bromus madritensis",
                 "*Avena barbata",	"Epilobium brachycarpum",	"*Sonchus oleraceus",	"*Bromus hordaceus",	
                 "Sambucus nigra ssp. caerulea",	"Bare ground"), names_to = "Species", values_to = "Cover_Class")
#assign new mean percent cover values for analysis

transect_3b_long <- mutate(transect_3b_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "3B") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
#add cover class value, new transect column, rename column

transect_3b_long$Species=gsub("\\*","",transect_3b_long$Species)

#3C
transect_3c <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "3C", range = "B4:AD24"))
transect_3c[is.na(transect_3c)] = 0 
transect_3c_long <- transect_3c %>%
  pivot_longer(c("Sanicula crassicaulis",	"Rubus ursinus",	"*Lysimachia arvensis",	"Baccharis pilularis",
                 "Toxicodendron diversilobum",	"*Avena barbata",	"Chlorogalum pomeridianum",	"Dipterostemon capitatus",
                 "*Festuca perennis",	"*Dipsacus sativus",	"*Euphorbia peplus",	"Micropus californicus",
                 "*Erodium cicutarium",	"Astragalus gambelianus",	"Melica californica",	"Calindrinia menziesii",	
                 "Sisyrinchium bellum",	"Plantago erecta",	"Stipa lepida",	"*Briza minor",	"*Brachypodium distachyon",
                 "*Sonchus oleraceus", "*Lactuca serriola",	"*Cirsium vulgare",	"*Bromus hordaceus",
                 "*Avena fatua",	"*Centaurea solstitialis",	"Hemizonia congesta ssp. luzulifolia"), names_to = "Species", values_to = "Cover_Class")
transect_3c_long <- mutate(transect_3c_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "3C") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
transect_3c_long$Species=gsub("\\*","",transect_3c_long$Species)

#3D
transect_3d <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "3D", range = "B4:AE24"))
transect_3d[is.na(transect_3d)] = 0 
transect_3d_long <- transect_3d %>%
  pivot_longer(c("*Lysimachia arvensis",	"*Dipsacus sativus",	"Rubus ursinus",
                 "*Cirsium vulgare",	"*Avena barbata",	"*Geranium dissectum",
                 "*Foeniculum vulgare",	"*Festuca perennis",	"Chlorogalum pomeridianum",
                 "*Euphorbia peplus",	"Calindrinia menziesii",	"Dipterostemon capitatus",
                 "Astragalus gambelianus",	"Claytonia perfoliata",	"Melica californica",
                 "Eschscholzia californica",	"*Medicago",	"Eriogonum nudum",	"Plantago erecta",
                 "Stipa lepida",	"*Sonchus oleraceus",	"*Bromus madritensis",	"*Avena fatua",	"*Brachypodium distachyon",
                 "*Festuca bromoides",	"*Bromus hordaceus",	"Epilobium brachycarpum",
                 "*Centaurea solstitialis",	"Hemizonia congesta ssp. luzulifolia"), names_to = "Species", values_to = "Cover_Class")
transect_3d_long <- mutate(transect_3d_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "3D") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
transect_3d_long$Species=gsub("\\*","",transect_3d_long$Species)

#3F
transect_3f <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "3F", range = "B4:X24"))
transect_3f[is.na(transect_3f)] = 0 
transect_3f_long <- transect_3f %>%
  pivot_longer(c("Claytonia perfoliata",	"Melica imperfecta",	"*Avena barbata",	"Calindrinia menziesii",	
  "*Festuca perennis",	"*Euphorbia peplus",	"Achillea millefolium",	"Chlorogalum pomeridianum",
  "Dipterostemon capitatus", "Plantago erecta",	"Astragalus gambelianus",	"Muilla maritima",
  "*Hypochaeris glabra",	"Stipa pulchra", "*Lysimachia arvensis",	"Triteleia laxa",
  "Stipa lepida",	"Hemizonia congesta ssp. luzulifolia",	"*Centaurea solstitialis",	"Elymus multisetus",
  "Danthonia californica",	"*Avena fatua"), names_to = "Species", values_to = "Cover_Class")
transect_3f_long <- mutate(transect_3f_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "3F") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
transect_3f_long$Species=gsub("\\*","",transect_3f_long$Species)

#4A
transect_4a <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "4A", range = "B4:Z24"))
transect_4a[is.na(transect_4a)] = 0 
transect_4a_long <- transect_4a %>%
  pivot_longer(c("Eschscholzia californica",	"*Festuca perennis",	"Cirsium quercetorum",	
                 "Dipterostemon capitatus",	"Acmispon wrangelianus",	"Eriogonum nudum",
                 "*Euphorbia peplus",	"Calindrinia menziesii",	"Astragalus gambelianus",
                 "*Avena barbata",	"Plantago erecta",	"Trifolium ciliolatum",	"Stipa pulchra",
                 "Claytonia perfoliata",	"Melica californica",	"Gilia clivorum",	"Stipa lepida",
                 "*Festuca bromoides",	"Elymus multisetus",	"*Centaurea solstitialis",	
                 "Lessingia arachnoidea",	"*Lactuca serriola",	"Koeleria macrantha",	
                 "Epilobium brachycarpum"), names_to = "Species", values_to = "Cover_Class")
transect_4a_long <- mutate(transect_4a_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "4A") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
transect_4a_long$Species=gsub("\\*","",transect_4a_long$Species)

#4B
transect_4b <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "4B", range = "B4:AB24"))
transect_4b[is.na(transect_4b)] = 0 
transect_4b_long <- transect_4b %>%
  pivot_longer(c("*Euphorbia peplus",	"Plantago erecta",	"*Holcus lanatus",	
                 "*Festuca perennis",	"*Avena barbata",	"Dipterostemon capitatus",	
                 "Eschscholzia californica",	"Calindrinia menziesii",	"Claytonia perfoliata",
                 "Achillea millefolium",	"Acmispon wrangelianus",	"Stipa pulchra",
                 "Astragalus gambelianus",	"Muilla maritima",	"Melica californica",	
                 "Delphinium variegatum",	"Stipa lepida",	"Hemizonia congesta ssp. luzulifolia",
                 "Elymus multisetus",	"*Centaurea solstitialis",	"Epilobium brachycarpum",	
                 "*Lactuca serriola",	"*Bromus hordaceus",	"Lessingia arachnoidea",
                 "*Avena fatua",	"Triteleia laxa"), names_to = "Species", values_to = "Cover_Class")
transect_4b_long <- mutate(transect_4b_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "4B") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
transect_4b_long$Species=gsub("\\*","",transect_4b_long$Species)

#5B
transect_5b <-as_tibble(readxl::read_excel("/Users/oliviawon/Data_Science_for_the_Environment/Final_Project/postfire/data/PeninsulaPostFireVegTransects.xlsx", sheet= "5B", range = "B4:AH24"))
transect_5b[is.na(transect_5b)] = 0 
transect_5b_long <- transect_5b %>%
  pivot_longer(c("Layia platyglossa",	"Eriogonum nudum",	"Eschscholzia californica",	"Dipterostemon capitatus",
  "*Avena barbata",	"*Festuca perennis",	"Clarkia purpurea ssp. quadrivulnera",	"Melica californica",
  "Stipa pulchra",	"*Euphorbia peplus",	"Acmispon wrangelianus",	"*Festuca bromoides",	"Cirsium quercetorum",	"Trifolium ciliolatum",
  "Astragalus gambelianus", "Gilia clivorum",	"*Bromus hordaceus",	"Sanicula bipinnatifida",	"Muilla maritima",
  "Chlorogalum pomeridianum",	"Delphinium variegatum",	"Eriophyllum confertiflorum",	"Calystegia subacaulis",
  "Calindrinia menziesii",	"Plantago erecta",	"*Centaurea solstitialis",	"Lessingia arachnoidea",	
  "Stipa lepida",	"Hemizonia congesta ssp. luzulifolia",	"Epilobium brachycarpum",	"*Lactuca serriola",	
  "*Avena fatua"), names_to = "Species", values_to = "Cover_Class")
transect_5b_long <- mutate(transect_5b_long, Percent_Cover = case_when(Cover_Class == 1 ~ 2.5
                                                                       ,Cover_Class == 2 ~ 8.75
                                                                       ,Cover_Class == 3 ~ 18.75
                                                                       ,Cover_Class == 4 ~ 37.5
                                                                       ,Cover_Class == 5 ~ 62.5
                                                                       ,Cover_Class == 6 ~ 87.5
                                                                       ,TRUE ~ 0), Transect= "5B") %>%
  rename("Quadrat" = "Quadrat (1 m x 0.5 m)") 
transect_5b_long$Species=gsub("\\*","",transect_5b_long$Species)


transect_all <- bind_rows(transect_1A_long, transect_1b_long, transect_2a_long, 
                          transect_2b_long, transect_3a_long, transect_3b_long,
                          transect_3c_long,transect_3d_long, transect_3e_long,
                          transect_3f_long, transect_4a_long, transect_4b_long, 
                          transect_5a_long, transect_5b_long, .id = "Dataset")

transect_species <- left_join(transect_all, species_dat, by= c("Species"))
transect_clean <- left_join(transect_species, transect_dat, by= c("Transect")) %>%
  filter(Percent_Cover>0)%>%
  replace_na(list(Class= "Bare Ground", Family = "Bare Ground"))
  

mean_percent_cover_clean <- transect_clean %>%
  group_by(Type, Class)%>%
  summarise(percent_cover_mean = mean(Percent_Cover), 
            percent_cover_sd = sd(Percent_Cover)) %>%
  ungroup()

species_count <- transect_clean %>%
  distinct(Type, Class, Species)%>%
  count(Type, Class)%>%
  filter(Class %in% c("Native", "Non-native")) 

  
as_tibble(mean_percent_cover_clean)

just_veg <- mean_percent_cover_clean %>%
  filter(Class!= "Bare Ground") 

just_veg <- left_join(just_veg, species_count, by= c("Type","Class"))  

just_veg

ggplot(just_veg)+
  geom_boxplot(aes(Type, percent_cover_mean, color=Class))


ggplot(data = just_veg, aes(y = Type, x = percent_cover_mean, fill=Class)) +
  geom_bar(stat = 'identity')+
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
ggplot(data = just_veg, aes(y = Type, x = n, fill=Class)) +
  geom_bar(stat = 'identity')

ggplot(data = just_veg, aes(y = Type, x = percent_cover_mean)) +
  geom_point()

#T-test

res <- t.test(Percent_Cover ~ Type, data = grassland, var.equal = TRUE)
res


