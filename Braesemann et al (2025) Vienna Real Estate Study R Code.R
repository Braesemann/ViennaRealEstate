#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Braesemann et al (2025)
# How have urban housing preferences developed in response to the COVID-19 pandemic? A case study of Vienna
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0322629
# July 2025
# Author: Dr Fabian Braesemann
# fabian.braesemann@oii.ox.ac.uk
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Read packages
library("tidyverse")    # Cleanup data with TIDYVERSE
library("rpart")        # Plot decision tree
library("rpart.plot")   # Plot decision tree
library("qdapTools")    # For mtabulate 
library("ggpubr")       # Combine plots
library("stargazer")    # Show regression tables
library(data.table)     # Fast data loading
library(RColorBrewer)   # More colours
library(viridis)        # Other colour schemes
library("glmnet")       # GLM modelling
library("fixest")       # Clustered standard errors
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command

# Function to replace NaN by NA
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

#=========================================
# READ & PREPARE DATA
#=========================================

df <- fread(paste(getwd(), "/Braesemann et al (2025) Vienna Real Estate Study Data.csv", sep = ""))

# Price and Size to numeric
df <- df %>% mutate(Price = as.numeric(Price), 
                    Size = as.numeric(Size),
                    No_Rooms = as.numeric(No_Rooms))

# Rename 'Badezimmer'
df <- df %>% rename(No_Badezimmer = `Anzahl Badezimmer`)
# Rename 'Schlafzimmer'
df <- df %>% rename(No_Schlafzimmer = `Anzahl Schlafzimmer`)
# Rename 'WC'
df <- df %>% rename(No_WC = `Anzahl WC`)

# Calculate Price per SQM
df <- df %>% mutate(price_sqm = Price / Size) 

df %>% group_by(Year) %>% summarise(price_sqm = mean(price_sqm))

price_change <- df %>% group_by(District, Year) %>% summarise(price_sqm = mean(price_sqm))


price_change <- price_change %>% spread(Year, price_sqm)

colnames(price_change) <- c("District", "Year2018", "Year2021")

price_change <- price_change %>% mutate(change = (Year2021 - Year2018) / Year2018 * 100)

price_change <- price_change %>% select(District, change)

price_change <- price_change %>% mutate(District = as.character(District))

#<><><><><><><><><><><><><>
#<><><><><><><><><><><><><>
#
# FIGURE 1B: Map of Vienna
#
#<><><><><><><><><><><><><>
#<><><><><><><><><><><><><>

library("rgdal") # Reading shapefiles
library("tmap")  # Plotting maps
library(classInt)# Class intervals

vienna_map <- readOGR(dsn = "BEZIRKSGRENZEOGD/BEZIRKSGRENZEOGDPolygon.shp", layer = "BEZIRKSGRENZEOGDPolygon")

vienna_map@data$DISTRICT_C <- as.character(vienna_map@data$DISTRICT_C)

vienna_map <- merge(vienna_map, price_change, by.x = "DISTRICT_C", by.y = "District")

breaks_man <- classIntervals(vienna_map@data[["change"]], n = 4, style = "quantile")
breaks_man <- breaks_man$brks
breaks_man

breaks_man <- c(0, 5, 10, 15,25)

tmap_mode("plot")

tm_shape(vienna_map) + tm_borders() +  tm_fill("change", title = "Change in %", alpha = 0.7) + 
  tm_layout("Changes in rental prices per sqm in Vienna from 2018 to 2021/22", asp = 1,
            legend.title.size = 1.2,
            legend.text.size = 1)

#<><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 1C: Rental development per district group
#
#<><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><>

df <- df %>% mutate(DistrictGroup = ifelse(District == 1010, "1.",
                                                               ifelse(District %in% c(1020,1030,1040,1050,1060,1070,1080,1090), "2. to 9.",
                                                                      ifelse(District %in% c(1100,1110,1120,1140,1150,1160,1170), "10. to 17. (w/o 13.)",
                                                                             ifelse(District %in% c(1130,1180,1190), "13., 18., 19.",
                                                                                    ifelse(District %in% c(1200,1210,1220,1230), "20. to 23.","other"))))))


price_change <- df %>% group_by(DistrictGroup, Year) %>% summarise(price_sqm = mean(price_sqm))

price_change <- price_change %>% spread(Year, price_sqm)

colnames(price_change) <- c("DistrictGroup", "Year2018", "Year2021")

price_change <- price_change %>% mutate(change = (Year2021 - Year2018) / Year2018 * 100)

price_change <- price_change %>% select(DistrictGroup, change)

price_change <- price_change %>% mutate(DistrictGroup = factor(DistrictGroup, levels = c("1.", "2. to 9.",
                                                                                       "13., 18., 19.", "10. to 17. (w/o 13.)", "20. to 23.")))



price_change %>% 
  ggplot(aes(x = DistrictGroup, y = change, fill = factor(change))) + geom_bar(stat = "identity", color = "black", lwd =0.3) +
  scale_fill_brewer(palette = "YlOrBr") +
  labs(x = "Districts", y = "Rent per sqm changes in %") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 21), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 2A: Show prevalence of features across Districts
#
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Group Districts ((a) 1010 first district, (b) 1020 - 1090 inner districts, (c) 1100 - 1190 + 1230 outer districts)
df <- df %>% mutate(Bezirke = ifelse(District == "1010", "1.", 
                                     ifelse(District %in% c("1020","1030","1040","1050"), "2. bis 5.", 
                                            ifelse(District %in% c("1060", "1070", "1080", "1090"), "6. bis 9.", 
                                                   ifelse(District %in% c("1100", "1110", "1120", "1140", "1150", "1160", "1170"), "10. bis. 17. (ohne 13.)", 
                                                          ifelse(District %in% c("1130", "1180", "1190"), "13., 18., 19.",
                                                                 ifelse(District %in% c("1200", "1210", "1220", "1230"), "20. bis 23.",NA)))))),
                    Bezirke = factor(Bezirke, levels = c("1.", 
                                                         "2. bis 5.", "6. bis 9.",
                                                         "13., 18., 19.", 
                                                         "10. bis. 17. (ohne 13.)",
                                                         "20. bis 23.")),
                    Gruppen = ifelse(Bezirke %in% c("1."), "Erster",
                                     ifelse(Bezirke %in% c("2. bis 5.", "6. bis 9.", "13., 18., 19."), "Innere plus 13./18./19.",
                                            ifelse(Bezirke %in% c("10. bis. 17. (ohne 13.)",
                                                                  "20. bis 23."), "Aeussere",NA))))

## Create long DF with relevant features
df_features <- df %>% dplyr::select(Year, District, DistrictGroup, price_sqm, 
                                    Bus, Strassenbahn, UBahn, Tiefgarage, Fahrradraum, Stellplatz,
                                    Balkon, Terrasse, Dachterrasse, Garten, Loggia,
                                    Zentralheizung, Etagenheizung, Gasheizung, Fernwarme, Fussbodenheizung,Elektroheizung,
                                    Erstbezug,Altbau,Neubau) %>% 
  gather(feature, value, -c(Year, District, DistrictGroup, price_sqm))

## Calculate frequency of features mentioned in each year
df.stats <- df_features %>% group_by(Year, DistrictGroup, feature) %>% summarise(mean = mean(value))
df.stats$feature_group <- ifelse(df.stats$feature %in% c("UBahn","Bus","Strassenbahn"),"OEPNV",
                                 ifelse(df.stats$feature %in% c("Tiefgarage","Stellplatz","Fahrradraum"),"P-Transport",
                                        ifelse(df.stats$feature %in% c("Terrasse","Loggia","Garten","Dachterrasse","Balkon"),"Aussenbereich",
                                               ifelse(df.stats$feature %in% c("Erstbezug","Altbau","Neubau"),"Bautyp",
                                               ifelse(df.stats$feature %in% c("Zentralheizung","Etagenheizung", "Gasheizung", "Fernwarme", "Fussbodenheizung","Elektroheizung"),"Energie",NA)))))

## Drop features if occurrence is lower than 1%
df.stats <- df.stats %>% filter(mean >= 0.01)
feature_set <- unique(df.stats$feature)

df.stats <- df.stats %>% mutate(DistrictGroup = factor(DistrictGroup, levels = c("1.", "2. to 9.",
                                                                                         "13., 18., 19.", "10. to 17. (w/o 13.)", "20. to 23.")))

df.stats <- df.stats %>% mutate(feature = ifelse(feature == "Terrasse", "Terrace",
                                                 ifelse(feature == "Garten", "Garden",
                                                        ifelse(feature == "Dachterrasse", "Rooftop terrace",
                                                               ifelse(feature == "Balkon", "Balcony",
                                                                      ifelse(feature == "Neubau", "New building",
                                                                             ifelse(feature == "Erstbezug", "First occupancy",
                                                                                    ifelse(feature == "Fernwarme", "District heating",       
                                                                                    ifelse(feature == "Altbau", "Old building",
                                                                                           ifelse(feature == "Zentralheizung", "Central heating",
                                                                                                  ifelse(feature == "Gasheizung", "Gas heating",
                                                                                                         ifelse(feature == "Fussbodenheizung", "Underfloor heating",
                                                                                                                ifelse(feature == "Etagenheizung", "Self-contained central heating",
                                                                                                                       ifelse(feature == "Elektroheizung", "Electrical heating",
                                                                                                                              ifelse(feature == "UBahn", "Subway",
                                                                                                                                     ifelse(feature == "Strassenbahn", "Tram",
                                                                                                                                            ifelse(feature == "Tiefgarage", "Underground parking",
                                                                                                                                                   ifelse(feature == "Stellplatz", "Parking space",
                                                                                                                                                          ifelse(feature == "Fahrradraum", "Bicycle storage", feature)))))))))))))))))))

df.stats <- df.stats %>% mutate(feature_group = ifelse(feature_group == "Aussenbereich", "Outdoor area",
                                                       ifelse(feature_group == "Bautyp", "Type of\nconstruction",
                                                              ifelse(feature_group == "Energie", "Energy",
                                                                     ifelse(feature_group == "OEPNV", "Public\ntransport",
                                                                            ifelse(feature_group == "P-Transport", "Private\ntransport",feature_group))))))

## Plot frequency of features mentioned in each year
df.stats %>% filter(Year == 2021, feature != "Electrical heating") %>% mutate(Jahr = as.factor(Year)) %>% group_by(feature) %>% 
  ggplot(aes(y=feature, x=mean*100, fill = DistrictGroup), color = "black", lwd = 0.3) + 
  facet_grid(cols = vars(DistrictGroup), rows = vars(feature_group), scales = "free_y", space = "free_y") +
  geom_bar(stat="identity", position=position_dodge(), color = "black", lwd = 0.3) + 
  geom_text(aes(label=round(mean,2)*100), hjust=-0, vjust=1, color="black",
            position = position_dodge(0.9), size=5) +
  labs(x = "", y = "", title = "") +
  scale_fill_manual(values = c(brewer.pal(5,"YlOrBr")[3],brewer.pal(5,"YlOrBr")[2],brewer.pal(5,"YlOrBr")[1],
                               brewer.pal(5,"YlOrBr")[4],brewer.pal(5,"YlOrBr")[5])) +
  theme_minimal() + theme(text = element_text(size = 19), legend.position = "none",
                          strip.text = element_text(face = "bold", size = 17),
                          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 2B: How share of certain amenities in the listings changed over time
#
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

## Create long DF with relevant features
df_features <- df %>% dplyr::select(Year, District, DistrictGroup, Bezirke, price_sqm, 
                         Bus, Strassenbahn, UBahn, Tiefgarage, Fahrradraum, Stellplatz,
                         Balkon, Terrasse, Dachterrasse, Garten, Loggia) %>% 
  gather(feature, value, -c(Year, District, DistrictGroup, Bezirke, price_sqm))


# Create new category 'historical' vs 'outer' districts
df_features <- df_features %>% mutate(historical = ifelse(DistrictGroup %in% c("1.", "2. to 9.",
                                                                               "13., 18., 19."), "Inner / historical districts", 
                                                          ifelse(DistrictGroup %in% c("10. to 17. (w/o 13.)", "20. to 23."), "Outder districts", 
                                                                 "Other")))

## Calculate frequency of features mentioned in each year
df.stats <- df_features %>% group_by(Year, feature) %>% summarise(mean = mean(value))

## Drop features if occurrence is lower than 1%
df.stats <- df.stats %>% filter(mean >= 0.01)

## Aggregate features
df_features <- df %>% summarise(Year, District,DistrictGroup, Bezirke, price_sqm, 
                             `Public transport` = ifelse(Bus == 1 | Strassenbahn == 1 | UBahn == 1, 1, 0),
                             `Private transport` = ifelse(Tiefgarage == 1 | Fahrradraum == 1 | Stellplatz == 1, 1, 0),
                             `Outdoor area` = ifelse(Balkon == 1 | Terrasse == 1 | Dachterrasse == 1 | Garten == 1 | Loggia == 1, 1, 0),
                             Energy = ifelse(Zentralheizung == 1 | Gasheizung == 1 | Fussbodenheizung == 1 | Etagenheizung == 1 | Elektroheizung == 1, 1, 0)) %>% 
  gather(feature, value, -c(Year, District,DistrictGroup, Bezirke, price_sqm))

# Create new category 'historical' vs 'outer' districts
df_features <- df_features %>% mutate(historical = ifelse(DistrictGroup %in% c("1.", "2. to 9.",
                                                                               "13., 18., 19."), "Inner / historical districts", 
                                                          ifelse(DistrictGroup %in% c("10. to 17. (w/o 13.)", "20. to 23."), "Outer districts", 
                                                                 "Other")))

## Calculate frequency of features mentioned in each year
df.stats <- df_features %>% group_by(Year, feature) %>% summarise(mean = mean(value))

## Drop features if occurrence is lower than 1%
df.stats <- df.stats %>% filter(mean >= 0.01)

## Plot frequency of features mentioned in each year
df.stats %>% mutate(Jahr = as.factor(Year)) %>% group_by(feature) %>% mutate(order = mean[Year==2018]) %>%
  ggplot(aes(x=reorder(feature, order), y=mean*100, fill=Jahr)) +
  #facet_wrap(~historical) +
  geom_bar(stat="identity", position=position_dodge(), color = "black", lwd = 0.3)+
  geom_text(aes(label=round(mean,3)*100), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5) +
  labs(x = "", y = "", title = "", fill = "Year") + 
  scale_fill_manual(values = c(brewer.pal(5,"YlOrBr")[2],brewer.pal(5,"YlOrBr")[4])) +
  theme_minimal() + theme(text = element_text(size = 21), legend.position = "bottom",
                          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

#<><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 3A: Show non-linearity via decision tree
#
#<><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><>

## Define relevant set of features for 2021

dfs2021 <- df %>% filter(Year==2021) %>% 
  dplyr::select(price_sqm,DistrictGroup,
                DG,Keller,Einbaukuche,Abstellraum,Dusche,Wanne,
                Parkettboden,Laminat,Fliesenboden,
                Balkon,Terrasse,Dachterrasse,Garten,Loggia,Wintergarten,
                Tiefgarage,Fahrradraum,UBahn,Strassenbahn,Bus,Stellplatz,
                Klimatisiert,Erdwarme,Fussbodenheizung,Fernwarme,Zentralheizung,Etagenheizung,Gasheizung,Elektroheizung)

dummies <- mtabulate(dfs2021$DistrictGroup)
colnames(dummies) <- paste("DistrictGroup: ", colnames(dummies), sep="")
dfs2021 <- cbind(dfs2021, dummies)
dfs2021 <- dfs2021 %>% dplyr::select(-DistrictGroup)

# Create a decision tree model
tree <- rpart(price_sqm ~ ., data = dfs2021, cp=.005)

tree$frame$var <- c("Underfloor heating", "1. District", "Terrace", "Self-contained\ncentral heating", "<leaf>", "Districts 2 to 9", "<leaf>","<leaf>",
                    "Districts 2 to 9", "<leaf>","<leaf>","<leaf>", "Shower", "Fitted kitchen", "<leaf>","<leaf>","<leaf>")
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="Oranges", shadow.col="gray", nn=TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 3B: Perform LASSO to find relevant features
#
#<><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><>

## Define relevant set of features for 2021
dfs2021 <- df %>% filter(Year==2021) %>% 
  dplyr::select(price_sqm,Bezirke,
                DG,Keller,Einbaukuche,Abstellraum,Dusche,Wanne,
                Parkettboden,Laminat,Fliesenboden,
                Balkon,Terrasse,Dachterrasse,Garten,Loggia,Wintergarten,
                Tiefgarage,Fahrradraum,UBahn,Strassenbahn,Bus,Stellplatz,
                Klimatisiert,Erdwarme,Fussbodenheizung,Fernwarme,Zentralheizung,Etagenheizung,Gasheizung,Elektroheizung)

dummies <- mtabulate(dfs2021$Bezirke)
colnames(dummies) <- paste("Bezirke_", colnames(dummies), sep="")
dfs2021 <- cbind(dfs2021, dummies)
dfs2021 <- dfs2021 %>% dplyr::select(-Bezirke)



## Reduce feature space with LASSO
x <- as.matrix(dfs2021 %>% dplyr::select(-price_sqm))
y <- as.double(as.matrix(dfs2021$price_sqm)) 
### Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, y, alpha=1, parallel=TRUE, standardize=TRUE)
### Plot Results
lasso.stats <- as.data.frame(round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2))
names(lasso.stats) <- "Feature Importance"
lasso.stats$VarName <- rownames(lasso.stats)

lasso.stats <- lasso.stats %>% filter(abs(`Feature Importance`) > 0.03)

important_vars <- lasso.stats$VarName
important_vars <- important_vars[c(-1)]

lasso.stats <- lasso.stats %>% mutate(key1 = ifelse(VarName %in% c("Bezirke_Äußere (1100-1230)","Bezirke_Erster (1010)","Bezirke_Innere (1020-1090)"), "Bezirke",
                                                    ifelse(VarName %in% c("DG","Keller","Einbaukuche","Abstellraum","Dusche","Wanne","Parkettboden","Laminat","Fliesenboden"), "Ausstattung",
                                                           ifelse(VarName %in% c("Balkon","Terrasse","Dachterrasse","Garten","Loggia","Wintergarten"), "Außenbereich",
                                                                  ifelse(VarName %in% c("Tiefgarage","Fahrradraum","UBahn","Strassenbahn","Bus","Stellplatz"), "Transport",
                                                                         ifelse(VarName %in% c("Klimatisiert","Erdwarme","Fussbodenheizung","Fernwarme","Zentralheizung",
                                                                                               "Etagenheizung","Gasheizung","Elektroheizung"),"Energie","None"))))))

lasso.stats <- lasso.stats %>% mutate(VarName = ifelse(VarName == "Terrasse", "Terrace",
                                                 ifelse(VarName == "Garten", "Garden",
                                                        ifelse(VarName == "Dachterrasse", "Rooftop terrace",
                                                               ifelse(VarName == "Balkon", "Balcony",
                                                                      ifelse(VarName == "Neubau", "New building",
                                                                             ifelse(VarName == "Erstbezug", "First occupancy",
                                                                                    ifelse(VarName == "Altbau", "Old building",
                                                                                           ifelse(VarName == "Zentralheizung", "Central heating",
                                                                                                  ifelse(VarName == "Gasheizung", "Gas heating",
                                                                                                         ifelse(VarName == "Fernwarme", "District heating",
                                                                                                                ifelse(VarName == "Fussbodenheizung", "Underfloor heating",
                                                                                                                       ifelse(VarName == "Etagenheizung", "Self-contained central heating",
                                                                                                                              ifelse(VarName == "Elektroheizung", "Electrical heating",
                                                                                                                                     ifelse(VarName == "UBahn", "Subway",
                                                                                                                                            ifelse(VarName == "Strassenbahn", "Tram",
                                                                                                                                                   ifelse(VarName == "Tiefgarage", "Underground parking",
                                                                                                                                                          ifelse(VarName == "Stellplatz", "Parking space",
                                                                                                                                                                 ifelse(VarName == "Fahrradraum", "Bicycle storage", VarName)))))))))))))))))))

lasso.stats <- lasso.stats %>% mutate(key1 = ifelse(key1 == "Außenbereich", "Outdoor area",
                                                       ifelse(key1 == "Ausstattung", "Amenities",
                                                              ifelse(key1 == "Energie", "Energy",
                                                                     ifelse(key1 == "OEPNV", "Public\ntransport",
                                                                            ifelse(key1 == "P-Transport", "Private\ntransport",key1))))))

# Get the number of unique sentiment values (necessary to get number of different colours)
unique_sentiments <- nrow(lasso.stats %>% group_by(`Feature Importance`) %>% summarise(n()))

# Use RColorBrewer to get a range of red-yellow-green colours
col_range <- rev(brewer.pal(9, "YlOrBr"))
col_range <- colorRampPalette(col_range)
cols <- col_range(unique_sentiments)

lasso.stats %>% filter(abs(`Feature Importance`) != 0, VarName != "(Intercept)", key1 != "None") %>%
  ggplot(aes(x = reorder(VarName, -`Feature Importance`), y = `Feature Importance`, fill = factor(`Feature Importance`))) +
  geom_bar(stat = "identity", colour = "black", lwd = 0.3) +
  labs(x = "", y = "Beta-Coefficient from\n LASSO regression", fill = "") +
  facet_grid(~key1, scales = "free", space = "free_x") + 
  scale_fill_manual(values = cols) +                                                               # Use colour range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none",
        text = element_text(size = 18), panel.grid.minor = element_blank(), #panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 3C: Regression results with clustered standard errors
#
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Calculate grouping variables and other variables for regression model
df <- df %>% mutate(Outdoor = Balkon + Terrasse + Dachterrasse + Garten + Loggia + Wintergarten,
                    AussenbereichDummy = ifelse(Balkon + Terrasse + 
                      Dachterrasse + Garten + Loggia + Wintergarten > 0, 1, 0),
                    PublicTransport = ifelse(UBahn + Strassenbahn + Bus > 0, 1, 0),
                    PrivateTransport = ifelse(Tiefgarage + Fahrradraum + Stellplatz > 0, 1, 0),
                    AtLeastTwoBaths = ifelse(No_Badezimmer > 1, 1, 0),
                    AtLeastTwoBaths = ifelse(is.na(AtLeastTwoBaths), 0, AtLeastTwoBaths),
                    AtLeastTwoWCs = ifelse(No_WC > 1, 1, 0),
                    AtLeastTwoWCs = ifelse(is.na(AtLeastTwoWCs), 0, AtLeastTwoWCs),
                    smallFlat = ifelse(Size < 31, 1, 0))

# Model 2018 MONTHLY RENT
M2018e_fixest <- feols(log(Price) ~
                         log(Size) + No_Rooms +  smallFlat +
                         DG + Parkettboden + Laminat + Keller +
                         AussenbereichDummy + PublicTransport + PrivateTransport +
                         Erstbezug + Altbau + Neubau +
                         Klimatisiert + Fussbodenheizung + Etagenheizung + Elektroheizung +
                         AtLeastTwoBaths + AtLeastTwoWCs | District, cluster = "District",
                       data = df  %>% filter(Year == 2018, price_sqm < 45, No_Rooms < 20))

# Model 2018 MONTHLY RENT PER SQUARE METRE
M2018f_fixest <- feols(log(price_sqm) ~
                         log(Size) + No_Rooms +  smallFlat +
                         DG + Parkettboden + Laminat + Keller +
                         AussenbereichDummy + PublicTransport + PrivateTransport +
                         Erstbezug + Altbau + Neubau +
                         Klimatisiert + Fussbodenheizung + Etagenheizung + Elektroheizung +
                         AtLeastTwoBaths + AtLeastTwoWCs | District, cluster = "District",
                       data = df  %>% filter(Year == 2018, price_sqm < 45, No_Rooms < 20))

# Model 2021 MONTHLY RENT
M2021e_fixest <- feols(log(Price) ~
                         log(Size) + No_Rooms +  smallFlat +
                         DG + Parkettboden + Laminat + Keller +
                         AussenbereichDummy + PublicTransport + PrivateTransport +
                         Erstbezug + Altbau + Neubau +
                         Klimatisiert + Fussbodenheizung + Etagenheizung + Elektroheizung +
                         AtLeastTwoBaths + AtLeastTwoWCs | District, cluster = "District",
                       data = df  %>% filter(Year == 2021, price_sqm < 45, No_Rooms < 20))

# Model 2021 MONTHLY RENT PER SQUARE METRE
M2021f_fixest <- feols(log(price_sqm) ~
                         log(Size) + No_Rooms +  smallFlat +
                         DG + Parkettboden + Laminat + Keller +
                         AussenbereichDummy + PublicTransport + PrivateTransport +
                         Erstbezug + Altbau + Neubau +
                         Klimatisiert + Fussbodenheizung + Etagenheizung + Elektroheizung +
                         AtLeastTwoBaths + AtLeastTwoWCs | District, cluster = "District",
                       data = df  %>% filter(Year == 2021, price_sqm < 45, No_Rooms < 20))

etable(M2018e_fixest, M2018f_fixest, M2021e_fixest, M2021f_fixest, digits = 2, tex = T)

#<><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><>
#
# FIGURE 3D: Distribution of monthly rent variable
#
#<><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><><><><><><><><><><><><><><><><><>

plot1 <- df %>% filter(price_sqm < 45, No_Rooms < 20) %>%
  ggplot(aes(x = Price )) + geom_density(fill = "orange", alpha = 0.5) + facet_wrap(~Year) +
  labs(subtitle = "(1) Monthly rent in EUR", x = "Rent", y = "Density") +
  geom_hline(yintercept = 0, lty = 1 , lwd = 0.2, colour = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.15), 
        legend.background = element_rect(colour = "grey", size = 0.2),
        legend.direction = "vertical", text = element_text(size = 12),
        axis.ticks.x = element_line(colour = "grey"),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(face = "bold",size = 12),
        plot.subtitle = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

plot2 <- df %>% filter(price_sqm < 45, No_Rooms < 20) %>%
  ggplot(aes(x = Price )) + geom_density(fill = "orange", alpha = 0.5)+ facet_wrap(~Year) + scale_x_log10() +
  labs(subtitle = "         (2) Monthly rent in EUR (log-10 scale)",  x = "Rent (log-10 scale)", y = "Density") +
  geom_hline(yintercept = 0, lty = 1 , lwd = 0.2, colour = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.15), 
        legend.background = element_rect(colour = "grey", size = 0.2),
        legend.direction = "vertical", text = element_text(size = 12),
        axis.ticks.x = element_line(colour = "grey"),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(face = "bold",size = 12),
        plot.subtitle = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

plot3 <- df %>% filter(price_sqm < 45, No_Rooms < 20) %>%
  ggplot(aes(x = price_sqm )) + geom_density(fill = "orange", alpha = 0.5) + facet_wrap(~Year) +
  labs(subtitle = "(3) Monthly rent in EUR per square metre", x = "Rent", y = "Density") +
  geom_hline(yintercept = 0, lty = 1 , lwd = 0.2, colour = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.15), 
        legend.background = element_rect(colour = "grey", size = 0.2),
        legend.direction = "vertical", text = element_text(size = 12),
        axis.ticks.x = element_line(colour = "grey"),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(face = "bold",size = 12),
        plot.subtitle = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

plot4 <- df %>% filter(price_sqm < 45, No_Rooms < 20) %>%
  ggplot(aes(x = price_sqm )) + geom_density(fill = "orange", alpha = 0.5)+ facet_wrap(~Year) + scale_x_log10() +
  labs(subtitle = "     (4) Monthly rent in EUR per square metre (log-10 scale)",  x = "Rent (log-10 scale)", y = "Density") +
  geom_hline(yintercept = 0, lty = 1 , lwd = 0.2, colour = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.15), 
        legend.background = element_rect(colour = "grey", size = 0.2),
        legend.direction = "vertical", text = element_text(size = 12),
        axis.ticks.x = element_line(colour = "grey"),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(face = "bold",size = 12),
        plot.subtitle = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

ggarrange(plot1, plot3, plot2, plot4, ncol = 2, nrow = 2)

#=======================================
# END OF SCRIPT
#=======================================
#=======================================