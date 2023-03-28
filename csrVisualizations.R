################################################################################
#                    PLOTS & VISUALISATIONS 
#                            FOR 
#      TEXT DATA ANALYSIS OF CSR REPORTS (ENERGY SECTOR)
#              
#   Submitted by: Sameera Muriyathuparambil, Sergine Mbacke Diop, Twan Veldhuis 
#   Submitted on: 16,March, 2023

################################################################################



##############################
# Load librairies
##############################

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(wordcloud2)

############################## 
# Source file 
##############################
# to make this work you have to set the R-file directory to the working directory using setwd()
path_main = "<PATH TO THE DATA FILE>"
main_file <- read.csv(path_main)

# create a working dataset
main_dataset <- tibble(Org=main_file$Organization,
                    Year = main_file$Year,
                    Size = main_file$Size,
                    Country = main_file$Country,
                    Region = main_file$Region,
                    Rep_Type = main_file$Type,
                    Status = main_file$Status,
                    Country_Status = main_file$Country.Status,
                    Title = main_file$Title,
                    Greenhouse = main_file$Greenhouse_Gas_Emissions,
                    Diversity = main_file$Diversity,
                    Empl_Health = main_file$Employee_Health_Safety,
                    Cust_welfare = main_file$Customer_Welfare,
                    Language = main_file$Language,
                    Compound = main_file$compound,
                    Sentiment = main_file$Overall.sentiment,
                    WordCount = main_file$WordCount)


# ----------------------
#   SENTIMENT ANALYSIS
# ----------------------

# creating dataframe for sentiments per region
df_sentiment <- main_dataset %>% 
  group_by(Region,Sentiment) %>% 
  summarise(total_count=n(),.groups = 'drop') %>% 
  as.data.frame()

# creating the plot
ggplot(df_sentiment[!(is.na(df_sentiment$Sentiment) | df_sentiment$Sentiment==""), ], aes(x=Region, y=total_count, fill=Sentiment)) + 
  geom_bar(stat = "identity", position="dodge") +
  geom_text(aes(Region, label=total_count),
            position = position_dodge(0.9),
            vjust = -0.4) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
         text = element_text(size = 12),
         axis.text.x = element_text(size = 12)) +
  scale_x_discrete(labels = function(Region) str_wrap(Region, width = 15)) +
  xlab("Continent") +
  ylab("Number of CSR Reports") +
  scale_y_continuous(limit = c(0, 300), breaks = seq(0, 300, 50)) +
  guides(fill=guide_legend(title="Sentiment Score"))


# ----------------------
#     WORD CLOUD
# ----------------------

#create a dataframe to for word count
df = data.frame()

# splitting string at column Wordcount
# by removing spaces and special characters
# to extract word and its respective count

for (row in 1:nrow(main_file)) {
  string <- main_file[row, "WordCount"]
  
  # split the string at each "," character
  string <- strsplit(string, ",")
  
  # extract the words and numbers from each element of the resulting list
  words <- sapply(string, function(x) trimws(gsub("\\D+", "", x), "left"))
  numbers <- sapply(string, function(x) as.numeric(trimws(gsub("\\D+", "", x), "right")))
  
  # combine the words and numbers into a data frame or vector
  data <- data.frame(words = gsub("\\d+", "", string[[1]]), numbers = as.numeric(trimws(gsub("\\D+", "", string[[1]]), "right")))
  
  # Remove all the list information and creating the DataFrame
  data[] <- lapply(data, gsub, pattern='\'', replacement='')
  data[] <- lapply(data, gsub, pattern='\\[', replacement='')
  data[] <- lapply(data, gsub, pattern='\\]', replacement='')
  data[] <- lapply(data, gsub, pattern='\\(', replacement='')
  data[] <- lapply(data, gsub, pattern='\\)', replacement='')
  data[] <- lapply(data, gsub, pattern=' ', replacement='')
  data <- as.data.frame(cbind(data[c(TRUE,FALSE), 1:1], data[c(FALSE, TRUE), 2:2]))
  data <- setNames(data, c("words","count"))
  df <- rbind(df,data)
}

# convert count column to numeric
df$count <- transform(df, count = as.numeric(count))
df_sum <- aggregate(count$count ~ words, df, sum)
df_sum <- setNames(df_sum, c("words","count"))

# edit this accordingly this now only uses the words that occur more than 500 times
df_sum2 <- df_sum[(df_sum$count > 6000),]
remove_list <- c("company","group","million","used","requirement","within","general",
                 "business","operation","note","target","director","operational","one",
                 "part","since","gri","u","sale","based","loss","period",
                 "net","empresa","condition","line","due","end","co","may",
                 "held","set","limited","level","term","increase","measure","principle",
                 "economic","annual","cost","issue","key","equity","state","construction",
                 "new","strategic","market","meeting","use","site","time",
                 "position","facility","asset","contrac","country","made","date","ensure",
                 "operating","provision","expense","need","profit","jsc","two","high",
                 "cash","page","joint","related","interest","december","amount","executive",
                 "unit","well","rate","tax","significant","number","basis","approach",
                 "agreement","value","work","result","capital","contract","addition","first",
                 "march","supply","working","standard","code","accordance","implementation","provide",
                 "total","project","system","corporate","board","financial","year","water",
                 "payment","independent","valor","external","case","field","main","aspect",
                 "day","three","event","best","event","using","objective","potential",
                 "improve","revenue","exchange","make","good","place","integrated","average",
                 "take","disclosure","party","mw","effort","award","flow","life",
                 "flow","act","base","focus","m","major","russian","legal",
                 "desarollo","incident","reserve","available","received","relevant","","",
                 "increased","department","pt","entity","compaa","continue","paid","building",
                 "hour","acquisition","additional","meet","carried","ended","june","csr",
                 "drilling","way","empresas","form","foreign","grupo","concern","item",
                 "hse","compared","decision","approved","cada","direct","th","see",
                 "ano","pr","content","ltd","le","st","april","nmero",
                 "taken","perusahaan","full","around","forma","table","would","regarding",
                 "parte","among","according","trabajo","type")
df_sum3 <- df_sum2[ ! df_sum2$words %in% remove_list, ]

# dataframe for words occuring more than 5000
set.seed(1)
wordcloud2(data = df_sum3, size = .7)



# -----------------------------
#     SAVING PLOTS(if required)
# -----------------------------

ggsave("perc_plot.png", perc_plot, width = 10, height = 5, dpi = 300)
ggsave("perc_year_plot.png", perc_year_plot, width = 10, height = 5, dpi = 300)
ggsave("size_year.png", size_year, width =12 , height = 4, dpi = 300)
ggsave("region_plot.png", region_plot, width = 10, height = 5, dpi = 300)
ggsave("gri_size_plot.png", gri_size_plot, width = 10, height = 5, dpi = 300)
ggsave("gri_year_plot.png", gri_year_plot, width = 10, height = 5, dpi = 300)
