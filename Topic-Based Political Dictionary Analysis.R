---
  title: "Four Topic-Based Political Dictionary Analysis: (i) populism.liberalism.lexicon; (ii) Policy-agendas dictionary; (iii) Laver-Garry dictionary; (iv) Maerz.lexicon"
---
  
  ```{r}
if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("readtext")) {install.packages("readtext"); library("readtext")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
theme_set(theme_bw())
```

```{r}
setwd("/Users/Desktop/textanalysis")
data<-read_csv("CleanedConspiracy.csv")

```

```{r}
data %>% glimpse()
```


```{r}
populism.liberalism.lexicon <- dictionary(list(
  populism = c("elit*", "consensus*", "undemocratic*", "referend*", "corrupt*", 
               "propagand", "politici*", "*deceit*", "*deceiv*", "*betray*", "shame*", 
               "scandal*", "truth*", "dishonest*", "establishm*", "ruling*"),
  liberalism = c("liber*", "free*", "indiv*", "open*", "law*", "rules", "order", 
                 "rights", "trade", "global", "inter*", "trans*", "minori*", "exchange", "market*")
))
```

Filter out the dates that do not have any values.

```{r}
data<-data %>% filter(!is.na(Date))
```

```{r}
body_vector<-data %>% select(Body) %>% as_vector()
```

```{r}
dfm_body <- dfm(body_vector, dictionary = populism.liberalism.lexicon)

```

```{r}
dfm_body_prop <- dfm_weight(dfm_body, scheme = "prop")

df_body <- convert(dfm_body_prop, "data.frame")

head(df_body)
```

```{r}
install.packages("writexl")
library(writexl)

```


```{r}
library(tidyverse)
data <- data %>%
  mutate(X = 1:1022048) %>%
  mutate(X = as.character(X))
output<-df_body %>%
  mutate(X=1:1022048) %>% mutate(X=as.character(X)) %>% 
  full_join(data) %>% 
  select(-doc_id,-X)

write.csv(output, "output_populism-liberalism.csv", row.names = FALSE)
write_xlsx(output, "output_populism-liberalism.xlsx")


```

```{r}
dates <- data %>% select(Date) %>% as_vector()
month_year <- format(as.Date(dates), "%Y-%m")
data$MonthYear <- month_year

```


```{r}

data$MonthYear <- format(as.Date(data$Date), "%Y-%m")
dfm_body <- dfm(data$Body, groups = data$MonthYear, dictionary = populism.liberalism.lexicon)
dfm_body_prop <- dfm_weight(dfm_body, scheme = "prop")
df_body <- convert(dfm_body_prop, "data.frame")
df_body$MonthYear <- rownames(df_body)
df_body <- df_body %>% select(MonthYear, populism, liberalism)

```

```{r}

month_year_unique <- unique(month_year)
month_year_id <- 1:length(month_year_unique)
names(month_year_id) <- month_year_unique

group_ids <- month_year_id[month_year]

dfm_body <- dfm(data$Body, groups = group_ids, dictionary = populism.liberalism.lexicon)
df_body <- convert(dfm_body_prop, "data.frame")
df_body$MonthYear <- names(month_year_id)[as.integer(rownames(df_body))]
df_body <- df_body %>% select(MonthYear, populism, liberalism)

head(df_body)

```

```{r}

df_long <- df_body %>% 
  gather(key = "Category", value = "Proportion", populism, liberalism) %>% 
  mutate(MonthYear=ym(MonthYear))

ggplot(df_long, aes(x = (MonthYear), y = Proportion, color = Category)) +
  geom_line(aes(group = Category), size = 1) +
  labs(title = "Proportions of Populism and Liberalism over Time",
       x = "Date",
       y = "Share of Topics [%]",
       color = "Category")  

```

Frequency

```{r}
dfm_body_freq <- dfm(data$Body, groups = group_ids, dictionary = populism.liberalism.lexicon)

df_freq <- convert(dfm_body_freq, "data.frame")

df_freq$MonthYear <- names(month_year_id)[as.integer(rownames(df_freq))]

df_freq

```


```{r}
df_long_freq <- df_freq %>% 
  gather(key = "Category", value = "Frequency", populism, liberalism) %>% 
  mutate(MonthYear=ym(MonthYear))

```


```{r}
ggplot(df_long_freq, aes(x = (MonthYear), y = Frequency, color = Category)) +
  geom_line(aes(group = Category), size = 1) +
  labs(title = "Frequencies of Populism and Liberalism Terms over Time",
       x = "Date",
       y = "Frequency",
       color = "Category")  

```

```{r}

group_dates <- as.character(data$Date)

dfm_body_freq_day <- dfm(data$Body, groups = group_dates, dictionary = populism.liberalism.lexicon)
df_freq_day <- convert(dfm_body_freq_day, "data.frame")
df_freq_day<-df_freq_day %>% rename(date=doc_id)

write.csv(df_freq_day, "frequencies_by_day.csv", row.names = FALSE)

``` 

Proportions by day

```{r}
group_dates <- as.character(data$Date)

dfm_body_day <- dfm(data$Body, groups = group_dates, dictionary = populism.liberalism.lexicon)
dfm_body_prop_day <- dfm_weight(dfm_body_day, scheme = "prop")
df_prop_day <- convert(dfm_body_prop_day, "data.frame")
df_prop_day<-df_prop_day %>% rename(date=doc_id)

write.csv(df_prop_day, "proportions_by_day.csv", row.names = FALSE)

```

```{r}
setwd("/Users/abigailohara/Desktop/Abigail_textanalysis")
lavergarry.lexicon <- dictionary(file = "LaverGarry.cat", format = "wordstat")

```

```{r}
head(lavergarry.lexicon, 2)

```


```{r}
month_year <- format(as.Date(data$Date), "%Y-%m")

dfm_body_lg <- dfm(data$Body, groups = month_year, dictionary = lavergarry.lexicon)

dfm_body_prop_lg <- dfm_weight(dfm_body_lg, scheme = "prop")

```

```{r}
df_body_lg <- convert(dfm_body_prop_lg, "data.frame")%>% 
  mutate(MonthYear = doc_id) %>% 
  mutate(culture = `CULTURE` + `CULTURE.CULTURE-HIGH` + `CULTURE.CULTURE-POPULAR` + `CULTURE.SPORT`) %>% 
  mutate(economy = `ECONOMY.+STATE+` + `ECONOMY.=STATE=` + `ECONOMY.-STATE-`) %>% 
  mutate(environment = `ENVIRONMENT.CON ENVIRONMENT` + `ENVIRONMENT.PRO ENVIRONMENT`) %>% 
  mutate(institutions = `INSTITUTIONS.CONSERVATIVE` + `INSTITUTIONS.NEUTRAL` + `INSTITUTIONS.RADICAL`) %>%
  mutate(values = `VALUES.CONSERVATIVE` + `VALUES.LIBERAL`) %>%
  mutate(law_and_order = `LAW_AND_ORDER.LAW-CONSERVATIVE`) %>%
  mutate(rural = `RURAL`) %>%
  mutate(other = `GROUPS.ETHNIC` + `GROUPS.WOMEN` + `LAW_AND_ORDER.LAW-LIBERAL` + `URBAN`)%>%
  select(MonthYear, culture:other)%>%
  gather(Topic, Share, culture:other) %>%
  filter(!Topic %in% c("economy", "institutions", "values", "culture", "other"))

```


```{r}
ggplot(df_body_lg, aes(MonthYear, Share, group = Topic, col = Topic)) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Topics in Conspiracy Comments_ Laver-Garry Dictionary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Share of topics [%]")

```

```{r}
dfm_body_lg_freq <- dfm(data$Body, groups = month_year, dictionary = lavergarry.lexicon)

df_body_lg_freq <- convert(dfm_body_lg_freq, "data.frame")

df_body_lg_freq$MonthYear <- rownames(df_body_lg_freq)

```

```{r}
dfm_body_lg_freq <- convert(dfm_body_lg_freq, "data.frame")%>% 
  mutate(MonthYear = doc_id) %>% 
  mutate(culture = `CULTURE` + `CULTURE.CULTURE-HIGH` + `CULTURE.CULTURE-POPULAR` + `CULTURE.SPORT`) %>% 
  mutate(economy = `ECONOMY.+STATE+` + `ECONOMY.=STATE=` + `ECONOMY.-STATE-`) %>% 
  mutate(environment = `ENVIRONMENT.CON ENVIRONMENT` + `ENVIRONMENT.PRO ENVIRONMENT`) %>% 
  mutate(institutions = `INSTITUTIONS.CONSERVATIVE` + `INSTITUTIONS.NEUTRAL` + `INSTITUTIONS.RADICAL`) %>%
  mutate(values = `VALUES.CONSERVATIVE` + `VALUES.LIBERAL`) %>%
  mutate(law_and_order = `LAW_AND_ORDER.LAW-CONSERVATIVE`) %>%
  mutate(rural = `RURAL`) %>%
  mutate(other = `GROUPS.ETHNIC` + `GROUPS.WOMEN` + `LAW_AND_ORDER.LAW-LIBERAL` + `URBAN`)%>%
  select(MonthYear, culture:other)%>%
  gather(Topic, Share, culture:other) %>%
  filter(!Topic %in% c("economy", "institutions", "values", "culture", "other"))
```


```{r}
ggplot(dfm_body_lg_freq, aes(MonthYear, Share, group = Topic, col = Topic)) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Frequencies of Topics in Conspiracy Comments_Laver-Garry Dictionary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Frequency of topics")
```


3rd Dictionary_Policy Agendas

```{r}
library(xml2)
```

```{r}
setwd("/Users/abigailohara/Desktop/Abigail_textanalysis")
xml_doc <- read_xml("policy_agendas_english.lcd")

```


```{r}
cat_nodes <- xml_find_all(xml_doc, "//cnode")
cat_names <- sapply(cat_nodes, function(node) xml_attr(node, "name"))
```


```{r}
cat_names <- sapply(cat_nodes, xml_attr, "name")

terms_list <- lapply(cat_nodes, function(node) {
  terms <- xml_find_all(node, "./pnode")
  return(sapply(terms, xml_attr, "name"))
})

dict_list <- setNames(terms_list, cat_names)

policy_agendas_dict <- dictionary(dict_list)

head(policy_agendas_dict, 2)

```


```{r}
dfm_body_pa <- dfm(data$Body, groups = month_year, dictionary = policy_agendas_dict)

df_body_pa <- convert(dfm_body_pa, "data.frame") %>%
  mutate(MonthYear = doc_id) %>%
  select(MonthYear, macroeconomics, finance, foreign_trade, labour, healthcare, immigration, education, intl_affairs, defence) %>%
  gather(macroeconomics:defence, key = "Topic", value = "Share") %>% 
  group_by(MonthYear) %>% 
  mutate(Share = Share/sum(Share)) %>% 
  mutate(Topic = as_factor(Topic))

ggplot(df_body_pa, aes(x = MonthYear, y = Share, colour = Topic, group = Topic)) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Topics in Conspiracy comments_Policy-Agendas Dictionary") +
  xlab("") +
  ylab("Share of topics [%]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Frequency of Topics

```{r}
dfm_body_pa_freq <- dfm(data$Body, groups = data$MonthYear, dictionary = policy_agendas_dict)

```

```{r}
df_body_pa_freq <- convert(dfm_body_pa_freq, "data.frame")

```

```{r}
df_body_pa_freq <- df_body_pa_freq %>%
  
  mutate(MonthYear = doc_id) %>%
  select(MonthYear, macroeconomics, finance, foreign_trade, labour, healthcare, immigration, education, intl_affairs, defence) %>%
  gather(macroeconomics:defence, key = "Topic", value = "Frequency") %>% 
  mutate(Topic = as_factor(Topic)) 

```



```{r}
ggplot(df_body_pa_freq, aes(x = MonthYear, y = Frequency, colour = Topic, group = Topic)) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Topic Frequencies in Conspiracy Comments_Policy-Agendas Dictionary") +
  xlab("") +
  ylab("Frequency of topics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

#Final dictionary_Maerz.lexicon


```{r}
setwd("/Users/abigailohara/Desktop/Abigail_textanalysis")
maerz.lexicon <- dictionary(file = "audemdic_valid.ykd", format = "yoshikoder")

```


```{r}
dfm_body_maerz <- dfm(data$Body, groups = data$MonthYear, dictionary = maerz.lexicon)

df_body_maerz <- convert(dfm_body_maerz, "data.frame") %>%
  mutate(MonthYear = doc_id) %>%
  rename(
    illiberalism = `autocratic vs. democratic.democratic.3 liberalism.autocratic.2 illiberalism`,
    democracy = `autocratic vs. democratic.democratic.4 democratic procedures.democracy`,
    maintenance_of_power = `autocratic vs. democratic.autocratic.1 autocratic procedures.maintenance of power`,
    reforms = `autocratic vs. democratic.democratic.4 democratic procedures.institutional reforms`
  ) %>%
  select(MonthYear, illiberalism, democracy, maintenance_of_power, reforms) %>%
  gather(key = "Maerz_Type", value = "Frequency", illiberalism:reforms)

df_body_maerz <- df_body_maerz %>%
  group_by(MonthYear) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

ggplot(df_body_maerz, aes(x = MonthYear, y = Proportion, colour = Maerz_Type, group = Maerz_Type)) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Political Dimensions in Conspiracy Comments_Maerz Dictionary (Proportions)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Proportion of terms")

```

```{r}
ggplot(df_body_maerz, aes(x = MonthYear, y = Frequency, colour = Maerz_Type, group = Maerz_Type)) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Political dimensions in Conspiracy Comments_Maerz dictionary (Frequency)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Frequency of terms")

```


###################
csv's
#################

2nd Dictionary_Laver-Garry

```{r}
dfm_body <- dfm(body_vector, dictionary = lavergarry.lexicon)

```

```{r}

dfm_body_prop <- dfm_weight(dfm_body, scheme = "prop")

df_body <- convert(dfm_body_prop, "data.frame")

head(df_body)
```

```{r}
install.packages("writexl")
library(writexl)

```


```{r}
library(tidyverse)
data <- data %>%
mutate(X = 1:1022048) %>%
mutate(X = as.character(X))
output<-df_body %>%
  mutate(X=1:1022048) %>% mutate(X=as.character(X)) %>% 
  full_join(data) %>% 
  select(-doc_id,-X)

library(writexl)
write.csv(output, "output_lavergarry.lexicon.csv", row.names = FALSE)
write_xlsx(output, "output_lavergarry.lexicon.xlsx")

```

```{r}
group_dates <- as.character(data$Date)
dfm_body_freq_day <- dfm(data$Body, groups = group_dates, dictionary = lavergarry.lexicon)
df_freq_day <- convert(dfm_body_freq_day, "data.frame")
df_freq_day<-df_freq_day %>% rename(date=doc_id)

write.csv(df_freq_day, "frequencies_by_day_lavergarry-lexicon.csv", row.names = FALSE)

``` 

Proportions by day

```{r}
group_dates <- as.character(data$Date)
dfm_body_day <- dfm(data$Body, groups = group_dates, dictionary = lavergarry.lexicon)
dfm_body_prop_day <- dfm_weight(dfm_body_day, scheme = "prop")
df_prop_day <- convert(dfm_body_prop_day, "data.frame")
df_prop_day<-df_prop_day %>% rename(date=doc_id)

write.csv(df_prop_day, "proportions_by_day_lavergarry-lexicon.csv", row.names = FALSE)

```

#Third Dictionary

```{r}
dfm_body <- dfm(body_vector, dictionary = policy_agendas_dict)

```

```{r}

dfm_body_prop <- dfm_weight(dfm_body, scheme = "prop")

df_body <- convert(dfm_body_prop, "data.frame")

head(df_body)

```


```{r}
library(tidyverse)
data <- data %>%
mutate(X = 1:1022048) %>%
mutate(X = as.character(X))
output<-df_body %>%
  mutate(X=1:1022048) %>% mutate(X=as.character(X)) %>% 
  full_join(data) %>% 
  select(-doc_id,-X)

library(writexl)
write.csv(output, "output_policy_agendas_dict.csv", row.names = FALSE)
write_xlsx(output, "output_policy_agendas_dict.xlsx")

```

```{r}

group_dates <- as.character(data$Date)

dfm_body_freq_day <- dfm(data$Body, groups = group_dates, dictionary = policy_agendas_dict)
df_freq_day <- convert(dfm_body_freq_day, "data.frame")
df_freq_day<-df_freq_day %>% rename(date=doc_id)

write.csv(df_freq_day, "frequencies_by_day_policy_agendas_dict.csv", row.names = FALSE)

``` 

Proportions by day

```{r}
group_dates <- as.character(data$Date)

dfm_body_day <- dfm(data$Body, groups = group_dates, dictionary = policy_agendas_dict)
dfm_body_prop_day <- dfm_weight(dfm_body_day, scheme = "prop")
df_prop_day <- convert(dfm_body_prop_day, "data.frame")
df_prop_day<-df_prop_day %>% rename(date=doc_id)

write.csv(df_prop_day, "proportions_by_day_policy_agendas_dict.csv", row.names = FALSE)

```

#Fourth dictionary

```{r}
dfm_body <- dfm(body_vector, dictionary = maerz.lexicon)

```

```{r}
dfm_body_prop <- dfm_weight(dfm_body, scheme = "prop")

df_body <- convert(dfm_body_prop, "data.frame") %>% rename(
    illiberalism = `autocratic vs. democratic.democratic.3 liberalism.autocratic.2 illiberalism`,
    democracy = `autocratic vs. democratic.democratic.4 democratic procedures.democracy`,
    maintenance_of_power = `autocratic vs. democratic.autocratic.1 autocratic procedures.maintenance of power`,
    reforms = `autocratic vs. democratic.democratic.4 democratic procedures.institutional reforms`
  )  %>% select(doc_id, illiberalism, democracy, maintenance_of_power, reforms)


head(df_body)
```


```{r}
library(tidyverse)
data <- data %>%
mutate(X = 1:1022048) %>%
mutate(X = as.character(X))
output<-df_body %>%
  mutate(X=1:1022048) %>% mutate(X=as.character(X)) %>% 
  full_join(data) %>% 
  select(-doc_id,-X)

library(writexl)
write.csv(output, "output_maerz.lexicon.csv", row.names = FALSE)
write_xlsx(output, "output_maerz.lexicon.xlsx")


```

```{r}

group_dates <- as.character(data$Date)

dfm_body_freq_day <- dfm(data$Body, groups = group_dates, dictionary = maerz.lexicon)

df_freq_day <- convert(dfm_body_freq_day, "data.frame") %>% rename(
    illiberalism = `autocratic vs. democratic.democratic.3 liberalism.autocratic.2 illiberalism`,
    democracy = `autocratic vs. democratic.democratic.4 democratic procedures.democracy`,
    maintenance_of_power = `autocratic vs. democratic.autocratic.1 autocratic procedures.maintenance of power`,
    reforms = `autocratic vs. democratic.democratic.4 democratic procedures.institutional reforms`
  )  %>% select(doc_id, illiberalism, democracy, maintenance_of_power, reforms)


df_freq_day<-df_freq_day %>% rename(date=doc_id)

write.csv(df_freq_day, "frequencies_by_day_maerz_lexicon.csv", row.names = FALSE)

``` 

Proportions by day

```{r}
group_dates <- as.character(data$Date)

dfm_body_day <- dfm(data$Body, groups = group_dates, dictionary = maerz.lexicon)

dfm_body_prop_day <- dfm_weight(dfm_body_day, scheme = "prop")

df_prop_day <- convert(dfm_body_prop_day, "data.frame")%>% rename(
    illiberalism = `autocratic vs. democratic.democratic.3 liberalism.autocratic.2 illiberalism`,
    democracy = `autocratic vs. democratic.democratic.4 democratic procedures.democracy`,
    maintenance_of_power = `autocratic vs. democratic.autocratic.1 autocratic procedures.maintenance of power`,
    reforms = `autocratic vs. democratic.democratic.4 democratic procedures.institutional reforms`
  )  %>% select(doc_id, illiberalism, democracy, maintenance_of_power, reforms)

df_prop_day<-df_prop_day %>% rename(date=doc_id)

write.csv(df_prop_day, "proportions_by_day_maerz_lexicon.csv", row.names = FALSE)

```
