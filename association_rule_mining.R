library(arules)
library(arulesViz)


# loading data #wczytujemy dane
data("AdultUCI") 

dim(AdultUCI)
View(AdultUCI)

names(AdultUCI) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", 
                 "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", 
                 "hours_per_week", "native_country", "income")

#checking missing data #sprawdzamy braki danych  
summary(AdultUCI)

#completing missing data #uzepełniamy brakujące dane 
res <- sapply(AdultUCI,function(x) {anyNA(x)})
idx = which(res == TRUE)

AdultUCI_U <-AdultUCI
for (i in 1:length(idx))
{
  if(is.factor(AdultUCI_U[[idx[i]]]))
  {
    tab = table(AdultUCI_U[idx[i]])
    value = names(tab[which(tab == max(tab))[1]])
    AdultUCI_U[which(is.na(AdultUCI_U[idx[i]])==TRUE),idx[i]] <- value
  }
  else if(is.numeric(AdultUCI_U[[idx[i]]])==TRUE)
  {
    AC[which(is.na(AdultUCI_U[idx[i]])==TRUE),idx[i]] <- mean(na.omit(AdultUCI_U[idx[i]]))        
  }
}
summary(AdultUCI_U)

#Removing attributes with one value
#Usuwamy atrybuty z jedną wartoscią

delOneValued <- function(inputData)
{
  res <- c(which(sapply(inputData, function(x) {length(unique(x))}) == 1));
  if(length(res) > 0)         
  {
    data11 <- inputData[,-res];
  }   
  else
  {
    data11 <-inputData;
  }
  data11 
}

which(sapply(AdultUCI_U, function(x) {length(unique(x))}) == 1);

AdultUCI_U <- delOneValued(AdultUCI_U)

str(AdultUCI_U)

#Removing attributes with only unique values
#Usuwanie atrybutów z tylko unikalnymi wartościami

delUniqueValueAtt<- function(inputData)
{
  res <- c(which(sapply(inputData, function(x) {length(unique(x))}) == nrow(inputData)));
  if(length(res) >0)         
  {
    data11 <- inputData[,-res];
  }   
  else
  {
    data11 <-inputData;
  }
  
  data11 
}

which(sapply(AdultUCI_U, function(x) {length(unique(x))}) == nrow(AdultUCI_U))

AdultUCI_U <- delUniqueValueAtt(AdultUCI_U)
str(AdultUCI_U)

#Removing duplicates
#Usuwamy duplikaty

which(duplicated(AdultUCI_U) == TRUE)
length(which(duplicated(AdultUCI_U) == TRUE))

AdultUCI_U <- unique(AdultUCI_U)
dim(AdultUCI_U)

# Initial data analysis
# Wstępna analiza danych

table(AdultUCI_U$age)
summary(AdultUCI_U$age)
png("Image_1.png", res=80, height=800, width=2400)
plot(table(AdultUCI_U$age), type = 'h', col = 'blue', xlab = 'Age', 
     ylab = 'Frequency')
grid()
dev.off()

summary(AdultUCI_U$workclass)
png("Image_2.png", res=80, height=800, width=2400)
barplot(table(AdultUCI$"workclass"), cex.names=0.7)
dev.off() 

summary(AdultUCI_U$fnlwgt)

summary(AdultUCI_U$education)
png("Image_3.png", res=80, height=800, width=2400)
plot(table(AdultUCI_U$education), type = 'h', col = 'blue', xlab = 'Age', 
     ylab = 'Frequency')
grid()
dev.off()

summary(AdultUCI_U$marital_status)

summary(AdultUCI_U$occupation)

summary(AdultUCI_U$relationship)
png("Image_4.png", res=80, height=800, width=2400)
barplot(table(AdultUCI$"relationship"), cex.names=0.8)
grid()
dev.off()

summary(AdultUCI_U$race)
png("Image_5.png", res=80, height=800, width=2400)
barplot(table(AdultUCI$"race"), cex.names=0.8)
grid()
dev.off()

summary(AdultUCI_U$sex)
png("Image_6.png", res=80, height=800, width=2400)
barplot(table(AdultUCI$"sex"), cex.names=0.8)
grid()
dev.off()

summary(AdultUCI_U$hours_per_week)
png("Image_7.png", res=80, height=800, width=2400)
hist(AdultUCI$hours_per_week, breaks = 15)
dev.off()

summary(AdultUCI_U$native_country)

## Grouping data from the 'education' attribute
## Grupowanie danych z atrybutu 'education'

group_education <- function(x) {
  x <- as.character(x)
  if(x == "HS-grad") {
    return("High_School_grad")
  } 
  else if(x == "Prof-school" | x == "Masters" | x == "Doctorate") {
    return("Prof_sch.-Mast-Doct")
  } 
  else if(x %in% c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th",       
                     "11th", "12th")) {
    return("Compulsory")
  } 
  else if(x == "Assoc-acdm" | x == "Assoc-voc") {
    return("Associate")
  } 
  else {
    return(x)
  }
}

AdultUCI_U$education<-sapply(AdultUCI_U$education, group_education)
table(AdultUCI_U$education)

## Grupowanie danych z atrybutu workclass

group_workclass<-function(job){
  job <- as.character(job)
  if(job=='State-gov'|job=='Local-gov'|job=='Federal-gov'){
    return('FSL-gov')
  }
  else if(job=='Self-emp-inc'|job=='Self-emp-not-inc'){
    return('self_emp')
  }
  else if(job=='Never-worked'|job=='Without-pay'){
    return('Unemployed')
  }
  else{
    return(job)
  }
}

AdultUCI_U$workclass<-sapply(AdultUCI_U$workclass, group_workclass)
table(AdultUCI_U$workclass)


summary(AdultUCI_U$occupation)

## Grouping data from the `marital_status` attribute into groups: Married, Not-Married, Never-Married
## Grupowanie danych z atrybutu `marital_status` na grupy: Married, Not-Married, Never-Married
group_marital<-function(mar){
  mar<-as.character(mar)
  if(mar=='Separated'|mar=='Widowed'|mar=='Divorced'){
    return('Not-Married')
  }
  else if (mar== 'Never-married'){
    return(mar)
  }
  else{
    return('Married')
  }
}

AdultUCI_U$marital_status <- sapply(AdultUCI_U$marital_status, group_marital)
table(AdultUCI_U$marital_status)

## Grouping data from the 'native_country' attribute into groups: Asia, North.America, Europe, Latin.and.South.America i Other
## Grupowanie danych z atrybutu 'native_country' na grupy: Asia, North.America, Europe, Latin.and.South.America i Other

Asia<-c('China','Hong','India','Iran','Cambodia','Japan', 'Laos',
        'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North_America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin_and_South_America')
  }else{
    return('Other_continent')      
  }
}

AdultUCI_U$native_country <- sapply(AdultUCI_U$native_country, group_country)
table(AdultUCI_U$native_country)

# Discretization of continuous attributes
# Dyskretyzacja atrybutów ciagłych 

AdultUCI_U[["age"]] <- ordered(cut(AdultUCI_U[[ "age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI_U[["hours_per_week"]] <- ordered(cut(AdultUCI_U[[ "hours_per_week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI_U[["capital_gain"]] <- ordered(cut(AdultUCI_U[["capital_gain"]],
                                           c(-Inf,0,median(AdultUCI_U[["capital_gain"]][AdultUCI_U[["capital_gain"]]>0]),Inf)),
                                       labels = c("None", "Low", "High"))

AdultUCI_U[["capital_loss"]] <- ordered(cut(AdultUCI_U[["capital_loss"]],
                                           c(-Inf,0, median(AdultUCI_U[["capital_loss"]][AdultUCI_U[["capital_loss"]]>0]),Inf)),
                                       labels = c("None", "Low", "High"))

summary(AdultUCI_U)
str(AdultUCI_U)

# Removing unnecessary attributes
# Usuwanie niepotrzebnych atrybutów

AdultUCI_U[["fnlwgt"]] <- NULL
AdultUCI_U[["education_num"]] <-NULL

# Data conversion from relational (data.frame) to transactional (transactions)
# Konwersja danych z postacji relacyjnej (data.frame) na transakcyjną (transactions)

adultTR <- as(AdultUCI_U, "transactions")
inspect(head(adultTR))
str(adultTR)
class(adultTR)
summary(adultTR)

# Initial analysis of transactional data
# Wstępna analizę danych transakcyjnych

# Checking the frequency of occurrence of items
# Sprawdzanie częstość występowania elementów

freqTbl = itemFrequency(adultTR, type = "relative")
summary(freqTbl)
print(freqTbl)

# Sorting items by relative support
# Sortowanie elementów wg wsparcia względnego
freqTbl = sort(freqTbl, decreasing= TRUE)

# Checking the number of elements with support from 1% to 36%
# Sprawdzenie liczby elementów ze wsparciem od 1% do 36%
sup = seq(0.01,0.36,0.05)
v<-vector(mode = 'integer', length = length(sup))

for( i in 1:length(sup))
{
  v[i] = length(freqTbl[freqTbl>=sup[i]])
  print(sup[i])
  print(v[i])
}
# For 1% support we have 55 elements, for 6% support - 38 elements, for 36% support - 11 elements.
# Dla wsparcia 1% mamy 55 el-w, dla wsparcia 6% - 38 el-w, dla wsparcia 36% - 11 el-w.

# Elements chart for 5% support
# Image elementów dla wsparcia 5%
png("Image_8.png", res=80, height=800, width=2400)
itemFrequencyPlot(adultTR, type ="relative", support= 0.05)
dev.off()

#Chart of the 10 most common elements
#Image 10 najczęściej występujących elementów
png("Image_9.png", res=80, height=800, width=2400)
itemFrequencyPlot(adultTR, topN = 10, main = "10 najczęsciej występujących elementów")
dev.off()

### Frequent itemsets

# Finding frequent itemsets using the Apriori algorithm 
aParam  = new("APparameter", "confidence" = 0.6, "support" =0.25, "minlen"= 2, maxtime = 20, target ="frequent itemsets") 
print(aParam)
asets <-apriori(adultTR,aParam)
str(asets)

# Analysis of found frequent itemsets
# Przeanalizujemy wykryte zbiory częste
length(asets) # 539 itemsets
summary(asets)
inspect(head(sort(asets, by="support"),10)) 
#[1]  {capital_loss=None,native_country=North_America}                   0.8786637 0                        42870
#[2]  {capital_gain=None,capital_loss=None}                              0.8705267 0                        42473
#[3]  {capital_gain=None,native_country=North_America}                   0.8450297 0                        41229
#[4]  {race=White,capital_loss=None}                                     0.8135889 0                        39695
#[5]  {capital_loss=None,income=small}                                   0.8084034 0                        39442
#[6]  {race=White,native_country=North_America}                          0.8064562 0                        39347
#[7]  {capital_gain=None,capital_loss=None,native_country=North_America} 0.8010863 0                        39085
#[8]  {capital_gain=None,income=small}                                   0.7910023 0                        38593
#[9]  {race=White,capital_gain=None}                                     0.7816561 0                        38137
#[10] {native_country=North_America,income=small}                        0.7716335 0                        37648

# "confidence" = 0.8; "support" = 0.25
aParam@confidence <-0.8
aParam@support <-0.3
asets <-apriori(adultTR,aParam)
length(asets) # 343 itemsets
inspect(head(sort(asets, by="support"),10))
#[1]  {capital_loss=None,native_country=North_America}                   0.8786637 0                        42870
#[2]  {capital_gain=None,capital_loss=None}                              0.8705267 0                        42473
#[3]  {capital_gain=None,native_country=North_America}                   0.8450297 0                        41229
#[4]  {race=White,capital_loss=None}                                     0.8135889 0                        39695
#[5]  {capital_loss=None,income=small}                                   0.8084034 0                        39442
#[6]  {race=White,native_country=North_America}                          0.8064562 0                        39347
#[7]  {capital_gain=None,capital_loss=None,native_country=North_America} 0.8010863 0                        39085
#[8]  {capital_gain=None,income=small}                                   0.7910023 0                        38593
#[9]  {race=White,capital_gain=None}                                     0.7816561 0                        38137
#[10] {native_country=North_America,income=small}                        0.7716335 0                        37648

size(asets)
# Sets with more than 5 items:
inspect(asets[size(asets)>5]) 

# Charts of sets with more than 5 items:
png("Image_10.png", res=80, height=800, width=2400)
plot(asets[size(asets)>5], method = "graph")
dev.off()

png("Image_11.png", res=80, height=800, width=2400)
plot(asets[size(asets)>5], method = "paracoord", control = list(reorder = TRUE))
dev.off()

# The itemsets containing "race=Black", "sex=Female", "native_country=North_America", "income=small"
setsRace <- subset(asets, subset = items %in% "race=Black")
inspect(setsRace) # 0 itemsets with race=Black
setsSex <- subset(asets, subset = items %in% "sex=Female")
inspect(setsSex) # 5 itemsets with "sex=Female" 
#[1] {sex=Female,income=small}                        0.3073376 0.0000e+00               14995
#[2] {sex=Female,capital_gain=None}                   0.3122771 0.0000e+00               15236
#[3] {sex=Female,native_country=North_America}        0.3070301 0.0000e+00               14980
#[4] {sex=Female,capital_loss=None}                   0.3201476 0.0000e+00               15620
#[5] {sex=Female,capital_gain=None,capital_loss=None} 0.3009223 2.0496e-05               14682

sets_NC_income <- subset(asets, subset = items %ain% c("native_country=North_America", "income=small"))
inspect(sets_NC_income) # 52 itemsets with "native_country=North_America", "income=small"
#[1]  {race=White,sex=Male}                                       0.5883583             0.000000e+00 28706
#[2]  {relationship=Husband, race=White, sex=Male}                0.3657922             0.000000e+00 17847     
#[3]  {marital_status=Married, race=White, sex=Male}              0.3657922             0.000000e+00 17847     
#[4]  {age=Middle-aged, race=White, sex=Male}                     0.3050420             0.000000e+00 14883
#[5]  {race=White, sex=Male, hours_per_week=Full-time}            0.3176881             0.000000e+00 15500

# Closed itemsets
# Zbiory zamknięte
table(is.closed(asets)) # 27 zbiory, które mają nadzbiory, oraz 316 zbiorów zamkniętych
# FALSE  TRUE 
#   27   316 


# Maximum itemsets
# Zbiory maksymalne
maxSets <- asets[is.maximal(asets)==TRUE]
inspect(maxSets) # 50 maximum itemsets
summary(maxSets) # to są zbiory wieloelementowe
# 2  3  4  5  6 
# 7 11 10  8 14

## Finding frequent itemsets using function eclat
## Dla porównania wyszukamy zbiory wykorzystując funkcję eclat
ecParam  = new("ECparameter", "confidence" = 0.8, "support" = 0.3, "minlen"= 2) 
print(ecParam)
fsets <- eclat(adultTR,ecParam)
length(fsets) # the same number of sets as with the apriory function - 343

#zmieńmy dla eksperymentu wartość parametru support = 0.4, żeby sprawdzić 
#jakie zbiory zostały wykryte przy użyciu funkcji apriori, a nie zostały wykryte przy uzyciu funkcji eclat
ecParam@support = 0.4
print(ecParam)
fsets_2 <- eclat(adultTR,ecParam)
length(fsets_2) # mamy 146 zbiory

inspect(asets[which(is.na(asets %in% fsets_2) == TRUE)])
#zostało wykryto 197 zbiorów


##### Finding association rules

# Finding association rules using the Apriori algorithm
# Użyjemy funkcję apriori do wykrycia regul asocjacyjnych

aParam@target ="rules"
aParam@minlen = 2L
aParam@confidence =0.8

aRules <-apriori(adultTR,aParam)

summary(aRules)
length(aRules) # 842 rules

# Zróbmy dataframa, żeby w bardziej czytelny sposób zobaczyć wykryte reguły wraz z ich właściwościami
rule_data <- DATAFRAME(aRules, 
                       separate = TRUE, 
                       setStart = '', 
                       itemSep = ',', 
                       setEnd = '')

View(rule_data)
inspect(head(aRules))
#    lhs                               rhs                            support   confidence coverage  lift     count
#[1] {education=High_School_grad}   => {capital_gain=None}            0.3021111 0.9347454  0.3232015 1.019020 14740
#[2] {education=High_School_grad}   => {native_country=North_America} 0.3048576 0.9432431  0.3232015 1.022367 14874
#[3] {education=High_School_grad}   => {capital_loss=None}            0.3111293 0.9626482  0.3232015 1.009882 15180
#[4] {marital_status=Never-married} => {income=small}                 0.3195122 0.9694652  0.3295757 1.155044 15589
#[5] {marital_status=Never-married} => {capital_gain=None}            0.3156999 0.9578980  0.3295757 1.044260 15403
#[6] {marital_status=Never-married} => {native_country=North_America} 0.3046116 0.9242537  0.3295757 1.001785 14862
inspectDT(head(aRules))

png("Image_12.png", res=80, height=800, width=2400)
plot(aRules, measure = c("support", "lift"), shading = "confidence")
dev.off()
# z grafika widzimy, że im wyższe wsparcie, tym niższe zaufanie i im wyższe zaufanie, tym niższe wsparcie
# przy dłuższych regułach mamy niższe wsparcie i wyższe zaufanie

# wykryjmy dla eksperymentu reguły z nastepnikiem (sex=Male, marital_status=Married)
rulesWithRHS <- apriori(adultTR, parameter = list(support=0.25, confidence = 0.6, minlen = 2), 
                        appearance = list(rhs = c("sex=Male", "marital_status=Married")))
length(rulesWithRHS) # wyszło nam 160 reguł
inspect(rulesWithRHS[1:10])
#     lhs                               rhs                      support   confidence coverage  lift      count
#[1]  {relationship=Husband}         => {marital_status=Married} 0.4038737 1.0000000  0.4038737 2.1183571 19705
#[2]  {relationship=Husband}         => {sex=Male}               0.4038532 0.9999493  0.4038737 1.4958157 19704
#[3]  {age=Middle-aged}              => {sex=Male}               0.3499078 0.6924637  0.5053085 1.0358506 17072
#[4]  {hours_per_week=Full-time}     => {sex=Male}               0.3722689 0.6363605  0.5849969 0.9519262 18163
#[5]  {workclass=Private}            => {sex=Male}               0.4878869 0.6494066  0.7512810 0.9714419 23804
#[6]  {income=small}                 => {sex=Male}               0.5319943 0.6338307  0.8393318 0.9481420 25956
#[7]  {race=White}                   => {sex=Male}               0.5883583 0.6881458  0.8549908 1.0293914 28706
#[8]  {capital_gain=None}            => {sex=Male}               0.6050215 0.6595688  0.9172986 0.9866434 29519
#[9]  {native_country=North_America} => {sex=Male}               0.6155770 0.6672146  0.9226071 0.9980808 30034
#[10] {capital_loss=None}            => {sex=Male}               0.6330805 0.6641438  0.9532281 0.9934871 30888

#zróbmy inny eksperyment z nastepnikiem "sex=Female" oraz "race=Black"
rulesWithRHS_2 <- apriori(adultTR, parameter = list(support=0.25, confidence = 0.6, minlen = 2), 
                        appearance = list(rhs = c("sex=Female", "race=Black")))
length(rulesWithRHS_2) # brak reguł z zadanymi następnikami

#spróbójmy zmienić wartości parametrów reguł na support=0.1, confidence = 0.4
rulesWithRHS_3 <- apriori(adultTR, parameter = list(support=0.1, confidence = 0.4, minlen = 2), 
                          appearance = list(rhs = c("sex=Female", "race=Black")))
length(rulesWithRHS_2) # też mamy brak reguł z zadanymi parametrami

#teraz wykryjmy reguły, które nie zawierają (capital_gain=None, capital-loss=none) 
rulesNotItems <- apriori(adultTR, parameter = list(support=0.25, confidence = 0.6, minlen = 2), 
                         appearance = list(none = c("capital_gain=None", "capital_loss=None")))
length(rulesNotItems) # wyszło nam 363 reguły
inspect(rulesNotItems[1:10])
#     lhs                               rhs                            support   confidence coverage  lift      count
#[1]  {education=High_School_grad}   => {workclass=Private}            0.2557286 0.7912360  0.3232015 1.0531825 12477
#[2]  {education=High_School_grad}   => {income=small}                 0.2888912 0.8938423  0.3232015 1.0649451 14095
#[3]  {education=High_School_grad}   => {race=White}                   0.2741340 0.8481831  0.3232015 0.9920378 13375
#[4]  {education=High_School_grad}   => {native_country=North_America} 0.3048576 0.9432431  0.3232015 1.0223670 14874
#[5]  {marital_status=Never-married} => {workclass=Private}            0.2755073 0.8359453  0.3295757 1.1126932 13442
#[6]  {marital_status=Never-married} => {income=small}                 0.3195122 0.9694652  0.3295757 1.1550440 15589
#[7]  {marital_status=Never-married} => {race=White}                   0.2702398 0.8199627  0.3295757 0.9590310 13185
#[8]  {marital_status=Never-married} => {native_country=North_America} 0.3046116 0.9242537  0.3295757 1.0017848 14862
#[9]  {sex=Female}                   => {workclass=Private}            0.2633941 0.7945468  0.3315024 1.0575894 12851
#[10] {sex=Female}                   => {income=small}                 0.3073376 0.9271052  0.3315024 1.1045753 14995

# teraz zmieńmy wartości parametrów na support=0.4 oraz confidence = 0.7 
rulesNotItems_2 <- apriori(adultTR, parameter = list(support=0.4, confidence = 0.7, minlen = 2), 
                         appearance = list(none = c("capital_gain=None", "capital_loss=None")))
length(rulesNotItems_2) # mamy 74 reguły
inspect(rulesNotItems_2[1:10])

#teraz wyszukamy interesujące reguły
#wyfiltrójemy reguły dla których wartość parametru lift > 1.3
rulesLift1.3 <- subset(aRules, subset =  lift > 1.3)
inspect(head(rulesLift1.3))
#    lhs                                              rhs                      support   confidence coverage  lift     count
#[1] {relationship=Husband}                        => {marital_status=Married} 0.4038737 1.0000000  0.4038737 2.118357 19705
#[2] {marital_status=Married}                      => {relationship=Husband}   0.4038737 0.8555488  0.4720639 2.118357 19705
#[3] {relationship=Husband}                        => {sex=Male}               0.4038532 0.9999493  0.4038737 1.495816 19704
#[4] {marital_status=Married}                      => {sex=Male}               0.4144907 0.8780392  0.4720639 1.313452 20223
#[5] {marital_status=Married,relationship=Husband} => {sex=Male}               0.4038532 0.9999493  0.4038737 1.495816 19704
#[6] {relationship=Husband,sex=Male}               => {marital_status=Married} 0.4038532 1.0000000  0.4038532 2.118357 19704
size(rulesLift1.3)
length(rulesLift1.3) # mamy 99 reguł

# Grafiki prezentujące wybrane reguły
png("Image_13.png", res=80, height=800, width=2400)
plot(rulesLift1.3, shading="order", control=list(main = "Two-key plot" ))
dev.off()
# Na Imageach widzimy, że reguły dłuższe mają większe zaufanie, ale nizsze wsparcie oraz odwrotnie mamy z krótszymi regułami
png("Image_14.png", res=80, height=800, width=2400)
plot(rulesLift1.3, method="matrix", measure="lift", engine = 'interactive')
dev.off()

# Wybierzmy dla eksperymentu reguły z następnikiem "marital_status=Married" oraz lift>=2
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "marital_status=Married" & lift >=2)

inspect(rulesInGivenConseq) # wyszło 28 reguł, na które możemy popatrzeć na interaktywnym grafiku
inspect(head(rulesInGivenConseq))
#    lhs                                                    rhs                      support   confidence coverage  lift     count
#[1] {relationship=Husband}                              => {marital_status=Married} 0.4038737 1          0.4038737 2.118357 19705
#[2] {relationship=Husband,sex=Male}                     => {marital_status=Married} 0.4038532 1          0.4038532 2.118357 19704
#[3] {relationship=Husband,race=White}                   => {marital_status=Married} 0.3658127 1          0.3658127 2.118357 17848
#[4] {relationship=Husband,capital_gain=None}            => {marital_status=Married} 0.3553802 1          0.3553802 2.118357 17339
#[5] {relationship=Husband,native_country=North_America} => {marital_status=Married} 0.3729453 1          0.3729453 2.118357 18196
#[6] {relationship=Husband,capital_loss=None}            => {marital_status=Married} 0.3780693 1          0.3780693 2.118357 18446
plot(rulesInGivenConseq, method = "graph", engine = "htmlwidget")

#zwiększamy lift do 2.3
rulesInGivenConseq_2 <- subset(aRules, subset = rhs %in% "marital_status=Married" & lift >=2.3)
inspect(rulesInGivenConseq_2) # brak reguł, gdy lift>=2.3 oraz następnikiem jest "marital_status=Married"

 
#wyszukanie reguł opartych o częste zbiory maksymalne. Zbiory maksymalne to są zbiory, które nie mają nadzbiorów
maxRul <- aRules[is.maximal(aRules) == TRUE]
summary(maxRul) # wyszło nam 137 reg. Mamy przewagę reguł długich.
#2  3  4  5  6 
#6 15 20 31 65 
inspect(maxRul[1:6])
#    lhs                               rhs                            support   confidence coverage  lift     count
#[1] {education=High_School_grad}   => {capital_gain=None}            0.3021111 0.9347454  0.3232015 1.019020 14740
#[2] {education=High_School_grad}   => {native_country=North_America} 0.3048576 0.9432431  0.3232015 1.022367 14874
#[3] {education=High_School_grad}   => {capital_loss=None}            0.3111293 0.9626482  0.3232015 1.009882 15180
#[4] {marital_status=Never-married} => {native_country=North_America} 0.3046116 0.9242537  0.3295757 1.001785 14862
#[5] {sex=Female}                   => {income=small}                 0.3073376 0.9271052  0.3315024 1.104575 14995
#[6] {sex=Female}                   => {native_country=North_America} 0.3070301 0.9261778  0.3315024 1.003870 14980

#teraz usuniemy reguły nadmiarowe
notRedun <- aRules[is.redundant(aRules) == FALSE]
summary(notRedun) # mamy 104 reguły, tutaj widzimy przewagę krótszych reguł
#2  3  4  5 
#62 26 13  3 
inspect(notRedun[1:6])
#lhs                               rhs                            support   confidence coverage  lift     count
#[1] {education=High_School_grad}   => {capital_gain=None}            0.3021111 0.9347454  0.3232015 1.019020 14740
#[2] {education=High_School_grad}   => {native_country=North_America} 0.3048576 0.9432431  0.3232015 1.022367 14874
#[3] {education=High_School_grad}   => {capital_loss=None}            0.3111293 0.9626482  0.3232015 1.009882 15180
#[4] {marital_status=Never-married} => {income=small}                 0.3195122 0.9694652  0.3295757 1.155044 15589
#[5] {marital_status=Never-married} => {capital_gain=None}            0.3156999 0.9578980  0.3295757 1.044260 15403
#[6] {marital_status=Never-married} => {native_country=North_America} 0.3046116 0.9242537  0.3295757 1.001785 14862

#wybirzemy reguły na podstawie wskaźnika improvement, żeby zobaczyć jakie i o ile dane reguły są atrakcyjne dla nas
resTbl <- interestMeasure(aRules,"improvement")
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)
intersRule <- aRules[intres] 
inspect(intersRule) # dostaliśmy zbiór z 74 reguł ze wskaznikiem improvement w przdziale (0.01; 1]
inspect(head(sort(intersRule, by="support")))# pierwsze 6 najlepszych reguł:
#    lhs                               rhs                            support   confidence coverage  lift     count
#[1] {education=High_School_grad}   => {capital_gain=None}            0.3021111 0.9347454  0.3232015 1.019020 14740
#[2] {education=High_School_grad}   => {native_country=North_America} 0.3048576 0.9432431  0.3232015 1.022367 14874
#[3] {education=High_School_grad}   => {capital_loss=None}            0.3111293 0.9626482  0.3232015 1.009882 15180
#[4] {marital_status=Never-married} => {income=small}                 0.3195122 0.9694652  0.3295757 1.155044 15589
#[5] {marital_status=Never-married} => {capital_gain=None}            0.3156999 0.9578980  0.3295757 1.044260 15403
#[6] {marital_status=Never-married} => {native_country=North_America} 0.3046116 0.9242537  0.3295757 1.001785 14862

#dla eksperymentu wybierzmy reguły na podstawie wskaźnika improvement w przdziale (0.5; 1]
resTbl_2 <- interestMeasure(aRules,"improvement")
intres_2 <- which(sapply(resTbl_2, function(x) {x > 0.5  && x <= 1 })==TRUE)
intersRule_2 <- aRules[intres_2] 
inspect(intersRule_2) # wyszło nam 62 reguły
inspect(head(sort(intersRule_2, by="support"))) # pierwsze 6 najlepszych reguł:
#[1] {native_country=North_America} => {capital_loss=None}            0.8786637 0.9523704  0.9226071 0.9991002 42870
#[2] {capital_loss=None}            => {native_country=North_America} 0.8786637 0.9217769  0.9532281 0.9991002 42870
#[3] {capital_gain=None}            => {capital_loss=None}            0.8705267 0.9490113  0.9172986 0.9955763 42473
#[4] {capital_loss=None}            => {capital_gain=None}            0.8705267 0.9132407  0.9532281 0.9955763 42473
#[5] {capital_gain=None}            => {native_country=North_America} 0.8450297 0.9212155  0.9172986 0.9984917 41229
#[6] {native_country=North_America} => {capital_gain=None}            0.8450297 0.9159150  0.9226071 0.9984917 41229

# wybierzmy reguły na podstawie współczynika pewności(certainty) w przedziale [0.5; 1]
resTbl_3 <- interestMeasure(aRules,"certainty")
intres_3 <- which(sapply(resTbl_3, function(x) {x >= 0.5  && x <= 1 })==TRUE)
intersRule_3 <- aRules[intres_3] 
inspect(intersRule_3) # mamy 108 reguł
inspect(head(sort(intersRule_3, by="support"))) # to są najlepsze reguły:
#    lhs                                              rhs                      support   confidence coverage  lift     count
#[1] {marital_status=Married}                      => {sex=Male}               0.4144907 0.8780392  0.4720639 1.313452 20223
#[2] {relationship=Husband}                        => {marital_status=Married} 0.4038737 1.0000000  0.4038737 2.118357 19705
#[3] {marital_status=Married}                      => {relationship=Husband}   0.4038737 0.8555488  0.4720639 2.118357 19705
#[4] {relationship=Husband}                        => {sex=Male}               0.4038532 0.9999493  0.4038737 1.495816 19704
#[5] {marital_status=Married,relationship=Husband} => {sex=Male}               0.4038532 0.9999493  0.4038737 1.495816 19704
#[6] {relationship=Husband,sex=Male}               => {marital_status=Married} 0.4038532 1.0000000  0.4038532 2.118357 19704

# wybierzmy reguły na podstawie współczynika pewności(certainty) w przedziale [-1; -0.5]
resTbl_4 <- interestMeasure(aRules,"certainty")
intres_4 <- which(sapply(resTbl_4, function(x) {x >= -1  && x <= -0.5 })==TRUE)
intersRule_4 <- aRules[intres_4] 
inspect(intersRule_4) # wyszło 30 reguł
inspect(head(intersRule_4))


# wybierzmy reguły na podstawie współczynika pewności(certainty) = 0
resTbl_5 <- interestMeasure(aRules,"certainty")
intres_5 <- which(sapply(resTbl_5, function(x) {x = 0})==TRUE)
intersRule_5 <- aRules[intres_5] 
inspect(intersRule_5) # brak reguł niezależnych
inspect(head(intersRule_5))

#### Algorithm eclat
# Dla porównania zgenerujemy reguły na podstawie wczesniej utworzonych zbiorów częstych używając metody eclat
ecParam  = new("ECparameter", "confidence" = 0.8, "support" = 0.3, "minlen"= 2) 
fsets <- eclat(adultTR,ecParam) # mamy 343 częste zbiory

#generowanie regul
iERules = ruleInduction(fsets, adultTR, confidence = 0.8, control = list(verobose = TRUE))

summary(iERules)
length(iERules) # mamy tyle samo reguł, co przy użyciu metody apriory - 842
inspect(head(iERules))
#    lhs                                                 rhs                            support   confidence lift     itemset
#[1] {education=High_School_grad}                     => {capital_loss=None}            0.3111293 0.9626482  1.009882 1      
#[2] {education=High_School_grad}                     => {native_country=North_America} 0.3048576 0.9432431  1.022367 2      
#[3] {education=High_School_grad}                     => {capital_gain=None}            0.3021111 0.9347454  1.019020 3      
#[4] {marital_status=Never-married,income=small}      => {capital_loss=None}            0.3103915 0.9714542  1.019120 4      
#[5] {marital_status=Never-married,capital_loss=None} => {income=small}                 0.3103915 0.9712673  1.157191 4      
#[6] {marital_status=Never-married,income=small}      => {capital_gain=None}            0.3087313 0.9662583  1.053374 5  


#Wnioski:
#1. Po użyciu metody Apriory i metody eklat do wykrywania zbiorów częstych oraz reguł asocjacyjnych dostaliśmy jednakowe wyniki
# w ilościach odzyskanych zbiorów i reguł. Rożnica jest tylko w szybkości wykonania algorytmu. Metoda eclat generuje zbiory oraz reguły szybciej.
#2. Do wykrywania reguł interesujących zostały wykorzystane takie wskazniki jak wspołczynnik podniesienia(lift), współczynnik pewności(cf) oraz
# współczynnik improvement dlatego, że współczynniki wsparcia i ufności są niewystarczające do oceny atrakcyjności reguł.
#3. Za pomocą współczynnika pewnośći wykryliśmy zależne pozytywnie reguły, gdy "cf > 0", zależne niegatywnie reguły, gdy "cf < 0",  
# nie wykryliśmy niezależne reguły. Zależność reguł również w eksperymentach jest widoczna po użyciu wskaźnika lift,
# gdy lift > 1 - zależne pozytywnie,  gdy lift < 1 - zależne negatywnie oraz  gdy lift = 1 - niezależne reguły.
#4. Do wykrywania zbiorów i reguł ustawialiśmy próg wsparcia, po którym reguły zostały wyszukiwane. Jeśli ten próg jest niewielki,
# to można dostać bardzo dużo reguł, dlatego te progi podnosimy. Ale to podejście jest nie do końca dobre, bo możemy nie wykryć jakieś unikatowe
# i atrakcyjne dla użytkownika reguły.
#5. Najlepsze reguły wyszukaliśmy po użyciu wskaźników cf oraz improvement sortując je po wsparciu, ale to są najlepsze reguły tylko dla tych przypadków.
#Jeślibyśmy zastosowali inne wskaźniki, to dostalibyśmy inne "lepsze" reguły.

