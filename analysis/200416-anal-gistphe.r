## R file to play with analysis

indta <- read.csv("../data/200104-GDO_data_wide_sarcoma.csv")

mydta <- indta[indta$Tumour.Type.2== "Gastrointestinal stromal sarcoma (GIST)", ]

##View(mydta)
##colnames(mydta)

selectcols <- c("Year", "Tumour.Type.3", "Age", "Incidence", "Population", "Incidence.Rate", "Incidence.Rate.LCI..95..", "Incidence.Rate.UCI..95..","Routes.Population", "Two.Week.Wait", "Two.Week.Wait.percentage", "Two.Week.Wait.LCI", "Two.Week.Wait.UCI", "GP.Referral", "GP.Referral.percentage", "GP.Referral.LCI", "GP.Referral.UCI", "Other.Outpatient", "Other.Outpatient.percentage", "Other.Outpatient.LCI", "Other.Outpatient.UCI", "Inpatient.Elective", "Inpatient.Elective.percentage", "Inpatient.Elective.LCI", "Inpatient.Elective.UCI","Emergency.Presentation", "Emergency.Presentation.percentage", "Emergency.Presentation.LCI", "Emergency.Presentation.UCI", "DCO", "DCO.percentage", "DCO.LCI", "DCO.UCI", "Unknown.Route", "Unknown.Route.percentage", "Unknown.Route.LCI", "Unknown.Route.UCI", "Route.not.classified", "Route.not.classified.percentage","Route.not.classified.LCI", "Route.not.classified.UCI")

colids  <- which(colnames(mydta) %in% selectcols)

mydta2 <- mydta[,colids]

View(mydta2)

library("ggplot2")
library("tidyr")

myyr <- c(2013, 2014, 2015, 2016, 2017)

mydta3 <- mydta2[mydta2$Year%in%myyr,]

mydta3[,4:41]<-apply(mydta3[,4:41],2,function(ind) as.double(as.character(ind)))

mydta3$Year <- as.integer(as.character(mydta3$Year))

mydta4<- gather(mydta3, "key", "route", Two.Week.Wait.percentage, GP.Referral.percentage, Other.Outpatient.percentage, Inpatient.Elective.percentage, Emergency.Presentation.percentage, DCO.percentage, Unknown.Route.percentage, Route.not.classified.percentage)

mydta4$key  <- substr(mydta4$key, 1, nchar(mydta4$key)-11)

unique(mydta4$key)

myordkey  <- rev(c("GP.Referral","Emergency.Presentation", "Other.Outpatient", "Two.Week.Wait", "Inpatient.Elective", "DCO",  "Route.not.classified"  , "Unknown.Route"))

mydta4<-mydta4[order(match(mydta4$key, myordkey)),]

mydta4$key <- factor(mydta4$key, levels=myordkey) 

## number not rate
mydta5<- gather(mydta3, "key", "route", Two.Week.Wait, GP.Referral, Other.Outpatient, Inpatient.Elective, Emergency.Presentation, DCO, Unknown.Route, Route.not.classified)

unique(mydta5$key)

myordkey  <- rev(c("GP.Referral","Emergency.Presentation", "Other.Outpatient", "Two.Week.Wait", "Inpatient.Elective", "DCO",  "Route.not.classified"  , "Unknown.Route"))

mydta5<-mydta5[order(match(mydta5$key, myordkey)),]

mydta5$key <- factor(mydta5$key, levels=myordkey) 



## plot 1 - incidence rate by year


ggplot(data = mydta3[mydta3$Age=="All ages",], aes(x = Year, y = Incidence.Rate, group=Tumour.Type.3)) +
    geom_line(aes(color=Tumour.Type.3, linetype=Tumour.Type.3), size=1) +
    geom_point(aes(color=Tumour.Type.3, shape=Tumour.Type.3), size=3) +
    geom_segment(aes(x = Year, xend = Year, y = Incidence.Rate.LCI..95.., yend = Incidence.Rate.UCI..95.., colour = Tumour.Type.3), size = 1) +
    ylim(0,1.8) +
    labs(title = "Crude GIST Incidence (95%CI) by year",
         x = "Year", y = "Incidence rate (per 100,000 population)") +
     theme(legend.title=element_blank())

##plot 2 - number by year

t.inc<- mydta3$Age=="All ages" & mydta3$Tumour.Type.3!="All"
ggplot(data = mydta3[t.inc,], aes(x = Year, y = Incidence)) +
    geom_col(aes(fill=Tumour.Type.3))+
    labs(title = "Number GIST diagnosed by year",
         x = "Year", y = "Number") +
     theme(legend.title=element_blank())


##plot 2 - route by year

p <- ggplot(mydta4[mydta4$Age=="All ages" & mydta4$Tumour.Type.3=="All",], aes(x = Year, y = route))+
  geom_col(aes(fill = key), width = 0.7) +
    labs(title = "Routes by year",
         x = "Year", y = "Percentage (%)") 







## plot 3 - Stomach by age group and year

bp <- ggplot(data = mydta3[mydta3$Tumour.Type.3=="Stomach" & mydta3$Age!="All ages",], aes(x = Year, y = Incidence.Rate, group=Age)) +
    geom_line(aes(color=Age, linetype=Age), size=1) +
    geom_point(aes(color=Age, shape=Age), size=3) +
    geom_segment(aes(x = Year, xend = Year, y = Incidence.Rate.LCI..95.., yend = Incidence.Rate.UCI..95.., colour = Age), size = 1) +
    labs(title = "GIST Stomach Tumour Incidence (95%CI)",
         x = "Year", y = "Incidence rate (per 100,000 population)") +
    theme(legend.title=element_blank())


bp <- ggplot(data = mydta3[mydta3$Tumour.Type.3=="Stomach" & mydta3$Age!="All ages",], aes(x = Year, y = Incidence, group=Age)) +
    geom_col(aes(fill=Age)) +
    labs(title = "GIST Stomach Tumour Number by year",
         x = "Year", y = "Number diagnosed") +
    theme(legend.title=element_blank())




##plot 3 - stomach route by age

p <- ggplot(mydta5[mydta5$Tumour.Type.3=="Stomach" & (mydta5$Age!="All ages"),], aes(x=Age,y=route))+
    geom_col(aes(fill = key), width = 0.7) +
    labs(title = "Stomatch routes (2013-16) by age group",
         x = "Age group", y = "Number (total 2013-16)") 



p <- ggplot(mydta5[mydta5$Tumour.Type.3=="Stomach",], aes(Age))+
    geom_bar(position="fill",aes(y=route,fill = key), width = 0.7, stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Stomatch routes (2013-16) by age group",
         x = "Age group", y = "Percentage (%)") 








