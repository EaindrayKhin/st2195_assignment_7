# ======== load libraries ========
#install.packages("tidyverse")
library(DBI)
library(dplyr)
library(tidyverse)

# ======== Import the data ========

titanic <- read.csv("titanic.csv")


# Initialize ggplot
ggplot(titanic)

# Generate a series of bar charts to describe the gender, ticket class and survival of the passengers onboard 
# Assign plot to a variable
# 1a - Bar Chart for Gender
ggplot(titanic, aes(x = Sex, fill = Sex)) + geom_bar() +
  labs(title = "Gender of the passengers onboard")
#ggtitle('Gender of the passengers') #alternative way to set title

#1b - Bar Chart for Ticket Class
ggplot(titanic, aes(x=Pclass, fill = factor(Pclass))) + geom_bar() +
  ggtitle("Ticket class of the pasengers onboard") +
  xlab("ticket class") +
  theme(legend.position = "none")

#1b _ Bar Chart for Ticket Class
#titanic %>%
 # mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  #ggplot(aes(x=Pclass, fill = factor(Pclass))) + geom_bar() +
  #ggtitle("Ticket class of the pasengers onboard") +
  #xlab("ticket class") +
  #theme(legend.position = "none")

#1c - Bar Chart for Survival
titanic %>% 
  mutate(Survived = factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  ggplot(aes(x=Survived, fill = Survived)) + geom_bar() +
  ggtitle("Survived of the passengers onboard") +
  xlab("ticket class") +
  theme(legend.position = "none")

# Generate histogram for the passengers' age. 
# Furthermore, describe the passengers’ age using the following two boxplots: age per ticket class and age based on survival 
# Assign plot to a variable

#2a - Histogram (show frequency)
ggplot(titanic, aes(x = Age)) + 
  geom_histogram(fill="Steelblue", bins=10, na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard")

#2b - Boxplot (group by ticket class)
titanic %>% 
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Pclass)) +
  geom_boxplot(na.rm=TRUE) +
  ggtitle("Age of the passengers onboard") +
  xlab("ticket class") +
  theme(legend.position = "none")

#2c - Boxplot (group by survival)
titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=Survived, y=Age, fill=Survived)) +
  geom_boxplot(na.rm=TRUE) +
  ggtitle("Age of the passengers onboard") +
  theme(legend.position = "none")

#Generate a histogram for the travel fare and a table showing the number of people who did not pay -
#you may want to check on Google why a handful of people was on board for free!

#3a - Histogram for Ticket Fare
ggplot(titanic, aes(x=Fare, y=..density..)) +
  geom_histogram(fill="steelblue", bins=30, na.rm=TRUE) +
  ggtitle("Ticket fare of the passengers onboard")

#3b - Table for Paid vs Unpaid
paid_table <- table(titanic$Fare !=0)
names(paid_table) <- c("did not pay", "paid")
paid_table

#4 - A chart of your choice to describe the family size per ticket class
titanic$family_size <- titanic$SibSp + titanic$Parch + 1
table(titanic$family_size)

titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=family_size, y=..density.., fill = Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(Pclass ~ ., scales="free") +
  ggtitle("Family size per ticket class")

titanic %>% 
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=family_size, y=..density.., fill = Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(~ Pclass) +
  ggtitle("Family size per ticket class")

# A series of stacked bar charts to show the how survival differs for different gender and ticket class 
#5a - Bar Chart (survival by gender)
titanic %>%
  mutate (Survived=factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  ggplot(aes(fill=Survived, x=Sex)) +
  geom_bar(position = "stack") +
  ggtitle("Survival by gender") +
  xlab("gender")

#5a - Bar Chart (survival by ticket class)
x <- titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(fill=Survived, x=Pclass)) +
  geom_bar(position = "stack") +
  ggtitle("Survival by ticket class") +
  xlab("ticket class")
x

#positoin='stack' with data labels (1)
x + geom_text(aes(label=..count..),
              stat="count",
              size=3,
              position="stack",
              vjust=1.5)

#position='stack' with data labels (2)
totals <- titanic %>% group_by(Pclass) %>% summarise(total=n())
x + geom_text(data=totals,
              aes(x=Pclass, y=total, label=total, fill=NULL),
              size=3,
              nudge_y = 30)

#6 - A violin chart describing how survival related to age and gende
titanic %>% 
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  ggplot(aes(x=Sex, y=Age, fill=Survived)) +
  geom_violin(na.rm = TRUE, adjust=0.5)

#7 - A violin chart describing the survival rate related to age and ticket class
titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Survived)) +
  geom_violin(na.rm=TRUE, adjust=0.5) +
  xlab("ticket class")

#From the graphs above, what kind of associations between the variables like age, 
#gender, ticket class and the survival do you observe

titanic %>% 
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No", "Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Survived)) +
  geom_violin(na.rm=TRUE, adjust=0.5) +
  facet_grid(Sex~.)
  xlab("ticket class")
