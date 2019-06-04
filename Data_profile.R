###############################
### Case Study eshop
### Katerina Lahodova
###############################

# Knihovny ---------------------------------------------------------------
library("readxl")
library("dplyr")
library(lubridate)
library(ggplot2)
library("lmtest")


# Nacteni a cisteni dat -------------------------------------------------------------
my_data <- read_excel("Online Retail.xlsx")

summary(my_data)

# Zakaznici bez ID
View(my_data[is.na(my_data$CustomerID),])
nrow(my_data[is.na(my_data$CustomerID),])

data_clean <- my_data %>%
  subset(!is.na(CustomerID) & Country != "Unspecified" & !(StockCode %in% c("POST", "PADS", "M", "DOT", "D", "CRUK", "C2", "BANK CHARGES"))) %>%
  mutate(TotalPrice = UnitPrice*Quantity,
         Cancelation = ifelse(grepl("C", InvoiceNo), 1, 0),
         InvoiceYear = lubridate::year(InvoiceDate),
         InvoiceMonth = lubridate::month(InvoiceDate),
         InvoiceWeek = lubridate::week(InvoiceDate))

#### DQ check
# Cancelations
data_clean %>%
  subset(Cancelation == 1 & (TotalPrice > 0)) %>% 
  nrow
# 0 rows - OK

# Stock codes a Description
DuplicateCodes <- data_clean %>%
  select(StockCode, Description) %>%
  unique %>%
  group_by(StockCode) %>%
  summarise(DescriptionNames = n()) %>%
  subset(DescriptionNames != 1) %>%
  select(StockCode)

data_clean %>%
  subset(StockCode %in% DuplicateCodes$StockCode) %>%
  select(StockCode, Description) %>%
  unique %>%
  View
# Vetsi mnozstvi preklepu a jinych nazvu, lepsi pouzivat code

# Data faktur
InvoiceMoreDates <- data_clean %>%
  select(InvoiceNo, InvoiceDate) %>%
  unique %>%
  group_by(InvoiceNo) %>%
  summarise(DatesNo = n()) %>%
  subset(DatesNo != 1) %>%
  select(InvoiceNo)

data_clean %>%
  subset(InvoiceNo %in% InvoiceMoreDates$InvoiceNo) %>%
  select(InvoiceNo, InvoiceDate) %>%
  unique %>%
  group_by(InvoiceNo) %>%
  summarise(DatesDiff = as.numeric(max(InvoiceDate) - min(InvoiceDate))) %>%
  select(DatesDiff) %>%
  max
# Maximalni rozdil v casech faktur je 1 minuta - zanedbame, v nasi analyze zpracovavame max tyden


# Agregace dat ------------------------------------------------------------

# Zakaznici, faktury
data_agg <- data_clean %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(InvoicePrice = sum(TotalPrice),
            InvoiceQuantity = sum(Quantity),
            InvoiceDate = min(InvoiceDate), #zanedbavame vice casu u faktur, bereme zahajeni fakturace
            InvoiceYear = unique(InvoiceYear),
            InvoiceMonth = unique(InvoiceMonth),
            InvoiceWeek = unique(InvoiceWeek),
            Cancelation = unique(Cancelation),
            Country = unique(Country))

summary(data_agg)

# agregace pres jednotlive zeme
data_country <- data_agg %>%
  group_by(Country) %>%
  summarise(TotalPrice = sum(InvoicePrice),
            TotalQuantity = sum(InvoiceQuantity),
            Customers = length(unique(CustomerID)),
            Invoices = n(),
            TotalCancelation = sum(Cancelation),
            PercenategCancelation = sum(Cancelation)/n(),
            MinDate = min(InvoiceDate),
            MaxDate = max(InvoiceDate),
            MonthsTotal = max(InvoiceDate) - min(InvoiceDate))

summary(data_country)


# Analyza Germany --------------------------------------------------------------

data_Ger <- data_clean %>%
  subset(Country == "Germany") %>%
  group_by(StockCode, Description) %>%
  summarise(QuantityTotal = sum(Quantity),
            AvgUnitPrice = mean(UnitPrice),
            MinUnitPrice = min(UnitPrice),
            MaxUnitPrice = max(UnitPrice),
            PriceTotal = sum(Quantity*UnitPrice))

data_Ger_time <- data_clean %>%
  subset(Country == "Germany") %>%
  group_by(InvoiceYear, InvoiceMonth, InvoiceWeek) %>%
  summarise(QuantityTotal = sum(Quantity),
            AvgUnitPrice = mean(UnitPrice),
            MinUnitPrice = min(UnitPrice),
            MaxUnitPrice = max(UnitPrice),
            PriceTotal = sum(TotalPrice)) %>%
  mutate(InvoiceYearWeek = paste(InvoiceYear, InvoiceWeek, sep = "_"),
         InvoiceYearWeekNumber = InvoiceYear*100 + InvoiceWeek) %>%
  arrange(InvoiceYearWeekNumber) %>%
  ungroup %>%
  mutate(Rank = 1:n())

# Graf trzeb
ggplot(data_Ger_time) +
  aes(y = PriceTotal, x = Rank) +
  geom_line(col = "dodgerblue3") +
  theme_minimal() +
  theme(text = element_text(size = 9),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = "Trzby [libra]",
       x = "Tyden") +
  scale_x_continuous(breaks = data_Ger_time$Rank[0:7*10+1], labels = data_Ger_time$InvoiceYearWeek[0:7*10+1]) +
  guides(col = guide_legend(nrow = 1))

# Polynomialni vyhlazeni
data_Ger_time <- data_Ger_time %>%
  mutate(Rank2 = Rank^2,
         Rank3 = Rank^3,
         Rank4 = Rank^4,
         Rank5 = Rank^5)

# Odhad trendu
model1 <- lm(PriceTotal ~ Rank, data = data_Ger_time)
summary(model1)

model2 <- lm(PriceTotal ~ Rank + Rank2, data = data_Ger_time)
summary(model2)

model3 <- lm(PriceTotal ~ Rank + Rank2 + Rank3, data = data_Ger_time)
summary(model3)
data_Ger_time$Model3 <- predict(model3, data_Ger_time[, c("Rank", "Rank2", "Rank3")])

# graficke zobrazeni modelu
f <- function(x) coef(model3)[1] + coef(model3)[2]*x + coef(model3)[3]*x^2 + coef(model3)[4]*x^3

ggplot(data_Ger_time) +
  aes(x = Rank) +
  geom_line(aes(y = PriceTotal, col = "Realne trzby")) +
  stat_function(fun = f, aes(colour = "Odhad trendu")) +
  theme_minimal() +
  theme(text = element_text(size = 7),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = "Trzby [libra]",
       x = "Tyden") +
  scale_x_continuous(breaks = data_Ger_time$Rank[0:6*10+1], labels = data_Ger_time$InvoiceYearWeek[0:6*10+1]) +
  scale_colour_manual(name = "", breaks = c("Realne trzby", "Odhad trendu"),
                      values = c("Realne trzby" = "dodgerblue3", 
                                 "Odhad trendu" = "firebrick2")) +
  guides(col = guide_legend(nrow = 1))


# tests
shapiro.test(residuals(model3))

bptest(model3, studentize = FALSE)

# correlogram
acf(residuals(model3), lag.max=12, main = "Correlogram of Residuals", ylab = "Autocorrelation")

### Zakaznici v Nemecku
data_Ger_cust <- data_clean %>%
  subset(Country == "Germany") %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(TotalPrice = sum(TotalPrice),
            TotalItems = n()) %>%
  group_by(CustomerID) %>%
  summarise(AvgPrice = mean(TotalPrice),
            AvgItems = mean(TotalItems),
            TotalPrice = sum(TotalPrice),
            TotalItems = sum(TotalItems))


# Analyza France ----------------------------------------------------------

data_Fra <- data_clean %>%
  subset(Country == "France") %>%
  group_by(StockCode, Description) %>%
  summarise(QuantityTotal = sum(Quantity),
            AvgUnitPrice = mean(UnitPrice),
            MinUnitPrice = min(UnitPrice),
            MaxUnitPrice = max(UnitPrice),
            PriceTotal = sum(Quantity*UnitPrice))

data_Fra_time <- data_clean %>%
  subset(Country == "France") %>%
  group_by(InvoiceYear, InvoiceMonth, InvoiceWeek) %>%
  summarise(QuantityTotal = sum(Quantity),
            AvgUnitPrice = mean(UnitPrice),
            MinUnitPrice = min(UnitPrice),
            MaxUnitPrice = max(UnitPrice),
            PriceTotal = sum(TotalPrice)) %>%
  mutate(InvoiceYearWeek = paste(InvoiceYear, InvoiceWeek, sep = "_"),
         InvoiceYearWeekNumber = InvoiceYear*100 + InvoiceWeek) %>%
  arrange(InvoiceYearWeekNumber) %>%
  ungroup %>%
  mutate(Rank = 1:n())

# Graf trzeb
ggplot(data_Fra_time) +
  aes(y = PriceTotal, x = Rank) +
  geom_line(col = "dodgerblue3") +
  theme_minimal() +
  theme(text = element_text(size = 9),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = "Trzby [libra]",
       x = "Tyden") +
  scale_x_continuous(breaks = data_Fra_time$Rank[0:6*10+1], labels = data_Fra_time$InvoiceYearWeek[0:6*10+1]) +
  guides(col = guide_legend(nrow = 1))

# Polynomialni vyhlazeni
data_Fra_time <- data_Fra_time %>%
  mutate(Rank2 = Rank^2,
         Rank3 = Rank^3,
         Rank4 = Rank^4,
         Rank5 = Rank^5)

# Odhad trendu
model1 <- lm(PriceTotal ~ Rank, data = data_Fra_time)
summary(model1)

model2 <- lm(PriceTotal ~ Rank + Rank2, data = data_Fra_time)
summary(model2)

model3 <- lm(PriceTotal ~ Rank + Rank2 + Rank3, data = data_Fra_time)
summary(model3)
data_Fra_time$Model3 <- predict(model3, data_Fra_time[, c("Rank", "Rank2", "Rank3")])

model4 <- lm(PriceTotal ~ Rank + Rank2 + Rank3 + Rank4, data = data_Fra_time)
summary(model4)
data_Fra_time$Model4 <- predict(model4, data_Fra_time[, c("Rank", "Rank2", "Rank3", "Rank4")])

model5 <- lm(PriceTotal ~ Rank + Rank2 + Rank3 + Rank4 + Rank5, data = data_Fra_time)
summary(model5)

# graficke zobrazeni modelu
# f <- function(x) coef(model4)[1] + coef(model4)[2]*x + coef(model4)[3]*x^2 + coef(model4)[4]*x^3 + coef(model4)[5]*x^4
f <- function(x) coef(model3)[1] + coef(model3)[2]*x + coef(model3)[3]*x^2 + coef(model3)[4]*x^3

ggplot(data_Fra_time) +
  aes(x = Rank) +
  geom_line(aes(y = PriceTotal, col = "Realne trzby")) +
  stat_function(fun = f, aes(colour = "Odhad trendu")) +
  theme_minimal() +
  theme(text = element_text(size = 7),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = "Trzby [libra]",
       x = "Tyden") +
  scale_x_continuous(breaks = data_Fra_time$Rank[0:6*10+1], labels = data_Fra_time$InvoiceYearWeek[0:6*10+1]) +
  scale_colour_manual(name = "", breaks = c("Realne trzby", "Odhad trendu"),
                      values = c("Realne trzby" = "dodgerblue3", 
                                 "Odhad trendu" = "firebrick2")) +
  guides(col = guide_legend(nrow = 1))


# tests
shapiro.test(residuals(model4))
shapiro.test(residuals(model3))


bptest(model4, studentize = FALSE)
bptest(model3, studentize = FALSE)

# correlogram
acf(residuals(model4), lag.max=12, main = "Correlogram of Residuals", ylab = "Autocorrelation")
acf(residuals(model3), lag.max=12, main = "Correlogram of Residuals", ylab = "Autocorrelation")


### Zakaznici ve Francii
data_Fra_cust <- data_clean %>%
  subset(Country == "France") %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(TotalPrice = sum(TotalPrice),
            TotalItems = n()) %>%
  group_by(CustomerID) %>%
  summarise(AvgPrice = mean(TotalPrice),
            AvgItems = mean(TotalItems),
            TotalPrice = sum(TotalPrice),
            TotalItems = sum(TotalItems))


### T.test pro porovnani rozdilu mezi Fr a Ger
t.test(data_Fra_cust$TotalPrice, data_Ger_cust$TotalPrice)
t.test(data_Fra_cust$TotalItems, data_Ger_cust$TotalItems)
