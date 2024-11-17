rm(list=ls())

Mydata <- read.csv("C:\\Users\\anish\\Desktop\\DI\\DI_Dataset.csv")

#Head and Summary of the data
head(Mydata)
summary(Mydata)

#Average of Dow jones Average
average_dow_jones <- Mydata %>%
  group_by(Year) %>%
  summarise(Average_Dow_Jones = mean(Dow.Jones.Average, na.rm = TRUE))

print(average_dow_jones)

Mydata$Dow.Jones.Average <- as.numeric(as.character(Mydata$Dow.Jones.Average))
Mydata$CPI <- as.numeric(as.character(Mydata$CPI))
Mydata$Inflation <- as.numeric(sub("%", "", as.character(Mydata$Inflation)))
Mydata$Interest.Rate <- as.numeric(as.character(Mydata$Interest.Rate))
Mydata$Unemployment <- as.numeric(as.character(Mydata$Unemployment))
Mydata$GDP <- as.numeric(as.character(Mydata$GDP))

#Removing missing values
Mydata <- na.omit(Mydata)

#Removing duplicates
Mydata <- unique(Mydata)

#Converting to a date format
Mydata$Time.Frame <- as.Date(paste("01-", Mydata$Time.Frame, sep = ""), format = "%d-%b-%Y")

Mydata$Inflation <- as.numeric(str_replace_all(Mydata$Inflation, "[^0-9.]+", ""))


#Heatmap

library(corrplot)

n_data <- Mydata[, sapply(Mydata, is.numeric)]

corr_matrix <- cor(n_data, use = "complete.obs") 

corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "white", col = colorRampPalette(c("lightblue", "darkblue"))(20))


#3D Scatter Plot

library(ggplot2)
library(plotly)
library(stringr)

Mydata$Time.Frame <- as.Date(paste("01-", Mydata$Time.Frame, sep = ""), format = "%d-%b-%Y")

Mydata$Inflation <- as.numeric(str_replace_all(Mydata$Inflation, "[^0-9.]+", ""))

plot_ly(Mydata, x = ~Inflation, y = ~Dow.Jones.Average, z = ~Year, color = ~Inflation,
        type = "scatter3d", mode = "markers", size = ~Inflation,
        marker = list(colorscale = "Viridis", opacity = 0.8)) %>%
  layout(scene = list(xaxis = list(title = "Inflation"),
                      yaxis = list(title = "Dow Jones Average"),
                      zaxis = list(title = "Year")))



#Line Graph for Inflation and Interest Rates

library(dplyr)


Mydata$Inflation <- as.numeric(gsub("%", "", Mydata$Inflation))

Yearly_data <- Mydata %>%
  group_by(Year) %>%
  summarise(Inflation = sum(Inflation, na.rm = TRUE),
            Interest.Rate = mean(Interest.Rate, na.rm = TRUE))

print(Yearly_data)

ggplot(Yearly_data, aes(x = Year)) +
  geom_line(aes(y = Inflation, color = "Inflation"), size = 1.2) +
  geom_line(aes(y = Interest.Rate, color = "Interest Rate"), size = 1.2, linetype = "dashed") +
  labs(title = "Inflation and Interest Rate Over Time",
       x = "Year",
       y = "Value",
       color = "Legend") +
  scale_color_manual(values = c("Inflation" = "blue", "Interest Rate" = "red"))



#Dow jones Average Yearly area chart

ggplot(Mydata, aes(x = Year, y = Dow.Jones.Average, fill = "pink")) +
  geom_area(color = "White", alpha = 0.6) +
  labs(title = "Cumulative Dow Jones Average Over Time",
       x = "Year",
       y = "Cumulative Dow Jones Average")


#Hypothesis Testing
#Hypothesis: specific inflation levels significantly influence stock market behavior.

model <- lm(Dow.Jones.Average ~ Inflation, data = Mydata)

summary(model)


#Statistical Inference

conf_intervals <- confint(model)

print(conf_intervals)


#Correlation Analysis

correlation_matrix <- cor(Mydata[, c("Dow.Jones.Average", "Inflation", "Interest.Rate", "Unemployment", "GDP")])

print(correlation_matrix)

library(corrplot)
corrplot(correlation_matrix, method = "circle")


#Applied Probability

library(MASS)

fit <- fitdistr(Mydata$Dow.Jones.Average, "normal")

threshold <- 20000

prob_increase <- 1 - pnorm(threshold, mean = fit$estimate[1], sd = fit$estimate[2])

prob_decrease <- pnorm(threshold, mean = fit$estimate[1], sd = fit$estimate[2])

cat("Probability of the Dow Jones Average exceeding threshold:", prob_increase, "\n")
cat("Probability of the Dow Jones Average falling under threshold:", prob_decrease, "\n")


#Regression : Multi Linear Regression

model <- lm(Dow.Jones.Average ~ Inflation + Interest.Rate + Unemployment + GDP, data = Mydata)


summary(model)

par(mfrow = c(2, 2))  # Create a 2x2 layout for plots

plot(model, which = 1, main = "Residuals vs. Fitted")

plot(model, which = 5, main = "Residuals vs. Leverage")

par(mfrow = c(1, 1))


