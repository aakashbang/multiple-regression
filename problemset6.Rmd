---
title: 'INFX 573: Problem Set 6 - Regression'
author: "Aakash Bang"
date: 'Due: Tuesday, November 15, 2016'
output: pdf_document
header-includes:
- \newcommand{\benum}{\begin{enumerate}}
- \newcommand{\eenum}{\end{enumerate}}
- \newcommand{\bitem}{\begin{itemize}}
- \newcommand{\eitem}{\end{itemize}}
---

<!-- This syntax can be used to add comments that are ignored during knitting process. -->

##### Collaborators: 

##### Instructions: #####

Before beginning this assignment, please ensure you have access to R and RStudio. 

1. Download the `problemset6.Rmd` file from Canvas. Open `problemset6.Rmd` in RStudio and supply your solutions to the assignment by editing `problemset6.Rmd`. 

2. Replace the "Insert Your Name Here" text in the `author:` field with your own full name. Any collaborators must be listed on the top of your assignment. 

3. Be sure to include well-documented (e.g. commented) code chucks, figures and clearly written text chunk explanations as necessary. Any figures should be clearly labeled and appropriately referenced within the text. 

4. Collaboration on problem sets is acceptable, and even encouraged, but each student must turn in an individual write-up in his or her own words and his or her own work. The names of all collaborators must be listed on each assignment. Do not copy-and-paste from other students' responses or code.

5. When you have completed the assignment and have **checked** that your code both runs in the Console and knits correctly when you click `Knit PDF`, rename the R Markdown file to `YourLastName_YourFirstName_ps6.Rmd`, knit a PDF and submit the PDF file on Canvas.

##### Setup: #####

In this problem set you will need, at minimum, the following R packages.

```{r Setup, message=FALSE}
# Load standard libraries
library(tidyverse)
library(MASS) # Modern applied statistics functions
```

\textbf{Housing Values in Suburbs of Boston}

In this problem we will use the Boston dataset that is available in the \texttt{MASS} package. This dataset contains information about median house value for 506 neighborhoods in Boston, MA. Load this data and use it to answer the following questions.

\benum

\item Describe the data and variables that are part of the \texttt{Boston} dataset. Tidy data as necessary.

The Boston dataset contains data about the housing values in suburbs of Boston.
Variables -
crim - per capita crime rate by town.
zn - proportion of residential land zoned for lots over 25,000 sq.ft.
indus - proportion of non-retail business acres per town.
chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
nox - nitrogen oxides concentration (parts per 10 million).
rm - average number of rooms per dwelling.
age - proportion of owner-occupied units built prior to 1940.
dis - weighted mean of distances to five Boston employment centres.
rad - index of accessibility to radial highways.
tax - full-value property-tax rate per \$10,000.
ptratio - pupil-teacher ratio by town.
black - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
ltstat - lower status of the population (percent).
medv - median value of owner-occupied homes in \$1000s

```{r}
#1

BostonData <- MASS::Boston

# Change column names
colnames(BostonData) <- c("Crime_Rate", "Zoned_Land", "Indus", "Tract_Bound", "NOX", 
"Avg_Rooms", "Owner_Occupied", "Distance", "Rad", 
"Tax", "PTRatio", "Blacks", "Lower_Status", "Median_Value")
```

\item Consider this data in context, what is the response variable of interest? Discuss how you think some of the possible predictor variables might be associated with this response.

Response variable - median value of owner-occupied homes

Possible Predictor Variables -
```{r}
#2
fit = lm(log(Median_Value) ~ ., data = BostonData)
summary(fit)

```

We can observe that Indus, Owner_Occupied are not statistically significant and can be removed from the model. The remaining variables are statistically significant based on the summary.


\item For each predictor, fit a simple linear regression model to predict the response. In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions. 

```{r}
#3
#Crime Rate Vs Median Value
fit_crim <- lm(Median_Value ~ Crime_Rate, data = BostonData)
summary(fit_crim)
plot(BostonData$Crime_Rate, BostonData$Median_Value, 
xlab = "Per Capita Crime Rate", ylab = "Median Value of Homes in $1000s")
```

```{r}
#Zoned_Land vs Median_Value
fit_zn <- lm(Median_Value ~ Zoned_Land, data = BostonData)
summary(fit_zn)
plot(BostonData$Zoned_Land, BostonData$Median_Value, 
xlab = "Proportion of Residential Land over 25k sq. ft.", ylab = "Median Value of Homes in $1000s")
abline(fit_zn)
```

```{r}
#Tract_Bound vs Median_Value
fit_tract <- lm(Median_Value ~ Tract_Bound, data = BostonData)
summary(fit_tract)
plot(BostonData$Tract_Bound, BostonData$Median_Value, 
xlab = "Tract Bounds River", ylab = "Median Value of Homes in $1000s")
abline(fit_tract)
```

```{r}
#NOX vs Median_Value
fit_nox <- lm(Median_Value ~ NOX, data = BostonData)
summary(fit_nox)
plot(BostonData$NOX, BostonData$Median_Value, 
xlab = "Nitrogen Oxide Poroportion (ppm)", ylab = "Median Value of Homes in $1000s")
abline(fit_nox)
```

```{r}
#Avg Rooms vs Median_Value
fit_rooms <- lm(Median_Value ~ Avg_Rooms, data = BostonData)
summary(fit_rooms)
plot(BostonData$Avg_Rooms, BostonData$Median_Value, 
xlab = "Avg No. of Rooms per Dwelling", ylab = "Median Value of Homes in $1000s")
abline(fit_rooms)
```

The plot shows a strong association between average number of rooms and median value of the houses. We will draw a plot of fitted values and residuals to back up this assertion.

```{r}
residual_rooms = lm(fit_rooms$residuals~ fit_rooms$fitted.values)
plot(fit_rooms$fitted.values, fit_rooms$residuals, 
xlab = "Avg Rooms Fitted Values", ylab = "Avg Rooms Residuals")
abline(residual_rooms)
```

The plot of fitted values vs residuals shows a strong grouping of observations around the zero line and the plot looks similar to the average rooms vs median value plot. Thus, average room is a significant variable in our model.

```{r}
#Distance vs Median_Value
fit_distance <- lm(Median_Value ~ Distance, data = BostonData)
summary(fit_distance)
plot(BostonData$Distance, BostonData$Median_Value, 
xlab = "Weighted Mean of Distances to Employment Centres", ylab = "Median Value of Homes in $1000s")
abline(fit_distance)
```

```{r}
#Rad vs Median_Value
fit_rad <- lm(Median_Value ~ Rad, data = BostonData)
summary(fit_rad)
plot(BostonData$Rad, BostonData$Median_Value, 
xlab = "Index of Accessibility to Radial Highways", ylab = "Median Value of Homes in $1000s")
abline(fit_rad)
```

```{r}
#Tax vs Median_Value
fit_tax <- lm(Median_Value ~ Tax, data = BostonData)
summary(fit_tax)
plot(BostonData$Tax, BostonData$Median_Value, 
xlab = "Property Tax Rate per $10k", ylab = "Median Value of Homes in $1000s")
abline(fit_tax)
```

```{r}
#PTRatio vs Median_Value
fit_ptr <- lm(Median_Value ~ PTRatio, data = BostonData)
summary(fit_ptr)
plot(BostonData$PTRatio, BostonData$Median_Value, 
xlab = "Pupil Teacher Ratio", ylab = "Median Value of Homes in $1000s")
abline(fit_ptr)
```

```{r}
#Blacks vs Median_Value
fit_blacks <- lm(Median_Value ~ Blacks, data = BostonData)
summary(fit_blacks)
plot(BostonData$Blacks, BostonData$Median_Value, 
xlab = "Proportion of Blacks", ylab = "Median Value of Homes in $1000s")
abline(fit_blacks)
```

```{r}
#Lower_Status vs Median_Value
fit_status <- lm(Median_Value ~ Lower_Status, data = BostonData)
summary(fit_status)
plot(BostonData$Lower_Status, BostonData$Median_Value, 
xlab = "Lower Status of the Population (%)", ylab = "Median Value of Homes in $1000s")
abline(fit_status)
```

The plot shows a strong association between lower status of the population and median value of the houses. We will draw a plot of fitted values and residuals to back up this assertion.

```{r}
residual_lower_status = lm(fit_status$residuals~ fit_status$fitted.values)
plot(fit_status$fitted.values, fit_status$residuals, 
xlab = "Lower Status Fitted Values", ylab = "Lower Status Residuals")
abline(residual_lower_status)
```

The plot of fitted values vs residuals shows a fairly decent grouping of observations around the zero line and the plot looks similar to the  lower status vs median value plot. Thus, lower status is a significant variable in our model.


\item Fit a multiple regression model to predict the response using all of the predictors. Describe your results. For which predictors can we reject the null hypothesis $H_0: \beta_j = 0$?

```{r}
#4
fit_multiple <- lm(formula = Median_Value ~ Crime_Rate + Zoned_Land + Tract_Bound + NOX + 
Avg_Rooms + Distance + Rad + Tax + PTRatio + Blacks + 
Lower_Status, data = BostonData)

summary(fit_multiple)


```

Multiple Regression Model:

M1 = 36.34 - 0.11 * Crime_Rate + 0.04 * Zoned_Land + 2.71 * Tract_Bound - 17.37 * NOX + 3.80 * Avg_Rooms - 1.49 * Distance + 0.30 * Rad - 0.01 * Tax - 0.94 * PTRatio + 0.009 * Blacks - 0.52 * Lower_Status

We can reject null hypothesis for all the predictors considering the t and p values from the summary above which show all of the predictors are statistically significant. Also the F-statistic = 128.2 >> 1 suggests there is at least one predictor that is related to the median value of houses and thus we can reject null hypothesis.

\item How do your results from (3) compare to your results from (4)? Create a plot displaying the univariate regression coefficients from (3) on the x-axis and the multiple regression coefficients from part (4) on the y-axis. Use this visualization to support your response.


```{r}
#5
Predictors = c("Crime_Rate", "Zoned_Land", "Tract_Bound" ,"NOX", "Avg_Rooms", "Distance", 
"Rad", "Tax", "PTRatio", "Blacks", "Lower_Status")
univariate_coeffs = c(fit_crim$coefficients[2], fit_zn$coefficients[2], 
fit_tract$coefficients[2], fit_nox$coefficients[2],
fit_rooms$coefficients[2], fit_distance$coefficients[2],
fit_rad$coefficients[2], fit_tax$coefficients[2],
fit_ptr$coefficients[2], fit_blacks$coefficients[2],
fit_status$coefficients[2])

multiple_coeffs = c(fit_multiple$coefficients[2],fit_multiple$coefficients[3],
fit_multiple$coefficients[4],fit_multiple$coefficients[5],
fit_multiple$coefficients[6],fit_multiple$coefficients[7],
fit_multiple$coefficients[8],fit_multiple$coefficients[9],
fit_multiple$coefficients[10], fit_multiple$coefficients[11],
fit_multiple$coefficients[12])

df = data.frame(Predictors, univariate_coeffs, multiple_coeffs)

ggplot(data = df, aes(x = univariate_coeffs, y = multiple_coeffs, color = Predictors)) +
geom_point() +
labs(x = "Univariate Coefficients", y = "Multiple Regression Coefficients")
```

The value of univariate regression coeffcients are more extreme compared to multiple regression coefficients. By more extreme I mean, if univariate coefficients are positive, the multiple regression coefficients are less in value while if the univariate regression coefficients are negative multiple regression coefficients take higher values. 

Also most values are between -1 and +1 with a cluster around (0,0).

An interesting observation is for predictor Distance - the 2 coefficients have opposite signs and we may need to inspect this variable closely while devising a regression model.

\item Is there evidence of a non-linear association between any of the predictors and the response? To answer this question, for each predictor $X$ fit a model of the form:

$$ Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \beta_3 X^3 + \epsilon $$
```{r}
#6

lm.out = lm(Median_Value ~ Crime_Rate + I(Crime_Rate^2) + I(Crime_Rate^3), data=BostonData)

summary(lm.out)

#Median_Value = 0.2519 - 1.136 * Crime_Rate + 0.02378 * Crime_Rate^2 - .0001489 * Crime_Rate^3
```


```{r}
lm.out = lm(Median_Value ~ Zoned_Land + I(Zoned_Land^2) + I(Zoned_Land^3), data=BostonData)

summary(lm.out)

# Median_Value = 20.45 + 0.64 * Zoned_Land - 0.0167 * Zoned_Land^2 + 0.000125 * Zoned_Land^3
```

```{r}
lm.out = lm(Median_Value ~ NOX + I(NOX^2) + I(NOX^3), data=BostonData)

summary(lm.out)
```

The summary shows the coefficients for the first and second power do not make a significant impact (from t and p values) and we can conclude there is absence of non-linear regression.

```{r}
lm.out = lm(Median_Value ~ Avg_Rooms + I(Avg_Rooms^2) + I(Avg_Rooms^3), data=BostonData)

summary(lm.out)

# Median_Value = 241.31 - 109.39 * Avg_Rooms + 16.49 * Avg_Rooms^2 - 0.74 * Avg_Rooms^3
```

```{r}
lm.out = lm(Median_Value ~ Distance + I(Distance^2) + I(Distance^3), data=BostonData)

summary(lm.out)

# Median_Value = 7.038 + 8.59 * Distance - 1.25 * Distance^2 + 0.056 * Distance^3
```

```{r}
lm.out = lm(Median_Value ~ Rad + I(Rad^2) + I(Rad^3), data=BostonData)

summary(lm.out)

# Median_Value = 30.25 - 3.8 * Rad + 0.61 * Rad^2 - 0.02 * Rad^3
```

```{r}
lm.out = lm(Median_Value ~ Tax + I(Tax^2) + I(Tax^3), data=BostonData)

summary(lm.out)
```

The summary suggests there isn't a strong non-linear relationship between Tax and Median_Value.

```{r}
lm.out = lm(Median_Value ~ PTRatio + I(PTRatio^2) + I(PTRatio^3), data=BostonData)

summary(lm.out)
```

The summary suggests there isn't a strong non-linear relationship between PTRatio and Median_Value.

```{r}
lm.out = lm(Median_Value ~ Blacks + I(Blacks^2) + I(Blacks^3), data=BostonData)

summary(lm.out)
# Median_Value = 0.126 - 0.01703 * Blacks + 0.0002036 * Blacks^2 - 2.22e-07 * Blacks^3
```

```{r}
lm.out = lm(Median_Value ~ Lower_Status + I(Lower_Status^2) + I(Lower_Status^3), data=BostonData)

summary(lm.out)
# Median_Value = 48.65 - 3.86 * Lower_Status + 0.15 * Lower_Status^2 - 0.002 * Lower_Status^3
```

\item Consider performing a stepwise model selection procedure to determine the best fit model. Discuss your results. How is this model different from the model in (4)?

```{r}
#7
#Backward Selection
stepwise_model <- step(lm(Median_Value ~ Crime_Rate + Zoned_Land + Tract_Bound + NOX + Avg_Rooms + 
Distance + Tax + Rad + PTRatio + Blacks + Lower_Status, 
data = BostonData), direction = "backward")

summary(stepwise_model)
```

After performing a backward selection of predictors, we observe that none of the predictors is removed from the model suggesting all the predictors are significant. The most significant predictors are Lower_Status, Avg_Rooms, Distance and PTRatio. 

This model is similar to (4).

\item Evaluate the statistical assumptions in your regression analysis from (7) by performing a basic analysis of model residuals and any unusual observations. Discuss any concerns you have about your model.

```{r}
plot(stepwise_model$fitted.values, stepwise_model$residuals, 
xlab = "Stepwise Model Fitted Values", ylab = "Stepwise Model Residuals")
```

The model residuals do not show a pattern and are mostly clustered around 0 which supports the assumption of linear regression. Also there is no overfitting of data which supports the assumptions of regression that the error terms musy be independent of each other.

One of the concerns is there are a lot of outliers which indicate this might not be the best model to represent this dataset.

As seen in (5), there are some predictor variables for which the coeefecients have oppsite signs for linear and multiple regression which might suggest there is some anomaly and needs more introspection.


\eenum
