#' ---
#' title: "THE SPREAD OF COVID-19 IN THE WORLD"
#' author: "Francesco Lorè"
#' output: github_document
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(car)
library(leaps)
library(faraway)
library(effects)
library(gridExtra)
library(tidyverse)
library(GGally)

#' 
#' The current pandemic emergency is widespread in all countries of the world. I created dataset from data of *Our World in Data* that provide researches to world’s largest problems. The dataset contains information about the spread of Covid-19 during the period from 01/03/2020 to 31/12/2020 over a large number of countries.
#' 
#' ## Some information about variables
#' 
#' The number of observations are 136 and the number of variables are 7. They are:
#' 
#' - **The average of stringency index (avg_si)**: The stringency index is a composite measure based on nine response indicators including school closures, workplace closures, and travel bans. It is the average of the daily data about the stringency index;
#' - **The average of reproduction rate (avg_rt)**: Also the reproduction rate is a composite measure that detect how many people are infected by one person infected. It is the average of the weakly data about the reproduction rate;
#' - **The infection rate (infection_rate)**: It is the ratio between the total confirmed cases of a country and the population of the same country;
#' - **The fatality rate (fatality_rate)**: It is the ratio between the total death cases of a country and the total confirmed cases of the same country;
#' - **Human development index (hdi)**: It is a statistic composite index of life expectancy, education, and per capita income indicators, which are used to rank countries by human development;
#' - **Life expectancy (life_expectancy)**;
#' - **The Europe variable (europe)**: The dummy variable to detect if a country is in Europe or not. The values can be *Yes* or *No*.
#' 
#' 
#' ## The goal of my project
#' 
#' The goal is to explore the correlation between the average of stringency index and the other measures by checking why a governments decide to increase or reduce the closure level of activity.
#' 
#' ## Load data
#' 
#' Loading the dataset from a *csv* file selecting the variables that I am going to use.
#' 
## ----echo=FALSE--------------------------------------------------------------
covid = read.csv2("assignment/owid-covid-data3.csv", header=TRUE)
rownames(covid) = covid$location
covid = covid %>% select(avg_stringency_index,avg_reproduction_rate,
                infection_rate,fatality_rate,hdi,life_expectancy,europe)
colnames(covid) = c("avg_si","avg_rt","infection_rate","fatality_rate", "hdi","life_exp","europe")
summary(covid)

#' 
#' ## Exploratory analysis
#' 
#' First of all, I create a scatterplot matrix by observing the correlation among variables.
#' 
## ----echo=FALSE, fig.align='center', fig.height=10, fig.width=10-------------
ggpairs(covid,mapping = aes(color=europe))

#'  
#'  By seeing the **avg_si** versus the other measures, It is clear there is not an high correlation.
#' 
#' Now, I want to investigate differences in average of stringency index between Europe and non-Europe countries, ignoring other variables at the moment.
#' 
## ----fig.align='center', fig.height=4, fig.width=6---------------------------
covid %>% ggplot() + geom_boxplot(mapping = aes(y = avg_si, x = europe), color = "blue", fill="blue", alpha=0.2) + labs(x = "Europe", y = "Avg Stringency Index")

#' 
#' There aren't sensible differences between Europe countries and non-Europe countries.
#' 
#' ## Fitting the model
#' 
#' After done the exploratory analysis, I fit the linear regression model using the *ordinary least squares* estimation. The response variable is the average value of stringency index, and the other variables are the predictors. 
#' 
## ----------------------------------------------------------------------------
ols = lm(avg_si ~ avg_rt + infection_rate + fatality_rate*europe + hdi + life_exp , data=covid)
(summ = summary(ols))

#' 
#' Since there are non-significant variables, I make a variable selection. 
#' 
#' ## Variable selection
#' 
#' The aim is to remove irrelevant predictors by getting the best subset model. 
#' 
## ----------------------------------------------------------------------------
ols_bss = regsubsets(avg_si ~ avg_rt + infection_rate + fatality_rate*europe + hdi + life_exp , data=covid)
summ_bss = summary(ols_bss)

#' 
#' The **regsubsets()** function included in the *leaps* library performs best subset selection by identifying the best model that contains a given number of predictors, where best is quantified using RSS. An asterisk indicates that a given variable is included in the corresponding model. 
#' 
#' For checking the best subset model, I compare the *BIC*, adjusted $R^2$ and Mallow’s *Cp* and Cross-Validation error among them.
#' 
## ----fig.height=3, fig.width=12----------------------------------------------
p <- 7
k <- 10
set.seed (1)
folds <- sample (1:k,nrow(covid),replace =TRUE)
cv.errors <- matrix (NA ,k, p, dimnames =list(NULL , paste (1:p) ))
for(j in 1:k){
    best.fit =regsubsets (avg_si ~ avg_rt + infection_rate + 
                            fatality_rate*europe + hdi + life_exp ,data=covid[folds!=j,])
    for(i in 1:p) {
        mat <- model.matrix(as.formula(best.fit$call[[2]]), covid[folds==j,])
        coefi <- coef(best.fit ,id = i)
        xvars <- names(coefi )
        pred <- mat[,xvars ]%*% coefi
        cv.errors[j,i] <- mean( (covid$avg_si[folds==j] - pred)^2)
    }
}
cv.mean <- colMeans(cv.errors)

# CV error
p1 = ggplot(mapping = aes(y = cv.mean,x = seq(1,p))) + geom_line()+
  geom_point() + geom_vline(xintercept = which.min(cv.mean), color = "red") + labs(title="CV error", y = "",x = "number of predictors")
# BIC
p2 = ggplot(mapping = aes(y = summ_bss$bic,x = seq(1,p))) + geom_line()+
  geom_point() + geom_vline(xintercept = which.min(summ_bss$bic), color = "red")+ labs(title="Drop in BIC", y = "",x = "number of predictors")
# Cp
p3 = ggplot(mapping = aes(y = summ_bss$cp,x = seq(1,p))) + geom_line()+
  geom_point()+ geom_vline(xintercept = which.min(summ_bss$cp), color = "red")+ labs(title="Mallow'Cp", y = "",x = "number of predictors")
#R2
p4 = ggplot(mapping = aes(y = summ_bss$adjr2,x = seq(1,p))) + geom_line()+
  geom_point()+ geom_vline(xintercept = which.max(summ_bss$adjr2), color = "red")+ labs(title="Adjusted R^2", y = "",x = "number of predictors")

grid.arrange(p1, p2,p3,p4, ncol = 4)

#' 
#' As shown by the graphs the best subset model is a model with 4 predictors:
#' The CV error, Mallow'Cp and Adjusted $R^2$ get the same result, only the BIC gets a different number of predictors.
#' 
## ----------------------------------------------------------------------------
ols1 = lm(avg_si ~ avg_rt + infection_rate + fatality_rate*europe, data=covid)
(summ1 = summary(ols1))

#' 
#' 
#' ## Collinearity
#' 
#' To detect collinearity issues, I use the *variance inflation factor (VIF)*.
#' 
## ----------------------------------------------------------------------------
vif(ols1)

#' 
#' 
#' I think there is not collinearity issues among predictors. also because the variance inflation factor is lower than 10 for each parameters (considering there is an interaction as well).
#' 
#' ## Diagnostics
#' 
#' The diagnostics analysis is important to improve a regression model by observing some assumptions:
#' 
#' ### Constant variance assumption
#' 
#' I plot residuals versus the fitted values.
#' 
## ----fig.height=4, fig.width=6, fig.align='center'---------------------------
ggplot(mapping = aes(y = residuals(ols1),x = fitted(ols1), color = covid$europe)) + geom_point()+ geom_hline(yintercept = 0, color = "red")+ labs(title="Residuals vs. Fitted values", y = "Residuals",x = "Fitted values")

#' 
#' The graph show a no strong right opening megaphone, therefore a non-constant variance. 
#' 
#' ### Structure of the relationship between the predictors and the response
#' 
## ----fig.height=8, fig.width=8, fig.align='center'---------------------------
residualPlots(ols1)

#' 
#' Since the p-value associated to **infection_rate** is very low we have to transform it.
#' 
#' ### Normality Assumption
#' 
#' I use the Q-Q plot to investigate the normality assumption.
#' 
## ----fig.height=4, fig.width=6, fig.align='center'---------------------------
ggplot(mapping = aes(sample = residuals(ols1))) + stat_qq() + stat_qq_line() + labs(title = "QQ-plot", y = "Residuals", x = "Theoretical Quantiles")

#' 
#' I can check this computing also Shapiro-Wilk normality test:
#' 
## ----------------------------------------------------------------------------
shapiro.test(residuals(ols1))

#' 
#' The null hypothesis is residuals are normal. Since the p-value is small, I reject $H_0$.
#' 
#' ### Leverage points
#' 
#' I compute the high leverage points:
#' 
## ----------------------------------------------------------------------------
hat = hatvalues(ols1)
hat[which(hat>=(2*sum(hat)/nrow(covid)))]

#' 
#' An high leverage point has a value greater than $2(p+1)/n$. There are 14 high-leverage points. In the half-normal I am looking for points that diverge substantially from the rest of the data.
#' 
## ----fig.height=4, fig.width=6, fig.align='center'---------------------------
halfnorm(hat, 14, labs = rownames(covid), ylab="Leverages")

#' 
#' ### Outliers
#' 
#' I compute standardized residuals and plot them to find possible outliers.
#' 
## ----fig.height=4, fig.width=6, fig.align='center'---------------------------
rsta = rstandard(ols1)
ggplot(mapping = aes(x = fitted(ols1), y = rsta, color=covid$europe)) + geom_point() + labs(title = "Standardize residuals vs. Fitted values", y = "Standardize Residuals", x = "Fitted values") + geom_hline(yintercept = c(-3,0, 3), color = c("green","red","green")) 

#' It is possible to see there are two outliers points (r_i > |3|).
#' 
#' ### Influential Points
#' 
#' I plot standardized residuals versus the leverages to find the influential points:
#' 
## ----fig.height=4, fig.width=6, fig.align='center'---------------------------
ggplot(mapping = aes(x = hat, y = rsta)) + geom_point() + labs(title = "Standardize residuals vs. Leverage", y = "Standardize Residuals", x = "Leverage") + geom_text(mapping = aes(hat[hat>0.088], rsta[hat>0.088], label = rownames(covid)[hat>0.088]))

#' 
#' 
#' I calculate the Cook's distance by fit R function:
#' 
## ----------------------------------------------------------------------------
cook <- cooks.distance(ols1)
tail(sort(cook))

#' 
#' ## Improvment of my model
#' 
#' To improve my model first of all I transform the fatality rate and the infection rate using the logarithmic function. Then I remove the biggest influential point computed by Cook's distance. I make the last operation two times.
#' 
## ----fig.height=8, fig.width=8, fig.align='center'---------------------------
ols2 = lm(avg_si ~ avg_rt + log(infection_rate) + log(fatality_rate)*europe,
          data=covid, subset = (cook < max(cook)))
cook2 = names(tail(sort(cooks.distance(ols2)),1))
ols2 = lm(avg_si ~ avg_rt + log(infection_rate) + log(fatality_rate)*europe,
          data=covid[which(row.names(covid) != cook2),])

cook3 = names(tail(sort(cooks.distance(ols2)),1))
ols2 = lm(avg_si ~ avg_rt + log(infection_rate) + log(fatality_rate)*europe,
          data=covid[which(row.names(covid) != cook2 & row.names(covid) != cook3 ),])

par(mfrow=c(2,2))
plot(ols2)

#' 
#' ## Analysis of estimate parameters
#' 
## ----------------------------------------------------------------------------
(summ2 = summary(ols2))$coefficients

#' 
#' To show graphically the relationship between the response and predictors, I plot the following graphs.
#' 
## ----echo=FALSE, fig.align='center', fig.height=7, fig.width=12--------------
e1.ols = predictorEffect("infection_rate", ols2)
e2.ols = predictorEffect("fatality_rate", ols2)
e3.ols = predictorEffect("avg_rt", ols2)
e4.ols = predictorEffect("europe", ols2)
grid.arrange(plot(e1.ols,
                  xlab="Infection rate",
                  ylab="Avg stringency index",main=""), 
             plot(e2.ols,
                  xlab="Fatality rate",
                  ylab="Avg stringency index",main=""), 
             plot(e3.ols,
                  xlab="Avg Reproduction rate",
                  ylab="Avg stringency index",main=""), 
             plot(e4.ols,
                  xlab="Europe",
                  ylab="Avg stringency index",main=""),nrow=2, ncol=2)

#' 
#' The response increase faster in countries with a smaller infection rate , there is relatively little change in countries with an infection rate more bigger.
#' An increment of one unit of $log(x_2)$ determine an increment of $\hat{y}$ equal to $\hat\beta_2$ points
#' 
#' About the fatality rate, there are two graphs because of the interaction with the **europe** dummy variable:
#' 
#' - *Europe = "No" ($x_4 = 0$)*: An increment of one unit of $log(x_3)$ determine an increment of $\hat{y}$ equal to $\hat\beta_3$ points;
#' - *Europe = "Yes" ($x_4 = 1$)*: In this case there is a less fast increment. $\hat\beta_0$ became 16.393 ($\hat\beta_0 + \hat\beta_4$) and an increment of $log(x_3)$ determine an increment of $\hat{y}$ equal to ($\hat\beta_3 + \hat\beta_5$).
#' 
#' The response increase in constant way by an increment of the reproduction rate.
#' An increment of one unit of $x_1$ determine a increment of $\hat{y}$ equal to $\hat\beta_1$ points.
#' 
#' The $\hat\beta_0$ is a value of $\hat{y}$ when all predictor are equal to 0.
#' 
#' Now I compute confidence intervals to check the uncertain of my coefficients.
#' 
## ----------------------------------------------------------------------------
x = cbind(summ2$coefficients[,1],confint(ols2)); x

#' 
#' As shown by the table, the uncertain of my parameters is relatively high in the whole.
#' 
#' ## $\hat\sigma$ and $R^2$
#' 
#' The $\hat\sigma$ explains the uncertain of the fitted values, The value is high:
#' 
## ----------------------------------------------------------------------------
summ2$sigma

#' 
#' 
#' 
#' The $R^2$ has a medium value. The same happens for adjusted $R^2$.
#' 
## ----------------------------------------------------------------------------
cat("R^2    :",summ2$adj.r.squared,"\nadj R^2:",summ2$r.squared)

#' 
#' The coefficient of determination  represents the proportional reduction of total variation associated with the use of the predictor variables X. Therefore, about 43% of the observed average of stringency index variability is explained by predictors.
#' 
#' ## Inferences about regression coefficients
#' 
#' ### testing all $\beta_j$ coefficients
#' 
#' We consider a test concerning $\beta_1$, the coefficient for the regressor **avg_rt**.
#' 
#' I am testing the effect of adding **avg_rt** to a mean function that already includes all other regressors.
#' 
## ----------------------------------------------------------------------------
# Z-score
z = summ2$coefficients[2,3]
# p-value
p_value = summ2$coefficients[2,4]

#' 
## ----echo=FALSE--------------------------------------------------------------
# z-score
p = t(c(z,p_value)); colnames(p) = c("z-score", "p-value"); p

#' 
#' The results provide some evidence that the effect of **avg_rt** on closure level, after adjusting for the other predictors, is different from 0.
#' I can make the same test to all coefficients, but the result is already given by **summary()** function:
## ----echo=FALSE--------------------------------------------------------------
summ2$coefficients[,3:4]

#' there are evidence against the null hypothesis for all predictors, hence they cannot be equal to 0.
#' 
#' ### testing a group of regressor
#' 
#'  I test the hypothesis that both **fatality_rate** and the interaction term may be excluded from the model because these have a p-value higher than others predictors.
#' 
#' I can compute this using the **anova()** function.
#' 
## ----------------------------------------------------------------------------
ols0 = lm(avg_si ~ avg_rt + log(infection_rate) + europe,
          data=covid[which(row.names(covid) != cook2 & row.names(covid) != cook3 ),])
anova(ols0,ols2)

#' 
#' There is evidence against the null hypothesis and the pair of predictor are significant within my model.
#' 
#' ## Prediction
#' 
#' Suppose I have the following information about a “new” country (suppose that it exist): *avg_rt=1.07, infection_rate=1.77, fatality_rate=1.99, europe="Yes"*. 
#' We obtain a prediction and 95% prediction interval for a such country as:
## ----------------------------------------------------------------------------
newdata = data.frame(avg_rt=1.07, infection_rate=1.77,fatality_rate=1.99, europe="Yes" )
(p = predict(ols2, newdata = newdata, interval="prediction",level=.95))

