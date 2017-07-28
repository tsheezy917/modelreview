install.packages("UsingR")
install.packages("dplyr")
library(UsingR)
library(dplyr)
data("diamond")


#fit with intercept interpreted as the price of a 0 carat diamond.
#the slope is the expected change in price of the for every 1 carat
#` in singapore dollars.  
fit <- lm(price ~ carat, data = diamond)
fit

#' this model is interpreted where the intercept is the price of
#' the average diamond size, not 0 carat (which makes more sense)
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
fit2

#' here we have the interpretation of beta1 (slope) as the inc
#' in price for every 10th of an increase in carat size.
#' Note that you multiply by the carat size (predictor) ends up 
#' dividing the coeficient.   
fit3 <- lm(price ~ I(carat * 10), data = diamond)
fit3

new_data <- c(.16, .27, .34)

#predict prices at the new value
predict(fit3, newdata = data.frame(carat = new_data))

y <- diamond$price ; x <- diamond$carat

e <- resid(fit)

#'if you don't give predict new data, it predicts at the 
#'observed x values from the model
yhat <- predict(fit)

#' sum of residuals equals 0
sum(e)

#' sum of residuals times the price variable is also 0
sum(e * x)

#' plot the residuals with lines to show the distances
plot(diamond$carat, e, 
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1:length(diamond))) 
  lines(c(x[i], x[i]), c(y[i], yhat[i], col = "red", lwd = 2))

#'non-linear data for residual plot example
#'x is random uniform, y is x plus a sin wave pattern and 
#'some random noise
x <- runif(100, -3 , 3); y <- x + sin(x) + rnorm(100, sd = .2)
g <- ggplot(data.frame(x = x, y = y), aes(x, y))  %>% + geom_smooth(method = "lm", color = "black") %>% 
  + geom_point(size = 7, color = "black", alpha = .4) %>% + geom_point(size = 5, color = "red", alpha = .4)
  
#' now we can plot the residual
g_resid <- g <- ggplot(data.frame(x = x, y = resid(lm(y ~ x))), aes(x, y))  %>% + geom_hline(yintercept = 0, size = 2) %>% 
  + geom_point(size = 7, color = "black", alpha = .4) %>% + geom_point(size = 5, color = "red", alpha = .4) %>% + xlab("X") + ylab("Residual")

#'add residuals to the diamond data set 
 
diamond$e <- resid(fit)
head(diamond)

#plot residiuals of the diamond data 

g_diam <- ggplot(diamond, aes(carat, e))  %>% + geom_hline(yintercept = 0, size = 2) %>% 
  + geom_point(size = 7, color = "black", alpha = .4) %>% + geom_point(size = 5, color = "red", alpha = .4)

#'create residual vectors: 1. just an intercept (variation around the ave price). 2. add in carat as a predictor
#'(variation around the regression line).  
e_vect <- c(resid(lm(price ~ 1, data = diamond)), 
            resid(lm(price ~ carat, data = diamond)))

#' create factor variable to separate intercept resid from intercept/slope resid.  
fit_resid <- factor(c(rep("Beta0", nrow(diamond)), 
                      rep("Beta0, Beta1", nrow(diamond))))
g_vect <- ggplot(data.frame(e_vect = e_vect, fit_resid = fit_resid), aes(y = e_vect, x = fit_resid, fill = fit_resid)) %>%
  + geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 25)
