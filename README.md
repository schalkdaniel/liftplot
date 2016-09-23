liftplot
========
This package/function gives just one function called 'lift'. With this function one can descriptive validate the goodnes of the prediction of a model.

Therefore some standard classes like 'lm', 'glm', 'randomForest' or 'nls' are implemented. If a specific class isn't supported one have the opportunity to give manually a prediction vector and a vector with the real values.

Example
-------
At first simulate some data:
```{r}
set.seed( pi )
x <- sort( runif(100, 0, 2) )
y <- 3 * exp(2 * x) + rchisq(100, 10)

DF <- data.frame( x = x, y = y )
```

Now we can compute some models and apply lift on every one:
```{r}
## For class lm:
mod1 <- lm( formula = y ~ x,
            data    = DF )

## For class nls:
mod2 <- nls( formula = y ~ a * exp(b * x),
             data    = DF,
             start   = list(a = 1, b = 1) )

## For class randomForest:
require( randomForest )
mod3 <- randomForest( formula = x ~ y,
                      data    = DF )
                     

par( mfrow = c(1,3) )
lift( mod1 )
lift( mod2 )
lift( mod3 )
par( mfrow = c(1,1) )
```
The plot looks like the following:

![liftplot](/images/example.png)
