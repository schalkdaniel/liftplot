\name{lift}
\alias{lift}
\title{Create a liftplot}
\usage{
	lift( pred, ist = NA, ngroups = 10L, legend.pos = "bottomleft", 
          ylim.default = TRUE, bullets = "normal", bullets.cex = 2, 
          ... )
}
\arguments{
	\item{pred}{A vector of predictions or a object of class 'lm', 'glm', 'rfsrc' or 'randomForest'}

	\item{ist}{An additional vector of the values which were predicted}

	\item{ngroups}{An integer number of groups}

	\item{legend.pos}{Position of the legend ('bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right', 'center', NA)}
	
	\item{ylim.default}{If the default ylim values should be used with additional parameters to plot, than set to 'ylim.default = TRUE'}

	\item{bullets}{If set to 'normal' (default) than just a normal bullet point is drawn for the mean in the groups. If bullets is set to 'boxplot', than over every bullet of the prediction values a boxplot will be drawn.}
	
	\item{bullets.cex}{Size of the 'normal' bullet points.}
	
	\item{col1}{Specify the color of the prediction curve}
	
	\item{col2}{Specify the color of the ist curve}
	
	\item{...}{Additional prameter which can be passed to plot}
}
\description{
	The function creates a liftplot. You can use it for validate the goodnes of the prediction of a model.
}
\examples{
## Simulate data with non-normal distribution
set.seed( pi )
x = sort( runif(100, 0, 2) )
y = 3 * exp(2 * x) + rchisq(100, 10)

DF = data.frame(x = x, y = y)

## For class lm:
mod1 = lm(formula = y ~ x,
          data    = DF)

## For class nls:
mod2 = nls(formula = y ~ a * exp(b * x),
           data    = DF,
           start   = list(a = 1, b = 1))

## For class randomForest:
require(randomForest)
mod3 = randomForest(formula = x ~ y,
                    data    = DF)
                     

par(mfrow = c(1,3))
lift(mod1, 
     col1    = c(red = 113, blue = 198, green = 113),
     col2    = c(red = 125, blue = 158, green = 192),
     bullets = "boxplot")
lift(mod2, col1 = c(red = 113, blue = 198, green = 113))
lift(mod3, col2 = c(red = 125, blue = 158, green = 192))
par(mfrow = c(1,1))
}
