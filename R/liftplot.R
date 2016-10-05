# =============================================================================#
# Create a lift plot:
# =============================================================================#

# -----------------------------------------------------------------------------#
# Auxiliary functions:
# -----------------------------------------------------------------------------#
## check if every entry is TRUE:
all.true <- function( bool )
{
  if( sum( !bool ) == 0 ) {
    return( TRUE )
  } else {
    return( FALSE )
  }
}

## Create data.frame for computation:
getDF <- function( pred, ist, weights )
{
  DF <- NULL
  cs <- NA
  
  if ( all.true( class(pred) %in% c('integer', 'numeric') ) || !is.na(ist) ) {
    if ( length(pred) != length(ist) ) { 
      stop( "Length of 'pred' and 'ist' must match" )
    }
    DF <- data.frame( Predict   = pred,
                      IstValues = ist )
    cs <- ''
  }
  if ( all.true( class(pred) %in% c('lm', 'glm') ) ) {
    DF <- data.frame( Predict   = predict(pred),
                      IstValues = pred$model[1] )
    cs <- class(pred)[1]
  }
  if ( all.true( c('rfsrc', 'regr') %in% class(pred) ) ) {
    DF <- data.frame( Predict   = predict(pred)$predicted,
                      IstValues = pred$yvar )
    cs <- 'rfsrc (Regression)'
  }
  if ( all.true( c('rfsrc', 'surv') %in% class(pred) ) ) {
    DF <- data.frame( Predict   = predict(pred)$predicted,
                      IstValues = pred$yvar[1] )
    cs <- 'rfsrc (Survival)'
  }
  if ( 'randomForest' %in% class(pred) && pred$type == 'regression' ) {
    DF <- data.frame( Predict   = predict(pred),
                      IstValues = pred$y )
    cs <- class(pred)[2]
  }
  if ( all.true( 'nls' %in% class(pred) ) ) {
    DF <- data.frame( Predict   = predict(pred),
                      IstValues = predict(pred) + resid(pred) )
    cs <- class(pred)
  }
  
  if ( is.na(weights)[1] ) {
    weights <- rep( 1, length(DF$Predict) )
  }
  if ( length(weights) != length(DF$Predict) ) {
    stop( "Length of 'weights' must match 'pred' and 'ist'" )
  }
  DF <- cbind( DF, weights = weights )
  
  if ( ! is.null(DF) ) { 
    names(DF)[1:2]      <- c('Predict', 'IstValues')
    attr(DF, 'cs') <- cs
  }
  
  return( DF )
}

# -----------------------------------------------------------------------------#
# Main plot function:
# -----------------------------------------------------------------------------#
lift <- function( pred, ist = NA, weights = NA, w.pred = FALSE, w.ist = FALSE,
                  ngroups = 10L, legend.pos = 'bottomleft', ... )
{
  if ( ngroups %% 2 == 0 ) { ngroups <- as.integer(ngroups) }
  if ( ngroups < 1 ) {
    stop( 'No values < 1 allowed for ngroups' )
  }
  if ( !is.integer(ngroups) ) {
    if ( is.double(ngroups) ) {
      ngroups <- floor(ngroups)
      warning( paste0('Floor ngroups to ', ngroups, ' (double) input') )
    }
  }
  
  if (! legend.pos %in% c('bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right', 'center') &&
      ! is.na(legend.pos) ) {
    warning( 'Remove legend, no match for any position' )
    legend.pos <- NA
  }
  
  DF <- getDF( pred = pred, ist = ist, weights = weights )
  at <- attr(DF, 'cs')
  if ( is.null(DF) ) { stop('Check your input values') }
  
  DF        <- DF[order(DF$Predict, decreasing = TRUE), ]
  nGroup    <- sort( rep(1:ngroups, floor(nrow(DF) / ngroups) ) )
  
  if ( length(nGroup) < nrow(DF) ) {
    nGroup <- c( nGroup, rep(ngroups, nrow(DF) - length(nGroup)) )
  }
  
  DF <- cbind( DF, nGroup = nGroup )
  attr(DF, 'cs') <- at
  
  DF_mean <- data.frame( PredictionMean = numeric(ngroups), 
                         IstVAluesMean  = numeric(ngroups) )
  
  for ( i in 1:ngroups ) {
    if ( w.pred ) {
      DF_mean[i, ] <- c( weighted.mean(x = DF[DF$nGroup == i, 'Predict'],
                                       w = DF[DF$nGroup == i, 'weights']),
                         mean(x = DF[DF$nGroup == i, 'IstValues']) )
    }
    if ( w.ist ) {
      
      DF_mean[i, ] <- c( mean(x = DF[DF$nGroup == i, 'Predict']),
                         weighted.mean(x = DF[DF$nGroup == i, 'IstValues'],
                                       w = DF[DF$nGroup == i, 'weights']) )
    }
    if ( ! w.ist && ! w.pred ) {
      DF_mean[i, ] <- c( weighted.mean(x = DF[DF$nGroup == i, 'Predict'],
                                       w = DF[DF$nGroup == i, 'weights']),
                         weighted.mean(x = DF[DF$nGroup == i, 'IstValues'],
                                       w = DF[DF$nGroup == i, 'weights']) )
    }
  }
  
  Args <- list( ... )
  
  if ( length(Args) == 0 ) {
    if ( nchar( attr(DF, 'cs') ) > 0 ) {
      main <- paste0( 'Liftplot: ', attr(DF, 'cs') )
    } else {
      main <- 'Liftplot'
    }
    plot( x    = DF_mean$PredictionMean,
          ylim = c( min(c(DF_mean$PredictionMean, 
                          DF_mean$IstVAluesMean)),
                    max(c(DF_mean$PredictionMean, 
                          DF_mean$IstVAluesMean)) ),
          xlab = 'Groups',
          ylab = 'Mean',
          main = main,
          axes = FALSE )
  } else {
    plot( x = DF_mean$PredictionMean, ... )
  }
  
  xaxp <- seq(par()$xaxp[1], par()$xaxp[2], length.out = par()$xaxp[3] + 1)
  yaxp <- seq(par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1)
  
  abline( h   = yaxp, 
          v   = xaxp,
          col = rgb(100, 100, 100, 50, maxColorValue = 255),
          lty = 2 )
  
  polygon( x = c( (1:ngroups), (ngroups:1) ),
           y = c( DF_mean$PredictionMean, DF_mean$IstVAluesMean[ngroups:1] ),
           col = rgb(255, 128, 0, 30, maxColorValue = 255),
           border = NA )
  
  lines(  x = DF_mean$PredictionMean, lwd = 2, col = rgb(135, 206, 235, 255, maxColorValue = 255) )
  points( x = DF_mean$PredictionMean, cex = 2, col = rgb(135, 206, 235, 255, maxColorValue = 255), pch = 19 )
  lines(  x = DF_mean$IstVAluesMean,  lwd = 2, col = rgb(240, 128, 128, 255, maxColorValue = 255) )
  points( x = DF_mean$IstVAluesMean,  cex = 2, col = rgb(240, 128, 128, 255, maxColorValue = 255), pch = 19 )
  
  axis( side = 1, at = 1:ngroups, labels = 1:ngroups )
  axis( side = 2, at = yaxp, labels = yaxp )
  
  if ( !is.na(legend.pos) ) {
    legend( legend.pos,
            legend = c('Prediction', 'Real values'),
            lty = c(1,1),
            lwd = c(2,2),
            col = c( rgb(135, 206, 235, 255, maxColorValue = 255),
                     rgb(240, 128, 128, 255, maxColorValue = 255)),
            bg = rgb(100, 100, 100, 10, maxColorValue = 255),
            box.col = NA )
  }
  
  box()
  
  invisible( DF_mean )
}
