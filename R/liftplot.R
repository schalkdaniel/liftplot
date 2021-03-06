# =============================================================================#
# Create a lift plot:
# =============================================================================#

# -----------------------------------------------------------------------------#
# Anciliary functions:
# -----------------------------------------------------------------------------#

## Get the data for a boxplot:
getBoxData = function(x)
{
  IQR        = quantile(x, 0.75) - quantile(x, 0.25)
  maxwhisker = abs( 1.5 * IQR )
  
  if ( min(x) < quantile(x, 0.25) - maxwhisker ) {
    lowerwhisker = sort(x)[ min( which(sort(x) >= quantile(x, 0.25) - maxwhisker) ) ]
  } else {
    lowerwhisker = min(x)
  }
  if ( max(x) > quantile(x, 0.25) - maxwhisker ) {
    upperwhisker = sort(x)[ max( which(sort(x) <= quantile(x, 0.75) + maxwhisker) ) ]
  } else {
    upperwhisker = max(x)
  }
  
  values     = c( lowerwhisker  = lowerwhisker,
                   lowerquantile = quantile(x, 0.25),
                   median        = median(x),
                   upperquantile = quantile(x, 0.75),
                   upperwhisker  = upperwhisker )
  
  return( values )
}

## check if every entry is TRUE:
all.true = function( bool )
{
  if( sum( !bool ) == 0 ) {
    return( TRUE )
  } else {
    return( FALSE )
  }
}

## Create data.frame for computation:
getDF = function( pred, ist )
{
  DF = NULL
  cs = NA
  
  if ( all.true( class(pred) %in% c('integer', 'numeric') ) || !is.na(ist) ) {
    if ( length(pred) != length(ist) ) { 
      stop( "Length of 'pred' and 'ist' must match" )
    }
    DF = data.frame( Predict   = pred,
                      IstValues = ist )
    cs = ''
  }
  if ( all.true( class(pred) %in% c('lm', 'glm') ) ) {
    DF = data.frame( Predict   = predict(pred),
                      IstValues = pred$model[1] )
    cs = class(pred)[1]
  }
  if ( all.true( c('rfsrc', 'regr') %in% class(pred) ) ) {
    DF = data.frame( Predict   = predict(pred)$predicted,
                      IstValues = pred$yvar )
    cs = 'rfsrc (Regression)'
  }
  if ( all.true( c('rfsrc', 'surv') %in% class(pred) ) ) {
    DF = data.frame( Predict   = predict(pred)$predicted,
                      IstValues = pred$yvar[1] )
    cs = 'rfsrc (Survival)'
  }
  if ( 'randomForest' %in% class(pred) && pred$type == 'regression' ) {
    DF = data.frame( Predict   = predict(pred),
                      IstValues = pred$y )
    cs = class(pred)[2]
  }
  if ( all.true( 'nls' %in% class(pred) ) ) {
    DF = data.frame( Predict   = predict(pred),
                      IstValues = predict(pred) + resid(pred) )
    cs = class(pred)
  }
  
  if ( ! is.null(DF) ) { 
    names(DF)[1:2]      = c('Predict', 'IstValues')
    attr(DF, 'cs') = cs
  }
  
  return( DF )
}

# -----------------------------------------------------------------------------#
# Main plot function:
# -----------------------------------------------------------------------------#
lift = function( pred, ist = NA, ngroups = 10L, legend.pos = 'bottomleft', 
                  ylim.default = TRUE, bullets = 'normal', bullets.cex = 2, 
                  col1 = NA, col2 = NA, ... )
{
  if ( ngroups %% 2 == 0 ) { ngroups = as.integer(ngroups) }
  if ( ngroups < 1 ) {
    stop( 'No values < 1 allowed for ngroups' )
  }
  if ( ! is.integer(ngroups) ) {
    if ( is.double(ngroups) ) {
      ngroups = floor(ngroups)
      warning( paste0('Floor ngroups to ', ngroups, ' (double) input') )
    }
  }
  
  if (! is.na(col1)[1]) {
    if (length(names(col1)) == 0) {
      warning("col1 needs to be specifyfied with red, green, blue")
      col1   = rgb(135, 206, 235, 255, maxColorValue = 255)
      col1_a = rgb(135, 206, 235, 100, maxColorValue = 255)
    } else {
      if (any(! names(col1) %in% c("red", "green", "blue"))) {
        warning("col1 needs to be specifyfied with red, green, blue")
        col1   = rgb(135, 206, 235, 255, maxColorValue = 255)
        col1_a = rgb(135, 206, 235, 100, maxColorValue = 255)
      } else {
        c1   = col1
        col1   = rgb(c1["red"], c1["green"], c1["blue"], 255, maxColorValue = 255)
        col1_a = rgb(c1["red"], c1["green"], c1["blue"], 100, maxColorValue = 255)
      }
    }
  } else {
    col1   = rgb(135, 206, 235, 255, maxColorValue = 255)
    col1_a = rgb(135, 206, 235, 100, maxColorValue = 255)
  }
  
  if (! is.na(col2)[1]) {
    if (length(names(col2)) == 0) {
      warning("col2 needs to be specifyfied with red, green, blue")
      col2   = rgb(240, 128, 128, 255, maxColorValue = 255)
      col2_a = rgb(240, 128, 128, 100, maxColorValue = 255)
      col2_f = rgb(240, 128, 128, 65, maxColorValue = 255)
    } else {
      if (any(! names(col2) %in% c("red", "green", "blue"))) {
        warning("col2 needs to be specifyfied with red, green, blue")
        col2   = rgb(240, 128, 128, 255, maxColorValue = 255)
        col2_a = rgb(240, 128, 128, 100, maxColorValue = 255)
        col2_f = rgb(240, 128, 128, 65, maxColorValue = 255)
      } else {
        c2     = col2
        col2   = rgb(c2["red"], c2["green"], c2["blue"], 255, maxColorValue = 255)
        col2_a = rgb(c2["red"], c2["green"], c2["blue"], 100, maxColorValue = 255)
        col2_f = rgb(c2["red"], c2["green"], c2["blue"], 65, maxColorValue = 255)
      }
    }
  } else {
    col2   = rgb(240, 128, 128, 255, maxColorValue = 255)
    col2_a = rgb(240, 128, 128, 100, maxColorValue = 255)
    col2_f = rgb(240, 128, 128, 65, maxColorValue = 255)
  }
  
  if ( ! bullets %in% c('normal', 'boxplot') ) { 
    warning( "No option for the bullets given, set bullets = 'normal'" ) 
    bullets = 'normal'
  }
  
  if ( ! legend.pos %in% c('bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right', 'center') &&
       ! is.na(legend.pos) ) {
    warning( 'Remove legend, no match for any position' )
    legend.pos = NA
  }
  
  DF = getDF( pred = pred, ist = ist )
  at = attr(DF, 'cs')
  if ( is.null(DF) ) { stop( 'Check your input values' ) }
  
  DF        = DF[order(DF$Predict, decreasing = TRUE), ]
  nGroup    = sort( rep(1:ngroups, floor(nrow(DF) / ngroups) ) )
  
  if ( length(nGroup) < nrow(DF) ) {
    nGroup = c( nGroup, rep(ngroups, nrow(DF) - length(nGroup)) )
  }
  
  DF = cbind( DF, nGroup = nGroup )
  attr(DF, 'cs') = at
  
  DF_mean = data.frame( PredictionMean = numeric(ngroups), 
                         IstValuesMean  = numeric(ngroups) )
  
  if ( bullets == 'boxplot' ) { 
    bp_data = matrix( data = numeric(5 * ngroups), 
                       ncol = 5 ) 
  }
  
  for ( i in 1:ngroups ) {
    
    DF_mean[i, ] = c( mean( x = DF[DF$nGroup == i, 'Predict']   ),
                       mean( x = DF[DF$nGroup == i, 'IstValues'] ) )
    
    if ( bullets == 'boxplot' ) { 
      bp_data[i, ] = getBoxData( x = DF[DF$nGroup == i, 'Predict'] )
    }
  }
  
  Args = list( ... )
  
  if ( ! is.null(Args$font) ) {
    font = Args$font
    Args = Args[-which(names(Args) == 'font')]
  } else {
    font = 1
  }
  
  if ( ! is.null(Args$font.main) ) {
    font.main = Args$font.main
    Args      = Args[-which(names(Args) == 'font.main')]
  } else {
    font.main = font + 1
  }
  
  if ( length(Args) == 0 ) {
    if ( nchar( attr(DF, 'cs') ) > 0 ) {
      main = paste0( 'Liftplot: ', attr(DF, 'cs') )
    } else {
      main = 'Liftplot'
    }
    if ( bullets == 'normal' ) {
      plot( x    = DF_mean$PredictionMean,
            cex  = 0,
            ylim = c( min( c(DF_mean$PredictionMean, 
                             DF_mean$IstValuesMean) ),
                      max( c(DF_mean$PredictionMean, 
                             DF_mean$IstValuesMean) ) ),
            xlab = 'Groups',
            ylab = 'Mean',
            main = main,
            axes = FALSE,
            font = font,
            font.main = font.main,
            font.lab  = font )
    } 
    if ( bullets == 'boxplot' ) {
      plot( x    = DF_mean$IstValuesMean,
            cex  = 0,
            ylim = c( min( c(DF_mean$PredictionMean, 
                             DF_mean$IstValuesMean,
                             bp_data[, 1]) ),
                      max( c(DF_mean$PredictionMean, 
                             DF_mean$IstValuesMean,
                             bp_data[, 5]) ) ),
            xlab = 'Groups',
            ylab = 'Mean',
            main = main,
            axes = FALSE,
            font = font,
            font.main = font.main,
            font.lab  = font )
    }
    
  } else {
    if ( ylim.default ) {
      if ( bullets == 'normal' ) {
        plot( x    = DF_mean$PredictionMean,
              cex  = 0,
              ylim = c( min( c(DF_mean$PredictionMean, 
                               DF_mean$IstValuesMean) ),
                        max( c(DF_mean$PredictionMean, 
                               DF_mean$IstValuesMean) ) ), 
              axes      = FALSE,
              font      = font,
              font.main = font.main,
              font.lab  = font,
              ... )
      } 
      if ( bullets == 'boxplot' ) {
        plot( x    = DF_mean$IstValuesMean,
              cex  = 0,
              ylim = c( min( c(DF_mean$PredictionMean, 
                               DF_mean$IstValuesMean,
                               bp_data[, 1]) ),
                        max( c(DF_mean$PredictionMean, 
                               DF_mean$IstValuesMean,
                               bp_data[, 5]) ) ), 
              axes      = FALSE,
              font      = font,
              font.main = font.main,
              font.lab  = font,
              ... )
      }
      
    } else {
      plot( x         = DF_mean$PredictionMean,
            cex       = 0, 
            axes      = FALSE, 
            font      = font,
            font.main = font.main,
            font.lab  = font,
            ... )
    }
    
  }
  
  xaxp = seq(par()$xaxp[1], par()$xaxp[2], length.out = par()$xaxp[3] + 1)
  yaxp = seq(par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1)
  
  abline( h   = yaxp, 
          v   = xaxp,
          col = rgb(100, 100, 100, 50, maxColorValue = 255),
          lty = 2 )
  
  polygon( x = c( (1:ngroups), (ngroups:1) ),
           y = c( DF_mean$PredictionMean, DF_mean$IstValuesMean[ngroups:1] ),
           col = col2_f,
           border = NA )
  
  lines(  x = DF_mean$PredictionMean, lwd = 2, col = col1 )
  
  
  if ( bullets == 'normal' ) {
    lines(  x = DF_mean$IstValuesMean,  lwd = 2, col = col2 )
    points( x = DF_mean$IstValuesMean,  cex = bullets.cex, col = col2, pch = 19 )
    
    points( x = DF_mean$PredictionMean, cex = bullets.cex, col = col1, pch = 19 )
  }
  
  if ( bullets == 'boxplot' ) {
    lines(  x = DF_mean$IstValuesMean,  lwd = 2, col = col2_a )
    points( x = DF_mean$IstValuesMean, cex = bullets.cex, col = col2, pch = 19 )
    
    for ( i in 1:nrow(bp_data) ) {
      rect( xleft   = i - 0.1,
            xright  = i + 0.1,
            ybottom = bp_data[i, 2],
            ytop    = bp_data[i, 4],
            col     = col1_a,
            border  = col1 )
      
      segments( x0  = i - 0.1,
                x1  = i + 0.1,
                y0  = bp_data[i, 3],
                y1  = bp_data[i, 3],
                col = col1,
                lwd = 2)
      
      segments( x0  = i,
                x1  = i,
                y0  = bp_data[i, 1],
                y1  = bp_data[i, 2],
                col = col1 )
      
      segments( x0  = i,
                x1  = i,
                y0  = bp_data[i, 4],
                y1  = bp_data[i, 5],
                col = col1 )
    }
  }
  
  if ( ! is.null(Args$las) ) {
    las = Args$las
  } else {
    las = 1
  }
  
  axis( side = 1, at = 1:ngroups, labels = 1:ngroups, las = las, font = font )
  axis( side = 2, at = yaxp, labels = yaxp, las = las, font = font )
  
  if ( !is.na(legend.pos) ) {
    legend( legend.pos,
            legend    = c('Prediction', 'Real values'),
            lty       = c(1,1),
            lwd       = c(2,2),
            col       = c( col1,
                           col2),
            bg        = rgb(100, 100, 100, 10, maxColorValue = 255),
            box.col   = NA,
            text.font = font )
  }
  
  box()
  
  invisible( DF_mean )
}
