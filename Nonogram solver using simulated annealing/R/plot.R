plot_nonogram = function(X, r_clues = r_clues, c_clues = c_clues){
  
  #The plot function has been inspired by coolbutuseless : 
  #https://github.com/coolbutuseless/nonogram/blob/master/R/plot.R
  
  nrows = length(r_clues)
  ncols = length(c_clues)
  
  df = data.frame(
    value = as.factor(X),
    y     = rev(1:nrows),
    x     = rep(1:ncols, each=nrows)
  )

  ylabels = rev(sapply(r_clues, function(x) paste(x, collapse = " ")))
  ybreaks = 1:nrows
  
  xlabels = sapply(c_clues, function(x) paste(x, collapse = "\n"))
  xbreaks = 1:ncols
  
  fig = ggplot(df) +
    geom_tile(aes(x, y, fill=value, colour=value, size=value), width=1, height=1) +
    coord_equal() +
    theme_void() +
    scale_fill_manual (values = c('0' = 'grey97' , '1' = 'lightblue')) +
    scale_color_manual(values = c('0' = 'grey50' , '1' = 'grey50'  )) +
    scale_size_manual (values = c('0' = 0.25     , '1' = 0.25 )) + 
    scale_x_continuous(breaks = xbreaks, labels = xlabels) +
    scale_y_continuous(breaks = ybreaks, labels = ylabels) + 
    theme(
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, size = 15, face = 'bold')
    ) + theme(
      plot.margin = unit(c(t=0, r=0, b=10, l=0), units='pt'),
      axis.text.x = element_text(size=11, vjust=1, margin=margin( -9,  0, 0, 0)),
      axis.text.y = element_text(size=11, hjust=1, margin=margin(  0, -6, 0, 0))
    )
  
  return(fig)
}

