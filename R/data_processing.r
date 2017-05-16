data_processing.plot_histogram_for_two_clases <- function(data_input,data_output,binwidth,y_title,x_title,title){
  df <- data.frame(data_output=factor(data_output),
                   data_input=data_input)
  mu <- ddply(df, "data_output", summarise, grp.mean=mean(data_input))
  ggplot(df, aes(x=data_input,fill=data_output,color=data_output)) + 
    geom_histogram(binwidth =binwidth,alpha=0.5,position="identity") +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=data_output),linetype="dashed", size=1,show.legend = F)+ 
    labs(title=title,x=x_title, y = y_title) +
    theme(plot.title = element_text(hjust = 0.5))
}

data_processing.convert_to_numerical_values <- function(data_frame){
  stopifnot(is.data.frame(data_frame))
  return (transform(data_frame, sales = as.numeric(sales),salary = as.numeric(salary)))
}

data_processing.scale_and_normalize <- function(data_frame){
  stopifnot(is.data.frame(data_frame))
  row_names <- row.names(data_frame)
  #metoda Z-score
  #scale(dat, center = TRUE, scale = apply(dat, 2, sd))
  result <- as.data.frame(lapply(data_frame, data_processing.normalize_min_max))
  row.names(result) <- row_names
  return (result)
}

data_processing.normalize_min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_processing.plot_cumulative_pca <- function(data.in.pca){
  # Take standard deviation
  data.in.pca.sdev <- data.in.pca$sdev
  # Compute variance
  data.in.pca.stddev <- data.in.pca.sdev^2
  # We aim to find the components which explain the maximum variance.
  # Compute proportion
  data.in.pca.stddev.prop <- data.in.pca.stddev/sum(data.in.pca.stddev)
  
  df <- data.frame(data_y=cumsum(data.in.pca.stddev.prop),
                   data_x=colnames(data.in.pca$rotation))
  
  ggplot(df, aes(data_x,data_y,group=1))+
    geom_line(colour="red") + 
    geom_point(size=3, fill="white")+
    ylim(0,1)+
    geom_text(size = 3, position = position_stack(vjust =0.97),label=round(cumsum(data.in.pca.stddev.prop),digits=2))+
    labs(title='Kumulatywna proporcja wyjasnianej wariancji cech wejściowych przez niezalezne komponenty',x='Niezalezne komponenty', y = 'Kumulatywna proporcja wyjasnionej wariancji')
}

data_processing.plot_biplot <- function(data.in.pca,data.out){
  g <- ggbiplot(data.in.pca , obs.scale = 1, var.scale = 1, 
                groups = data.out, ellipse = TRUE, 
                circle = TRUE)
  g <- g + scale_color_continuous(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  return (g)
  }