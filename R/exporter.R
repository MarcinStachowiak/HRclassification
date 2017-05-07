exporter.save_as_png <- function(toSave,name,prefix){
  if(!file.exists(getCustomProperty("target"))){
    dir.create(getCustomProperty("target"))
  }
  file_name <- file.path(getCustomProperty("target"),sprintf('%s_%s%s',prefix,name,'.png'))
  png(file_name, width=2000, height=2000, units="px", res=200)
  plot(toSave)
  dev.off()
  loginfo(sprintf('Saved file %s',file_name))
}

exporter.save_correlation_as_png <- function(COR,name,prefix){
  if(!file.exists(getCustomProperty("target"))){
    dir.create(getCustomProperty("target"))
  }
  file_name <- file.path(getCustomProperty("target"),sprintf('%s_%s%s',prefix,name,'.png'))
  png(file_name, width=2000, height=2000, units="px", res=200)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  p <- corrplot(COR, method="color", col=col(200),  
                type="upper", order="hclust", 
                addCoef.col = "black", # Add coefficient of correlation
                tl.col="black", tl.srt=45, #Text label color and rotationcorrplot
                diag=T 
  )
  dev.off()
  loginfo(sprintf('Saved file %s',file_name))
}