#tworzenie wektora
dane <- c(1,3,5) 
dane <- 1:10 # tworzenie sekwencji

#wybór pól z vektora
dane[1]  # wybór pojedynczy
dane[c(1,3)] # wybór wielu pól

table(vector) # liczba wystąpień poszczegolnych elementow

#Stringi
paste("string", "jeden", sep="--")
substring("string", 2, 3)

# listy
list(nazwa=1) # lista z nazwami
names(lista) # nazwy pól w liście

# matrix
matrix(1:9, nrow=3)
rownames(matrix3)
colnames(matrix3)

lapply(x= lista, FUN=funkcja)
apply(x=matrix, MARGIN=1, FUN = funkcja) # ile  wierszy grupować

print(cos)
View() # poczatek i koniec


dataframe[,kolumna] # zwraca  kolumne 

str()
head()
tail()

barplot(  
  height=dtSurvived[, percentage],  
  names.arg=dtSurvived[, Survived],  
  col=dtSurvived[, colorPlot],  
  ylim=c(0, 1),  
  legend.text=dtSurvived[, textPercentage],  
  ylab=ylabel,  
  main=plotTitle 
  )