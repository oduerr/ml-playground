#https://github.com/brry/rdwd
# https://bookdown.org/brry/rdwd/
library(rdwd)
library(tidyverse)

#Stations 
# 'Hamburg-Fuhlsbuettel'
# 'Kostanz'

HH = selectDWD('Hamburg-Fuhlsbuettel', res='daily', var='kl', per='recent')
HH = dataDWD(HH, read = TRUE, varnames=TRUE)
HH$ort = 'HH'

KN_pos = selectDWD('Konstanz', res='daily', var='kl', per='recent')
KN_pos
KN = dataDWD(KN_pos, read = TRUE, varnames=TRUE)
KN$ort = 'KN'
findID('Konstanz')

df = bind_rows(KN, HH)

df %>% ggplot(aes(x=MESS_DATUM, y=SDK.Sonnenscheindauer)) +
  geom_smooth(aes(col=ort)) + ylim(0,9)

df %>% ggplot(aes(x=MESS_DATUM, y=TXK.Lufttemperatur_Max)) +
  geom_smooth(aes(col=ort)) + 
  geom_line(aes(col=ort)) 

df %>% ggplot(aes(x=MESS_DATUM, y=RSK.Niederschlagshoehe)) +
  geom_smooth(aes(col=ort)) + 
  geom_line(aes(col=ort)) 

#Regentag mindestens 0.1 liter regen / qm 
#https://www.wetter.net/wetterlexikon/eintrag/regentag
# 1 liter / qm = (10^3 cm^3 / 100*100 cm^2) = 0.1 cm 
# 0.1 liter / qm --> 0.01 cm = 0.1 mm

#RSK.Niederschlagshoehe in mm https://www.dwd.de/DE/leistungen/klimadatendeutschland/beschreibung_tagesmonatswerte.html
mean(HH$RSK.Niederschlagshoehe > 0.1)
mean(KN$RSK.Niederschlagshoehe > 0.1)

mean(KN$SDK.Sonnenscheindauer)
mean(HH$SDK.Sonnenscheindauer > 5)
mean(KN$SDK.Sonnenscheindauer > 5)

df$regen = df$RSK.Niederschlagshoehe > 0.1
df$sonne = !df$regen & df$SDK.Sonnenscheindauer > 5
df$be = !df$regen & !df$sonne


#df %>% select(sonne, regen ,be) %>% rowSums() all 1
library(keras)
library(tensorflow)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 3, input_shape = 3, activation = 'softmax', use_bias = FALSE) 

# compile model and initialize weights
model %>%
  compile(
    optimizer = 'adam',
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )

summary(model)
get_weights(model)
#X = df %>% filter(ort == 'KN') %>% select(sonne, regen, be) 
X = df %>% filter(ort == 'HH') %>% select(sonne, regen, be) 
head(X,2)

Y = X[-1,] #Remove first row (one day up)
head(Y,2)

X = X[-nrow(X),] #Remove last row
X = as.matrix(X)
Y = as.matrix(Y)

model(X[1:3,])
model$evaluate(X,Y)

history <- model %>% #Takes about 30sec
  fit(x = X,
      y = Y,
      validation_split = 0.2,
      verbose = 0,
      epochs = 100,
      shuffle = TRUE) 
plot(history)

X = matrix(c(1,0,0), nrow = 1)
model(X) #
predict(model,X) #HH 0.6293542 0.259009 0.1116367

# Extract the weights from your neural network
W <- lapply(model$weights, as.matrix)
z = X %*% W[[1]]#   + W[[2]][1]
(p = exp(z)/sum(exp(z)))


library(igraph)
# Create an edge list representing the weights
edges <- list()
for (i in 1:length(weights)) {
  weight_matrix <- weights[[i]]
  edge_list <- as.data.frame(cbind(row(weight_matrix), col(weight_matrix), weight_matrix))
  colnames(edge_list) <- c("from", "to", "weight")
  edges[[i]] <- edge_list
}



# Create a graph from the edge list
g <- graph_from_data_frame(do.call(rbind, edges[[1]]), directed = TRUE)
plot(g)
# Plot the graph, using the edge weight as the edge width
plot(g, edge.width = E(g)$weight / max(E(g)$weight) * 8, edge.curved = 0.2, vertex.size = 0)


