library(rdwd)
id = findID('Konstanz')
KN_pos = selectDWD(id=id, res='hourly',  per='recent') #recent ist bis gestern muß manuell auswählen
KN_recent = dataDWD(KN_pos, read = TRUE, varnames=TRUE)



