install.packages("bnlearn")
install.packages("snow")
install.packages("graph")
install.packages("Rgraphviz")
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(bnlearn)
#get data
data(learning.test)
str(learning.test)

#learn network
#constraint based (produce all the same network)
bn.gs = gs(learning.test)
bn.gs
bn2 <- iamb(learning.test)
bn3 <- fast.iamb(learning.test)
bn4 <- inter.iamb(learning.test)
compare(bn3, bn2)
#score based (hill climbing produces different network)
bn.hc <- hc(learning.test, score = "aic")
bn.hc

#compare them in plots
par(mfrow = c(1, 2))
plot(bn.gs, main = "Constraint-based algorithms", highlight = c("A", "B"))
plot(bn.hc, main = "Hill-Climbing", highlight = c("A", "B"))
#or with plots by rgraphviz
par(mfrow = c(1, 2))
highlight.opts <- list(nodes = c("A", "B"), arcs = c("A", "B"), col = "red", fill = "grey")
graphviz.plot(bn.hc, highlight = highlight.opts)
graphviz.plot(bn.gs, highlight = highlight.opts)

#bn can also be stated as a single string
modelstring(bn.hc)

#manipulate certain arcs
undirected.arcs(bn.gs)
bn.dag <- set.arc(bn.gs, "A", "B")
modelstring(bn.dag)
compare(bn.dag, bn.hc)

#fit parameters
fitted.gs= bn.fit(bn.hc, data = learning.test)

#create a graph manually from scratch
other <- empty.graph(nodes = nodes(bn2))
a = data.frame(from = c("A", "A", "B", "D"), to = c("E", "F", "C", "E"))
a$from = as.character(a$from)
a$to = as.character(a$to)
str(a)
arcs(other) <- a
other

#more examples
#alarm data set
alarm.gs <- gs(alarm)
alarm.iamb <- iamb(alarm)
alarm.fast.iamb <- fast.iamb(alarm)
alarm.inter.iamb <- inter.iamb(alarm)
alarm.mmpc <- mmpc(alarm)
alarm.hc <- hc(alarm, score = "bic")

 dag <- empty.graph(names(alarm))
 modelstring(dag) <- paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
                              "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA][HRSA|ERCA:HR]",
                              "[ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK][MINV|INT:VLNG][FIO2]",
                              "[PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB][SHNT|INT:PMB][INT]",
                              "[PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS][VTUB|DISC:VMCH]",
                              "[VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV][CCHL|ACO2:ANES:SAO2:TPR]",
                              "[HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
 alarm.gs <- gs(alarm, test = "x2")
 alarm.mc <- gs(alarm, test = "mc-x2", B = 10000)
 par(mfrow = c(1,2), omi = rep(0, 4), mar = c(1, 0, 1, 0))
 graphviz.plot(dag, highlight = list(arcs = arcs(alarm.gs)))
 graphviz.plot(dag, highlight = list(arcs = arcs(alarm.mc)))
