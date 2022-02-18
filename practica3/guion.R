library(lattice)
library(gridExtra)
library(gRain)
library(Rgraphviz)
library(bnlearn)

dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")
dag

survey <- read.table("survey.txt", header = TRUE, colClasses = "factor")

bn.mle <- bn.fit(dag, data = survey, method = "mle")

bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 10)


ci.test("T", "E", c("O", "R"), test = "mi", data = survey)
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)
ci.test("T", "O", "R", test = "x2", data = survey)


arc.strength(dag, data = survey, criterion = "x2")

score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)
score(dag, data = survey, type = "aic")


learned <- hc(survey)
modelstring(learned)
score(learned, data = survey, type = "bic")

learned2 <- hc(survey, score = "bde")
learned2


dag.bnlearn <- model2network("[G][E][V|G:E][N|V][W|V][C|N:W]")
crop.nodes <- nodes(dag.bnlearn)

E.dist <- list(coef = c("(Intercept)" = 50), sd = 10)
G.dist <- list(coef = c("(Intercept)" = 50), sd = 10)
V.dist <- list(coef = c("(Intercept)" = -10.35534, E = 0.70711, G = 0.5), sd = 5)
N.dist <- list(coef = c("(Intercept)" = 45, V = 0.1), sd = 9.949874)
W.dist <- list(coef = c("(Intercept)" = 15, V = 0.7), sd = 7.141428)
C.dist <- list(coef = c("(Intercept)" = 0, N = 0.3, W = 0.7), sd = 6.25)

dist.list = list(E = E.dist, G = G.dist, V = V.dist, N = N.dist, W = W.dist, C = C.dist)
gbn.bnlearn <- custom.fit(dag.bnlearn, dist = dist.list)
gbn.bnlearn

graphviz.plot(gbn.bnlearn)


# ejercicio





