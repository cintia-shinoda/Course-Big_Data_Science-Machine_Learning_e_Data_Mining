# AULA 4 - MODELOS REGRESSIVOS

pib <- read.csv("PIB.csv", sep=";", dec=",")

# Separar Base de Treino e de Teste
treino <- pib[1:132,]
teste <- pib[133:138,]

# Modelo de Regressão Linear
mod1 <- lm(PIB~BR_LEVES, data=treino)
summary(mod1)

# Modelo de Regressão Linear MULTIVARIADO
mod2 <- lm(PIB~BR_LEVES+BR_PESADOS, data=treino)
summary(mod2)

# Modelo Autoregressivo AR2
mod3 <- lm(PIB~PIB.i.1+PIB.i.2+PIB.i.12, data=treino)
summary(mod3)

# Modelo de Regressão Linear MULTIVARIADO + Sazonalidade
mod4 <- lm(PIB~BR_LEVES+BR_PESADOS+D2+D5+D6+D7, data=treino)
summary(mod4)

# Previsão em TESTE
p <- predict(mod4, newdata = teste)

cbind(teste$PIB, p, teste$PIB - p)
sse <- sum((teste$PIB - p)^2)
