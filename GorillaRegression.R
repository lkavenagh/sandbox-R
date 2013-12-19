file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
read.csv(file) -> gorilla
str(gorilla)

glm.out = glm(seen ~ W*C*CW, family = binomial(logit), data = gorilla)
summary(glm.out)
anova(glm.out, test="Chisq")


# make a prediction for a specific observation
x = c(100, 66, 48)

h = glm.out$coef[1]

p = 2;
for (i in x) {
  h = h + (glm.out$coef[p] * i)
  p = p + 1
}

H = 1 / (1 + exp(-h))
print(H)