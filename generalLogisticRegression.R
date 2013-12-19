library("MASS")
data(menarche)
str(menarche)
summary(menarche)
plot(Menarche/Total ~ Age, data=menarche)

glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)

lines(menarche$Age, glm.out$fitted, type="l", col="red")

summary(glm.out)

x = 13

h = glm.out$coef[1]

p = 2;
for (i in x) {
  h = h + (glm.out$coef[p] * i)
  p = p + 1
}

H = 1 / (1 + exp(-h))
print(H)