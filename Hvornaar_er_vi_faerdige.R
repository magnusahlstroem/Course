library(readr)
library(dplyr)
library(data.table)
when <- read_csv2("H:/Dokumenter/Analyser/Jens/Data/MultiExport_15362_2017-10-13.csv") %>%
  group_by(Registreringsdato) %>%
  summarize(n = sum(n())) %>%
  mutate(Dato = as.Date(Registreringsdato, format = "%d-%m-%Y") - as.Date("2017-07-01"),
         Dato = as.numeric(Dato)) %>%
  arrange(Dato) %>%
  mutate(cumn = cumsum(n),
         logcumn = log2(cumn)) %>%
  select(Dato, n, cumn, logcumn)


model0 <- lm(cumn ~ Dato - 1, data = when)
model2 <- lm(cumn ~ Dato + I(Dato^2) - 1, data = when)
model3 <- lm(cumn ~ Dato + I(Dato^2) + I(Dato^3) - 1, data = when)

anova(model0, model2, model3)

model.exp.poly <- lm(logcumn ~ Dato + I(Dato^2) - 1, data = when)

predicted0 <- predict(model.exp, newdata = data.frame(Dato = seq(1:1000)))

anova(model.exp, model.exp.poly)

pdf("H:/Dokumenter/Analyser/Jens/Output/Hvornaar_er_vi_faerdige.pdf", height = 7, width = 12)
with(when, plot(cumn ~ Dato, 
                type = "h", lwd =3, ylim = c(0,13000), xlim = c(0,250), 
                ylab = "Antal registrede",
                xlab = "Dato", 
                axes = F))
predicted <- predict(model2, newdata = data.frame(Dato = seq(1:1000)))
lines(predicted)
ticks.x <- c(as.Date(c(0,30,60,90,120,150,180,210,240), origin = "2017-07-01"), "")
abline(h = 12500, lty = 2)
abline(v = min(which(predicted > 12500)), lty = 2)
axis(side = 2, at = c(0,4000,8000,12000, 16000), labels = c("0", "4000", "8000", "12000", ""))
axis(side = 1, at = c(0,30,60,90,120,150,180,210,240, 270), labels = ticks.x)
dev.off()