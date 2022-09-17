library("mlogit")
library("dfidx")
library("Formula")





res <- data.frame(ID = c(1:60),
                  BLOCK = c(1,2,3,1,2,3,3,2,1,1,2,3,1,2,1,3,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,3,1,2),
                  q1 = c(1,2,3,1,2,3,3,4,4,1,2,3,3,2,3,3,2,1,4,2,3,1,2,3,1,3,2,1,4,3,3,4,3,2,2,1,2,2,3,3,2,3,3,2,3,3,2,1,1,2,1,3,2,1,1,2,3,3,3,2),
                  q2 = c(1,3,2,1,1,2,2,4,4,1,3,2,1,3,1,2,3,2,1,2,2,4,4,4,2,1,3,4,4,2,1,3,2,2,3,2,2,3,1,1,2,1,1,3,2,3,3,2,2,4,1,1,3,2,1,3,2,2,1,2),
                  q3 = c(2,2,1,2,2,1,1,3,4,1,3,1,1,3,2,1,3,1,2,4,1,4,2,1,3,1,2,4,2,1,2,3,4,2,3,1,1,3,2,1,3,1,2,3,1,1,3,1,4,3,1,2,2,1,1,3,1,1,2,3))
res

ma=make.design.matrix(choice.experiment.design = rm,
                      optout = TRUE,
                      categorical.attributes = c("inches","leitourgia","resolution"),
                      continuous.attributes ="Price",
                      unlabeled=TRUE,
                      common = NULL,
                      binary=FALSE)
ma

dt=make.dataset(respondent.dataset =res,
                design.matrix = ma,
                choice.indicators = c("q1","q2","q3"))
dt

ds<- dfidx(dt, choice = "RES",
             idx = list(c("STR", "ID")),
             idnames = c(NA, "ALT"))
head(ds,6)
ml.tv=mlogit(RES~sixty.five+sevanty.five+Hdmi.2.1+HDR+Full.HD+X4K+Price|0|0,ds)
summary(ml.tv)
library("lmtest")

-coef(ml.tv)[1]/coef(ml.tv)[6]
-coef(ml.tv)[2]/coef(ml.tv)[6]
-coef(ml.tv)[3]/coef(ml.tv)[6]
-coef(ml.tv)[4]/coef(ml.tv)[6]
-coef(ml.tv)[5]/coef(ml.tv)[6]

op1=c(1,0,1,0,0,1,500)
op2=c(0,1,0,1,1,0,700)
op3=c(0,0,0,0,0,0,0)
simulation=matrix(rbind(op1, op2,op3), nrow = 3, ncol=7,
                     dimnames = list(c("Option1","Option2","no choice"),
                                     c("sixty.five","sevanty.five", "Hdmi.2.1",
                                       "HDR", "Full.HD","X4K","Price")))
utility=simulation%*%ml.tv$coef
utility
share =exp(utility)/sum(exp(utility))
cbind(share , simulation)
