install.packages('support.CEs')
library('support.CEs')

rm<- rotation.design(attribute.names = list(leitourgia= c('smart tv','Hdmi 2.1','HDR'),
                                            inches = c('43','65','75'),
                                            resolution= c('HD','Full HD','4K'),
                                            price= c('200','500','800')),
                     nblocks=3,
                     nalternative = 3,
                     randomize = TRUE,
                     seed = 123)

rm$alternatives$alt.1
rm$alternatives$alt.2
rm$alternatives$alt.3

Questions<- questionnaire(choice.experiment.design = rm,
                          common = NULL,
                          quote = TRUE)
  