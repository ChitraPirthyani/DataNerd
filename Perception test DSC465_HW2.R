percept=read.csv("PerceptionExperiment2007-2015Fall1.csv")

percept$Error <- percept$Response-percept$TrueValue
head(percept$Error)
percept$AbsoluteError <- abs(percept$Error)
head(percept$AbsoluteError)


plot(percept$Test, percept$AbsoluteError)

library(ggplot2)
ggplot(percept, aes(percept$Test, percept$AbsoluteError))+geom_point()

ggplot(percept, aes(y=percept$AbsoluteError, x=percept$Test, fill= percept$AbsoluteError)) + geom_density_ridges(0.5)+scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0,0)) 

Test = percept$Test
AbsoluteError = percept$AbsoluteError
ggplot(percept, mapping = aes(Test,AbsoluteError, fill=AbsoluteError )) + geom_density_ridges2(0,alpha=0.5)

ggplot(percept, mapping = aes(Test, AbsoluteError, fill= AbsoluteError) + geom_density_ridges(0,alpha=0.5)+ scale_y_continuous(expand = c(0.5,0.5)+ scale_x_continuous(expand = c(0,0)))

       
ggplot(percept) + geom_density(aes(x=AbsoluteError, fill= Test), alpha=0.2)


ggplot(barley) + geom_density(aes(x = yield, fill = site), alpha = 0.2) 

ggplot(percept, mapping = aes(x=AbsoluteError, fill= Test))
