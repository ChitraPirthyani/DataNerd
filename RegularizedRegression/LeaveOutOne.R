LeaveOutOne=function(fit)
{
  h=lm.influence(fit)$h
  sqrt(mean((residuals(fit)/(1-h))^2))
}
