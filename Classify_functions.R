####################################################################
#functions for classification
# classify(plist, t) computes predicted Y using a list of predicted probabilities
# plist and a threshold t, as plist[1]>t --> y[i]=1
classify = function(plist, t){
  yclass=c()
  for (prob in plist)
    if (prob < t) yclass=c(yclass, 0)
  else yclass=c(yclass, 1)
  yclass}

#compare(plist, yvar) compares two binary lists (predicted Y and actual Y), 
# and returns the classification matrix
compare=function(plist, yvar){
  i=1
  tp=0
  tn=0
  fp=0
  fn=0
  for (pred in plist) {
    if (pred==yvar[i])
      if (yvar[i]==1)
        tp=tp+1
    else
      tn=tn+1
    else
      if (yvar[i]==1)
        fn=fn+1
    else
      fp=fp+1
    i=i+1}
  matrix(c(tp, fp, fn, tn), nrow=2, ncol=2, dimnames=list(c("Actual 1", "Actual 0"),c("Predict 1", "Predict 0")))
}
#compute classification metrics using classification matrix m
# proportion of correctly classified positives TP/(TP+FN) (recall)
sensitivity= function(m){
  (m[1,1])/(m[1,1]+m[1,2])
}

# proportion of correctly classified positives and negatives
#(TP+TN)/(TP+TN+FP+FN)
accuracy=function(m){ 
  (m[1,1]+m[2,2])/sum(m)} 

# proportion of correctly classified negatives
#specificity TN/(TN+FP)
specificity =function(m){ m[2,2]/(m[2,2]+m[2,1])}

# proportion of correctly classified positives
#recall TN/(TN+FP)
precision =function(m){ m[1,1]/(m[1,1]+m[2,1])}

##########################################################################