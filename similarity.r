## main function: get_similarity
##
#### creates a similarity matrix between states
###### $ns$ is the number of states 
##
## example call:
##
#### get_similarity(10)
##
## visualize results:
##
#### plot_similarity(get_similarity(10))

require('ggplot2')
require('reshape2')


get_similarity = function(ns, weber_fraction = 0.2, epsilon = 0) {
  # $ns$ is the maximal number to be considered
  # $epsilon$ is an additive constant on the $sigma_n$
  states = 0:ns
  sigma = sapply(states, function(n) weber_fraction * n + epsilon)
  number_confusion_probs = matrix(0, ns+1, ns+1, 
                                  dimnames = list(states,states))
  for (n in states) {
    for (m in states) {
      # prob that $m$ is perceived when $n$ is actual
      number_confusion_probs[n+1, m+1] = pnorm(m+0.5, n, sigma[n+1]) - pnorm(m-0.5, n, sigma[n+1])
    }
  }
  
  scene_confusion_probs = matrix(0, ns+1, ns+1, 
                                 dimnames = list(states,states))
  for (n in states) {
    for (m in states) {
      # prob that scene $m$ is perceived when scene $n$ is actual
      scene_confusion_probs[n+1, m+1] = number_confusion_probs[n+1,m+1] * number_confusion_probs[10-n+1,10-m+1]
    }
  }
  scene_confusion_probs = prop.table(scene_confusion_probs,1)
  
  similarity = matrix(0, ns+1, ns+1, 
                      dimnames = list(states,states))
  for (n in states) {
    for (m in states) {
      similarity[n+1, m+1] = (scene_confusion_probs[n+1,m+1] * scene_confusion_probs[m+1,n+1])
    }
    similarity[n+1, ] = similarity[n+1, ] / max(similarity[n+1, ])
  }
  return(similarity)
}

plot_similarity = function(similarity){
  plotData = melt(similarity)
  SimPlot = ggplot(plotData, aes(x = Var2, y = value, color = factor(Var1))) + geom_line( )
  show(SimPlot)
  return(SimPlot)
}

