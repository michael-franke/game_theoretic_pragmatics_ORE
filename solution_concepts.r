##################################################
## solution concepts defined here: 
##
#### 1. replicator dynamics -> RD
#### 2. iterated best response (vanilla) -> IBR
#### 3. iterated quantal response -> IQR
#### 4. rational speech act -> RSA
##
## main functions: apply_XXX
##### with XXX in {RD, IBR, IQR, RSA}
##
## additional helper functions:
##
#### 1. get_EU_sender
#### 2. get_receiver_belief
#### 3. get_EU_receiver
#### 4. best_response
#### 5. quantal_response
#### 6. get_EU_sender_RSA
##
## example call:
##
#### apply_RD(scalar_implicature_game)
##################################################

source('games.r')

##################################################
## helper functions
##################################################

get_EU_sender = function(rec, game) {
  sEU = matrix(0, nrow = length(game$states), ncol = length(game$messages),
               dimnames = list(game$states, game$messages))
  for (t in 1:length(game$states)) {
    for (m in 1:length(game$messages)) {
      sEU[t,m] = sum(sapply(1:length(game$states), function(a) rec[m,a] * game$utils[t,a]) ) * game$message_preferences[m]
    }
  }
  return(sEU)
}

get_receiver_belief = function(sen, game){
  mu = matrix(0, ncol = length(game$states), nrow = length(game$messages),
              dimnames = list(game$messages, game$states))
  for (m in 1:length(game$messages)) {
    normalizingConstant = sum(sapply(1:length(game$states), function(t) game$prior[t] * sen[t,m] ) )
    if (normalizingConstant > 0) {
      mu[m,] = sapply(1:length(game$states), function(t) game$prior[t] * sen[t,m] / normalizingConstant)
    } else {
      mu[m,] = 1/length(game$states)
    }
  }
  return(mu)
}

get_EU_receiver = function(sen, game){
  rEU = matrix(0, ncol = length(game$states), nrow = length(game$messages),
               dimnames = list(game$messages, game$states))
  mu = get_receiver_belief(sen, game)
  for (m in 1:length(game$messages)) {
    for (a in 1:length(game$states)) {
      rEU[m,a] = sum(sapply(1:length(game$states), function(t) mu[m,t] * game$utils[t,a]) )
    }
  }
  return(rEU)
}

best_response = function(EU) {
  BR = EU
  for (i in 1:(dim(EU)[1])) {
    BR[i,] = ifelse(max(EU[i,]) == EU[i,], 1, 0)
  }
  return(prop.table(BR,1))
}

quantal_response = function(EU, lambda = 1) {
  QR = prop.table(exp(lambda*EU),1)
  return(QR)
}

get_EU_sender_RSA = function(rec, game) {
  sEU = matrix(0, nrow = length(game$states), ncol = length(game$messages),
               dimnames = list(game$states, game$messages))
  for (t in 1:length(game$states)) {
    for (m in 1:length(game$messages)) {
      sEU[t,m] = log(rec[m,t]) + rec[m,t] * game$message_preferences[m]
    }
  }
  return(sEU)
}

##################################################
## main functions
##################################################

apply_RD = function(game, iterations = 100, initialPertubation = 0, add = 0.01) {
  # start with literal language use
  sen = prop.table(t(game$semantics),1) 
  rec = prop.table(game$semantics,1)
  # perturb slightly
  for (t in 1:length(game$states)) {
    for (m in 1:length(game$messages)) {
      sen[t,m] = sen[t,m] + abs(rnorm(n= 1, mean = 0, sd = initialPertubation))
      rec[m,t] = rec[m,t] + abs(rnorm(n= 1, mean = 0, sd = initialPertubation))
    }
  }
  # renormalize
  sen = prop.table(sen,1)
  rec = prop.table(rec,1)
  for (i in 1:iterations){
    sEU = get_EU_sender(rec, game) + add 
    rEU = get_EU_receiver(sen, game) + add
    sen = prop.table(sen*sEU,1)
    rec = prop.table(rec*rEU,1)
  }
  return(list(sender = sen, receiver = rec))
}

apply_IBR = function(game, depth = 10) {
  # start with literal language use
  sen = prop.table(t(game$semantics),1) 
  rec = prop.table(game$semantics,1)
  if (depth >= 1) {
    for (i in 1:depth) {
      sen_new = best_response(get_EU_sender(rec, game))
      rec_new = best_response(get_EU_receiver(sen, game))
      sen = sen_new
      rec = rec_new
    }  
  }
  return(list(sen = sen, rec = rec))
}

apply_IQR = function(game, depth = 10, lambda = 5) {
  # start with literal language use
  sen = prop.table(t(game$semantics),1) 
  rec = prop.table(game$semantics,1)
  if (depth >= 1) {
    for (i in 1:depth) {
      sen_new = quantal_response(get_EU_sender(rec, game), lambda)
      rec_new = quantal_response(get_EU_receiver(sen, game), lambda)
      sen = sen_new
      rec = rec_new
    }  
  }
  return(list(sen = sen, rec = rec))
}

apply_RSA = function(game, depth = 10, lambda = 5) {
  # start with literal language use
  rec = prop.table(game$semantics,1)
  sen = NA
  if (depth >= 1) {
    for (i in 1:depth) {
      sen = quantal_response(get_EU_sender_RSA(rec, game), lambda)
      rec = get_receiver_belief(sen,game)
    }  
  }
  return(list(sen = sen, rec = rec))
}