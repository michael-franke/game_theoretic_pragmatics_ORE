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

##################################################
## helper functions
##################################################


#' Calculate expected utility for sender
#'
#' Calculates the expected utility of a sender, given a game and the receiver's behavioral strategy.
#'
#' @param rec receiver strategy: Message X State row-stochastic matrix
#' @param game the game that is being played
#'
#' @return A State X Message matrix of expected utilities for the sender.
#'
#' @examples
#' get_EU_sender(rec, scalar_implicature_game)
#'
#' @export
get_EU_sender = function(rec, game) {
  sEU = t(apply(game$utils %*% t(rec), 1, function(x) x * game$message_preferences))
  return(sEU)
}

#' Receiver's posterior beliefs
#'
#' Calculates the posterior beliefs about the actual state of the receiver, given a game and the sender's behavioral strategy.
#'
#' @param sen sender strategy: State X Message row-stochastic matrix
#' @param game the game that is being played
#'
#' @return A Message X States row-stochastic matrix of posterior beliefs for the receiver.
#'
#' @examples
#' get_receiver_belief(sen, scalar_implicature_game)
#'
#' @export
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

#' Calculate expected utility for receiver
#'
#' Calculates the expected utility of a receiver, given a game and the sender's behavioral strategy.
#'
#' @param sen sender strategy: State X Message row-stochastic matrix
#' @param game the game that is being played
#'
#' @return A Message X State matrix of expected utilities for the receiver.
#'
#' @examples
#' get_EU_receiver(sen, scalar_implicature_game)
#'
#' @export
get_EU_receiver = function(sen, game){
  mu = get_receiver_belief(sen, game)
  rEU = mu %*% utils
  return(rEU)
}

#' Calculate best response for sender
#'
#' Calculates the best response for a sender, given a game and the receiver's behavioral strategy.
#'
#' @param EU expected utility matrix
#'
#' @return A sender strategy matrix (states times messages) with only best responses in the support.
#'
#' @examples
#' best_response(get_EU_sender(rec, scalar_implicature_game))
#'
#' @export
best_response = function(EU) {
  BR = EU
  for (i in 1:(dim(EU)[1])) {
    BR[i,] = ifelse(max(EU[i,]) == EU[i,], 1, 0)
  }
  return(prop.table(BR,1))
}

#' @export
quantal_response = function(EU, lambda = 1) {
  QR = prop.table(exp(lambda*EU),1)
  return(QR)
}

#' Calculate expected utility for sender in RSA model
#'
#' Calculates the expected utility of a sender, given a game and the receiver's behavioral strategy.
#'
#' @param rec receiver strategy: Message X State row-stochastic matrix
#' @param game the game that is being played
#'
#' @return A State X Message matrix of expected utilities for the sender.
#'
#' @examples
#' get_EU_sender(rec, scalar_implicature_game)
#'
#' @export
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

#' Discrete time replicator dynamics in behavioral strategies
#'
#' Calculates discrete time replicator dynamics in behavioral strategies for a signaling game
#'
#' @param game the signaling game that is played
#' @param iterations how many discrete-time update steps
#' @param initialPerturbation how much to wiggle the initial (semantic-play) state
#' @param add background fittness; influences speed of change;
#'
#' @return A list with the final sender and receiver strategies.
#'
#' @examples
#' apply_RD(scalar_scalar_implicature_game)
#'
#' @export
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

#' Iterated Best Response solution
#'
#' Calculates the IBR solution for the signaling game
#'
#' @param game the signaling game that is played
#' @param depth depth of recursive reasoning
#'
#' @return A list with the final sender and receiver strategies.
#'
#' @examples
#' apply_IBR(scalar_scalar_implicature_game)
#'
#' @export
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

#' Iterated Quantal Response solution
#'
#' Calculates the IQR solution for the signaling game
#'
#' @param game the signaling game that is played
#' @param depth depth of recursive reasoning
#' @param lambda "rationality"/soft-max parameter
#'
#' @return A list with the final sender and receiver strategies.
#'
#' @examples
#' apply_IQR(scalar_scalar_implicature_game)
#'
#' @export
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


#' Rational Speech Act solution
#'
#' Calculates the RSA solution for the signaling game
#'
#' @param game the signaling game that is played
#' @param depth depth of recursive reasoning
#' @param lambda "rationality"/soft-max parameter
#'
#' @return A list with the final sender and receiver strategies.
#'
#' @examples
#' apply_RSA(scalar_scalar_implicature_game)
#'
#' @export
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