###################################################
## game models
##
## main function: create_game
##
#### takes a vector of state names and message names
#### as input, together with optional:
#### semantics -> matrix of semantic meanings
#### utils -> matrix of utilities
#### prior -> vector of state priors
#### message_preferences -> vector of preferences
##
## pre-defines the games:
##
#### scalar_implicature_game
#### scalar_implicature_alternative_game
#### I_implicature_game
#### M_implicature_game
#### free_choice_game
#### numerosity_game
###### latter game requires function get_similarity
###################################################

#' Create an interpretation game
#'
#' Creates a signaling game with conventionally meaningful messages
#'
#' @param states character vector of state names
#' @param messages character vector of messages names
#' @param semantics a States X Messages matrix of conventional meanings
#' @param utils States X States matrix of utilities
#' @param prior vector of prior probabilities of states
#' @param message_preferences vector of factors of relative message preference
#'
#' @return A list representation of the interpretation game.
#'
#' @examples
#' show(scalar_implicature_game)
#'
#' @export
create_game = function(states, messages, semantics = NULL, 
                       utils = NULL, prior = NULL, message_preferences = NULL) {
  if (is.null(semantics)) {
    semantics = matrix(1, nrow = length(messages), ncol = length(states))
  }
  colnames(semantics) = states
  rownames(semantics) = messages
  if (is.null(utils)) {
    utils = diag(length(states))
    colnames(utils) = states
    rownames(utils) = states
  }
  if (is.null(prior)) {
    prior = rep(1 / length(states), length(states))
  }
  names(prior) = states
  if (is.null(message_preferences)) {
    message_preferences = rep(1, length(messages))
  }
  names(message_preferences) = messages
  return(
    list(
      states = states,
      messages = messages,
      semantics = semantics,
      utils = utils,
      prior = prior,
      message_preferences = message_preferences
    )
  )
}


######################################
### define some games
######################################

# scalar implicature game
semantics <- matrix(0,nrow = 2,ncol = 2)
semantics[1,] <- 1
semantics[2,2] <- 1
#' @export
scalar_implicature_game = create_game(
  states = c('e','a'),
  messages = c('some','all'),
  semantics = semantics
)

# scalar implicature game with extra alternative "some but not all"
semantics <- matrix(0,nrow = 3,ncol = 2)
semantics[1,] <- 1
semantics[2,2] <- 1
semantics[3,1] <- 1
#' @export
scalar_implicature_alternative_game = create_game(
  states = c('e','a'),
  messages = c('some','all', 'some but not all'),
  semantics = semantics,
  message_preferences = c(1, 1, 0.8)
)

# M-implicature game
#' @export
M_implicature_game = create_game(
  states = c('norm','abnorm'),
  messages = c('norm','marked'),
  prior = c(0.7,0.25),
  message_preferences = c(1, 0.8)
)

# I-implicature game
semantics <- matrix(1,nrow=3,ncol=2)
semantics[2,2] <- 0
semantics[3,1] <- 0
#' @export
I_implicature_game = create_game(
  states = c('cow','goat'),
  messages = c('milk',"cow's milk", "goat's milk"),
  semantics = semantics,
  prior = c(0.75,0.25),
  message_preferences = c(1,0.8,0.8)
)

# free-choice game
semantics <- matrix(0,nrow = 3,ncol = 3)
semantics[1,c(1,3)] <- 1
semantics[2,2:3] <- 1
semantics[3,] <- 1
#' @export
free_choice_game = create_game(
  states = c('a','b','ab'),
  messages = c('may A','may B', 'may (A or B)'),
  semantics = semantics
)

######################################
### define numerosity game
######################################

#' Create a utility matrix based on similarity
#'
#' Creates a utility matrix loosely based on ideas from number perception.
#'
#' @param ns number of states
#' @param weber_fraction positive real governing perceptual noise
#' @param epsilon baseline perceptual noise
#'
#' @return An ns X ns utility matrix
#'
#' @examples
#' get_similarity(10)
#'
#' @export
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

# numerosity game (small)
semantics <- matrix(0,nrow=7,ncol=11)
semantics[1,1] = 1 # none
semantics[2,2] = 1 # one
semantics[3,3] = 1 # two
semantics[4,4] = 1 # three
semantics[5,2:11] = 1 # some
semantics[6,7:11] = 1 # most
semantics[7,11] = 1 # all
#' @export
numerosity_game = create_game(
  states = paste("t_", 0:10, collapse = NULL, sep = ""),
  messages = c("none", "one", "two", "three", "some", "most", "all"),
  utils = get_similarity(10, weber_fraction = 0.5, epsilon = 0.2),
  semantics  = semantics,
  message_preferences = c(1,0.8,0.7,0.6,1,1,1)
)


