##################################################
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
##################################################

source('similarity.r')

# generic function to create a game
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

# scalar implicature game
semantics <- matrix(0,nrow = 2,ncol = 2)
semantics[1,] <- 1
semantics[2,2] <- 1
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
scalar_implicature_alternative_game = create_game(
  states = c('e','a'),
  messages = c('some','all', 'some but not all'),
  semantics = semantics,
  message_preferences = c(1, 1, 0.8)
)

# M-implicature game
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
free_choice_game = create_game(
  states = c('a','b','ab'),
  messages = c('may A','may B', 'may (A or B)'),
  semantics = semantics
)

# numerosity game (small)
semantics <- matrix(0,nrow=7,ncol=11)
semantics[1,1] = 1 # none
semantics[2,2] = 1 # one
semantics[3,3] = 1 # two
semantics[4,4] = 1 # three
semantics[5,2:11] = 1 # some
semantics[6,7:11] = 1 # most
semantics[7,11] = 1 # all
numerosity_game = create_game(
  states = paste("t_", 0:10, collapse = NULL, sep = ""),
  messages = c("none", "one", "two", "three", "some", "most", "all"),
  utils = get_similarity(10, weber_fraction = 0.5, epsilon = 0.2),
  semantics  = semantics,
  message_preferences = c(1,0.8,0.7,0.6,1,1,1)
)


