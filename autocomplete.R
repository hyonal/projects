library(data.tree)

add_child = function(current_node, char, weight) {

  if (length(char) == 0) {
    return(0)
  }

  if (char[1] %in% names(current_node$children)) {
    t = current_node[[char[1]]]
    t$weight = max(t$weight, weight)
    if (length(char) == 1) {
      t$length = t$level
    } else {
      add_child(current_node[[char[1]]], char[-1], weight)
    }
  } else {
    t = current_node$AddChild(char[1])
    t$weight = weight
      if (length(char) == 1) {
      t$length = t$level
      } else {
      t$length = 0
      add_child(t, char[-1], weight)
      }
  }
}

read_terms = function(file) {
  
  words = readLines(file)
  words = words[-1]
  words = sapply(words, function(x) strsplit(x, "\t"))
  words = unlist(words)
  words = matrix(words, ncol = 2, byrow = TRUE)
  weights = as.numeric(words[,1])
  words = as.list(tolower(words[,2]))
  words = lapply(words, function(x) strsplit(x, "")[[1]])
  
  trie = Node$new("0")
  for (i in 1:length(words)) {
    add_child(trie, words[[i]], weights[i])
  }
  return(trie)
}

get_weight = function(node, str) {
  char = strsplit(str, "")[[1]]
  return(node$Climb(char)$weight)
}


autocomplete_helper = function(node) {
  if (isLeaf(node)) {
    return(node$name)
  } else {
    children = node$children
    name = lapply(children, function(child) autocomplete_helper(child))
    name = lapply(name, function(n) paste(node$name, n, sep = ""))
    name = unlist(name)
    if (node$length == node$level) {
      name = c(name, node$name)
    }
    return(name)
  }
}

autocomplete = function(query, tree, ncount) {
  char = strsplit(tolower(query), "")[[1]]
  node = tree$Climb(char)
  words = autocomplete_helper(node)
  words = sapply(words, function(x) paste(paste(char[-length(char)], collapse = ""), x, sep = ""))
  weights = sapply(words, function(x) get_weight(tree, x))
  ord = order(weights, decreasing = TRUE)
  words = words[ord]
  weights = weights[ord]
  if (length(words) > ncount) {
    words = words[1:ncount]
    weights = weights[1:ncount]
  }
  return(cbind(words, weights))
}
