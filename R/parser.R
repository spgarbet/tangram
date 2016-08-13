library(stringr)
library(R6)

#' A Node in an Abstract Syntax Tree (AST)
#'
#' This is the root R6 class of any term of the AST which is created
#' when parsing a table formula. This should only be used as a base class
#' as the class information carries the semantic meaning of a given node.
#'
#' @field symbol A string which tells what this node in the AST represents.
#' @field value  A string of addtional information contained by the node.
#'
#' @examples
#'
#' ASTNode$new("some information")
#' ASTNode$new("some information")$string()
#' ASTNode$new("some information")$terms()
#'
ASTNode <- R6Class("ASTNode",
  public = list(
    value  = "character",
    terms      = function() { "Return terms of AST below this node"; return(self$value) },
    distribute = function() { "Distribute multiplication right. This returns self, since a terminal node needs no distribution.";     return(self)       },
    string     = function() { "String representation of this node";  return(self$value) }
  )
)

#' A Variable in an Abstract Syntax Tree (AST)
#'
#' This node represents a variable of interest in the AST. A variable's name
#' is recorded in the value field, and must conform to the rules of identifiers
#' in R. 
#'
#' @field value  A string containing the variable identifier
#' @field format A format string that is either a string containing a number representing significant digits for output, or a C-style printf string.
#' @field type A string that represents the type specifier for that variable
#'
#' @examples
#' ASTVariable$new("x", "2", "Continuous")$string()
#'
ASTVariable <- R6Class("ASTVariable",
  inherit = ASTNode,
  public  = list(
    format = "character",
    type   = "character",
    initialize = function(identifier, format, type)
    {
      self$value  <- identifier
      self$format <- format
      self$type   <- type
    },
    terms = function() {return(self$value)},
    string   = function()
    {
      fmt <- ""
      typ <- ""  
      if(!is.na(self$format)) {fmt <- paste("[",self$format,"]",sep='')}
      if(!is.na(self$type))   {typ <- paste("::",self$type,sep='')}
      paste(self$value, fmt, typ, sep="")
    }
  )
)

#' A left/right branch in an Abstract Syntrax Tree. This inherits from ASTNode, and
#' is intended to be a base class as well. Should never be instantiated directly
#' as once again the semantic information is contained in the class name.
#'
#' @field left A pointer to the left node below this one
#' @field right A pointer to the right node below this one
#'
#' @examples
#' ASTBranch$new(ASTNode$new(""), ASTNode$new(""))
#'
ASTBranch <- R6Class("ASTBranch",
  inherit = ASTNode,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    distribute = function()
    {
      "The distribute function does a depth first call, reassigning pointers from the result of the distribute function."
      if(inherits(self$left,  "ASTNode"))
      {
        self$left <- self$left$distribute()
      }
      if(inherits(self$right, "ASTNode"))
      { 
        self$right <- self$right$distribute()
      }
      
      return(self)
    }
  )
)

#' A requested function call.
#'
#' @field value  A string containing the function name.
#' @field r_expr A string containing the raw r expression.
#'
#' @examples
#' ASTBranch$new(ASTNode$new(""), ASTNode$new(""))
#'
ASTFunction <- R6Class("ASTFunction",
  inherit = ASTNode,
  public   = list(
    r_expr = "character",
    initialize = function(value, r_expr)
    {
      self$value  <- value
      self$r_expr <- r_expr
    },
    string = function()
    { 
      paste(self$value, "(", self$r_expr, ")", sep="")
    }
  )
)

ASTPlus <- R6Class("ASTPlus",
  inherit = ASTBranch,
  public = list (
    left = "ASTNode",
    right = "ASTNode",
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$value  <- ""
    },
    terms = function()
    {
      return(c(self$left$terms(), self$right$terms()))
    },
    string = function()
    { 
      paste(self$left$string(), "+", self$right$string(), sep="")
    }
  )
)

ASTMultiply <- R6Class("ASTMultiply",
  inherit = ASTBranch,
  public = list (
    left  = "ASTNode",
    right = "ASTNode",
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$value  <- ""
    },
    distribute = function()
    {
      super$distribute() # Depth-first is required, allows lower changes to bubble up
      if(inherits(self$left, "ASTPlus"))
      {
        return(ASTPlus$new(
          ASTMultiply$new(self$left$left , self$right$clone())$distribute(),
          ASTMultiply$new(self$left$right, self$right        )$distribute()
        ))
      }
      if(inherits(self$right, "ASTPlus"))
      {
        return(ASTPlus$new(
          ASTMultiply$new(self$left$clone(), self$right$left )$distribute(),
          ASTMultiply$new(self$left,         self$right$right)$distribute()
        ))
      }
      return(self)
    },
    string = function()
    { 
      paste(self$left$string(), "*", self$right$string(), sep="")
    }
  )
)

# The top level table
ASTTableFormula <- R6Class("ASTTableFormula",
  inherit = ASTBranch,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$value  <- ""
    },
    terms = function()
    {
      list(self$left$terms(), self$right$terms())
    },
    string = function()
    { 
      paste(self$left$string(), " ~ ", self$right$string(), sep="")
    }
  )
)

# A token in the grammar
# Composed of an id (e.g. "PLUS", "LPAREN")
# and a name, the actual contents of it, in case of long tokens
Token <- R6Class("Token",
  public = list(
    id         = "character",
    name       = "character",
    initialize = function(id, name="")
    {
      self$id   <- id
      self$name <- name
#cat("Token[",id,",",name,"]\n")
    })
)

# The parser for the table formulas
Parser <- R6Class("Parser",
  public  = list(
    input = "character",
    pos   = "numeric",
    len   = "numeric",
    initialize = function()
    {
    },
    # Expect is a function that requires the next term in the grammar to be the given id
    expect = function(id)
    {
      t <- self$nextToken()
      if(t$id != id)
      {
        stop(paste("Expecting",id,"before '",substr(self$input,self$pos,self$len),"'",sep=""))
      }

      t
    },
    # Peek returns the next token id in the input without consuming it
    peek = function()
    {
#cat("peeking at...")
       nt       <- self$nextToken()
       self$pos <- self$pos - nchar(nt$name) # Push the token back
       return(nt$id)
    },
    eat_whitespace = function()
    {
      while(substr(self$input, self$pos, self$pos) %in% c(" ","\t") &&
            self$pos < self$len)
      {
        self$pos = self$pos + 1
      }
    },
    # Next Token, returns and consumes the next lexical token in the input stream
    nextToken = function()
    {
      self$eat_whitespace()

      # The end?
      if (self$pos == (self$len+1)) {return(Token$new("EOF"))}
      # The parser kept asking for tokens when it shouldn't have
      if (self$pos > self$len)    { stop("Internal Error. No remaining input") }

      x <- substr(self$input, self$pos, self$pos)
      self$pos <- self$pos + 1

      # Look for reserved characters
      if (x == '*')  {return(Token$new("TIMES",   "*") )}
      if (x == '+')  {return(Token$new("PLUS",    "+") )}
      if (x == '(')  {return(Token$new("LPAREN",  "(") )}
      if (x == ')')  {return(Token$new("RPAREN",  ")") )}
      if (x == '~')  {return(Token$new("TILDE",   "~") )}
      if (x == ':')  {return(Token$new("COLON",   ":") )}
      if (x == '[')  {return(Token$new("LBRACKET","[") )}
      if (x == ']')  {return(Token$new("RBRACKET","]") )}

      # Scan for Name
      #   A syntactically valid name consists of letters, numbers and the dot
      #   or underline characters and starts with a letter or the dot not
      #   followed by a number.
      match <- str_match(substr(self$input,self$pos-1,self$len),
                         "^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9\\._]*")
#cat("Match[1,1]=",match[1,1],"\n")
      if(is.na(match[1,1]))
      {
        stop(paste("Unparseable input starting at",substr(self$input,self$pos-1,self$pos+10),sep=""))
      }

      self$pos <- self$pos + nchar(match[1,1]) - 1

      return(Token$new("IDENTIFIER", match[1,1]))
    },
    format = function()
    {
      match <- str_match(substr(self$input, self$pos, self$len), "[^\\]]*")
      starting <- self$pos
      self$pos <- self$pos + nchar(match[1,1])

      return(match[1,1])
    },
    r_expression = function()
    {
      match <- str_match(substr(self$input, self$pos, self$len), "^[^\\(\\)]*")
      starting <- self$pos
      self$pos <- self$pos + nchar(match[1,1])
      # Didn't call tokenizer for peek, due to different grammar of R expressions
      c <- substr(self$input, self$pos, self$pos)
      if (c == "(" )
      {
        self$pos <- self$pos + 1 # Eat that character
        rexpr <- self$r_expression()
        self$expect("RPAREN")
        rexpr <- self$r_expression() # Continue the r_expr
        return(substr(self$input, starting, self$pos-1))
      }

      return(substr(self$input, starting, self$pos-1))
    },
    factor = function()
    {
      nt <- self$nextToken()
      if(nt$id == "LPAREN")
      {
        expr <- self$expression()
        self$expect("RPAREN")
        return(expr)
      }
      if(nt$id != "IDENTIFIER") # An factor starts with either an identifier or a '('
      {
        stop(paste("Unrecognized token",nt$name,"before",substr(self$input,self$pos,self$len)))
      }

      pk <- self$peek() # What follows the name determines next grammar term

      # function-name -- with r-expression
      if(pk == "LPAREN")
      {
        self$expect("LPAREN")
        r_expr <- self$r_expression()
        self$expect("RPAREN")
        return(ASTFunction$new(nt$name, r_expr))
      }

      # Only valid thing left is a variable, check for additional specifiers on variable
      format <- NA
      if(pk == "LBRACKET")
      {
        self$expect("LBRACKET")
        format <- self$format()
        self$expect("RBRACKET")
        pk <- self$peek()
      }

      type_override <- NA
      if(pk == "COLON")
      {
        self$expect("COLON")
        self$expect("COLON")
        nt2 <- self$nextToken()
        if(nt2$id != "IDENTIFIER") # Type override must be an identifier
        {
          stop(paste("Unrecognized token",nt$name,"before",substr(self$input,self$pos,self$len)))
        }

        type_override <- nt2$name
      }
      return(ASTVariable$new(nt$name, format, type_override))

    },
    term = function()
    {
      l_term <- self$factor()
      if(self$peek() == "TIMES")
      {
        self$expect("TIMES")
        r_term <- self$term()
        return(ASTMultiply$new(l_term, r_term))
      }

      return(l_term)
    },
    expression = function()
    {
      l_expr  <- self$term()
      if(self$peek() == "PLUS")
      {
        self$expect("PLUS")
        r_expr <- self$expression()
        return(ASTPlus$new(l_expr, r_expr))
      }

      return(l_expr)
    },
    tableFormula = function()
    {
      cs <- self$expression()
      self$expect("TILDE")
      rs <- self$expression()

      return(ASTTableFormula$new(cs, rs))
    },
    run       = function(x)
    {
      if(class(x) == "formula")
      {
        y <- as.character(x)
        x <- paste(x[2], x[1], x[3])
      }
      self$input <- x
      self$pos   <- 1
      self$len   <- nchar(self$input)

      tf <- self$tableFormula()
      self$expect("EOF")
      return(tf$distribute())
    }
  )
)
