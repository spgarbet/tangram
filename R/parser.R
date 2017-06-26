# tangram a general purpose table toolkit for R
# Copyright (C) 2017 Shawn Garbett
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' A Node in an Abstract Syntax Tree (AST)
#'
#' This is the root R6 class of any term of the AST which is created
#' when parsing a table formula. This should only be used as a base class
#' as the class information carries the semantic meaning of a given node.
#'
#' @docType class
#' @importFrom stringr str_match
#' @importFrom R6 R6Class
#' @keywords data
#'
#' @field symbol A string which tells what this node in the AST represents.
#' @field value  A string of addtional information contained by the node.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{terms()}}{Returns the node itself}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node.}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTNode <- R6Class("ASTNode",
  public = list(
    value  = "character",
    terms      = function()     { return(c(self))    },
    distribute = function()     { return(self)       },
    string     = function()     { return(self$value) },
    reduce     = function(data) { return(self)       }
  )
)

#' A Variable in an Abstract Syntax Tree (AST)
#'
#' This node represents a variable of interest in the AST. A variable's name
#' is recorded in the value field, and must conform to the rules of identifiers
#' in R. This class inherits from \code{\link{ASTNode}}.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @export
#' @format \code{\link{R6Class}} object.
#'
#' @examples
#' ASTVariable$new("x", "2", "Continuous")$string()
#'
#' @field value  A string containing the variable identifier
#' @field format A format string that is either a string containing a number representing significant digits for output, or a C-style printf string.
#' @field type A string that represents the type specifier for that variable
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(identifier, format=NA, type=NA)}}{This method creates an AST node representing a variable of a given identifier. An optional format consisting of a string of a number or a c-style printf string. An option type denoting a forced type cast of that variable.}
#'   \item{\code{terms()}}{Returns the node}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node.}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{name()}}{Return a human representation of a node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTVariable <- R6Class("ASTVariable",
  inherit = ASTNode,
  public  = list(
    format = "character",
    type   = "character",
    data   = NULL,
    initialize = function(identifier, format=NA, type=NA)
    {
      self$value  <- identifier
      self$format <- format
      self$type   <- type
      self$data   <- NULL
    },
    factors  = function()     { return(c(self))    },
    name     = function()
    {
      if(self$value=="1") "All" else {
        x <- NULL
        try({
          x <- attr(self$data, "label")
        })
        if(is.null(x)) self$value else x
      }
    },
    string   = function()
    {
      fmt <- ""
      typ <- ""
      if(!is.na(self$format)) {fmt <- paste("[",self$format,"]",sep='')}
      if(!is.na(self$type))   {typ <- paste("::",self$type,sep='')}
      paste(self$value, fmt, typ, sep="")
    },
    reduce   = function(d)
    {
      if(self$value == "1")
      {
        self$data  <- factor(rep(1, length(d[,1])), labels="All")
        attr(self$data, "label") <- "All"
        return(self)
      }
      self$data <- d[,self$value]
      self
    }
  )
)

#' A left/right branch in an Abstract Syntrax Tree. This inherits from ASTNode, and
#' is intended to be a base class as well. Should never be instantiated directly
#' as once again the semantic information is contained in the class name.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @format \code{\link{R6Class}} object.
#' @field left A pointer to the left node below this one
#' @field right A pointer to the right node below this one
#'
#' @section Methods:
#' \describe{
#'   \item{\code{distribute()}}{Depth first application of distribute() to left and right nodes, then modifies left and right and returns self.}
#'   \item{\code{terms()}}{Returns the node}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTBranch <- R6Class("ASTBranch",
  inherit = ASTNode,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    distribute = function()
    {
      if(inherits(self$left,  "ASTNode"))
      {
        self$left  <- self$left$distribute()
      }
      if(inherits(self$right, "ASTNode"))
      {
        self$right <- self$right$distribute()
      }

      return(self)
    },
    reduce = function(df)
    {
      self$left  <- self$left$reduce(df)
      self$right <- self$right$reduce(df)

      self
    }
  )
)

#' A specified function call as an ASTNode
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @export
#' @format \code{\link{R6Class}} object.
#'
#' @field value  The name of the function.
#' @field r_expr A string containing the raw r expression from inside the parenthesis
#'
#' @examples
#' ASTFunction$new("log", "x+2")$string()
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(value, r_expr)}}{Create one with the given value and r_expr}
#'   \item{\code{terms()}}{Returns the node}
#'   \item{\code{factors()}}{Returns self as a factor}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node.}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTFunction <- R6Class("ASTFunction",
  inherit = ASTNode,
  public   = list(
    r_expr = "character",
    data   = NULL,
    initialize = function(value, r_expr)
    {
      self$value  <- value
      self$r_expr <- r_expr
    },
    factors    = function()     { return(c(self))    },
    name       = function()     { paste(self$value, "(", self$r_expr, ")", sep="") },
    string     = function()     { paste(self$value, "(", self$r_expr, ")", sep="") },
    reduce = function(data)
    {
      expr <- paste(self$value,"(",self$r_expr,")", sep='')
      x <- eval(parse(text=paste("with(data,",expr,")",sep='')))
      if(inherits(x, "ASTNode")) {return(x)}

      name <- expr
      try({
        l2 <- attr(x, "label")
        if(!is.null(l2))
        {
          name <- paste(self$value,"(",l2,")",sep='')
        }
      })

      var <- ASTVariable$new(name)
      var$data <- x
      var
    }
  )
)

#' The addition of two terms, in an ASTNode.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @export
#' @format \code{\link{R6Class}} object.
#'
#' @field left  The AST tree to the left.
#' @field right The AST tree to the right.
#'
#' @examples
#' ASTPlus$new(ASTVariable$new("x"), ASTVariable$new("y"))$string()
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(left, right)}}{Create addition node of given left and right node.}
#'   \item{\code{terms()}}{Returns the left and right branches terms}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node.}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTPlus <- R6Class("ASTPlus",
  inherit = ASTBranch,
  public  = list (
    left   = "ASTNode",
    right  = "ASTNode",
    data   = "ASTPlus",
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

#' The multiplication of two terms, as an ASTNode.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @export
#' @format \code{\link{R6Class}} object.
#'
#' @field left  The AST tree to the left.
#' @field right The AST tree to the right.
#'
#' @examples
#' ASTMultiply$new(ASTVariable$new("x"), ASTVariable$new("y"))$string()
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(left, right)}}{Create addition node of given left and right node.}
#'   \item{\code{terms()}}{Returns the node as a term vector}
#'   \item{\code{factors()}}{Returns all terminal nodes under this as a list}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node. This is the actual workhorse of the disributing multiplication across the tree.}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTMultiply <- R6Class("ASTMultiply",
  inherit = ASTBranch,
  public = list (
    left  = "ASTNode",
    right = "ASTNode",
    type   = "character",
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$type   <- "ASTMultiply"
      self$value  <- ""
    },
    distribute = function()
    {
      super$distribute()
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
    factors = function()
    {
      return(c(self$left$terms(), self$right$terms()))
    },
    string = function()
    {
      paste(self$left$string(), "*", self$right$string(), sep="")
    }
  )
)

#' The root ASTNode of a formula.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @export
#' @format \code{\link{R6Class}} object.
#'
#' @field left  The AST tree for the columns.
#' @field right The AST tree for the rows.
#'
#' @examples
#' ASTTableFormula$new(ASTVariable$new("x"), ASTVariable$new("y"))$string()
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(left, right)}}{Create addition node of given left and right node.}
#'   \item{\code{terms()}}{Returns the a list of the left hand terms and right hand terms}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node. This is the actual workhorse of the disributing multiplication across the tree.}
#'   \item{\code{string()}}{Returns the string representation of the formula}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the entire AST.}
#' }
ASTTableFormula <- R6Class("ASTTableFormula",
  inherit = ASTBranch,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$value  <- NA
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

#' A token in the formula grammar
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @format \code{\link{R6Class}} object.
#'
#' @field id    The token identifier, E.g. "LPAREN"
#' @field name  Information about the token, useful with IDENTIFIERs.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id, name="")}}{Create a new token in the grammar.}
#' }
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

#' The parser class for generating abstract syntax trees for given table formulas.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @keywords data
#' @export
#' @format \code{\link{R6Class}} object.
#'
#' @field input Storage for input string of a formula
#' @field pos   The current parsing position
#' @field len   The length of the input
#'
#' @examples
#' Parser$new()$run("col1 + col2 + col3 ~ drug*age+spiders")
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Create a parser.}
#'   \item{\code{expect(id)}}{Require the next token parsed to have the specified id and consume it.}
#'   \item{\code{peek()}}{Return the next token parsed without consuming it.}
#'   \item{\code{eat_whitespace()}}{Consume any spaces or tabs in input. }
#'   \item{\code{next_token()}}{Return the next token parsed and consume it.}
#'   \item{\code{format()}}{ Parse a format class and return it's string.}
#'   \item{\code{r_expression()}}{Parse an R expression class and return it's string. }
#'   \item{\code{factor()}}{Parse a factor class and return it's AST Node.}
#'   \item{\code{term()}}{Parse a term class and return it's AST Node.}
#'   \item{\code{expression()}}{Parse an expression class and return it's AST Node.}
#'   \item{\code{table_formula()}}{Parse a table formula class and return it's AST Node.}
#'   \item{\code{run(input)}}{Run the parser on the given input, and return an AST}
#' }
#' @section References:
#' \describe{
#'      Aho, A. V., Lam, M. S., Sethi, R., and Ullman, J. D. (2006) \emph{Compilers: Principles, Techniques, and Tools}, 2nd edition. Addison Wesley.
#' }
Parser <- R6Class("Parser",
  public  = list(
    input = "character",
    pos   = "numeric",
    len   = "numeric",
    initialize = function()
    {
    },
    expect = function(id)
    {
      t <- self$next_token()
      if(t$id != id)
      {
        stop(paste("Expecting",id,"before '",substr(self$input,self$pos,self$len),"'",sep=""))
      }

      t
    },
    peek = function()
    {
#cat("peeking at...")
       nt       <- self$next_token()
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
    next_token = function()
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

      # Scan for "1"
      if(substr(self$input, self$pos-1, self$pos-1) == "1")
      {
        return(Token$new("IDENTIFIER", "1"))
      }

      # Scan for Name
      #   A syntactically valid name consists of letters, numbers and the dot
      #   or underline characters and starts with a letter or the dot not
      #   followed by a number.
      match <- str_match(substr(self$input,self$pos-1,self$len),
                         "^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9\\._]*")
      if(is.na(match[1,1]))
      {
        stop(paste("Unparseable input starting at",substr(self$input,self$pos-1,self$pos+10),sep=""))
      }

      self$pos <- self$pos + nchar(match[1,1]) - 1

      return(Token$new("IDENTIFIER", match[1,1]))
    },
    format = function()
    {
      match <- str_match(substr(self$input, self$pos, self$len), "\"?([^\\]\"]*)\"?")
      starting <- self$pos
      self$pos <- self$pos + nchar(match[1,1])

      return(match[1,2])
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
      nt <- self$next_token()
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
      type_override <- NA
      if(pk == "COLON")
      {
        self$expect("COLON")
        self$expect("COLON")
        nt2 <- self$next_token()
        if(nt2$id != "IDENTIFIER") # Type override must be an identifier
        {
          stop(paste("Unrecognized token",nt$name,"before",substr(self$input,self$pos,self$len)))
        }

        type_override <- nt2$name
        pk <- self$peek() # Refresh peek ahead
      }

      format <- NA
      if(pk == "LBRACKET")
      {
        self$expect("LBRACKET")
        format <- self$format()
        self$expect("RBRACKET")
        pk <- self$peek()
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
    table_formula = function()
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

      tf <- self$table_formula()
      self$expect("EOF")
      return(tf)
    }
  )
)
