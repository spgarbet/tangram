# tangram a general purpose table toolkit for R
# Copyright (C) 2017-2018 Shawn Garbett
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
#' @field format Any formatting directive passed to this node.
#' @field value  A string of addtional information contained by the node.
#'
ASTNode <- R6Class("ASTNode",
  public = list(
    value      = "character",
    format     = "character",
    #' @description Returns this node
    terms      = function()     { return(c(self))    },
    #' @description Distributes data across multiplications and rearranges nodes
    distribute = function()     { return(self)       },
    #' @description Returns string representation of node
    string     = function()     { return(self$value) },
    #' @description Given a set of data, associates it with AST nodes
    #' @param data (data.frame) data to associate across nodes
    reduce     = function(data) { return(self)       },
    #' @description Override the formatting directive for this node
    #' @param x (numeric,character) the formatting directive
    set_format = function(x)    { self$format <- x   }
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
#' @field data The associated data post reduction
#' @field type The identified type of this node (defaults: Categorical, Numeric)
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(identifier, format=NA, type=NA)}}{}
#'   \item{\code{terms()}}{Returns the node}
#'   \item{\code{distribute()}}{Applies the distributive property to the node, and returns the resulting node.}
#'   \item{\code{string()}}{Returns the string formula of the node}
#'   \item{\code{name()}}{Return a human representation of a node}
#'   \item{\code{reduce(data)}}{Given a set of data, perform the logical reduction of the current node.}
#' }
ASTVariable <- R6Class("ASTVariable",
  inherit = ASTNode,
  public  = list(
    type   = "character",
    data   = NULL,

    #' @description This method creates an AST node representing a variable of a given identifier. An optional format consisting of a string of a number or a c-style printf string. An option type denoting a forced type cast of that variable.
    #' @param identifier (character) Variable name
    #' @param format (character, numeric) Formatting directive
    #' @param type (character) any additional type information
    initialize = function(identifier, format=NA, type=NA)
    {
      self$value  <- identifier
      self$format <- format
      self$type   <- type
      self$data   <- NULL
    },
    #' @description Returns all terminal nodes under this. Since this is a terminal node, returns self
    factors  = function()     { return(c(self))    },
    #' @description Returns the text name of this node. For an intercept, returns "All"
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
    #' @description Returns name of variable with optional format and type information
    string   = function()
    {
      fmt <- ""
      typ <- ""
      if(!is.na(self$format)) {fmt <- paste("[",self$format,"]",sep='')}
      if(!is.na(self$type))   {typ <- paste("::",self$type,sep='')}
      paste(self$value, fmt, typ, sep="")
    },
    #' @description Given a data.frame, associates correct variable with this node
    #' @param d (data.frame) data.frame to reduce
    reduce   = function(d)
    {
      if(!(self$value=="1") && (is.null(d) || !(self$value %in% names(d))))
      {
        self$data <- get(self$value) # Pull from current environment
        if(is.null(self$data)) stop(paste(self$value, "not found in supplied data or environment"))
        if(is.null(attr(self$data, "label"))) attr(self$data, "label") <- self$value
        return(self)
      }

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
ASTBranch <- R6Class("ASTBranch",
  inherit = ASTNode,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    #' @description Call to distribute multiplication nodes, just recursively calls left and right node distribute functions
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
    #' @description Attached data to nodes by processing data.frame appropriatly. Recursively calls left and right nodes to reduces on data.frame
    #' @param df (data.frame) Data frame to reduce over
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
#' @field r_expr A string containing the raw r expression from inside the parenthesis
#' @field data Data stored as a result of reduction
#'
#' @examples
#' ASTFunction$new("log", "x+2")$string()
#'
ASTFunction <- R6Class("ASTFunction",
  inherit = ASTNode,
  public   = list(
    r_expr = "character",
    data   = NULL,
    #' @description Construct a node representing a function call
    #' @param value (character) The name of the function call
    #' @param r_expr Any r expression to be evaluated inside the call
    initialize = function(value, r_expr)
    {
      self$value  <- value
      self$r_expr <- r_expr
    },
    #' @description Returns all terminal nodes, this is a terminal node so returns self
    factors    = function()     { return(c(self))    },
    #' @description Returns the function call as character
    name       = function()     { paste(self$value, "(", self$r_expr, ")", sep="") },
    #' @description Returns a re-parsable representation of the node
    string     = function()     { paste(self$value, "(", self$r_expr, ")", sep="") },
    #' @description Given a data.frame execute the function in that environment and associate the result as data.
    #' @param data (data.frame) The data.frame to use as the enviroment for the function execution
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
#' @field data Just returns the R6 name 'ASTPlus'
#' @field left The node to the left of this node
#' @field right The node to the right of this node
#'
#' @examples
#' ASTPlus$new(ASTVariable$new("x"), ASTVariable$new("y"))$string()
#'
ASTPlus <- R6Class("ASTPlus",
  inherit = ASTBranch,
  public  = list (
    left   = "ASTNode",
    right  = "ASTNode",
    data   = "ASTPlus",
    #' @description Construct a new node that represents addition
    #' @param left (ASTNode) Node on the left side of the addition
    #' @param right (ASTNode) Node on the right side of the addition
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$value  <- ""
    },
    #' @description Returns a vector of the left and right terms
    terms = function()
    {
      return(c(self$left$terms(), self$right$terms()))
    },
    #' @description A reparsable string representation of this node.
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
#' @field type The specified type of this node
#'
#' @examples
#' ASTMultiply$new(ASTVariable$new("x"), ASTVariable$new("y"))$string()
#'
ASTMultiply <- R6Class("ASTMultiply",
  inherit = ASTBranch,
  public = list (
    left  = "ASTNode",
    right = "ASTNode",
    type   = "character",
    #' @description Construct a multiplication node
    #' @param left (ASTNode) nodes to the left of the multiplication
    #' @param right (ASTNode) nodes to the right of the multiplication
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$type   <- "ASTMultiply"
      self$value  <- ""
    },
    #' @description Rearrange nodes distribution multiplication across parenthesis
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
    #' @description return all terminal nodes on left and right
    factors = function()
    {
      return(c(self$left$terms(), self$right$terms()))
    },
    #' @description Return a re-parseable string
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
ASTTableFormula <- R6Class("ASTTableFormula",
  inherit = ASTBranch,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    #' @description Create a new formula node
    #' @param left The left side of the "~" as an AST
    #' @param right The right side of the "~" as an AST
    initialize = function(left, right)
    {
      self$left   <- left
      self$right  <- right
      self$value  <- NA
    },
    #' @description Returns all terminal nodes from left and right
    terms = function()
    {
      list(self$left$terms(), self$right$terms())
    },
    #' @description A re-parseable string representing the AST
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
Token <- R6Class("Token",
  public = list(
    id         = "character",
    name       = "character",
    #' @description Construct a lexical token
    #' @param id (character) The lexical id of the token
    #' @param name (character) Additional token information if needed
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
#' @section References:
#' \describe{
#'      Aho, A. V., Lam, M. S., Sethi, R., and Ullman, J. D. (2006) \emph{Compilers: Principles, Techniques, and Tools}, 2nd edition. Addison Wesley.
#' }
Parser <- R6Class("Parser",
  public  = list(
    input = "character",
    pos   = "numeric",
    len   = "numeric",
    #' @description Create a parser
    initialize = function()
    {
    },
    #' @description Specify expectation of next token from lexer
    #' @param id The token id expected in stream, otherwise it's an error
    expect = function(id)
    {
      t <- self$next_token()
      if(t$id != id)
      {
        stop(paste("Expecting",id,"before '",substr(self$input,self$pos,self$len),"'",sep=""))
      }

      t
    },
    #' @description Peek at the next token from parser
    peek = function()
    {
#cat("peeking at...")
       nt       <- self$next_token()
       self$pos <- self$pos - nchar(nt$name) # Push the token back
       return(nt$id)
    },
    #' @description Remove white space to find start of next token
    eat_whitespace = function()
    {
      while(substr(self$input, self$pos, self$pos) %in% c(" ","\t","\n","\r") &&
            self$pos < self$len)
      {
        self$pos = self$pos + 1
      }
    },
    #' @description Returns next lexical token
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
      #   followed by a number, or is contained within backticks.
      match <- str_match(substr(self$input,self$pos-1,self$len),
        "^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9\\._]*|^\\`[^\\`]+\\`")
      if(is.na(match[1,1]))
      {
        browser()
        stop(paste("Unparseable input starting at",substr(self$input,self$pos-1,self$pos+10),sep=""))
      }

      self$pos <- self$pos + nchar(match[1,1]) - 1

      if(substr(match[1,1], 1, 1) == "`") match[1,1] <- substr(match[1,1], 2, nchar(match[1,1])-1)

      return(Token$new("IDENTIFIER", match[1,1]))
    },
    #' @description Return format string as token from lexical stream
    format = function()
    {
      match <- str_match(substr(self$input, self$pos, self$len), "\"?([^\\]\"]*)\"?")
      starting <- self$pos
      self$pos <- self$pos + nchar(match[1,1])

      return(match[1,2])
    },
    #' @description Return R expression as token from lexical stream
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
    #' @description Return next factor as token.
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
    #' @description Parse and return next term in stream
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
    #' @description Parse and return next expression in stream
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
    #' @description Parse and return table formula from stream
    table_formula = function()
    {
      cs <- self$expression()
      self$expect("TILDE")
      rs <- self$expression()

      return(ASTTableFormula$new(cs, rs))
    },
    #' @description Run the parser
    #' @param x (character,formula) The table specification to parse
    run       = function(x)
    {
      if(inherits(x,"formula"))
      {
        x <- as.character(x)
        x <- paste0(c(x[2], x[1], x[3]), collapse='')
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
