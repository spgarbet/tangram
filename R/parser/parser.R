library(stringr)
library(R6)
library(RUnit)

# A terminal node in the Abstract Syntax Tree
ASTNode <- R6Class("ASTNode",
  public = list(
    symbol = "character",
    value  = "character",
    initialize = function(symbol, value) 
    {
      self$symbol <- symbol
      self$value  <- value
    },
    elements = function() {return(self$value)}
  )
)

# A branch node in the Abstract Syntax Tree, may contain a value
ASTBranch <- R6Class("ASTBranch",
  inherit = ASTNode,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    initialize = function(symbol, left, right, value="")
    {
      self$symbol <- symbol
      self$left   <- left
      self$right  <- right
      self$value  <- value
    },
    elements = function()
    {
      if(self$symbol == "plus") 
      {
        return(c(self$left$elements(), self$right$elements()))
      }
      
      return(self$name()) 
    },
    name = function() {return (self$value)} # What to call other stuff?
  )
)

# A branch node in the Abstract Syntax Tree, may contain a value
ASTTable <- R6Class("ASTTable",
  inherit = ASTBranch,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    initialize = function(left, right)
    {
      self$symbol <- "table"
      self$left   <- left
      self$right  <- right
      self$value  <- ""
    },
    elements = function()
    {
      list(self$left$elements(), self$right$elements())
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
    # Expect is a function that requires the next element in the grammar to be the given id
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
    # Next Token, returns and consumes the next lexical token in the input stream
    nextToken = function()
    {
      # The end?
      if (self$pos == (self$len+1)) {return(Token$new("EOF"))}
      # The parser kept asking for tokens when it shouldn't have
      if (self$pos > self$len)    { stop("Internal Error. No remaining input") }
  
      x <- substr(self$input, self$pos, self$pos)
      self$pos <- self$pos + 1
  
      # Look for reserved characters
      if (x == '*')  {return(Token$new("TIMES",  "*") )}
      if (x == '+')  {return(Token$new("PLUS",   "+") )}
      if (x == '(')  {return(Token$new("LPAREN", "(") )}
      if (x == ')')  {return(Token$new("RPAREN", ")") )}
      if (x == '~')  {return(Token$new("TILDE",  "~") )}
  
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
  
      return(Token$new("NAME", match[1,1]))
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
        return(ASTNode$new("r_expr", substr(self$input, starting, self$pos-1)))
      }

      return(ASTNode$new("r_expr", substr(self$input, starting, self$pos-1)))
    },
    expression = function()
    {
      nt <- self$nextToken()
      if(nt$id == "LPAREN")
      {
        tf <- self$tableFormula()
        self$expect("RPAREN")
        return(tf)
      }
      if(nt$id != "NAME") # An expression starts with either a name or a '('
      {
        stop(paste("Unrecognized token",nt$name,"before",substr(self$input,self$pos,self$len)))
      }
      if(nt$name == "I") # R-expression
      {
        self$expect("LPAREN")
        r_expr <- self$r_expression()
        self$expect("RPAREN") 
        return(r_expr)
      }
      pk <- self$peek() # What follows the name determines next grammar element
      if(pk == "TIMES")
      {
        self$expect("TIMES")
        expr <- self$expression()
        return(ASTBranch$new("permute", ASTNode$new("name", nt$name), expr))
      }
      if(pk == "LPAREN")
      {
        self$expect("LPAREN")
        expr <- self$expression()
        self$expect("RPAREN")
        return(ASTBranch$new("transform", expr, NA, nt$name))
      }
      # Else it's just a name
      return(ASTNode$new("name",nt$name))
    },
    formula = function()
    {
      l_expr  <- self$expression()
      if(self$peek() == "PLUS")
      {
        self$expect("PLUS")
        r_expr <- self$formula()
        return(ASTBranch$new("plus", l_expr, r_expr))
      }

      return(l_expr)   
    },
    columnSpecification = function() {
      self$formula()
    },
    rowSpecification    = function() {
      self$formula()
    },
    tableFormula = function()
    {
      cs <- self$columnSpecification()      
      self$expect("TILDE") 
      rs <- self$rowSpecification()
      
      return(ASTTable$new(cs, rs))
    },
    run       = function(x)
    {
      if(class(x) == "formula")
      {
        y <- as.character(x)
        x <- paste(x[2], x[1], x[3])
      }
      self$input <- str_replace_all(x, "[[:space:]]", "")
      self$pos   <- 1    
      self$len   <- nchar(self$input)
   
      tf <- self$tableFormula()
      self$expect("EOF")
      return(tf)
    }
  )
)
