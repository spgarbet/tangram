# A Grammar of Tables 'tangram'

## Release Notes
Jun 4 2018  v0.4 Numerous bug fixes. The package was used for DSMB report submissions to the FDA, and several examples of IRR and other wonderful things have been produced. The full shakedown is complete, and fixes and updates are becoming smaller and smaller as the package stabilizes. LaTeX is a workable render format. It has rtf output that works acceptibly. Work continues towards formatted output in Word, with traceability.

Jun 27 2017 v0.3 A major refactor, cells in the table object are no longer 'special' and are just straight S3 objects. Two adapter layers of code are deleted, interface is stablizing. Used `tangram` as an S3 object and replaced various table generating calls to a single overloaded function.

Sep 27 2017 v0.3.2 Multiple bug fixes from earlier refactor. Added support for LaTeX. A new example from FDA work.

## Goal

The idea of creating a quick summary of a data set has been around a good while. The use of a statistical formulas to create summaries exists in SAS in PROC REPORT, and in the R package Hmisc. The SAS has a rich syntax which allows for generation of a wide array of summary tables, but is limited to a subset of SAS functions. The SAS generation is further limited to a fairly crude appearing table, with limited options for output generation. Hmisc offers wonderful output, but is fixed in the analysis that can be performed.

This project intends to create a table grammar that is simple to use, while providing ultimate freedom to the end user when generating summary tables from data sets. This project contains the reference implementation in the language R, but is not limited to R.

For an example using Rmarkdown, see [example.html](http://htmlpreview.github.io/?https://github.com/spgarbet/tg/blob/master/vignettes/example.html)


## General Outline
A formula, a data frame (spreadsheet), and a transform function input into the framework will output an abstract table, that can be rendered into text, LaTeX, Word, or HTML5. 

Formulas will be in the `Columns ~ Rows` syntax. 

A user supplies a set of data and a formula which produces a summary object. This summary object is then passed to a renderer which is responsible for the final production of a table in a target language. The user can alter labeling, variable type detection, output table data genertion or add or alter output format. Each concern of the pipeline is separated from other concerns.

For example, one may wish for summary tables which match the New England Journal of Medicine format in LaTeX. A provided bundle of table generation will create the desired analysis directly from the data, and allow for specifying a style to the rendered LaTeX. The same formula and data could be used for a statistical report inside a department and the Hmisc table generation could be selected. In the end, the user is no longer bound to any decision in the table summary chain, beyond the grammar, and is free to change at will--or contribute more target bundles to share with others. 


## High Level Requirements

1. It must render to LaTeX, Text, HTML5, RMarkdown, Index table.
2. It must allow for user override of any summary generation function.
3. It must allow for user override of any rendering function.
4. Determination of type/class of a statistical variable is user overrideable.
5. Control over labeling of variables must be user overrideable.
6. It must be easily extensible. I.e., any user overrides should require a minimum of fuss / syntax for the end user.
7. Index table must be user specified name based, and not numeric numbers.
8. Index table must be repeatible, and contain search information.
9. It should reproduce by default as much as possible Hmisc summaryM behaviors.
10. It must be algebraically well formed.

## Table 9 Example

_Statistical Tables and Plots using S and LaTeX_ by FE Harrell, has an example, *Table 9*, that will be used for demonstration.

```R
library(Hmisc)

getHdata(pbc)

table <- tangram(drug ~ bili + albumin + stage + protime + sex + age + spiders, data = pbc)

table
html5(table)
latex(table)
index(table)
```

## Grammar Definition

A formula consists of a column specification, a tilde "~" and a row-specification.

A specification is a combination of expressions with a "+" joining them. Note one can add more variables to either columns or rows in this manner.

An expression can be a variable name from the data, or a variable joined with an expression via the "\*" operator. 

a either variable name from the data, or a variable name joined with an expression.

    <table-formula>        ::= <expression> "~" <expression>
    <expression>           ::= <term> "+" <expression> | <term>
    <term>                 ::= <factor> "\*" <term> | <factor>
    <factor>               ::= <data-name>                             |
                                "(" <expression>" ")"                  |
                                <function-name> "(" <r-expression> ")" |

The operators + and \* are distributive, i.e. term1 \* (term2 + term3) == term1 \* term2 + term1 \* term3

The operators are *not* commutative, i.e. term1 + term2 =/= term2 + term1

The operators are associative. 

Thus this grammar loosely corresponds to a noncommutative ring (+, \*), which is non-albelian, a monoid under multiplication, and is distributive. It is not a true ring, in that elements once reduced do not appear back in the set operated on, as the grammar is describing a final product that is non-reducible, the final table.

If a function is encountered, this is executed and expected to return a variable with a label that is useable in generating summaries.

A parser creates an abstract syntax tree of the formula. It will apply any distributive requests to requested variables. Functions will create additional data, by passing in the current dataframe and executing the command.

This concludes the syntax phase of compiling a table. The next phase is where semantic meaning of the formula is created.

## Statistical Analysis and Summary

The user now has the choice what semantic content is desired for constructing the statistical summary. One might appreciate the default summary statistics and asthetic layout of Hmisc. One might want to generate data ready for the New England Journal of Medicine or it might just be statistics about a particular model that is central to the idea.

At this point, if statistical *p*-values are to be used a table should have a consistent viewpoint of what the null hypothesis is. A consistent viewpoint is essentially to a readers understanding the collection of information being presented. For example, Hmisc takes the viewpoint that the null hypothesis is indepedence of variables between row and column. Thus the table is exploring what possible relationships exist, and giving the reader a feel for the ranges of the data. Then based on what data type a variable is an appropriate statistical test is chosen.

## Notes on Data Types

In preparing this reference implementation, it was discovered that there are some fundamental basic types of data in relationship to statistical operations. Unsurprisingly, most class or type definitions in languages represent the underlying machine storage format. This viewpoint of type is at odds with being able to succienctly define how table summaries are generated from provided data. A formal definition of the types of data available is required. However, the user of the library can freely change or amend the types provided and/or type determination.

The default of this library is similar choices as made by Hmisc. That is, a column of data in a data frame will be classified into one of the following types:

* Binomial
* Categorical
* Numerical

The consequence of supporting these types must be explored in terms of the algebraic operators. First of all a Categorical or Binomial will expand to be a number of columns or rows corresponding to their groups. Binomial is kept as a special case for handling dropping of one for terseness in expression. Numerical is used as is, and will only have a single row or column in correspondence with it's variable.

A `Categorical * Categorical` creates nested groups for consideration, which results in a categorical.

A `Numerical * Numerical` will just treat this as the numerical product of the two variables.

A `Categorical * Numerical`, or vice versa creates several numerical variables for consideration, filtered by the category they are in.

Please, note that this is not a constraint of the table grammar language, but simply compiler choices. One is free to consume the abstract syntax tree and make different decisions about the meaning of `Numerical * Numerical`, and for that matter how types are determined and what they are. *The important thing to remember is that all combinations of types be considered if writing a table compiler!*

### Hmisc Defaults

As mentioned default analysis bundle mimics Hmisc. An intersection occurs between variables defined on the columns and rows.

It performs a Chi^2 test for `Categorical X Categorical`. Each intersection of groups contains the overall fraction in that category.

The `Continuous X Numerical` intersection provides quantiles, and the results of a Kruskal–Wallis test.

Hmisc does not provide for a `Continuous X Continous` variable, but in remaining consistent with other tests a Spearman correlation test is provided.

## Design as a Table of Tables

Internally, a table consists of cells. A cell may be renderable, or it might be another table. An expansion function for flattening a table is used prerendering. 

This choice forces a consistency requirement upon any author of compiler packages for tables. The number of rows and columns that analysis generates must be consistent across types. For example, for the default Hmisc descriptive compiler, the following table shows how many cells (rows X columns) are generated when analysis is done between a row type and a column type:

|                        | Binomial      | Categorical (M values) |  Numerical     |
|------------------------|:-------------:|:----------------------:|:--------------:|
| Binomial               |       1 X 3   |          1 X (M + 2)   |       1 X 3    |
| Categorical (N values) |   (N+1) X 3   |      (N+1) X (M + 2)   |   (N+1) X 3    |
| Numerical              |       1 X 3   |          1 X (M + 2)   |       1 X 3    |

Note that the first term is consistent across each row, and the second term is consistent across each column. This insures that upon flattening that the number of rows and columns remain consistent.

*Note: Multicolumn and multirow formatting is on the todo list.*

## Full BNF of formula syntax

    <table-formula>        ::= <expression> "~" <expression> 
    <expression>           ::= <term> "+" <expression> | <term> 
    <term>                 ::= <factor> "\*" <term> | <factor> 
    <factor>               ::=  "(" <expression>" ")"                   |
                                <variable>                              |
                                <function-name> "(" <r-expression> ")"
    <function-name>        ::= <identifier>
    <variable>             ::= <identifier>
                                ( "::" <identifier> )
                                ( "["  ( <integer>  | '"' <format> '"' "]" )
    <identifier>           ::= 1 | ( [A-Za-z\_] | .[A-Za-z\_] ) [A-Za-z0-9\_.]+
     
     
    <format>               ::= "%" (<flags>) (<width>) (. <precision>) <specifier> 
    <flags>                ::= <flag> | <flag> <flags>
    <flag>                 ::=  "-" | "+" | " " | "#" | "0"
    <width>                ::= <integer>
    <precision>            ::= <integer>
    <specifier>            ::= [diuoxXfFeEgGaAcspn]
    
    <integer>              ::= [0-9]+
 
A variable identifier can specify desired resolution, and / or the type it should be treated as. For example: `albumin[2]::Numerical` specifies that albumin should be reported with 2 significant digits and treated as a Numerical variable. An alternate approach allows for sprintf specification, such as `albumin["%0.2g"]`

### Aside on Statistical Data Types

Instead of focusing on machine representation as type, what if statistical type were the focus? The following types I feel are more sensible and useful to real world measurements:

* Binomial
* Categorical
* Ordinal
* Count
* Integer
* Rational
* DateTime
* String
* Complex
* Vector of any of the above
* Matrix of any of the above

This information defines what operations and tests can be done on data far better than worrying about the number of bits in the storage format. The type could define exactly what tests are allowed on data.




