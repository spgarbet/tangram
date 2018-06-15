# A Grammar of Tables 'tangram'



<img src="https://upload.wikimedia.org/wikipedia/commons/c/cb/Tangram_set_00.jpg" width="100px"/>

Quick show me some really impressive results in Rmarkdown! See [example.html](http://htmlpreview.github.io/?https://github.com/spgarbet/tg/blob/master/vignettes/example.html)

<pre class="r"><code>tbl &lt;- tangram(&quot;drug ~ bili[2] + albumin + stage::Categorical[1] + protime + sex[1] + age + spiders[1]&quot;, 
              data=pbc,
              pformat = 5)
html5(tbl,
      fragment=TRUE,
      inline=&quot;lancet.css&quot;,
      caption = &quot;HTML5 Table Lancet Style&quot;, id=&quot;tbl4&quot;
      )</code></pre>

<div class="figure" id="tbl4"><style>

#tbl4 body .figbody {
	font-family: Corbel, "Lucida Grande", "Lucida Sans Unicode", "Lucida Sans", "DejaVu Sans", "Bitstream Vera Sans", "Liberation Sans", Verdana, "Verdana Ref", sans-serif;
  font-size: 66%;
  -webkit-print-color-adjust: exact;
}

#tbl4 .figure  {  margin-left:  auto;
			      margin-right: auto;
}
#tbl4 .caption { text-align:   left;
           text-indent: 2em;
	         padding: 0.5em 0 0.5em 0;
	         font-weight: bold;
	     }

#tbl4 .figbody {
	text-align:   center;
  border:       1px solid #c7496d;
}

#tbl4 .figbody table {
  margin: 0;
  width: 100%;
}

#tbl4 .figbody td {
	padding: 0.2em 0.5em 0.2em 0.5em;
}

#tbl4 .figbody thead
{
  border-top: 10px solid #f5e0dc;
  border-right: 10px solid #f5e0dc;
  border-left: 10px solid #f5e0dc;
  border-bottom: 1px solid black;
  background: #f5e0dc !important;
}


#tbl4 .figbody thead tr td
{
    font-weight: bold;
}

#tbl4 .figbody tbody
{
  border-bottom: 10px solid #f5e0dc;
  border-right: 10px solid #f5e0dc;
  border-left: 10px solid #f5e0dc;
}

#tbl4 .figbody tbody .odd {background: #f5e0dc !important;}
#tbl4 .figbody tbody .even {background: #ffffff;}

#tbl4 .header td {text-align: center;}

#tbl4 .subheader    { font-size: smaller; }
#tbl4 .subheader td { text-align: center; /* border-bottom: 1pt solid black; */ }

#tbl4 .subheader em {font-style: normal;}

#tbl4 .header.N:before { content: "(n="; }
#tbl4 .header.N:after  { content: ")"; }

#tbl4 .quantile {
  display: flex;
  flex-direction: row;
}
#tbl4 .quantile .q50 {
  order: 1;
  padding-right: 0.5em;
}
#tbl4 .quantile .q25:before {content: "(";}
#tbl4 .quantile .q25 {
  order: 2
}
#tbl4 .quantile .q75:before {content: ",";}
#tbl4 .quantile .q75 {order: 3}
#tbl4 .quantile .q75:after {content: ")";}
#tbl4 tbody .variable {
	float:      left;
	text-align: left;
}

#tbl4 .units {
	float:      right;
	font-size:  x-small;
	text-align: right;
	padding-left: 1em;
	vertical-align: text-bottom; /* FIXME why doesn't this work */
}

#tbl4 .fraction {
  display: flex;
  flex-direction: row;
}
#tbl4 .fraction .numerator   { order: 1 }
#tbl4 .fraction .denominator { order: 2; display: none; }
#tbl4 .fraction .ratio       { order: 3; display: none; }
#tbl4 .fraction .percentage  { order: 4; }
#tbl4 .fraction .percentage:before {content: "\00A0(";}
#tbl4 .fraction .percentage:after {content: "%)";}

#tbl4 .statistics .description {font-style: italic;}
#tbl4 .statistic {padding-right: 0.5em;}


#tbl4 td .align{
	display: inline-block;
	margin: 0 auto;
}

#tbl4 .nobr {
   white-space: nowrap;
}
#tbl4 .supsub {
   display: inline-block;
   margin: -9em 0;
   vertical-align: -0.55em;
   line-height: 1.35em;
   font-size: 70%;
   text-align: left;
}
</style><div class="caption">HTML5 Table Lancet Style</div><div class="figbody"><table class="tangram">
<thead><tr><td class="header even tg-label"><span class="variable"></span></td><td class="header even tg-label"><span class="variable">N</span></td><td class="header even tg-label"><span class="variable">D-penicillamine</span></td><td class="header even tg-label"><span class="variable">placebo</span></td><td class="header even tg-label"><span class="variable">not randomized</span></td><td class="header even tg-label"><span class="variable">Test Statistic</span></td></tr><tr class="subheaderrow"><td class="subheader header even tg-label"><span class="variable"></span></td><td class="subheader header even tg-label"><span class="variable"></span></td><td class="subheader header even data N" data-clipboard-text="{list(index = "NTM3", src = "tangram:bili:drug[D-penicillamine]:N", value = "154") N=NULL}"><span class="N">154</span></td><td class="subheader header even data N" data-clipboard-text="{list(index = "OTRl", src = "tangram:bili:drug[placebo]:N", value = "158") N=NULL}"><span class="N">158</span></td><td class="subheader header even data N" data-clipboard-text="{list(index = "ZjNi", src = "tangram:bili:drug[not randomized]:N", value = "106") N=NULL}"><span class="N">106</span></td><td class="subheader header even tg-label"><span class="variable"></span></td></tr></thead>
<tbody><tr> <td class="header odd tg-label"><span class="variable">Serum Bilirubin</span><span class="units">mg/dl</span></td><td class="odd data N" data-clipboard-text="{list(index = "MWFj", src = "tbl4:bili:drug:cell_n1", value = "418") N=NULL}"><span class="N">418</span></td><td class="odd"><span class="odd data quantile"><span class="q25">0.70</span><span class="q50">1.30</span><span class="q75">3.60</span></td><td class="odd"><span class="odd data quantile"><span class="q25">0.80</span><span class="q50">1.40</span><span class="q75">3.22</span></td><td class="odd"><span class="odd data quantile"><span class="q25">0.70</span><span class="q50">1.40</span><span class="q75">3.12</span></td><td class="odd data statistics" data-clipboard-text="{list(index = "YzMz", src = "tbl4:bili:drug:F", value = "0.03") list(index = "Mzkw", src = "tbl4:bili:drug:df2", value = "415")}"><span class="statistic"><span class="description">F<sub>2,415</sub> = </span>0.03,</span><span class="pvalue"><span class="description">P = </span>0.97248<sup>1</sup></span></td> </tr><tr> <td class="header even tg-label"><span class="variable">Albumin</span><span class="units">gm/dl</span></td><td class="even data N" data-clipboard-text="{list(index = "YTY4", src = "tbl4:albumin:drug:cell_n1", value = "418") N=NULL}"><span class="N">418</span></td><td class="even"><span class="even data quantile"><span class="q25">3.34</span><span class="q50">3.54</span><span class="q75">3.78</span></td><td class="even"><span class="even data quantile"><span class="q25">3.21</span><span class="q50">3.56</span><span class="q75">3.83</span></td><td class="even"><span class="even data quantile"><span class="q25">3.12</span><span class="q50">3.47</span><span class="q75">3.73</span></td><td class="even data statistics" data-clipboard-text="{list(index = "NzUz", src = "tbl4:albumin:drug:F", value = "2.13") list(index = "OTNm", src = "tbl4:albumin:drug:df2", value = "415")}"><span class="statistic"><span class="description">F<sub>2,415</sub> = </span>2.13,</span><span class="pvalue"><span class="description">P = </span>0.11996<sup>1</sup></span></td> </tr><tr> <td class="header odd tg-label"><span class="variable">Histologic Stage, Ludwig Criteria</span></td><td class="odd">412</td><td class="odd"></td><td class="odd"></td><td class="odd"></td><td class="odd data statistics" data-clipboard-text="{list(index = "MmM5", src = "tbl4:stage:drug:χ^{2}", value = "5.33") list(index = "YzUy", src = "tbl4:stage:drug:P", value = "0.50235")}"><span class="statistic"><span class="description"><span class="nobr">&chi;<span class="supsub">2<br/>6</span></span> = </span>5.33,</span><span class="pvalue"><span class="description">P = </span>0.50235</span><sup>2</sup></td> </tr><tr> <td class="subheader header odd tg-label"><span class="variable">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1</span></td><td class="odd tg-label"><span class="variable"></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "Yjc1", src = "tbl4:stage[   1]:drug[D-penicillamine]:numerator", value = "4") list(index = "YmZh", src = "tbl4:stage[   1]:drug[D-penicillamine]:ratio", value = "0.0")}"><span class="ratio">0<div class="align">.</div>0</span><span class="percentage">2.6</span><span class="numerator">  4</span><span class="denominator">154</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "OWI1", src = "tbl4:stage[   2]:drug[D-penicillamine]:numerator", value = "12") list(index = "Y2Ri", src = "tbl4:stage[   2]:drug[D-penicillamine]:ratio", value = "0.1")}"><span class="ratio">0<div class="align">.</div>1</span><span class="percentage">7.6</span><span class="numerator"> 12</span><span class="denominator">158</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "ZjYy", src = "tbl4:stage[   3]:drug[D-penicillamine]:numerator", value = "5") list(index = "Njcy", src = "tbl4:stage[   3]:drug[D-penicillamine]:ratio", value = "0.1")}"><span class="ratio">0<div class="align">.</div>1</span><span class="percentage">5.0</span><span class="numerator">  5</span><span class="denominator">100</span></span></td><td class="odd"></td> </tr><tr> <td class="subheader header odd tg-label"><span class="variable">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;2</span></td><td class="odd tg-label"><span class="variable"></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "N2Vh", src = "tbl4:stage[   1]:drug[placebo]:numerator", value = "32") list(index = "NmY0", src = "tbl4:stage[   1]:drug[placebo]:ratio", value = "0.2")}"><span class="ratio">0<div class="align">.</div>2</span><span class="percentage">20.8</span><span class="numerator"> 32</span><span class="denominator">154</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "YTY0", src = "tbl4:stage[   2]:drug[placebo]:numerator", value = "35") list(index = "MDBh", src = "tbl4:stage[   2]:drug[placebo]:ratio", value = "0.2")}"><span class="ratio">0<div class="align">.</div>2</span><span class="percentage">22.2</span><span class="numerator"> 35</span><span class="denominator">158</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "NDI0", src = "tbl4:stage[   3]:drug[placebo]:numerator", value = "25") list(index = "NmQ5", src = "tbl4:stage[   3]:drug[placebo]:ratio", value = "0.2")}"><span class="ratio">0<div class="align">.</div>2</span><span class="percentage">25.0</span><span class="numerator"> 25</span><span class="denominator">100</span></span></td><td class="odd"></td> </tr><tr> <td class="subheader header odd tg-label"><span class="variable">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;3</span></td><td class="odd tg-label"><span class="variable"></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "NmNl", src = "tbl4:stage[   1]:drug[not randomized]:numerator", value = "64") list(index = "YWEw", src = "tbl4:stage[   1]:drug[not randomized]:ratio", value = "0.4")}"><span class="ratio">0<div class="align">.</div>4</span><span class="percentage">41.6</span><span class="numerator"> 64</span><span class="denominator">154</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "OTU0", src = "tbl4:stage[   2]:drug[not randomized]:numerator", value = "56") list(index = "Nzc0", src = "tbl4:stage[   2]:drug[not randomized]:ratio", value = "0.4")}"><span class="ratio">0<div class="align">.</div>4</span><span class="percentage">35.4</span><span class="numerator"> 56</span><span class="denominator">158</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "MmU5", src = "tbl4:stage[   3]:drug[not randomized]:numerator", value = "35") list(index = "MzBh", src = "tbl4:stage[   3]:drug[not randomized]:ratio", value = "0.3")}"><span class="ratio">0<div class="align">.</div>3</span><span class="percentage">35.0</span><span class="numerator"> 35</span><span class="denominator">100</span></span></td><td class="odd"></td> </tr><tr> <td class="subheader header odd tg-label"><span class="variable">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;4</span></td><td class="odd tg-label"><span class="variable"></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "ODg0", src = "tbl4:stage[   1]:drug[NA]:numerator", value = "54") list(index = "NTI5", src = "tbl4:stage[   1]:drug[NA]:ratio", value = "0.4")}"><span class="ratio">0<div class="align">.</div>4</span><span class="percentage">35.1</span><span class="numerator"> 54</span><span class="denominator">154</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "NjEz", src = "tbl4:stage[   2]:drug[NA]:numerator", value = "55") list(index = "NGYz", src = "tbl4:stage[   2]:drug[NA]:ratio", value = "0.3")}"><span class="ratio">0<div class="align">.</div>3</span><span class="percentage">34.8</span><span class="numerator"> 55</span><span class="denominator">158</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "MmJk", src = "tbl4:stage[   3]:drug[NA]:numerator", value = "35") list(index = "ZTU0", src = "tbl4:stage[   3]:drug[NA]:ratio", value = "0.3")}"><span class="ratio">0<div class="align">.</div>3</span><span class="percentage">35.0</span><span class="numerator"> 35</span><span class="denominator">100</span></span></td><td class="odd"></td> </tr><tr> <td class="header even tg-label"><span class="variable">Prothrombin Time</span><span class="units">sec.</span></td><td class="even data N" data-clipboard-text="{list(index = "OGIx", src = "tbl4:protime:drug:cell_n1", value = "416") N=NULL}"><span class="N">416</span></td><td class="even"><span class="even data quantile"><span class="q25">10.0</span><span class="q50">10.6</span><span class="q75">11.4</span></td><td class="even"><span class="even data quantile"><span class="q25">10.0</span><span class="q50">10.6</span><span class="q75">11.0</span></td><td class="even"><span class="even data quantile"><span class="q25">10.1</span><span class="q50">10.6</span><span class="q75">11.0</span></td><td class="even data statistics" data-clipboard-text="{list(index = "MDZk", src = "tbl4:protime:drug:F", value = "0.23") list(index = "MjIy", src = "tbl4:protime:drug:df2", value = "413")}"><span class="statistic"><span class="description">F<sub>2,413</sub> = </span>0.23,</span><span class="pvalue"><span class="description">P = </span>0.79472<sup>1</sup></span></td> </tr><tr> <td class="header odd tg-label"><span class="variable">sex : female</span></td><td class="odd">418</td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "M2Iw", src = "tbl4:sex[sex : female]:drug[D-penicillamine]:numerator", value = "139") list(index = "Yzk4", src = "tbl4:sex[sex : female]:drug[D-penicillamine]:ratio", value = "0.9")}"><span class="ratio">0<div class="align">.</div>9</span><span class="percentage">90.3</span><span class="numerator">139</span><span class="denominator">154</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "MTg0", src = "tbl4:sex[NA]:drug[D-penicillamine]:numerator", value = "137") list(index = "MGEw", src = "tbl4:sex[NA]:drug[D-penicillamine]:ratio", value = "0.9")}"><span class="ratio">0<div class="align">.</div>9</span><span class="percentage">86.7</span><span class="numerator">137</span><span class="denominator">158</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "MTg0", src = "tbl4:sex[NA]:drug[D-penicillamine]:numerator", value = "98") list(index = "MGEw", src = "tbl4:sex[NA]:drug[D-penicillamine]:ratio", value = "0.9")}"><span class="ratio">0<div class="align">.</div>9</span><span class="percentage">92.5</span><span class="numerator"> 98</span><span class="denominator">106</span></span></td><td class="odd data statistics" data-clipboard-text="{list(index = "Y2Qy", src = "tbl4:sex:drug:χ^{2}", value = "2.38") list(index = "NDEx", src = "tbl4:sex:drug:P", value = "0.30387")}"><span class="statistic"><span class="description"><span class="nobr">&chi;<span class="supsub">2<br/>2</span></span> = </span>2.38,</span><span class="pvalue"><span class="description">P = </span>0.30387</span><sup>2</sup></td> </tr><tr> <td class="header even tg-label"><span class="variable">Age</span></td><td class="even data N" data-clipboard-text="{list(index = "MTQz", src = "tbl4:age:drug:cell_n1", value = "418") N=NULL}"><span class="N">418</span></td><td class="even"><span class="even data quantile"><span class="q25">41.4</span><span class="q50">48.1</span><span class="q75">55.8</span></td><td class="even"><span class="even data quantile"><span class="q25">42.9</span><span class="q50">51.9</span><span class="q75">59.0</span></td><td class="even"><span class="even data quantile"><span class="q25">46.0</span><span class="q50">53.0</span><span class="q75">61.1</span></td><td class="even data statistics" data-clipboard-text="{list(index = "OTE5", src = "tbl4:age:drug:F", value = "6.10") list(index = "ZDdl", src = "tbl4:age:drug:df2", value = "415")}"><span class="statistic"><span class="description">F<sub>2,415</sub> = </span>6.10,</span><span class="pvalue"><span class="description">P = </span>0.00245<sup>1</sup></span></td> </tr><tr> <td class="header odd tg-label"><span class="variable">spiders : present</span></td><td class="odd">312</td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "YzUy", src = "tbl4:spiders[spiders : present]:drug[D-penicillamine]:numerator", value = "45") list(index = "MmNi", src = "tbl4:spiders[spiders : present]:drug[D-penicillamine]:ratio", value = "0.3")}"><span class="ratio">0<div class="align">.</div>3</span><span class="percentage">29.2</span><span class="numerator"> 45</span><span class="denominator">154</span></span></td><td class="odd"><span class="odd fraction" data-clipboard-text="{list(index = "MDk1", src = "tbl4:spiders[NA]:drug[D-penicillamine]:numerator", value = "45") list(index = "NmEy", src = "tbl4:spiders[NA]:drug[D-penicillamine]:ratio", value = "0.3")}"><span class="ratio">0<div class="align">.</div>3</span><span class="percentage">28.5</span><span class="numerator"> 45</span><span class="denominator">158</span></span></td><td class="odd"></td><td class="odd data statistics" data-clipboard-text="{list(index = "ODYy", src = "tbl4:spiders:drug:χ^{2}", value = "0.02") list(index = "MDYx", src = "tbl4:spiders:drug:P", value = "0.88534")}"><span class="statistic"><span class="description"><span class="nobr">&chi;<span class="supsub">2<br/>1</span></span> = </span>0.02,</span><span class="pvalue"><span class="description">P = </span>0.88534</span><sup>2</sup></td> </tr></tbody>
</table></div><div class="footnote">N is the number of non-missing value. <sup>1</sup>Kruskal-Wallis. <sup>2</sup>Pearson. <sup>3</sup>Wilcoxon.</div></div><script>new Clipboard('.data');</script>
</div>


## Quick Overview

What began as an extensible library to quickly generate tables from formulas, has evolved into a library that supports magrittr `%>%` style commands on abstract table objects. The formula interface is a complicated piece of code in it's own right, but is only one of many methods now available in the generation of tables. There were a lot of lessons learned to get to this final point, and it's worth talking about what is now the core of the library, and what has become the best practices in the design of the interface.

It's now been used to make 30-40 page reproducible DSMB (data safety monitoring board) reports on multiple clinical trials. Internally, several biostatistical reports are using it to improve the quality of presentation. This shakedown of formats and usage has vastly improved the overall quality of the package. See [fda-example.html](http://htmlpreview.github.io/?https://github.com/spgarbet/tg/blob/master/vignettes/fda-example.html) for some examples--don't be put off by size of the examples. They are reusable over and over for custom content devoted to a given task. These two transforms and their constructions, once built have to date been used on at least 5 different submitted reports. 

A tangram object is at it's heart a list of lists containing `cells` which can be subclassed from just about anything, but the best overall choice is basically a vector of  `character`, which contains text with minor extensions to Rmarkdown. There are two types of style, one is internal to a cell and it's formatting of text. The other is overall styling of a table which is a choice best left to the rendering call. The Rmarkdown/extensions supported are as follows:

* `*italic*` or `_italic_` Makes the font chosen *italic*.
* `**bold**` or `__bold__` Makes the font chosen **bold**.
* \`inline code\` for `inline code`.
* `~~strikethrough~~` for ~~strikethrough~~. 
* `# Header` for header font, and the various other multiples of hash marks.
* `~subscript~` for a subscript (*extension*). 
* `^superscript^` for a superscript (*extension*). 

The original library had custom objects for specific statistical meanings. However, most of these are not required or needed anymore and with each version I am working to eliminate most if not all of them. There is one exception that is difficult and that is the fraction. There is no clean way to support fractions in Rmarkdown that I've come up with. I am considering another extension in the support. However, in the long run all of these custom cell objects and their handling will probably vanish.

This `tangram` object representing the abstract table in memory now has all the internal formatting of a cell representable in a simple and direct manner. A cell object can have the following attributes:

* `sep` a seperator to use for rendering between character strings in a vector
* `reference` a reference character to append as superscript to the field. Not required, as this can be handled now via the Rmarkdown above. Will probably deprecate soon.
* `units` the units of the described label. Maintains compability with Hmisc handling of units on objects.

A `tangram` object itself can have an attribute `footnote` to contain footnotes to display. Some formula transforms automatically supply this with their bundle for reference.

Additional subclassing of cells carries through to special handling. For example in the HTML rendering code all of these become CSS classes so that one can specify CSS rendering however an end user likes. Several of the core statistical cells also allow for flexbox rendering in CSS. LaTeX is more fixed in it's rendering, but deals with these issues handily. 

### Cell Helpers

Functions like `cell_label` will generate a cell label. 

```
> cell_label("Joe")
Joe
> class(cell_label("Joe"))
[1] "cell_label" "cell"       "character" 
```
The class information is important for style decisions later in the manner of CSS, and helper functions exist for the top level class all labeled as `cell_*`. The current handled information by the provided styles (you can of course write your own) is as follows:

* cell, character
* cell_label, cell, character
* cell_header, cell_label, cell, character
* cell_subheader, cell_label, cell, character
* cell_iqr, cell, character
* cell_value, cell, *any base class*
* cell_range, cell, numeric
* cell_fraction, cell_value, cell, character
* cell_n, cell_value, cell, numeric

#### Additional

These are still supported but likely to be deprecated in favor of the above simplicity. The functions to generate them will still be supported. The additional class information is providing no value at present for rendering.

* cell_estimate, cell, list
* cell_fstat, statistics, cell_value, cell, numeric
* cell_chi2, statistics, cell_value, cell, numeric
* cell_studentt, statistics, cell_value, cell, numeric
* cell_spearman, statistics, cell_value, cell, numeric

## Wickham Style

Tables are composible
```
> tbl <- tangram(drug ~ sex, pbc) + tangram(drug ~ bili, pbc)
> tbl
===========================================================================================================
                          N   D-penicillamine       placebo        not randomized       Test Statistic     
                                    154               158               106                                
-----------------------------------------------------------------------------------------------------------
sex : female             418   0.903  139/154    0.867  137/158    0.925   98/106     X^2_2=2.38, P=0.304  
Serum Bilirubin (mg/dl)  418  0.70 *1.30* 3.60  0.80 *1.40* 3.22  0.70 *1.40* 3.12  F_{2,415}=0.03, P=0.972
===========================================================================================================
```

There are some basic operators, but adding more is quite easy. Just drop me a suggestion and I can generally turn it around pretty quickly. In fact, I'm focused on adding these in general right now.

```
> tbl %>% 
  del_row(2) %>%
  del_col(2) %>%
  insert_row(1, "", "Yabba", "Dabba", cell_header("DOOOO"), "", class="cell_header") %>%
  drop_statistics() %>%
  add_indent(2)
===============================================================================
                           D-penicillamine       placebo        not randomized 
                                Yabba             Dabba             DOOOO      
-------------------------------------------------------------------------------
  sex : female              0.903  139/154    0.867  137/158    0.925   98/106 
  Serum Bilirubin (mg/dl)  0.70 *1.30* 3.60  0.80 *1.40* 3.22  0.70 *1.40* 3.12
===============================================================================
```

By the way, referring to specific rows and columns in a table and using operators on these introduce brittleness into your table reproducibility. Say a variable is added or removed, all the absolute references are now broken! Sometimes it's necessary, but in general to be avoided if possible.

The formula interface allows for *reproducible* and *consistent* formatting into tables from data frames. I cannot stress the idea of *consistent* enough in this regard. While working on this project I've seen numerous professional tables where the method of display for information of the same type changes several times in the same table. The reader is forced to adapt his cognitition multiple times and this makes communication of the message in the data more difficult. Consistency of representation of data in a table is paramount to good design.

## dplyr Lovers Delight

But I want to use dplyr to generate my table! Well, you can have your cake and eat it too.

```
> library(dplyr)
> mtcars %>%
  group_by(cyl) %>%
  summarise(disp = round(mean(disp),2), N=n()) %>%
  tangram()
===============
cyl   disp   N 
---------------
4    105.14  11
6    183.31  7 
8    353.1   14
===============
```

This allows for all the downstream rich rendering choices into LaTex, HTML5, rmd, or rtf to work with your summaries from dplyr.


## I want percents from Hmisc style summary transform

So just override the renderer for the cell_fraction object.

```
> tangram(1 ~ sex+drug+bili, pbc, test=FALSE)
==============================================
                          N         All       
                                    418       
----------------------------------------------
sex : female             418   0.895  374/418 
drug                     418                  
   D-penicillamine             0.368  154/418 
   placebo                     0.378  158/418 
   not randomized              0.254  106/418 
Serum Bilirubin (mg/dl)  418  0.80 *1.40* 3.40
==============================================
> summary.cell_fraction <- function(object, ...) { paste0('%', object['percentage']) }
> assignInNamespace("summary.cell_fraction", summary.cell_fraction, "tangram")
> tangram(1 ~ sex[1]+drug[1]+bili, pbc, test=FALSE)
==============================================
                          N         All       
                                    418       
----------------------------------------------
sex : female             418       %89.5      
drug                     418                  
   D-penicillamine                 %36.8      
   placebo                         %37.8      
   not randomized                  %25.4      
Serum Bilirubin (mg/dl)  418  0.80 *1.40* 3.40
==============================================
```

To get into this level of overrides I recommend reading the code for each renderer in the available source code. The only really complex renderer is the LaTeX one, the rest are really simple and I've written a couple of them in an afternoon. So don't be afraid to change things.

### Email 

P.S. I tested copy and paste from the HTML of a vignette into an email with gmail and it worked flawlessly. Nice.


## Release Notes
Jun 4 2018  v0.4 Numerous bug fixes. The package was used for DSMB report submissions to the FDA, and several examples of IRR and other wonderful things have been produced. The full shakedown is complete, and fixes and updates are becoming smaller and smaller as the package stabilizes. LaTeX is a workable render format. It has rtf output that works acceptibly. Work continues towards formatted output in Word, with traceability.

Jun 27 2017 v0.3 A major refactor, cells in the table object are no longer 'special' and are just straight S3 objects. Two adapter layers of code are deleted, interface is stablizing. Used `tangram` as an S3 object and replaced various table generating calls to a single overloaded function.

Sep 27 2017 v0.3.2 Multiple bug fixes from earlier refactor. Added support for LaTeX. A new example from FDA work.

## Original Goal

The idea of creating a quick summary of a data set has been around a good while. The use of a statistical formulas to create summaries exists in SAS in PROC REPORT, and in the R package Hmisc. The SAS has a rich syntax which allows for generation of a wide array of summary tables, but is limited to a subset of SAS functions. The SAS generation is further limited to a fairly crude appearing table, with limited options for output generation. Hmisc offers wonderful output, but is fixed in the analysis that can be performed.

This project intends to create a table grammar that is simple to use, while providing ultimate freedom to the end user when generating summary tables from data sets. This project contains the reference implementation in the language R, but is not limited to R.

For an example using Rmarkdown, see [example.html](http://htmlpreview.github.io/?https://github.com/spgarbet/tg/blob/master/vignettes/example.html)


## General Outline of Formula Transforms
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

## Grammar Definition of Formulas

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




