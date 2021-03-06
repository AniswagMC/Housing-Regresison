# Estimating Housing Prices in the US 

## Team members

* Zack Moss
* Steve Kim
* Aishwarya More
* Anirudh Duggal


## Description of the data 
Our data is organized by state, so we have 50 observations. We have a predictor of the average house price per stata for every month from January 2000 to August 2021 so 248 predictor variables. Note this data may be transformed in order to have it better displayed for observation (ex splitting the observations into 12 month "chunks" to increase the number of observations.) 

Data is obtained from: https://www.zillow.com/research/data/

## Precise description of the question(s)

As housing prices continue to rise dramatically, can we create a model that will allow us to predict future prices from decades of past data? From the data, we first infer what predictors may affect housing price the most. After selecting based on appropriate criterion, we form a model to predict the range of the value of homes in the future. 

As housing prices continue to rise dramatically, can we create a model that will allow us to predict future prices from decades of past data? From the data, we first infer what predictors may affect housing price the most. After selecting based on appropriate criterion, we form a model to predict the range of the value of homes in the future. 

## Why this question/dataset

As we near the end of our undergradute lives, and step into the adult world, it will not be long before we begin to look to buying houses for oursevles. However, with the current housing market and the way it is behaving, one cannot shake the forboding feeling about their future. We wanted to look at the dataset to see if we could look at past data and get a better understanding of what the market might do. In turn, this would give us a better expectation of what the market might look like in the near future. Futhermore, we could also estimate the cost of our 'dream home' by placing it on our current data to get a monetary output for our expectation.

## Reading list 

Zietz, Joachim, and Anca Traian. ???When was the US housing downturn predictable? A comparison of univariate forecasting methods.??? The Quarterly Review of Economics and Finance 54.2 (2014): 271-281.

Tan, Chang Wei, et al. ???Time series extrinsic regression.??? Data Mining and Knowledge Discovery 35.3 (2021): 1032-1060.

## Team contract. 

For each area, write 1-2 sentences and including any rules to which your team collectively agrees (e.g. "We agree to make 1 commit per week." or "We agree to meet in the library every other Friday.")

**Participation**  
We agree to participate equally as part of the team. This means taking an active role in team discussions, contributing meaninfully to the team code base, via regular commits, and writing a portion of the final report. 

**Communication**  
We agree to use Discord as our prefered means of communication. We agree to check discord on a minimum of a daily basis, in order to stay current on team discussions.  We will also provide our phone numbers to our team members to ensure that we can be reached outside of discord. 

**Meetings**  
We agree to meet once a week on Monday from 3 - 4 pm. The meetings will be held in a hybrid format over Discord.

**Conduct**  
We agree to treat our teammates with respect. Should any team disagreements arise we will settle that by a simple vote, should that end up tied we will determine the outcome using chance. 

***
Do not make any changes from here on. Only the TAs will edit the following.


# Checkpoint 1 grade

(5 / 5)



# Checkpoint 2 grade

__Total__ (27 / 30)

__Words__ (6 / 6) The text is laid out cleanly, with clear divisions
and transitions between sections and sub-sections. The writing itself
is well-organized, free of grammatical and other mechanical errors,
divided into complete sentences logically grouped into paragraphs and
sections, and easy to follow from the presumed level of knowledge. 

__Numbers__ (1 / 1) All numerical results or summaries are reported to
suitable precision, and with appropriate measures of uncertainty
attached when applicable. 

__Pictures__ (5 / 7) Figures and ~tables~ are easy to read, with
informative ~captions~, axis labels and legends, and are placed near the
relevant pieces of text or referred to with convenient labels. 

__Code__ (3 / 4) The code is formatted and organized so that it is easy
for others to read and understand. It is indented, ~commented~, and uses
meaningful names. It only includes computations which are actually
needed to answer the analytical questions, and avoids redundancy. Code
borrowed from the notes, from books, or from resources found online is
explicitly acknowledged and sourced in the comments. Functions or
procedures not directly taken from the notes have accompanying tests
which check whether the code does what it is supposed to. The text of
the report is free of intrusive blocks of code. With regards to R Markdown,
all calculations are actually done in the file as it knits, and only
relevant results are shown.

__Exploratory data analysis__ (12 / 12) Variables are examined individually and
bivariately. Features/observations are discussed with appropriate
figure or tables. The relevance of the EDA to the questions and
potential models is clearly explained.

## Comments:1

1. Codes should be hided when knitting .PDF.
2. Some plots are missing captions. 
3. It would be better to summarize your dataset with a table, which gives an overview
about your explanatory varabies. 
4. I couldn't see an obvious pattern for most of scatter plots so it's not proper to make 
a regression model to them? For example, the scatter plot on page 4 doesn't indicate there
exists a linear regression model that is proper to fit with.


# Checkpoint 3 grade

__Total__ (44 / 65)

__Words__ (5 / 8) The text is laid out cleanly, with clear divisions and
transitions between sections and sub-sections.  The writing itself is
well-organized, free of grammatical and other mechanical errors, ~divided into
complete sentences logically grouped into paragraphs and sections~, and easy to
follow from the presumed level of knowledge.

__Numbers__ (1 / 1) All numerical results or summaries are reported to
suitable precision, and with appropriate measures of uncertainty attached when
applicable.

__Pictures__ (5 / 7) Figures and tables are ~easy to read~, with informative
captions, axis labels and legends, and are placed near the relevant pieces of
text.

__Code__ (1 / 4) The code is formatted and organized so that it is easy
for others to read and understand.  It is indented, ~commented~, and uses
meaningful names.  It only includes computations which are actually needed to
answer the analytical questions, and avoids redundancy.  Code borrowed from the
notes, from books, or from resources found online is explicitly acknowledged
and sourced in the comments.  Functions or procedures not directly taken from
the notes have accompanying tests which check whether the code does what it is
supposed to. The text of the report is free of intrusive blocks of code.  If
you use R Markdown, all calculations are actually done in the file as it knits,
and only relevant results are shown. 

__Exploratory Data Analysis__ (10 / 12) Variables are examined individually and
bivariately. Features/observations are discussed with appropriate
figure or tables. ~The relevance of the EDA to the questions and
potential models is clearly explained~.

__Results and analysis__ (17 / 25) ~The statistical summaries
are clearly related to, or possibly derive from, the substantive questions of interest~.  Any
assumptions are checked by means of appropriate diagnostic plots or
formal tests. Limitations from un-fixable problems are
clearly noted. The actual estimation
of parameters, predictions, or other calculations are technically correct.  ~All calculations
based on estimates are clearly explained~, and also technically correct.  All
estimates or derived quantities are accompanied with appropriate measures of
uncertainty. 

__Conclusions__ (5 / 8) The substantive questions are answered as
precisely as the data and the model allow.  ~The chain of reasoning from
estimation results about models, or derived quantities, to substantive
conclusions is both clear and convincing~.  Contingent answers ("if $X$, then
$Y$, but if $Z$, then $W$") are likewise described as warranted by the
and data.  If uncertainties in the data mean the answers to some
questions must be imprecise, this too is reflected in the conclusions.

Comments: 1.The label of correlation matrix is crowded and not easy to read.
2. It's kinda weird to choose your reponse variable based on the standard that how 
many explanatory variables are related to it. It should be chosen based on your research
question you want to investigate.
3. Code and raw outputs from R shouldn't be displayed in your report, and warning should be suppresed.
4. It's expected to write more about what Ridge and Lasso are, and how you train your random forest.
If you want to investigate your predicted values and true values, it's not assumed that there
is a linear relationship between them. 
5. Discuss why the performance model is deviated from what you thought before the experiments.
#   H o u s i n g - R e g r e s i s o n  
 