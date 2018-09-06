# Bayesian Sequential Survey Estimator for Now-casting with Non-representative High-Frequency Surveys

This repository maintains the codebase for WFP's now-casting tool for constructing near real-time, near representative estimates from a combination of high-frequency, possibly non-representative surveys with pre-existing, representative, high quality, but dated survey data.

### Why it matters
The World Food Programme of the United Nations since 2014 has deployed _mVAM_ a unique, high-frequency survey system, to monitor food-security in some of the most difficult, conflict-affected, and hard-to-reach environments in the world. This includes the countries of Liberia, Sierra Leone, and Guinea during the Ebola crisis of 2014, Yemen and Syria since 2015, and most recently DRC and Northeast Nigeria.  Surveys are typically conducted monthly by trained enumerators over the phone following a rigorous sampling scheme, and are considered cross-sectional, representing a snapshot in time spanning one or two weeks in a given month. Estimates are then constructed using the traditional Horowitz-Thompson estimator for stratified survey designs and  are post-stratified by one or two demographic factors depending upon what data is available.

However, this method of doing high-frequency surveys problems presents several problems:
 1. The cross-sections do not capture what happens between monthly survey rounds. When WFP is responding to an emergency, waiting an additional month to produce new food insecurity estimates is often too late.
 2. Data is often noisy, introduced by fluctuations in the enumerator mix for the month or respondent attrition. Hence there is need to smooth results over time.
 3. **Bias** - the fact of the matter is that in the contexts where WFP operates, it is the poorest and most vulnerable, those most in-need of assistance, that often do not have access to a phone. Furthermore respondents' willingness to even speak to an enumerator for 15 minutes over the phone highly correlates with socioeconomic characteristics such as age, sex, and level of education. 

This forced us to rethink our approach to high-frequency surveys. 

### Who did it first

Microsoft in collaboration with Columbia University produced the attached paper “Forecasting with Non-Representative Polls.” Instead of using representative polls for their election forecast, they used data collected from Xbox Live players who self-selected into Microsoft’s weekly tracking poll for the 2012 US presidential election. Given that mostly young males that use Xbox Live, the data was highly biased, but by combining it with data from 2008 election and census data, they were able to correctly predict which presidential candidate won every state in the 2012 US election.

The technique the Microsoft researchers proposed is known as model-based estimates and incorporates Bayesian statistical theory. The idea of this approach is to not only consider the newest batch of data in the analyses, but to build a model which takes into account previous assessments and demographic information to predict better estimates of an indicator, such as the Food Consumption Score (FCS) in our case. Hence data is never thrown away. Models are successively updated at each new round of information by using the previous model as an informative prior in a Bayesian update process.

### How it works

The central idea, known as _Multi-Level_ _Regression_ _Poststratification_ (MRP), is to partition the data into thousands of demographic cells, estimate the variable of interest in each cell, and finally re-aggreggate cell-level estimates with respect to the target population's demographic composition. Of course as the number of demographic variables incrases, the number of observations per unique cell grow increasingly sparse and thus estimates increasingly unstable. This resolved by constructing cell-level estimates through multi-level regression modelling, whereby only the marginal distributions, of what are independent demographic variables, are estimated. The cell-evel estimates then simply arise from the joint-probabilities of the marginals. In this manner sparse cells are able to 'borrow strength' from neighbouring cells.

It is this multi-level regression model than then undergoes a Bayesian update process with each new round of data. To compute final estimates we then simulate the posterior model that results from the Bayesian update process on a known representative dataset characterizing the population's demographic composition.

Of importance to note is that each Bayesian update round corresponds to a moving time-window of a pre-specified length (e.g. 30 days) and step (e.g. 7 days) over a near continuous stream of survey data, i.e. at each round only part of the data at each new round is 'new', while the majority is data from the previous round. In this manner we are smoothing the estimates and mitigating artifacts due to noise. 

Finally, we seed the initial Bayesian update round by constructing the initial model not on the mVAM data, but a much more reliable face-to-face survey.

### Work in Progress

Next steps include:
 + refactor code-base to generalize to any indicator
 + better specification of priors for random effects
 + accounting for spatio-temporal correlation perhaps by using Gaussian Processes
 + incorporatte with a Kalman Filtering approach (see http://folk.uio.no/jlind/papers/surveys.pdf)

### Special Thanks to

Dr Seth Flaxman of the Department of Mathematics and the Data Science Institute, Imperial College London was of great help in getting this project off the ground. There is much more work to be done--but we would like to give him recognition for his generous contribution of time.

### Contacts

For more info to collaborate, use or just to know more, you may reach us at gaurav.singhal@wfp.org, lorenzo.riches@wfp.org, silvia.passeri@wfp.org, or submit an issue.
