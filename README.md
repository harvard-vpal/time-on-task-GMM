# time-on-task-GMM
Determining time on task from clickstream timestamps, using a Gaussian mixture model.


totmix(cs, username='username', time='time', category=NULL, nusers=NA, timein=0.1, timeout=7200, minevents=50, maxit=10000, maxrestarts=100, K=NA, Kmin=3, Kmax=7)

This function applies a mixture of log-normal distributions to time-intervals to estimate time on task.


# Input:

Related to the data passed into the function:

cs: a clickstream dataframe containing usernames, timestamps of clicks (as numeric, in seconds) and possibly other columns. In particular, it may contain a category column which says what category of course resource the click came from, e.g. a video, or a problem. 
username: the name of a column in cs to use as username.
time: the name of the column in cs to use as timestamps.
category: if left NULL, all clicks will be used. Otherwise, should be a named character vector. The name of the first element in the vector is the name of the column in cs to use as category column. The elements are the names of the categories clicks from which should be used. E.g. if this input is c(‘category’=’video’), totmix will use only those time intervals, where both clicks are from rows in cs where cs$category entry is ‘video’, and input c(‘category’=’video’,’problem’) will use those where cs$category is ‘video’ or ‘problem’ for both clicks. If you are interested in mixed intervals, write the two categories in the chronological order and separate them with a forward slash. E.g. the input c(‘category’=’video/problem’) will make totmix take only those time intervals where the first click is in a ‘video’ and the second - in a ‘problem’

Related to subsetting the data:

timein: the minimum time interval to take into account, in seconds.
timeout: the maximum time interval to take into account, in seconds.
minevents: upon creating the list of time intervals that satisfy the category, timein and timeout, include into analysis only the users with at least this number of clicks left.
nusers: If not NA, this many users with the largest number of events will be used

Related to the mixture model fitting:

maxit: max number of iterations to use in fitting the mixture model.
maxrestart: max number of restarts to use in fitting the mixture model.
K: number of components in the mixture model. If K=NA, the number of components will be decided independently for each user using Bayesian information criterion.
Kmin: smallest number of components to try with Bayesian information criterion (will be used only if K is NA)
Kmax: largest number of components to try with Bayesian information criterion (will be used only if K is NA)

# Output:

A list with the following components

users: a dataframe with columns “username” (the users used in estimation) and “nevents” (the number of clicks used in estimation for each user). All the other components of the output are lists or vectors of length equal to the number of rows in this dataframe, and the users are listed in them in the same order.

Directly relevant results:

tau: effective threshold across all users, in seconds.
u_tau: vector of user-specific effective thresholds, in seconds.
tau_older: same as tau, but assuming older threshold calculation where long intervals are wholly omitted.
u_tau_older: same as u_tau, but assuming older threshold calculation where long intervals are wholly omitted.
tot: vector of user-specific time on task.
tot_to_net: vector of user-specific fractions of time-on-task (ratio of time on task to net time).
mean_tot_interval: vector of user-specific mean durations of an on-task time intervals
conv: vector of user-specific convergence (TRUE/FALSE) of the mixture model.
gof: vector of user-specific goodness of fit of the mixture model. It is the correlation of u_data$ecdf and u_data$mix_cdf (see below).

Specifics of mixture model fitting:

ncomp: vector of user-specific numbers of components used in the mixture model (if the input K was not NA, all values of will be equal to K).
iter: vector of user-specific number of iterations of the mixture model.
u_data: a list with a component per user. Each component is a dataframe with the following columns:
dt: time intervals
ecdf: cumulative distribution of log-time-intervals
mix_cdf: cumulative distribution of log-time-intervals from the mixture model
density: distribution density of log-time-intervals estimated non-parametrically
mix_density: distribution density of log-time-intervals from the mixture model
u_mixture: a list with a component per user. Each component is the preserved output returned from fitting the mixture model,structured as the output of mixtools::normalmixEM, with the following two extra components:
iter: number of iterations, integer
conv: whether the model converged, boolean.


runtime: seconds elapsed during the function run.
