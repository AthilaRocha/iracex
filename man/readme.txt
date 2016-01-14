
Our presentation (for the future)

Modified I/F-Race Method

This file contents some instructions to running a testing version of a Modified I/F-Race. This version is not using any algorithm and any problem, but simulating expected costs statistically no different,
in order to compare the behavior of the two approachs that form this method (an approach which deal with pseudo replication and another that does not.

Instructions for use:

This tool can be used in two different ways: 1) By a command line, simulating the execution of a black-box of algorithms (in another languages) which are connected with the method and 2) By executing explicitily an algorithm that is implemented as a function in R.

For running a black-box of algorithms, obviously this black-box must be installed in the user machine. Once installed, the user must type in the R enviroment:
A) library("devtools")
B) load_all() (a message will be showed, informing the loaded of Modified Irace)
C) irace.cmdline() (some texts will be showed, explaining about the execution and about the results)


For running an explicit R algorithm, type in the R environment:
A) Construct the functions that load parameters and group of instances of the problem. The technical manual available in https://cran.r-project.org/web/packages/irace/irace.pdf can help you
B) Type the following command in the R environment:

result<-irace(tunerConfig=list(hookRun=hook.run,instances=vector of instances (ids),maxExperiments=number,logFile=""),parameters=parameters, media=number,effects=matrix(data=NA,nrow=number,ncol=number), effectsagg=matrix(data=NA,nrow=number,ncol=number))

The aditional parameters media, effects and effectsagg are aditioned to the function irace to implement the simulation here proposed. An example of an execution could be:

result<-irace(tunerConfig=list(hookRun=hook.run,instances=weight[1:100],maxExperiments=1000,logFile=""),parameters=parameters, media=5,effects=matrix(data=NA,nrow=500,ncol=100), effectsagg=matrix(data=NA,nrow=500,ncol=100))

During both two ways of running, 3 data files will be saved in the work directory: irace.RData and iraceagg.RData. These files contains all the parameters and results of observations of the simulated algorithms. The file iraceaggresults.csv contains more descritive informations about the observations of the simulated algorithms, separated by statistical test and its respective values.

