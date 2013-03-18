#This is a sample R script to test passing arguments from the command line to R and outputting them.
# A sample command line is as follows: R --slave --no-save --no-restore --no-environ --silent --args arg1=abc < testargs.R
# This is inspiration gained from:  http://www.perlmonks.org/?node_id=644967
cat("-- reading arguments\n", sep = "");
cmd_args = commandArgs();
for (arg in cmd_args) cat("  ", arg, "\n", sep="");
