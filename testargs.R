cat("-- reading arguments\n", sep = "");
cmd_args = commandArgs();
for (arg in cmd_args) cat("  ", arg, "\n", sep="");