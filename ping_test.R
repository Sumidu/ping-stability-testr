# load Libraries required


# If you want to install libraries, set to TRUE
if(F){
  install.packages("pingr")
  install.packages("doParallel")
  install.packages("tidyverse")
  install.packages("tcltk")
  install.packages("scales")
}

# Load libraries ----
library(pingr)
library(doParallel)
library(tidyverse)
require(tcltk)      # Load the TclTk package
library(scales)

# Detect the cores that are available for parellelaization
cores <- parallel::detectCores()

# Configuration ----
# hard limit the amount of runs
max_runs <- 100000
# lists of hosts to detect
hostlist <- c("www.google.com", "192.168.0.16", "192.168.0.1", "8.8.8.8")
# Wait time between pings in seconds
wait_time = 0.5

# create doPar Cluster
cl <- makeCluster(cores)
registerDoParallel(cl)


# Setup mini UI ----
tt <- tktoplevel()  # Create a new toplevel window

tktitle(tt) <- "Simple Dialog"  # Give the window a title

# Create a variable to keep track of the state of the dialog window:
#  If the window is active,
#       done = 0
#  If the window has been closed using the OK button,
#      done = 1
#  If the window has been closed using the Cancel button or destroyed,
#      done = 2
done <- tclVar(0)   # tclVar() creates a Tcl variable
label.text <- tclVar("This is a text label")
# Create two buttons and for each one, set the value of the done
# variable
# to an appropriate value
label.count <- tklabel(tt, textvariable = label.text)
OK.but <- tkbutton(tt, text = "  Stop  ",
                   command = function() tclvalue(done) <- 1)


# Place the two buttons on the same row in their assigned window (tt)
tkgrid(label.count, OK.but)

# Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this
# happens,
# assign 2 to done

tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)

tkfocus(tt)         # Place the focus to our tk window

# Run routine ----
result <- data.frame()
for(i in 1:max_runs){
  # now do our 1000 iterations of whatever, checking the 'done' variable
  # to see
  # if the user clicked the dialog while we were busy.
  
  res <- foreach(i=seq_along(hostlist), .combine = cbind) %dopar% {
    
    pingr::ping(hostlist[i], count = 1)
  }
  
  doneVal <- as.integer(tclvalue(done))   # Get Tcl variable
  tclvalue(label.text) <- paste("Runs: ", str_pad(i, 8, pad = "0"))
  if(doneVal != 0)break
  result <- rbind(result, cbind(res, Sys.time()))
  Sys.sleep(wait_time)
}
tkdestroy(tt)

# shut down cluster
stopCluster(cl)

# Prepare results for plotting
names(result) <- c(hostlist, "time")
result %>% 
  rownames_to_column(var = "trial") %>% 
  gather(key = website, value=ping, -trial, -time) %>% 
  mutate(trial = as.numeric(trial)) -> plotres

# Create plots ----
(plotres %>% 
    ggplot() + 
    aes(y=(ping), x=time, group=website, color=website) + geom_line() +
    scale_x_continuous() + 
    labs(title="Pings by website", color="Domain or IP", x="UnixTimeStamp", y="Ping in ms") + geom_smooth() -> plot_ping)

(plotres %>% group_by(website) %>% tally(is.na(ping)) %>% 
    ggplot() + aes(y=n, x=website, fill = website) + geom_col() + scale_y_continuous() + 
    labs(title="Count of packetlosses") -> plot_packetloss)

# Save plots ---- 
ggsave(plot = plot_ping, filename = paste0("pings-", as.numeric(Sys.time()),".pdf"), width=30)
ggsave(plot = plot_packetloss, filename = paste0("packetloss-", as.numeric(Sys.time()),".pdf"))
