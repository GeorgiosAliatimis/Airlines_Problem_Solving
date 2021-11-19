T <- 48    # Number of coordination time intervals
cap <- 4   # Runway capacity for each time period
l <- 1     # Minimum turnaround time

req_data_fn <- "request_data.csv"
data <- read.csv(req_data_fn)

sched <- read.csv("schedule_python.csv")

######################################################
## Plot histograms of requested and allocated slots ##
######################################################
pdf("histograms.pdf")
par(mfrow=c(1,2))
hist(c(data$request.arrs, data$request.deps), ylim = c(0, 9), xlim=c(0,T), breaks=0:T, xlab='Time', main='Histogram of requested times')
hist(c(sched$alloc.arrs, data$alloc.deps), ylim = c(0, 9), xlim=c(0,T), breaks=0:T, xlab='Time', main='Histogram of scheduled times')
dev.off()

##############################################
## Calculate and plot airline displacements ##
##############################################

### Calculates displacement for each airline given
### original requests and a schedule
airline_displacements <- function(sched, req_data) {
    airlines <- levels(req_data$airline)
    n_req_pairs <- nrow(req_data)
    disps <- list()
    
    # Initial displacement for all airlines to zero
    for (a in airlines) { disps[[a]] <- 0.0 }
    for (i in 1:n_req_pairs) {
        disp = abs(req_data$request.arrs[i] - sched$alloc.arrs[i]) +
            abs(req_data$request.deps[i] - sched$alloc.deps[i])
        a <- req_data$airline[i]
        disps[[a]] = disps[[a]] + disp
    }
        
    return(disps)
}

pdf("airline_displacement.pdf")
disps <- airline_displacements(sched, data)
barplot(unlist(disps), xlab="Airlines", ylab="Displacement")
dev.off()
