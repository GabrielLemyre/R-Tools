# ----------------------------------------------------------------------------------------------------
# Getting current time and calculating Run time with correct formatting
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# Under the supervision of :
# Maciej AUGUSTYNIAK
# ----------------------------------------------------------------------------------------------------
# Last version : November 14th, 2019
# ----------------------------------------------------------------------------------------------------

# ——————————————————————————————————————————————————————————————————————————
# GETTING TIME IN THE RIGHT FORMAT SEPERATED FROM HOURS, MINUTES AND SECONDS
# ——————————————————————————————————————————————————————————————————————————
get.Sys.Time <- function(){
	actual.Time <- Sys.time()
	H <- start_time.H <- as.numeric(format(actual.Time, "%H"))
	M <- start_time.M <- as.numeric(format(actual.Time, "%M"))
	S <- start_time.S <- as.numeric(format(actual.Time, "%S"))
	
	return(list(H=H,
				M=M,
				S=S))
}

get.Time.Diff <- function(start,end){
	
	# start.sec <- 3600*start$H + 60*start$M + start$S
	# end.sec <- 3600*end$H + 60*end$M + end$S
	
	# sec.diff <- end.sec - start.sec
	sec.diff <- end - start
	print(sec.diff)
	
	Hours <- (sec.diff - sec.diff %% 3600)/3600
	rem.Time <- sec.diff %% 3600
	
	Minutes <- (rem.Time - rem.Time %% 60)/60
	rem.Time <- rem.Time %% 60
	
	Seconds <- rem.Time - rem.Time %% 1
	rem.Time <- rem.Time %% 1
	
	MilliSeconds <- round(rem.Time*1000)
	
	timeSpent.String <- ""
	if (Hours>0){
		timeSpent.String <- paste(timeSpent.String,Hours,"hour")
		if (Hours>1){
			timeSpent.String <- paste(timeSpent.String,"s",sep="")
		}
	}
	if (Minutes>0){
		timeSpent.String <- paste(timeSpent.String,Minutes,"minute")
		if (Minutes>1){
			timeSpent.String <- paste(timeSpent.String,"s",sep="")
		}
	}
	if (Seconds>0){
		timeSpent.String <- paste(timeSpent.String,Seconds,"second")
		if (Seconds>1){
			timeSpent.String <- paste(timeSpent.String,"s",sep="")
		}
	}
	if (MilliSeconds>0){
		timeSpent.String <- paste(timeSpent.String,MilliSeconds,"millisecond")
		if (MilliSeconds>1){
			timeSpent.String <- paste(timeSpent.String,"s",sep="")
		}
	}
	
	return(timeSpent.String)
}