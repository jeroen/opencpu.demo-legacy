install.opencpu <- function(pkgs, lib="/mnt/export/opencpu-admin-library"){

	#package name instead of file
	if(!file.exists(pkgs)){
		pkgs <- download.packages(pkgs=pkgs, destdir=tempdir())[,2];
	}
	if(length(pkgs) != 1){
		stop("pkgs is not of length 1: ", pkgs);
	}
	
	#build command
	cmd <- "R CMD INSTALL"
	cmd <- paste(cmd, pkgs);
	cmd <- paste(cmd, " --no-test-load --library=", lib, sep="");
	
	#capture output
	outfile <- tempfile();
	cmd <- paste(cmd, ">", outfile, "2>&1");
	output <- system(cmd, intern=FALSE);
	installlog <- readLines(outfile);
	
	#check output
	if(output == 0){
		#seems like success;
		return(installlog);
	} else {
		if(length(installlog) > 20){
			installlog <- c(head(installlog,10), c("","...",""), tail(installlog,10));
		}
		stop(paste(installlog, collapse="\n"));
	}
}
