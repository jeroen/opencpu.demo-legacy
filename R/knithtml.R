#' Knit to html converter
#' 
#' Use knitr and markdown to convert knitr to html
#' 
#' @param base64text knitr string 
#' @param ... arguments passed on to markdownToHTML
#' @return HTML string
#' @author Jeroen and Yihui
#' @export
knithtml <- function(base64text, ...){
	basefile <- tempfile();
	inputfile <- tempfile(fileext=".Rmd")
	outputfile <- tempfile(fileext=".Rmd");
	htmlfile <- tempfile(fileext=".out");
	
	writeLines(base64text, basefile)
	base64::decode(basefile, inputfile);
	knitr::knit(inputfile, outputfile);
	markdown::markdownToHTML(outputfile, output=htmlfile, fragment.only=TRUE, ...);
	htmldoc <- readLines(htmlfile);
	
	for(figfile in list.files("figure", full.names=TRUE)){
		#upload figures to imgur
		figurl <- imguR::imguRupload(figfile)[["links.original"]]	
		htmldoc <- sub(figfile, figurl, htmldoc)
	}
	
	newhtmlfile <-  tempfile(fileext=".html");
	writeLines(htmldoc, newhtmlfile);
	return(newhtmlfile);
}

#' Publish HTML snippet.
#' 
#' Uploads an HTML snipped that was generated using markdown to the RPubs servers.
#' 
#' @param title title of the document
#' @param base64html base64 encoded html string
#' @param ... args passed on to markdown::rpubsUpload
#' @author jeroen
#' @export
rpubhtml <- function(title="OpenCPU Generated Markdown Report", base64html, ...){
	
	basefile <- tempfile(fileext=".bin");
	htmlfile <- tempfile(fileext=".html");	
	
	writeLines(base64html, basefile)
	base64::decode(basefile, htmlfile);	

	result <- markdown::rpubsUpload(title=title, htmlFile=htmlfile, ...)
	return(result)
}