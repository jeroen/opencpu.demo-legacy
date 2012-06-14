\name{rpubhtml}
\alias{rpubhtml}
\title{Publish HTML snippet.}
\usage{
  rpubhtml(title = "OpenCPU Generated Markdown Report",
    htmldoc, ...)
}
\arguments{
  \item{title}{title of the document}

  \item{htmldoc}{html string}

  \item{...}{args passed on to markdown::rpubsUpload}
}
\description{
  Uploads an HTML snipped that was generated using markdown
  to the RPubs servers.
}
\author{
  jeroen
}

