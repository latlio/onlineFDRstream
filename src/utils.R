help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
                         lines=NULL, before=NULL, after=NULL) {
  format = match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  topic <- as.character(topic)
  helpfile = utils:::.getHelpFile(help(topic))
  
  hs <- capture.output(switch(format,
                              text = tools:::Rd2txt(helpfile),
                              html = tools:::Rd2HTML(helpfile),
                              latex = tools:::Rd2latex(helpfile),
                              Rd = tools:::prepare_Rd(helpfile)
  )
  )
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep="\n")
  invisible(hs)
}
