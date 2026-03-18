local({
  if (nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {
    return(invisible())
  }

  if (.Platform$OS.type != "windows") {
    return(invisible())
  }

  candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Positron/resources/app/quarto/bin/tools"
  )

  pandoc_dir <- candidates[file.exists(file.path(candidates, "pandoc.exe"))][1]

  if (!is.na(pandoc_dir) && nzchar(pandoc_dir)) {
    Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  }

  invisible()
})
