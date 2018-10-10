#
# represearch.R 
#
# created on Wed Oct 10 17:18:05 2018
# Philipp Homan, <phoman1 at northwell dot edu>
#-----------------------------------------------------------------------
#' create_package 
#'
#' This function creates an R package from scratch.
#' @param project_name Name of the project.
#' @keywords module R 
#' @export
#' @examples
#' create_package("myproject", "~/tmp")
create_package <- function(project_name) {
  # credit:
  # hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
  devtools::create(project_name)
}

#' create_project
#'
#' This function sets up the typical file structure for projects
#' @param name The name of the project
#' @param title The title of the project
#' @param author The author of the project
#' @param email The email of the project's author
#' @param inst The institution of the project's author
#' @param root The root folder for the project
#' @keywords module R reproducible research
#' @export
#' @examples create_project(tags=list("date"=date()), flavor="org-mode")
create_project <- function(tags=list("name"="myproject",
                                     "title"="My Project",
                                     "shorttitle"="My short title",
                                     "author"="Firstname Lastname",
                                     "email"="lastname at inst dot edu",
                                     "git"="https://github.com/mypath",
                                     "bibliography"="master",
                                     "bibstyle"="nature",
                                     "keywords"="git; org-mode; R",
                                     "date"=date(),
                                     "inst"="My Institution",
                                     "root"="~\\/projects"),
                           flavor="org-mode") {
  subdirs <- c(
    "src",
    "lib",
    "doc",
    "templates",
    "output/figures",
    "output/R",
    "output/tmp",
    "pub",
    "ext/LaTeX",
    "ext/logos"
  )
  files <- c(
    "README.org",
    "LICENSE",
    "Makefile",
    "ms.org",
    "ms.Rmd",
    "clean.R",
    "func.R",
    "do.R",
    "load.R",
    "elisp-header.org",
    "header.org",
    "version.R",
    "Module1.xba",
    "dialog.xlb",
    "script.xlb",
    "scientifictemplate.ott",
    "install_macro.sh",
    "uninstall_macro.sh",
    "dotemacs",
    "logo.pdf",
    "logo_feinstein.pdf",
    "github.pdf",
    "twitter.pdf",
    "beamerthemefeinstein.sty"
  )
  targets <- c(
    paste(tags$name, "README.org", sep="/"),
    paste(tags$name, "LICENSE", sep="/"),
    paste(tags$name, "Makefile", sep="/"),
    paste(tags$name, "/src/",tags$name, "_ms.org", sep=""),
    paste(tags$name, "/src/",tags$name, "_ms.Rmd", sep=""),
    paste(tags$name, "/src/",tags$name, "_clean.R", sep=""),
    paste(tags$name, "/src/",tags$name, "_load.R", sep=""),
    paste(tags$name, "/src/",tags$name, "_func.R", sep=""),
    paste(tags$name, "/src/",tags$name, "_do.R", sep=""),
    paste(tags$name, "/templates/", "elisp-header.org", sep=""),
    paste(tags$name, "/templates/", "header.org", sep=""),
    paste(tags$name, "/templates/", "version.R", sep=""),
    paste(tags$name, "/ext/LaTeX/", "Module1.xba", sep=""),
    paste(tags$name, "/ext/LaTeX/", "dialog.xlb", sep=""),
    paste(tags$name, "/ext/LaTeX/", "script.xlb", sep=""),
    paste(tags$name, "/ext/", "scientifictemplate.ott", sep=""),
    paste(tags$name, "/ext/", "install_macro.sh", sep=""),
    paste(tags$name, "/ext/", "uninstall_macro.sh", sep=""),
    paste(tags$name, "/ext/", "dotemacs", sep=""),
    paste(tags$name, "/ext/logos/", "logo.pdf", sep=""),
    paste(tags$name, "/ext/logos/", "logo_feinstein.pdf", sep=""),
    paste(tags$name, "/ext/logos/", "github.pdf", sep=""),
    paste(tags$name, "/ext/logos/", "twitter.pdf", sep=""),
    paste(tags$name, "/ext/", "beamerthemefeinstein.sty", sep="")
  )
  
  #dir.create(project_tags$name)
  for (i in 1:length(subdirs)) {
    fn <- paste(tags$name, "/", subdirs[i], sep="")
    print(fn)
    dir.create(fn, recursive=TRUE)
    system(paste("touch ", fn, "/NULL", sep=""))
  }
  for (i in 1:length(files)) { 
    #from <- paste(project_tags$name, "/templates/", files[i], sep=""))
    from <- system.file("templates", files[i], package="represearch")
    to <- targets[i]
    file.copy(from, to)

    # parse the file
    parse_template(to, tags=tags)
  }
}

#' parse_template 
#'
#' This function parses a file and replaces all @@tags@@.
#' @param filename File to parse.
#' @param tags List of tags.
#' @keywords parsing, template 
#' @export
#' @examples
#' parse_template("mytemplate.txt", list("author"="My Author")
parse_template <- function(filename, tags) {
  tmpfn <- paste(rand_str(1), ".txt", sep="")
  for (i in 1:length(tags)) {
    system(paste("sed -e 's/@@", names(tags)[i], "@@/",
                 tags[[i]], "/g' ", filename, " > ", tmpfn, sep=""))
    file.copy(tmpfn, filename, overwrite=TRUE)
    unlink(tmpfn)
  }
}
  
#' rand_str
#'
#' This function returns a random string.
#' @param n Number of samples.
#' @keywords parsing, template, random
#' @export
#' @examples
#' return 1000 random strings
#' rand_fn(1000)
rand_str <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)),
         sample(LETTERS, n, TRUE))
}
      

#' starsfromp
#'
#' This function returns asterisks for your P-value.
#' @param pval p-value of interest.
#' @param c1 character for P < 0.1.
#' @param c2 character(s) for P < 0.05.
#' @keywords P-value 
#' @export
#' @examples
#' starsfromp(0.02, "")
starsfromp <- function(pval, c1="~", c2="*") {
  as <- vector(mode="character", length=length(pval))
  for (i in 1:length(pval)) {
    p <- pval[i]
    if (p < 0.1) as[i] <- c1 
    if (p < 0.05) as[i] <- paste(c2, sep="")
    if (p < 0.01) as[i] <- paste(c2, c2, sep="")
    if (p < 0.001) as[i] <- paste(c2, c2, c2, sep="")
  }
  return(as)
}


#' dic 
#'
#' This function calculates the Deviance Information Criterion (DIC).
#' @param stanfit Stan object.
#' @keywords stan 
#' @export
#' @examples
#' dic(stanfit)
dic <- function(stanfit) {
  p <- rstan::extract(stanfit, permuted=T)
  return(mean(p$dev) + 0.5 * (sd(p$dev))^2)
}

#' parse_vals 
#'
#' This function parses and formats a P-value.
#' @param action action string. Defaults to pval
#' @param param P-value
#' @keywords P-value
#' @export
#' @examples
#' parse_vals("pval", 0.0001)
parse_vals <- function(action="pval", param) {
switch(action,
	pval={
		if (param < 0.001) {
			 return("< 0.001")
		} else {
			 return(paste("=", round(param, 3)))
		}
	})
}

#' parse_msd 
#'
#' This function parses and formats a mean and SD.
#' @param m Mean 
#' @param sd Standard deviation 
#' @keywords format
#' @export
#' @examples
#' parse_msd(0, 1)
parse_msd <- function(m, sd) {
	print(paste("M = ", round(m, 2), ", SD = ", round(sd, 2),  sep=""))
}

#' parse_tstat 
#'
#' This function parses and formats a t statistic.
#' @param tstat t-test object 
#' @keywords stats, t-test
#' @export
#' @examples
#' tstat <- t.test(rnorm(100, 0, 1))
#' parse_tstat(tstat)
parse_tstat <- function(tstat) {
	print(paste("/t/ (", round(tstat$parameter,2), ") = ",
				round(tstat$statistic, 2), ", /P/ ", parse_vals("pval", 
				tstat$p.value), sep=""))
}

#' parse_rstat 
#'
#' This function parses and formats an r statistic.
#' @param rstat cor.test object 
#' @param method Type of correlation. Defaults to Pearson. 
#' @keywords stats, correlation
#' @export
#' @examples
#' rstat <- cor.test(rnorm(100, 0, 1), rnorm(100, 0, 1))
#' parse_rstat(rstat)
parse_rstat <- function(rstat, method="pearson") {
  switch(method,
         pearson = {
           print(paste("/r/ (", round(rstat$parameter, 2), ") = ",
                       round(rstat$estimate, 2), ", /P/ ",
                       parse_vals("pval", rstat$p.value), sep=""))
         },
         spearman = {
           print(paste("\rho = ",
                       round(rstat$estimate, 2), ", /P/ ",
                       parse_vals("pval", rstat$p.value), sep=""))
         })
}

#' parse_lm 
#'
#' This function parses and formats an linear model object.
#' @param lmfit lm object 
#' @param index Line index in data frame 
#' @param scaled Was the predicor scaled. Defaults to FALSE. 
#' @param style Output format. Defaults to beta. 
#' @keywords stats, linear model
#' @export
#' @examples
#' lmfit <- lm(rnorm(100, 0, 1) ~ rnorm(100, 0, 1))
#' parse_lm(lmfit, 1, FALSE, "beta")
parse_lm <- function(lmfit, index=2, scaled=FALSE, style="beta") {
	lsm <- as.data.frame(coef(summary(lm.beta(lmfit))))[index, ]
	if (scaled==FALSE) {
	  beta <- lm.beta::lm.beta(lmfit)$standardized.coefficients[index]
  } 
  else {
	  beta <- lsm$Estimate
  }

	def <- summary(lmfit)$df[2]
  switch(style,
         beta = {print(paste("\\beta = ",
                             round(beta, 2),
                             ", /t/ (", round(def, 2), ") = ",
                             round(lsm$"t value", 2), ", /P/ ",
                             parse_vals("pval", lsm$"Pr(>|t|)"),
                             sep=""))
         },
         ci = {print(paste("b = ",
                           round(beta, 2),
                           ", 95% CI [",
                           round(confint(lmfit, index)[1]),
                           "; ",
                           round(confint(lmfit, index)[2]),
                           "], ",
                           ", /t/ (", round(def, 2), ") = ",
                           round(lsm$"t value", 2), ", /P/ ",
                           parse_vals("pval", lsm$"Pr(>|t|)"),
                           sep=""))
         })
}

#' parse_fstat
#'
#' This function parses and formats an anova table
#' @param anovatab anova table object 
#' @param index Line index in data frame 
#' @keywords stats, anova 
#' @export
#' @examples
#' anovatab <- anova(lm(rnorm(100, 0, 1) ~ rnorm(100, 0, 1)))
#' parse_fstat(anovatab, 1)
parse_fstat <- function(anovatab, index) {
	a <- as.data.frame(anovatab)[index, ]
	print(paste("/F/ (", round(a$NumDF, 2), ", ", round(a$DenDF, 2), 
				") = ", round(a$"F value", 2), ", /P/ ", parse_vals("pval", 
				a$"Pr(>F)"), sep=""))
}

#' parse_chi
#'
#' This function parses and formats a chi square test result
#' @param chisq Test result 
#' @param def Degrees of freedom
#' @keywords stats, anova 
#' @export
parse_chi <- function(chisq, def) {
	pval <- pchisq(chisq, def, lower=FALSE)
	print(paste("\\chi^{2} = ",
				round(chisq, 2), ", df=", def,  
				", /P/ ", parse_vals("pval", pval), sep=""))
}

#' parse_table
#'
#' This function removes any nils by replacing NA by whitespace
#' @param tabdf data frame 
#' @keywords org-mode, table 
#' @export
parse_table <- function(tabdf) {
	tabdf[is.na(tabdf)] <- ""
	return(tabdf)
}


#' parse_pval
#'
#' This function parses and formats a P-value 
#' @param pval P-value 
#' @param rf Decimal point for rounding 
#' @param chs String for equality test. Defaults to "steq" ("<=") 
#' @keywords org-mode, stats 
#' @export
#' @examples
#' parse_pval(0.0001, 2)
parse_pval <- function(pval, rf=2, chs="steq") {
  if(pval < 0.001) {
    fp <- "0.001"
    char <- "< "
  } else if(pval == 0) {
    return(parse_pval(0.0001))
  } else {
    fp <- as.character(round(pval, rf))
    if (chs == "steq") {
      char <- "= "
    } else {
      char <- NULL
    }
  }
  return(paste(char, fp, sep=""))
}

  
#' get_partcorr_vec
#'
#' This function returns partial correlation vectors 
#' @param lmfit Linear model object 
#' @param xc Name of predictor x 
#' @keywords linear model, partial correlation 
#' @export
get_partcorr_vec <- function(lmfit, xc) {
  #
  # return partial correlations vectors
  #
  ff <- tempfile()
  png(filename=ff)
  a <- car::avPlots(lmfit)
  dev.off()
  unlink(ff)
  x <- as.data.frame(a[xc])[, 1]
  y <- as.data.frame(a[xc])[, 2]
  return(data.frame(x=x, y=y))
}

#df_sum_stats <- function(df) { 
# dfm <- df %>% gather(key=Characteristic, value=Value) %>%
#				group_by(Characteristic) %>%
#				dplyr::summarize(N=sum(!is.na(Value)),
#				                 Mean=round(mean(Value, na.rm=TRUE), 1),
#												 SD=round(sd(Value, na.rm=TRUE), 1),
#												 Min=round(min(Value, na.rm=TRUE), 1),
#												 Max=round(max(Value, na.rm=TRUE), 1)) 
#
#}
