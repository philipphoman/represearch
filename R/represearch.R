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
                                     "git"="https:\\/\\/github.com\\/mypath",
                                     "bibliography"="myproject",
                                     "bibstyle"="nature",
                                     "keywords"="git; org-mode; R",
                                     "date"=date(),
                                     "inst"="My Institution",
                                     "root"="~\\/projects"),
                           flavor="org-mode") {
  subdirs <- c(
    "data",
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
    "README.md",
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
    "beamerthemefeinstein.sty",
    "nature.bst"
  )
  targets <- c(
    paste(tags$name, "README.org", sep="/"),
    paste(tags$name, "README.md", sep="/"),
    paste(tags$name, "LICENSE", sep="/"),
    paste(tags$name, "Makefile", sep="/"),
    paste(tags$name, "/src/",tags$name, "_ms.org", sep=""),
    paste(tags$name, "/src/",tags$name, "_ms.Rmd", sep=""),
    paste(tags$name, "/src/",tags$name, "_clean.R", sep=""),
    paste(tags$name, "/src/",tags$name, "_func.R", sep=""),
    paste(tags$name, "/src/",tags$name, "_do.R", sep=""),
    paste(tags$name, "/src/",tags$name, "_load.R", sep=""),
    paste(tags$name, "/templates/", "elisp-header.org", sep=""),
    paste(tags$name, "/templates/", "header.org", sep=""),
    paste(tags$name, "/templates/", "version.R", sep=""),
    paste(tags$name, "/ext/LaTeX/", "Module1.xba", sep=""),
    paste(tags$name, "/ext/LaTeX/", "dialog.xlb", sep=""),
    paste(tags$name, "/ext/LaTeX/", "script.xlb", sep=""),
    paste(tags$name, "/ext/", "scientifictemplate.ott", sep=""),
    paste(tags$name, "/ext/", "install_macro.sh", sep=""),
    paste(tags$name, "/ext/", "uninstall_macro.sh", sep=""),
    paste(tags$name, "/ext/", tags$name, "_dotemacs", sep=""),
    paste(tags$name, "/ext/logos/", "logo.pdf", sep=""),
    paste(tags$name, "/ext/logos/", "logo_feinstein.pdf", sep=""),
    paste(tags$name, "/ext/logos/", "github.pdf", sep=""),
    paste(tags$name, "/ext/logos/", "twitter.pdf", sep=""),
    paste(tags$name, "/ext/", "beamerthemefeinstein.sty", sep=""),
    paste(tags$name, "/ext/", "nature.bst", sep="")
  )
  
  #dir.create(project_tags$name)
  for (i in 1:length(subdirs)) {
    fn <- paste(tags$name, "/", subdirs[i], sep="")
    print(fn)
    dir.create(fn, recursive=TRUE)
    #system(paste("touch ", fn, "/NULL", sep=""))
    file.create(paste(fn, "NULL", sep="/"))
  }

  # create mock data file
  file.create(paste(tags$name, "/data/", tags$name, ".csv", sep=""))

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
    system(paste("sed -e \"s/@@", names(tags)[i], "@@/",
                 tags[[i]], "/g\" ", filename, " > ", tmpfn, sep=""))
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
           print(paste("rho = ",
                       round(rstat$estimate, 2), ", /P/ ",
                       parse_vals("pval", rstat$p.value), sep=""))
         })
}

#' parse_lm 
#'
#' This function parses and formats an linear model object.
#' @param lmfit lm object 
#' @param index Line index in data frame 
#' @param scaled Was the predicor scaled? Defaults to FALSE. 
#' @param style Output format. Defaults to beta. 
#' @keywords stats, linear model
#' @export
#' @examples
#' lmfit <- lm(rnorm(100, 0, 1) ~ rnorm(100, 0, 1))
#' parse_lm(lmfit, 1, FALSE, "beta")
parse_lm <- function(lmfit, index=2, scaled=FALSE, style="beta") {
	lsm <- as.data.frame(coef(summary(lm.beta::lm.beta(lmfit))))[index, ]
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


#' parse_estci
#'
#' This function parses and formats an lsmeans estimate with 95% CI.
#' @param dat Object from lsmeans.
#' @keywords stats, estimate, confidence interval
#' @export
parse_estci <- function(lsm) {
  dat <- as.data.frame(confint(lsm$contr))
  pval <- summary(lsm$contr)$p.value
  str <- paste("b = ", round(dat$estimate, 2),
               ", 95% CI: [", round(dat$lower.CL, 2),
               "; ", round(dat$upper.CL, 2),
               "], ",
               "P ", parse_pval(pval), sep="")
  print(str)
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
              ") = ", round(a[, grep("F.value", colnames(a))], 2),
              ", /P/ ", parse_vals("pval", a$"Pr(>F)"), sep=""))
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

#' parse_effectsize
#'
#' This function parses and formats an effect size 
#' @param es d effect size
#' @param type type of effect size
#' @param  digits number of decimal places
#' @keywords stats 
#' @export
parse_effectsize <- function(es, type="d", digits=2) {
	pval <- pchisq(chisq, def, lower=FALSE)
	print(paste0("/", type, "/ = ",
				round(es, digits)))
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
parse_pval <- function(pval, rf=3, chs="steq") {
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
  data <- as.data.frame(eval(lmfit$call$data))
  vdf <- get_all_vars(lmfit, data=data)
  yc <- colnames(vdf)[1]
  vdfs <- vdf[, -1]
  covsc <- colnames(subset(vdfs, select=-which(names(vdfs) %in% xc)))
  fm1 <- formula(paste0(yc, " ~ ", paste0(covsc, collapse="+")))
  fm2 <- formula(paste0(xc, " ~ ", paste0(covsc, collapse="+")))
  yr <- lm(fm1, data=vdf)$residuals
  xr <- lm(fm2, data=vdf)$residuals
  return(data.frame(x=xr, y=yr))
}

#' represearch_slides
#'
#' This function dowloads the pdf presentation 
#' @param destfile Destination for downloaded slides.
#' @keywords reproducible research 
#' @export
represearch_slides <- function(destfile="~/Downloads/rep.pdf") {
  #url1 <- 'https://github.com/philipphoman/mrr/'
  #url2 <- 'raw/master/src/mrr_presentation.pdf'
  url1 <- "https://raw.githubusercontent.com/philipphoman/"
  url2 <- "mrr/master/src/mrr_presentation.pdf"
  url <- paste(url1, url2, sep="")
  download.file(url, destfile=destfile, method="curl")
# https://github.com/philipphoman/mrr/raw/master/src/mrr_presentation.pdf
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

#' parse_lmer 
#'
#' This function parses and formats an lmerfit object 
#' @param lmerfit lmer object.
#' @param ind1 First index.
#' @param ind2 Second index.
#' @param sqrt_b Take the square root of the coefficient. Default FALSE.
#' @keywords parsing, text, org-mode, lme4, mixed models 
#' @export
parse_lmer   <- function(lmerfit, ind1, ind2, sqrt_b=FALSE){
  #
  # report lmer results in text
  # print beta, df, t-, and p-value
  b <- ifelse(sqrt_b,
              round(sqrt(fixef(m1)[ind1]), 2),
              round(fixef(m1)[ind1], 2))
  
              
  print(paste("*b* = ",  b,
              ", 95% CI: ", round(confint(lmerfit)[3, 1], 2), 
              ", ", round(confint(lmerfit)[3, 2], 2),
              "; ", "*t*(", round(summary(lmerfit)$coef[ind1, 3], 2), 
              ") = ", round(summary(lmerfit)$coef[ind1, 4], 2), 
              ", *p* = ", round(summary(lmerfit)$coef[ind1, 5], 3), 
              sep = ""))
}


#' parse_propstat 
#'
#' This function parses and formats a equal proportions statistic.
#' @param propstat prop.test object 
#' @keywords stats, proportions
#' @export
#' @examples
#' propstat <- prop.test(c(100, 110), c(200, 200))
#' parse_propstat(propstat)
parse_propstat <- function(propstat) {
	print(paste("\\chi^{2} (", round(propstat$parameter,2), ") = ",
				round(propstat$statistic, 2), ", /P/ ", parse_vals("pval", 
				propstat$p.value), sep=""))
}

#' annotate_regression
#'
#' This function parses an lmfit object, extracts the partial
#' correlation between the last predictor of the model and the outcome,
#' and adds it to a ggplot object in the right hand corner. Assumes
#' that the variables are standardized and that the predictor of
#' interest is the last one in the model.
#' @param lmfit an linear model fit object
#' @param ggp a ggplot2 object
#' @param font_size integer
#' @param location a string
#' @keywords regression, ggplot2, annotation
#' @export
annotate_regression <- function(lmfit, ggp, font_size=10,
                                location="topright") {
  #
  # annotate a regression plot
  slmfit <- coef(summary(lmfit))
  nr <- nrow(slmfit)
  nc <- ncol(slmfit)
  r <- slmfit[nr, 1]
  p <- slmfit[nr, nc]
  a <- represearch::starsfromp(p)
  if (location == "topright") {
    x <- Inf
    y <- Inf
  }
  if (location == "topleft") {
    x <- -Inf
    y <- Inf
  }
  if (location == "bottomright") {
    x <- Inf
    y <- -Inf
  }
  if (location == "bottomleft") {
    x <- -Inf
    y <- -Inf
  }

  ggp2 <- ggp +
    annotate("text", x=x, y=y, size=font_size,
             hjust=1, vjust=1,
             label=paste0("r=", round(r, 2), starsfromp(p)))
  return(ggp2)
}


