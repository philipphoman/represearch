#-----------------------------------------------------------------------
# Makefile
#
#-----------------------------------------------------------------------

# root folder
# unfortunately this is needed for the latex2rtf conversion with macros
ROOT = @@root@@

# default project name
PROJ = @@name@@ 
#-----------------------------------------------------------------------

# Usually no edits below this line
#-----------------------------------------------------------------------
# Output directory
OUTP = output

EXT = ext

# Figures directory
FIGS = $(OUTP)/figures

# Tables directory
TABLES = $(OUTP)/tables

# R output directory
ROUT = $(OUTP)/R

# tex output directory
TEXOUT = $(SRC)

# Source directory
SRC = src

# Data directory
DATA = data

# preprocessing script
PREPROCR = $(SRC)/$(PROJ)_clean.R

# Preprocessing directory
PREPROC = preproc

# tmp directory
TMP = $(OUTP)/tmp

# directory for additional pdf files
LIB = lib

# Filename for merged pdf figures
PDFMERGEDFIG = $(OUTP)/tmp/Fig.pdf

# Filename for manuscript
MANUSCRIPT =$(SRC)/$(PROJ)_ms.pdf

# Filename for rtf 
RTF = $(SRC)/$(PROJ)_ms.rtf

# Filename for docx 
DOCX = $(SRC)/$(PROJ)_ms.docx

# git dir for export
SUBGIT = $(TMP)/$(PROJ)
SUBGITSTR = git@github.com:/philipphoman

# Python directory
PYTHONDIR = $(PREPROC)
PYOUT = $(OUTP)/python
PYINPUT = $(PREPROC)/*pyinput*

# executables
RCC = R CMD BATCH
RM = rm -Rf
#TEX = xelatex -output-directory=../$(TEXOUT)
TEX = xelatex 
LATEXMK = latexmk -pdflatex='xelatex -interaction nonstopmode'  -shell-escape -pdf -bibtex -f 
BIBTEX = bibtex
PYTHONBIN = python
#LATEX2RTF = latex2rtf -f3 -M64 -o 
LATEX2RTF = latex2rtf -f0 -M64 -o 
SOFFICE = soffice
SOFFICEARGS= --invisible
SOFFICEMACROSTR  = macro:///LaTeX.Module1.AdjustLaTeXimport
MACROINSTALL = bash install_macro.sh
MACROUNINSTALL = bash uninstall_macro.sh
#LATEX2RTF = latex2rtf -f0 -M4 -o 
WORD = wordopen
EMACSINIT = ~/.emacs 
EMACS = emacs -l $(EMACSINIT)
EMACSMSARGS = --batch -f org-latex-export-to-latex --kill
EMACSPARGS =  --batch -f org-beamer-export-to-latex --kill
VIEWBIN = pdfview
PDFMERGEBIN = ext/pdfmerge
CPBIN = cp
MKDIRBIN = mkdir

# to extract all citations and compile bibliography
BIBEXP = bibexport -o
BIBLIOGRAPHY = $(SRC)/$(PROJ).bib 

# list R files
#RFILES = $(wildcard $(SRC)/*.R)
RFILES = $(SRC)/$(PROJ)_do.R \

# list data files
DATAFILES = $(DATA)/$(PROJ).csv \

# list tex files
#TEXFILES = $(wildcard $(SRC)/*.tex)
TEXFILES = $(ORGFILES:$(SRC)/$(PROJ)_ms.org=$(SRC)/$(PROJ)_ms.tex)

# list org files
ORGFILES = $(wildcard $(SRC)/$(PROJ)*.org)

# list additional library files
PDFLIB = $(wildcard $(LIB)/$(PROJ)*.*)

# indicator files to show R file has been run
ROUTFILES = $(RFILES:$(SRC)/%.R=$(ROUT)/%.Rout)

# indicator for python files
PYOUTFILES = $(PYTHONFILES:$(PYTHONDIR)/%.py=$(PYOUT)/%.pyout)

# replace Rout with pdf to get figure files
#PDFFIGS = $(wildcard $(FIGS)/*.pdf)
PDFFIGS = $(FIGS)/$(PROJ)_plots.pdf $(FIGS)/$(PROJ)_sim.pdf

# indicator files to show tex has run
TEXOUTFILES = $(TEXFILES:$(SRC)/%.tex=$(SRC)/%.aux)

# replace tex with pdf to get pdf tex files
PDFTEXFILES = $(TEXOUTFILES:$(SRC)/%.aux=$(SRC)/%.pdf)

# preprocessing dependencies
#$(DATAFILES): $(PREPROC)/*.csv 
#	cd $(SRC) && $(RCC) $(notdir $(PREPROCR))

# R file dependencies
#$(ROUT)/%.Rout: $(SRC)/%.R $(DATAFILES) \
#                $(SRC)/$(PROJ)_load.R $(SRC)/$(PROJ)_func.R
#	cd $(SRC) && $(RCC) $(notdir $<) ../$(ROUT)/$*.Rout 
$(ROUT)/%.Rout: $(SRC)/%.R $(DATAFILES) \
                $(SRC)/$(PROJ)_load.R 
  #cd $(SRC) && $(RCC) $(notdir $<) ../$(ROUT)/$*.Rout 
	echo "Running $(notdir $<), this may take a while ..." \
	&& cd $(SRC) && $(RCC) $(notdir $<) 

# Python dependencies
$(PYOUT)/%.pyout: $(PYTHONDIR)/%.py $(PYINPUT) 
	echo "Creating python figures ..." \
	&& cd $(PYTHONDIR) && $(PYTHONBIN) $(notdir $<);

# Bayes stan simulation
$(HBRWSIMOUT): $(HBRWSIM)
	cd $(PREPROC)/bayes && $(RCC) $(notdir $(HBRWSIM)) $(notdir $(HBRWSIMOUT))

# Bayes stan estimation
$(HBRWOUT): $(HBRW)
	cd $(PREPROC)/bayes && $(RCC) $(notdir $(HBRW)) $(HBRWOUT)

# Rule for $(TEXFILES)
# Convert every org file to LaTeX this is done from within the subfolder
# so be careful with relative paths
$(SRC)/%.tex: $(SRC)/%.org $(ROUTFILES) $(PDFLIB)
	@if [ "$(notdir $<)" = "$(PROJ)_ms.org" ]; then \
		echo "Exporting manuscript from org to LaTeX" \
		&& cd $(SRC) && $(EMACS) $(PROJ)_ms.org $(EMACSMSARGS); \
	else \
		echo "Exporting $(notdir $<) from org to LaTeX" \
		&& cd $(SRC) && $(EMACS) $(notdir $<) $(EMACSPARGS); \
	fi

#$(SRC)/%.tex: $(SRC)/%.org $(ROUTFILES) $(PDFLIB)
#	cd $(SRC) && $(EMACS) $(notdir $<) $(EMACSMSARGS)

# Rule for $(TEXOUTFILES)
# Run every tex file this is done from within the subfolder so be
# careful with relative paths
$(SRC)/%.aux: $(SRC)/%.tex $(ROUTFILES) $(PDFLIB)
	#cd $(SRC) && $(TEX) $(notdir $<)
	#$(BIBTEX) $(SRC)/$*
	#cd $(SRC) && $(TEX) $(notdir $<)
	#cd $(SRC) && $(TEX) $(notdir $<) 
	cd $(SRC) && $(LATEXMK) $(notdir $<)
	$(BIBEXP) $(BIBLIOGRAPHY) $(SRC)/$(PROJ)_ms.aux

# Default entry
all: figures manuscript presentation

# make manuscript
manuscript: figures tex

# Make presentation
presentation: figures tex

# make figures
figures: analysis 

# run tex files
tex: analysis $(TEXOUTFILES) $(TEXFILES) 

# convert org files
# org: analysis $(TEXFILES)

# run R files
analysis: preproc $(ROUTFILES)

# run python script
python: $(PYOUTFILES) 

# preprocessing
preproc: $(DATAFILES)

# Bayesian analysis
#
# Although this analysis uses the final database to obtain the SCR data,
# we do not build a dependency between bayes and the database as this
# would risk of making this rule circular
bayes: $(HBRWSIMOUT) $(HBRWOUT) 

# fmri group analysis
#fmri: 

# convert manuscript to rtf
# this is really dirty code
# but necessary for the moment
 $(RTF): $(ROUTFILES) $(TEXOUTFILES)
# $(RTF): 
	$(CPBIN) $(SRC)/$(PROJ)_ms.* $(TMP)
	$(CPBIN) $(SRC)/$(PROJ)_ms.tex $(TMP)
	$(CPBIN) $(PDFLIB) $(TMP)
	$(CPBIN) $(FIGS)/*.pdf $(TMP)

  # comment out graphic path
	sed -e 's/\\\graphicspath/%\\\graphicspath/g' $(SRC)/$(PROJ)_ms.tex > $(TMP)/$(PROJ)_ms.tmp.tex

  # grep page instructions and extract the corresponding pages
	cd $(TMP); sed -e 's/\\\includegraphics.*page=\(.*\),.*{\(.*.pdf\)}/pdftk \2 cat \1 output \2.\1.pdf/g' $(PROJ)_ms.tex | eval "$$(grep pdftk)"

  # update graphic files and create final tex file 
	sed -e 's/\(\\\includegraphics.*\)\(page=\)\(.*\),\(.*{\)\(.*.pdf\)}/\1\4\5.\3.pdf}/g'  $(TMP)/$(PROJ)_ms.tmp.tex > $(TMP)/$(PROJ)_ms.tex 

  # switch to tmp directory and do the actual conversion
	cd $(TMP); $(LATEX2RTF) $(PROJ)_ms.rtf  $(PROJ)_ms.tex

  # copy to texout
	cp $(TMP)/$(PROJ)_ms.rtf $(RTF)

  # clean up
	#$(RM) $(SRC)/*.png
	make tmpclean
	$(RM) $(SRC)/scientifictemplate.ott

rtfmacro:
	# Convert any math in /tmp to png
	#EPS = /tmp/*.eps 
	#PNG =$(EPS:.eps=.png)
	for eps in /tmp/*.eps; do \
		echo $$eps ; \
		convert -colorspace sRGB \
						-density 150 $$eps \
						-background white \
						-flatten \
						-units pixelsperinch \
						-density 150 \
						$$eps.png ; \
	done

	# Copy pngs to same directory as manuscript
	#cp /tmp/*.png $(SRC)

	# Copy word template to same directory as manuscript
	cp $(EXT)/scientifictemplate.ott $(SRC)

  # install and run macro (can only pass one arg so far?)
	cd $(EXT) && bash install_macro.sh  && cd .. \
	&& $(SOFFICE) $(SOFFICEARGS) $(SOFFICEMACROSTR)\
	#\("~/projects/ptsd/src/$(PROJ)_ms"\)
	\("~/$(ROOT)/$(PROJ)/src/$(PROJ)_ms"\)

	# test if files are present as expected
	@if [ -e $(RTF) ]; then \
		echo "***************************"; \
		echo "      rtf produced!"; \
		echo "***************************"; \
	fi
	@if [ -e $(DOCX) ]; then \
		echo "***************************"; \
		echo "      docx produced!"; \
		echo "***************************"; \
	fi

# convert manuscript to rtf
msrtf: $(RTF)
msdocx: $(RTF)
renewmsrtf: uninstallmacro rtfclean msrtf rtfmacro msrtfview

# view rtf version of manuscript
msrtfview: msrtf
	$(WORD) $(RTF) &

msdocxview: msrtf
	$(WORD) $(DOCX) &

# view manuscript
manuscriptview: manuscript
	$(VIEWBIN) $(MANUSCRIPT) 

# view presentation
presentationview: presentation
	$(VIEWBIN) $(PRESENTATION) &

# alias for viewing manuscript
msview: manuscriptview

# simulate that R analyses had been done
Rdone:
	for f in $(ROUTFILES); do \
		touch $$f; \
		echo $$f; \
	done

# install latex formatting macro for soffice
installmacro:
	cd $(EXT) && $(MACROINSTALL)

uninstallmacro:
	cd $(EXT) && $(MACROUNINSTALL)

# alias for viewing presentation
pview: presentationview

.PHONY: clean texclean Rclean

clean: texclean Rclean rtfclean figclean

texclean: 
	$(RM) $(TEXOUT)/$(PROJ)*.tex
	$(RM) $(TEXOUT)/$(PROJ)*.aux

Rclean: 
	$(RM) $(ROUT)/$(PROJ)*.*

pyclean: 
	$(RM) $(PYOUT)/$(PROJ)*.*

tmpclean:
	$(RM) $(TMP)/*.*

rtfclean:
	$(RM) $(TMP)/*.*
	$(RM) $(SRC)/$(PROJ)*.rtf

figclean: Rclean
	$(RM) $(FIGS)/$(PROJ)*.*
	$(RM) $(TMP)/Fig.pdf

test:
	echo $(PYINPUT)

git:
	@if [ ! -e $(SUBGIT) ]; then \
		curl -u 'philipphoman' https://api.github.com/user/repos \
		-d '{"name": $(PROJ),"description": $(PROJ)}'  && \
	  sleep 5 && \
	  echo "Creating pub repo for github..." && \
		mkdir -p $(SUBGIT) && \
		mkdir -p $(SUBGIT)/src && \
		mkdir -p $(SUBGIT)/lib && \
		mkdir -p $(SUBGIT)/data && \
		mkdir -p $(SUBGIT)/ext && \
		mkdir -p $(SUBGIT)/src && \
		mkdir -p $(SUBGIT)/output/figures && \
		mkdir -p $(SUBGIT)/output/tables && \
		mkdir -p $(SUBGIT)/output/tmp && \
		mkdir -p $(SUBGIT)/output/R && \
		mkdir -p $(SUBGIT)/output/python && \
		cd $(SUBGIT) && git init && \
		git remote add origin git@github.com:/philipphoman/$(PROJ).git; \
	else \
		echo "Updating pub repo for github..." && \
		cd $(SUBGIT) && git pull origin master; \
	fi 
	cp data/$(PROJ)_* $(SUBGIT)/data
	cp src/$(PROJ)_* $(SUBGIT)/src
	cp ext/$(PROJ)_* $(SUBGIT)/ext
	cp lib/$(PROJ)_* $(SUBGIT)/lib
	cp output/figures/$(PROJ)_* $(SUBGIT)/output/figures
	cp output/tables/$(PROJ)_* $(SUBGIT)/output/tables
	cp output/R/$(PROJ)_* $(SUBGIT)/output/R
	cp output/python/$(PROJ)_* $(SUBGIT)/output/python
	cp README.md $(SUBGIT)/
	cp LICENSE $(SUBGIT)/
	cp ~/db/bibtex/master.bib $(SUBGIT)/src/
	cp ~/db/bibtex/bst/nature.bst $(SUBGIT)/src/
	date > $(SUBGIT)/timestamp
	cp $(EXT)/Makefile.tmp $(SUBGIT)/Makefile
	cd $(SUBGIT)/ && \
	tar czvf $(PROJ).tgz . && \
	sleep 5 && \
	git add $(PROJ).tgz && \
	git add README.md && \
	git add LICENSE && \
	git commit -m $(COMMITMSG) && \
	git push -u origin master;

gitclean:
	$(RM) $(SUBGIT)

gitmake:
	cd $(SUBGIT) && make
