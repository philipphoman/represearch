(require 'ox-md nil t)
; set timestamp format
;(setq org-export-date-timestamp-format "%FT%T%z")
(require 'org-wc)
(flyspell-mode t)
(synosaurus-mode t)
(auto-complete-mode t)
(linum-mode t)
(whitespace-mode t)
(setq org-babel-inline-result-wrap "%s")
(setq org-export-with-broken-links "mark")
(setq fill-column 72)
(setq whitespace-line-column 72)
;(setq org-latex-caption-above '(table image))
(setq org-latex-caption-above nil)
(org-toggle-link-display)
; don't remove logfiles at export
(setq org-latex-remove-logfiles nil)

; keybindings
; (global-set-key (kbd "<f7> c") "#+CAPTION: ")
(defun setfillcolumn72 ()
	(interactive)
	(setq fill-column 72)
)

(defun setfillcolumn42 ()
	(interactive)
	(setq fill-column 42)
)
(define-key org-mode-map (kbd "C-c #") "#+CAPTION: ")
(define-key org-mode-map (kbd "C-c f c 4 2") 'setfillcolumn42)
(define-key org-mode-map (kbd "C-c f c 7 2") 'setfillcolumn72)

(setq org-odt-category-map-alist
	 '(("__figure__" "*figure*" "value" "figure" org-odt--enumerable-image-p)))

; let ess not ask for starting directory
(setq ess-ask-for-ess-directory nil)

;(setq org-latex-pdf-process '("latexmk -pdflatex='xelatex
;-output-directory=../output/tex/ -interaction nonstopmode' -pdf
;-bibtex -f %f"))

;(setq org-latex-pdf-process '("latexmk -pdf 
;	-pdflatex='xelatex -shell-escape -interaction nonstopmode' -bibtex -f %f "))
(setq org-latex-pdf-process '("latexmk -pdflatex='xelatex -interaction nonstopmode' -shell-escape -pdf -bibtex -f %f"))

(setq org-latex-logfiles-extensions 
	 (quote("bcf" "blg" "fdb_latexmk" "fls" 
	 "figlist" "idx" "log" "nav" "out" "ptc" 
	 "run.xml" "snm" "toc" "vrb" "xdv")))

(add-to-list 'org-structure-template-alist
 '("ca" "#+CAPTION: "))

(add-to-list 'org-structure-template-alist
 '("he" "#+LATEX_HEADER: "))

(add-to-list 'org-structure-template-alist
 '("dc" "src_R[:session]{}"))

(add-to-list 'org-structure-template-alist
 '("sr" "#+HEADER: :exports none
,#+begin_src R :colnames yes :results silent :session\n")) 

(add-to-list 'org-structure-template-alist
 '("er" "#+END_SRC"))
 
(let ((case-fold-search t)) ; or nil

  (goto-char (point-min))
  (while (search-forward "'" nil t)
    (replace-match "'"))

  (goto-char (point-min))
  (while (search-forward "-" nil t)
    (replace-match "-"))

  ;; repeat for other string pairs
  )

