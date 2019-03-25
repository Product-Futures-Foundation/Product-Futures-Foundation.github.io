;;; lens.el --- Focus on text. -*- emacs-lisp -*-

;; Copyright (C) 2013->oo Product Futures Foundation

;;  This program is free software: you can
;;  redistribute it and/or modify it under the
;;  terms of the GNU Affero General Public License
;;  as published by the Free Software Foundation,
;;  either version 3 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that
;;  it will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.  See the GNU Affero General Public
;;  License for more details.
;;
;;  You should have received a copy of the GNU
;;  Affero General Public License along with this
;;  program.  If not, see http://GNU.org/licenses


;;; Features
;; Link plain-text documents without markup.
;; Preserve whitespace and guess formatting.

;;  Header: =, ==, ===, ====
;;  Shell: $, #
;;  Bullet: :, *, .
;;  Quote: >, >>, "''"
;;  Paren: (), {}, []
;;  Implicit: GNU.org
;;  Explicit: ftp://GNU.org

;;; Installation
;; Copy this file somewhere like ~/doc/.src/lens.el

;; Put this in your .emacs:
;; (autoload 'lens-mode "~/doc/.src/lens.el" "lens" t)
;; (autoload 'lens-mode "lens" "" t)

;; (setq auto-mode-alist'
;;       (append
;;        '(("/\\.txt/.*" . lens-mode))
;;        auto-mode-alist))

;;; Example scenario
;; Write ~/doc/.txt/profit to define that term.
;; While editing that file in Emacs, run  M-x lens
;; Output is written to  ~/doc/profit.htm


;;; Limitations
;; Filenames must be all lowercase and
;; will match case variations in text.


;;;  Main entry points
;; `lens', `lens-mode', `lens-make-page'
;;  `lens-dired-make-marked-pages' and `lens-clean'

;;  In `lens-mode': RET or `mouse-2' at beginning of term visits that file.

;; `lens-mode' keys:
;; f5       `lens-mode-rebuild-mode'
;; C-cC-c   `lens-make-page'
;; C-cC-p   Preview generated HTML of this source file
;; RET      Follow link
;; C-m      Insert line-feed


;;; Version:
;; .01 new
;; .02 cleanup
;; .03 do not link to self

;;; Customizations:
(defcustom lens-global-page-title nil "Title of _all_ generated pages.  If set to `nil' the title is the name of the input file.")
(defcustom lens-output-dir ".." "where to write the output")
(defcustom lens-img-dir ".img" "where <img> content is located")
(defcustom lens-css ".src/preferred.css" "stylesheet")
(defcustom lens-host-mail "AGNUcius@Gmail.com" "mail address to send edits")
(defcustom lens-shortest-inner
  ;; 3 ;too many inner matches
  4 ;not enough prefix coverage
  "Shortest term to match _within_ other terms.
All terms less than this match only at the beginning of words (using `\\b')")

(defcustom lens-encoding "UTF-8" "XML encoding attribute") ;"ISO-8859-1"

(defcustom lens-pure nil "do not add header and footer")

;;; CODE:
(require 'time-date)
(require 'interpreter-minor)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; ;; The following effect only `font-lock'
;; ;; see `lens-build-mapping' for HTML regexps
;; (defconst lens-explicit-URL
;;   "\\([a-zA-Z0-9]\\)+://[^ \t\n]*"
;;   "This allows illegal domain names and
;; includes chars 'banned' from implicit-HTTP.")

;; (defconst lens-implicit-HTTP
;;   ;;"\\(\\([a-zA-Z0-9-]\\)+\\.\\)+\\([a-zA-Z][a-zA-Z]\\)[^])}>:,; \t\n]*"
;;   "\\(\\([a-zA-Z0-9-]\\)+\\.\\)+[^])}>:,; \t\n]*"
;;   ;;maybe use `regexp-opt' here?
;;   "This only allows legal domain names.
;; I don't know what the  [a-zA-Z][a-zA-Z]  part does.
;; Characters  ])}>:,; \\t\\\n  end implicit URL.
;; ")

(defconst lens-implicit-HTTP
  "\\(\\([a-zA-Z0-9_-]\\)+\\.\\)+\\([a-zA-Z][a-zA-Z]\\)[^])}>:,; \t\n]*"
  ;;maybe use `regexp-opt' here?
  "Characters ])}>:,; \\t\\\n end the implicit HTTP URL.")

(defconst lens-explicit-URL
  "\\([a-zA-Z0-9]\\)+://[^ \t\n]*"
  "This is to include chars banned from implicit-HTTP.")

(defvar lens-mapping nil)
(defvar lens-input-dirs '("."))
(defvar lens-closing nil)

(defun lens-shortest-inner (term)
  (if (< (length term) lens-shortest-inner)
	  (if (<= (length term) 2)
		  (concat "\\b" (regexp-quote term) "\\b") ;;find tiny terms only if they stand alone
		(concat "\\b" (regexp-quote term))) ;;find short terms only at beginning of word
	(regexp-quote term)))					 ;;Term otherwise

;;This doesn't really recurse yet, but it really should be a collection of files and directories to allow posting of .files etc.
(defun lens-recurse (process-file)
  (mapcar
   (lambda (term)
     (if (not (file-directory-p term))
         (funcall process-file term)))
   ;; longer terms take precedence
   (sort (directory-files ".")
         (lambda (x y)
           (< (length x) (length y))))))

(defun lens-clean ()
  "like \"make clean\""
  (interactive)
  (shell-command (concat "rm " lens-output-dir "/*.htm")))

(defun lens-build-mapping ()
  (setq lens-mapping nil)

  ;;the longest file name has the highest priority
  ;;  (lens-recurse (file-name-directory (expand-file-name "."))
  (lens-recurse
   (lambda (file)
     (setq lens-mapping
           (cons
			(list
			 (lens-shortest-inner file)
			 (concat
			  "<a href=\"" file ".htm"
			  ;; 			  (if (file-name-extension file)
			  ;; 				  nil
			  ;; 				".htm")
			  "\">\\&</a>"))
			lens-mapping))))

  (dolist
	  (term ;; regexp followed by HTML markup and optional HTML closing markup
	   '(
         ;;;; NOTE: Entries at the BOTTOM of this list have HIGHEST priority

		 ;;must appear at BOL
		 ;;   		 ("^.[^ \n\t]+?:" "<span class=\"type\">\\&" "</span>")

		 ;;  		 ("^.*?:" "<span class=\"h4\">\\&" "</span>");anything followed by a : is a title?

         ("^Related:"  "<span class=\"rel\">\\&" "</span>")
 		 ;;("^Related:.*"  "<small>\\&"  "</small>")

         ;; This is date markup for the diary
		 ("^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}:"  "<hr/><span class=\"date\">\\&</span>")

 		 ("^>"  "<span class=\"quot\">\\&"  "</span>")

 		 ("^="  "<span class=\"h1\">\\&"  "</span>")
 		 ("^=="  "<span class=\"h2\">\\&"  "</span>")
 		 ("^==="  "<span class=\"h3\">\\&"  "</span>")
 		 ("^===="  "<span class=\"h4\">\\&"  "</span>")
 		 ;; ("^----"  "<hr/>\\&")

 		 ("^\\$ "  "<span class=\"shell\">\\&"  "</span>")

         ;; If the first char is a number or any of  :*.o
 		 ;; ("^[ \t]*\\([0-9]\\|:\\|\\*\\|\\.\\|o\\)" "<span class=\"bullet\">\\&</span>")
         ("^[ \t]*\\(:\\|\\*\\|\\.\\)" "<span class=\"bullet\">\\&</span>")


         ;;; BOL not required
 		 ("(\\|{\\|\\[" "<small>\\&")
 		 (")\\|\\}\\|]" "\\&</small>")

 		 ("\"'" "<span class=\"quot2\">\\&")
 		 ("'\"" "\\&</span>")

		 ;;This is not correct.  We should emit to align at column mod(4)
		 ;;Wikipedia.org/wiki/Tab_key#Tabs_in_HTML
		 ("\t" "&nbsp;&nbsp;&nbsp;&nbsp;") ;;TAB
		 ;; 		 ("\t" "\t") ;;doesn't align
		 ;; 		 ("\t" "&#09;") ;;same as \t
		 ;; 		 ("\t" "&#11;") ;;disallowed in SGML (HTML) and XML 1.0

		 ("  " "&nbsp;&nbsp;")
		 ("^ " "&nbsp;") ;; this seems wrong

 		 (">>" "<span class=\"quot2\">\\&" "</span>")
 		 (">>>" "<span class=\"quot3\">\\&" "</span>")

		 ;;TODO: encode & to &amp;
		 ;;see `lens-explicit-URL'
		 ("\\([a-zA-Z0-9]\\)+://[^ \t\n]*"
		  "<a class=\"ext\" href=\"\\&\">\\&</a>")

		 ;;see `lens-implicit-HTTP'
         ("\\(\\([a-zA-Z0-9_-]\\)+\\.\\)+\\([a-zA-Z][a-zA-Z]\\)[^])}>:,; \t\n]*"
          ;("\\(\\([a-zA-Z0-9_-]\\)+\\.\\)+[^])}>:,; \t\n\j]*"
		  "<a class=\"ext\" href=\"http://\\&\">\\&</a>")

		 ;;XML entities
		 ("<" "&lt;")
		 ("&" "&amp;")

 		 ("<i>" "&lt;i><span class=\"itlc\">")
 		 ("</i>" "</span>&lt;/i>")

 		 ("<b>" "&lt;b><span class=\"bold\">")
 		 ("</b>" "</span>&lt;/b>")

		 ;;whitespace
		 ("\n" "<br/>\n")

;;;;;;;; ---- ADD TERMS ABOVE THIS LINE ---- ;;;;;;;;
         ;;;; NOTE: Entries at the BOTTOM of this list have HIGHEST priority
		 ))
    (setq lens-mapping (cons term lens-mapping))))


(defun lens ()
  (interactive)
  (let ((lens-started (current-time)))
    (lens-gen-missing-image-files)
    (shell-command "ls -1 > -index") ;;BUGBUG
    (lens-build-mapping)

    (if (not (file-exists-p lens-output-dir))
        (make-directory lens-output-dir))

    (lens-recurse
	 'lens-make-page-internal)

	(copy-file "../.src/index.src" (concat lens-output-dir "/index.htm") t)

	(if (file-exists-p "../.src/post.el")
		(load-file "../.src/post.el"))
    (message
     (concat "lens started at "
             (format-time-string
              "%I:%M:%S \n"
			  lens-started)

			 "and finished at "
             (format-time-string
              "%I:%M:%S"
              (current-time) )))))


(defun lens-make-page (file)
  (interactive "fFile: ")
  (setq file (file-name-nondirectory file))
  (lens-build-mapping)
  (lens-make-page-internal file))

(defun lens-dired-make-marked-pages ()
  (interactive)
  (lens-build-mapping)
  (dired-map-over-marks
   (lens-make-page-internal
    (file-name-nondirectory (dired-get-filename nil t))) nil))

(defun lens-make-page-internal (file)
  (let ((new-file (concat lens-output-dir "/" file))
        (process ".htm")
		;; 		 (if (file-name-extension file)
		;;                      nil
		;;                    ".html"))
		(modified (format-time-string "%B %e, %Y %l:%M %p" (nth 5 (file-attributes file)))))

    (setq new-file (concat new-file process))

    (if (file-newer-than-file-p new-file file)
        (message (concat new-file " is up to date"))

      (copy-file file new-file t)

      (if process
          (let ((found nil))
            (save-excursion)
            (set-buffer (find-file-literally new-file))
			;;			(setq buffer-file-coding-system default-buffer-file-coding-system)
            (setq case-fold-search t)
            (goto-char (point-min))
            (let ((term-count (length lens-mapping)))

              (while (< (point) (point-max)) ;;while not EOF
                (let ((cur-point (point))
                      (max-point (point-max))
                      (cur-term 0)
                      (found nil))

                  (while (< cur-term term-count) ;;while more terms to consider
                    (let ((regexp (car (nth cur-term lens-mapping)))
						  (replacement (car (cdr (nth cur-term lens-mapping))))
						  (closing (cdr (cdr (nth cur-term lens-mapping)))))
                      (if (and (looking-at regexp)
                               (not (string-equal file regexp))) ;; do not link to self
                          (progn ;;found the term

							;;if `cur-term' represents a file (not just a regexp)
							;;  then add `file' to `cur-term's backlink list
							;;after `lens-make-pages' is complete, iterate through all files again, appending all found backlinks

							;;if this map has a closing tag
							(if closing
								;;prepended to the current string so it closes first
								(setq lens-closing (concat (car closing) lens-closing)))

							(if (and (string= regexp "\n") ;if at EOL,
									 lens-closing);and there are tags to close,
								(progn
								  ;;close tags and follow with EOL replacement
								  ;;(insert lens-closing replacement)
								  (insert lens-closing)
								  (setq lens-closing nil);empty our bucket
								  )
							  ;;else, just replace as normal
							  (replace-match replacement t nil))

                            (setq cur-term term-count) ;;break out of while
                            (setq found t)
							;;`re-search-forward' already set the point to the end of the replacement text
                            )
                        (setq cur-term (+ 1 cur-term))) ;;didn't find a match, so try the next term at the same point
                      ))

                  (if (not found) ;;if no match was found
                      (forward-char))))


			  ;;header
              (goto-char (point-min))
              (insert
			   "<?xml version=\"1.0\" encoding=\"" lens-encoding "\"?>\n"
               "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
               "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
               "\n"
               "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n"
               "\n"
               "<head>\n"
			   "  <title>"
			   (if lens-global-page-title
				   lens-global-page-title
				 file)
			   "</title>\n"
			   "  <link href=\"" lens-css "\" rel=\"stylesheet\" type=\"text/css\"/>\n"
               "</head>\n"
               "\n"
               "<body>\n"
			   )

			  ;;top menu
			  (if (not lens-pure)
				  (insert
				   (concat
					"<p class='header'>\n"
					" <a href=\"-home.htm\">Home</a> |"
					;; " <a href=\"-faq.htm\">FAQ</a> |"
					" <a href=\"-diary.htm\">Diary</a> |"
					;; " <a href=\"-projects.htm\">Projects</a> |"
					" <a href=\"-todo.htm\">Todo</a> |"
					" <a href=\"-index.htm\">Index</a> |"
					" <a href=\"-about.htm\">About</a> |"
					"</p>\n"
					)))
			  (insert "<p class='main'>")

			  ;;images
              (mapcar
               (lambda (type)
                 (if (and (file-exists-p lens-img-dir)
						  (file-exists-p
						   (concat lens-img-dir "/" file "." type)))
                     (insert (concat "<img src=\"" lens-img-dir "/" file "." type "\" alt=\"\"/>"))))
               '("jpg" "gif" "png"))

			  ;;footer
              (goto-char (point-max))

			  (if (not lens-pure)
				  (insert
				   "</p>\n"
				   "<p class='footer'>\n"
				   " Page generated from <a href=\".txt/" file "\">" file "</a> by <a href=\".src/lens.el\">lens.el</a>."))

			  (insert
			   "</p>\n"
			   "</body>\n"
			   "</html>\n")

              (save-buffer)
              (kill-buffer (current-buffer))
              ))))))


;; generate placeholders for image files that don't yet have commentary
(defun lens-gen-missing-image-files ()
  (interactive)
  (mapcar
   (lambda (file)
     (let ((f (file-name-sans-extension file)))
       (if (not (file-exists-p f))
           (progn
             (find-file f)
             (insert " ")
             (save-buffer)
             (kill-buffer (current-buffer))))))
   (if (file-exists-p lens-img-dir)
	   (directory-files (concat lens-output-dir "/" lens-img-dir)))))


;;lens-mode
(defvar lens-font-lock nil)

(defun lens-build-font-lock ()
  "fill `lens-font-lock' with all terms"
  (setq lens-font-lock nil)
  (lens-recurse
   (lambda (term)
     (setq lens-font-lock
           (concat
			(lens-shortest-inner term)
			"\\|" lens-font-lock)))) ;;fill with filenames

  ;;encase and terminate
  (setq lens-font-lock
        (concat "\\(\\)\\("
				lens-explicit-URL "\\|"
				lens-implicit-HTTP "\\|"
				lens-font-lock "zzzzzzzz\\)")))
;; what a mess.


(defun lens-follow ()
  (save-excursion
    (re-search-forward lens-font-lock nil t))
  (let
	  ((match (match-string 2)))
	(if match
		(progn
		  (message match)
		  (if (file-exists-p match)
			  (find-file-read-only match)
			(if (file-exists-p (capitalize match))
				(find-file-read-only (capitalize match))
			  (if (file-exists-p (upcase match))
				  (find-file-read-only (upcase match))
				(find-file-read-only (downcase match)))))))))



(defun lens-mode-rebuild-mode ()
  (interactive)
  (lens-build-font-lock)
  ;; something wrong with lens-font-lock here,
  ;; so sending  nil  instead
  (im-make-mode lens "lens --- focus on text." nil lens-follow)
  (lens-mode))

;; TODO: rewrite as a minor mode
(lens-mode-rebuild-mode) ;;needed so we don't get error:  (void-variable lens-mode-map)

(define-key lens-mode-map [(f5)] 'lens-mode-rebuild-mode)

(defvar lens-C-c-map (make-sparse-keymap))
(define-key lens-mode-map [(control c)] lens-C-c-map)

(define-key lens-C-c-map [(control c)]
  (lambda () (interactive) "Compile this file."
	(save-buffer)
    (lens-make-page buffer-file-name)))

(define-key lens-C-c-map [(control p)]
  (lambda () (interactive)
	"Preview generated HTML of this source file"
	(save-buffer)
    (lens-make-page buffer-file-name)
    (browse-url-of-file
	 ;;(message
     (file-truename
      (concat
	   lens-output-dir "/"
	   (file-relative-name
		(buffer-file-name)) ".htm")))))

(provide 'lens)

;;; lens.el ends here
