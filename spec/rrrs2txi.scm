;"rrrs2txi.scm", A program to convert Scheme Reports to TeXInfo format.
; Copyright (c) 1998, 2001 Aubrey Jaffer
; Time-stamp: "2001-03-28 21:33:49 jaffer"
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.
;;			     -=-=-=-=-=-

;; This program translates the LaTeX source ".tex" files of the

;;   "Revised(3) Report on the Algorithmic Language Scheme",
;;   "Revised(4) Report on the Algorithmic Language Scheme", or
;;   "Revised(5) Report on the Algorithmic Language Scheme"

;; to TeXInfo format.  If "rrrs2txi.scm" is LOADed into Scheme in a
;; directory containing "r3rs.tex", "r4rs.tex", or "r5rs.tex", then
;; calling the Scheme procedure GO with no arguments (go) will
;; translate the report contained in that directory.  The resulting
;; ".txi" file is written to the current directory.

;; Otherwise, calling the GO procedure will translate all reports
;; contained in "r3rs", "r4rs", or "r5rs" subdirectories of the
;; current directory to texinfo format.  The resulting ".txi" files
;; are written to the current directory.

;; If a Scheme report is successfully converted, GO also tries to
;; compile the new ".txi" file to INFO format.  The resulting info
;; files are placed in the directory named by the variable
;; *INFO-VICINITY*, defined here.
(define *info-vicinity* (make-vicinity "/usr/local/info/"))
;; Change *INFO-VICINITY*'s value to suit your platform.

(require 'common-list-functions)
(require 'string-search)
(require 'string-port)
(require 'string-case)
(require 'fluid-let)
(require 'line-i/o)
(require 'printf)

(define *input-vicinity* (user-vicinity))
(define *output-vicinity* (user-vicinity))
(define *input-basename* #f)
(define *tex-input-port* #f)
(define *txi-output-port* #f)
(define *begin-stack* '())
(define *entry-type* #f)
(define *brace-depth* 0)
(define *paren-depth* 0)
(define *previous-char* #f)
(define *closer* #f)
(define *tab-index* 0)
(define *column* 0)
(define *html* #f)

(define (tex:errwarn class . args)
  (define cep (current-error-port))
  (force-output (current-output-port))
  (force-output *txi-output-port*)
  (if (null? *new-labels*)
      (if *input-basename*
	  (fprintf cep "%s: \"%s.tex\": %s: " class *input-basename* *name*)
	  (fprintf cep "%s: %s: " class *name*))
      (if *input-basename*
	  (fprintf cep "%s: \"%s.tex\": %s: %s: " class *input-basename* *name*
		   (caar *new-labels*))
	  (fprintf cep "%s: %s: %s: " class *name* (caar *new-labels*))))
  (if (not (null? args))
      (begin (display (car args) cep)
	     (for-each (lambda (x) (display #\  cep) (write x cep))
		       (cdr args))))
  (newline cep)
  (force-output cep))
(define (tex:warn . args)
  (apply tex:errwarn "WARN" args))
(define (tex:error . args)
  (apply tex:errwarn "ERROR" args)
  '(print '*closer* *closer* '*brace-depth* *brace-depth*
	  '*previous-char* *previous-char*
	  'peek-char (pc))
  (require 'pretty-print)
  (pretty-print *begin-stack*)
  (abort))

(define *name* "?")
(define *previous-labels* '())
(define *new-labels* '(("define-syntax" . "Syntax definitions")))
(define (define-label label)
  ;;(print 'define-label label)
  (let ((pair (assoc label *new-labels*))
	(old-pair (assoc label *previous-labels*)))
    (cond ((not pair)
	   (set! *new-labels* (cons (cons label *name*) *new-labels*)))
	  ((equal? (cdr pair) *name*))
	  ((and old-pair (not (equal? (cdr old-pair) *name*)))
	   (set-cdr! pair *name*)
	   (tex:warn label 'changed-from (cdr old-pair) 'to *name*))
	  (else (tex:warn label 'changed-from (cdr pair) 'to *name*)
		(set-cdr! pair *name*)))))
;;; \pproto has some weird stuff.  Try to extract a label; the only
;;; one seems to be `do'
(define (extract-do-label proc)
  (if (and (>= (string-length proc) 4)
           (equal? "(do " (substring proc 0 4)))
      (define-label "do")))
(define (label->name label)
  (let ((pair (or (assoc label *new-labels*)
		  (assoc label *previous-labels*))))
    (cond (pair (cdr pair))
	  (else (tex:warn 'label-not-found label) label))))

;;;; space and special character utilities

(define (string-whitespace? str)
  (do ((idx (+ -1 (string-length str)) (+ -1 idx)))
      ((or (negative? idx) (not (char-whitespace? (string-ref str idx))))
       (negative? idx))))
(define (output-text-lines . lines)
  (for-each (lambda (str)
	      (display str *txi-output-port*)
	      (tex:newline #f #f))
	    lines))
(define (escape-special chr)
  (case chr
    ((#\@ #\{ #\}) (string #\@ chr))
    ;;((#\#) "@pounds{}")
    (else chr)))
(define tabs (make-vector 10 0))
(define (bump-column str)
  (set! *column*
	(+ (or (substring? "}" str)
	       (and (substring? "{" str)
		    (- (string-length str) (substring? "{" str) 1))
	       (string-length str))
	   *column*)))
(define (space-to-column col)
  ;;(print 'space-to-column col '*column* *column*)
  (cond ((negative? (- col *column*))
	 (tex:warn 'spacing-backwards? *column* col))
	(else (display (make-string (- col *column*) #\ )
		       *txi-output-port*)))
  (set! *column* col))
(define (space-to-tab-stop idx)
  ;;(print 'space-to-tab-stop idx)
  (space-to-column (vector-ref tabs idx))
  (set! *tab-index* (+ idx 1)))
(define (set-tabs! . args)
  (define idx 0)
  (set! *column* 0)
  (set! *paren-depth* 0)
  (for-each (lambda (tbcl)
	      (vector-set! tabs idx tbcl)
	      (set! idx (+ 1 idx)))
	    args))
(define (pc) (peek-char *tex-input-port*))
(define (rc)
  (let ((chr (read-char *tex-input-port*)))
    (cond ((eqv? #\newline chr)
	   (set! *tab-index* 0)
	   (set! *column* 0))
	  (else (set! *column* (+ 1 *column*))))
    chr))
(define (rl)
  ;;(print 'rl *tab-index* *column*)
  (set! *column* 0)
  (set! *tab-index* 0)
  (read-line *tex-input-port*))
(define (close-parens)
  (cond ((positive? *paren-depth*)
	 (let ((closes (make-string *paren-depth* #\))))
	   (tex:warn "Closing Scheme expression!")
	   (fprintf *txi-output-port* "\\n%s\\n" closes))))
  (set! *paren-depth* 0))

;;;; Make index entries

(define *index-memory* '())
(define *index-entries* '())
(define (make-index-entry name before after)
  (let ((new-index (sprintf #f "@%s%s%s" before (string-downcase name) after)))
    (cond ((not (member new-index *index-memory*))
	   (set! *index-memory* (cons new-index *index-memory*))
	   (set! *index-entries* (cons new-index *index-entries*))))))
(define (rule:index cmd me before after)
  (fluid-let ((*tex-rules* (append (rule #\newline " ") *tex-rules*)))
    (make-index-entry (capture-argument) before after))
  #t)
(define (tex:newline chr me)
  (set! *tab-index* 0)
  (set! *column* 0)
  ;;(case (pc) ((#\newline #\ ) (read-char *tex-input-port*)))
  (newline *txi-output-port*)
  (for-each (lambda (str)
	      (display str *txi-output-port*)
	      (newline *txi-output-port*))
	    *index-entries*)
  (set! *index-entries* '())
  #t)

;;;; Process files.

(define (check-file-end iport)
  (do ((chr (read-char iport) (read-char iport)))
      ((or (eof-object? chr) (not (char-whitespace? chr)))
       (if (not (eof-object? chr))
	   (tex:warn 'process-rrrs-file (string-append *input-basename* ".tex")
		     'ended-prematurely chr (read-line iport))))))
(define (process-rrrs-file base-name)
  (fluid-let ((*input-basename* base-name))
    (fprintf (current-error-port) "Translating \"%s.tex\"\\n" base-name)
    (call-with-input-file
	(in-vicinity *input-vicinity* (string-append base-name ".tex"))
      (lambda (iport)
	(fluid-let ((*tex-input-port* iport))
	  (process-tex-input #t)
	  (check-file-end iport)
	  #t)))))
(define (tex->txi . optargs)
  (cond ((null? optargs)
	 (set! *txi-output-port* (current-output-port))
	 (set! *tex-input-port* (current-input-port))
	 (process-tex-input #t))
	(else
	 (newline)
	 (cond ((not (null? (cdr optargs)))
		(set! *input-vicinity* (car optargs))
		(set! optargs (cdr optargs))))
	 (cond ((not (null? (cdr optargs)))
		(set! *output-vicinity* (cadr optargs))))
	 (let* ((basename (car optargs))
		(labels (in-vicinity *output-vicinity*
				     (string-append basename "-nod.scm"))))
	   (if (file-exists? labels)
	       (call-with-input-file labels
		 (lambda (lport)
		   (set! *previous-labels* (read lport))
		   (set! *previous-nodes*  (read lport))
		   (if (eof-object? *previous-nodes*)
		       (set! *previous-nodes* '())))))
	   (call-with-output-file
	       ;;"/dev/tty"
	       (in-vicinity *output-vicinity* (string-append basename ".txi"))
	     (lambda (oport)
	       (set! *txi-output-port* oport)
	       (process-rrrs-file basename)))
	   (if (and (not (null? *new-labels*)) (not (null? *new-nodes*)))
	       (call-with-output-file labels
		 (lambda (oport)
		   (fprintf oport "(\\n")
		   (for-each (lambda (pair) (fprintf oport " %#a\\n" pair))
			     *new-labels*)
		   (fprintf oport ")\\n")
		   (fprintf oport "(\\n")
		   (for-each (lambda (lst) (fprintf oport " %#a\\n" lst))
			     *new-nodes*)
		   (fprintf oport ")\\n"))))
	   (and (equal? *previous-labels* *new-labels*)
		(equal? *previous-nodes* *new-nodes*))))))

;;;; Use `system' to convert to info format
(define (txi->info vic base . info-vic)
  (newline)
  (set! info-vic (if (null? info-vic) *info-vicinity* (car info-vic)))
  (and (provided? 'system)
       (zero? (system (sprintf #f "makeinfo %s%s.txi -o %s%s.info"
			       vic base info-vic base)))
       (zero? (system (sprintf #f "install-info %s%s.info %sdir"
			       info-vic base info-vic)))))

;;;; Rules

(define rules append)
(define (rule toks . args)
  (map (lambda (tok) (cons tok args))
       (if (pair? toks) toks (list toks))))

(define (process-rule rul)
  (cond ((procedure? (cadr rul))
	 (let ((ans (apply (cadr rul) rul)))
	   (set! *previous-char* (car rul))
	   ans))
	((char? (cadr rul))
	 (set! *previous-char* (car rul))
	 (display (cadr rul) *txi-output-port*)
	 #t)
	((symbol? (cadr rul))
	 (cadr rul))
	((string? (cadr rul))
	 (set! *column* (+ -1 *column*
			   (if (null? (cddr rul))
			       (string-length (cadr rul))
			       (if (negative? (caddr rul))
				   (- 1 *column*)
				   (caddr rul)))))
	 (set! *previous-char* (car rul))
	 (apply fprintf *txi-output-port* (cdr rul))
	 #t)
	(else
	 (set! *previous-char* (car rul))
	 (tex:error (car rul) '=> 'malformed-rule rul))))
(define (process-one left)
  (let* ((tok (if (eqv? #t left) (rc) left))
	 (rul (or (assv tok *tex-rules*))))
    ;;(print *brace-depth* (make-string (max 0 *brace-depth*) #\ ) tok '*closer* *closer*)
    (cond ((eof-object? tok)
	   (cond ((not (zero? *brace-depth*))
		  (tex:error '*brace-depth* 'not-zero *brace-depth*)))
	   tok)
	  ((not rul)
	   (set! *previous-char* tok)
	   (display tok *txi-output-port*)
	   (if (not (char? tok)) (tex:warn tok 'unknown))
	   #t)
	  (else (process-rule rul)))))
(define (process-tex-input left)
  (do ((left (process-one left) (process-one left)))
      ((or (eqv? #f left) (eof-object? left))
       left)))

;;;; Arguments to backslash commands

(define (process-full-bracketed capture?)
  (if capture?
      (case (read-char *tex-input-port*)
	((#\[)
	 (call-with-output-string
	  (lambda (oport)
	    (fluid-let
		((*closer* #\])
		 (*txi-output-port* oport)
		 (*tex-rules*
		  (append (rule #\] tex:close)
			  (rule 'tt (lambda (cmd me)
				      (fprintf *txi-output-port* "@t{")
				      (process-tex-input #t)
				      (fprintf *txi-output-port* "}")
				      #f))
			  *tex-rules*)))
	      (process-tex-input #t)))))
	(else (tex:error 'missing #\[)))
      (case (read-char *tex-input-port*)
	((#\[)
	 (fluid-let ((*closer* #\])
		     (*tex-rules*
		      (append (rule #\] tex:close)
			      (rule 'tt
				    (lambda (cmd me)
				      (fprintf *txi-output-port* "@t{")
				      (process-tex-input #t)
				      (fprintf *txi-output-port* "}")
				      #f))
			      *tex-rules*)))
	   (process-tex-input #t)))
	(else (tex:error 'missing #\[)))))
(define (process-opt-arg cmd)
  (case cmd
    ((linebreak) (process-full-bracketed #t) #t)
    ((documentstyle documentclass)
     (let* ((optarg (process-full-bracketed #t))
	    (arg (capture-braced-expression)))
       (output-text-lines "\\input texinfo @c -*-texinfo-*-"
			  "@c %**start of header")
       (fprintf *txi-output-port* "@setfilename %s\\n"
		(string-append *input-basename* ".info"))
       (fprintf *txi-output-port* "@settitle Revised(%c) Scheme\\n"
		(string-ref *input-basename* 1))))
    ((topnewpage)
     (cond
      (*html*
       (fluid-let
	   ((*tex-rules*
	     (append
	      (rule #\newline "")
	      (rule '(begin-center end-center) "")
	      (rule '(#\& bs-bs) "\\n@author ")
	      (rule 'begin-tabular
		    (lambda (cmd me)
		      (capture-braced-expression)
		      (fluid-let
			  ((*closer* 'end-tabular)
			   (*tex-rules*
			    (append (rule 'end-tabular tex:close "" "")
				    (rule '(#\& bs-bs) "\\n@author ")
				    collapse-spaces
				    *tex-rules*)))
			(process-tex-input #t)
			#t)))
	      (rule 'multicolumn
		    (lambda (cmd me . ruls)
		      (fluid-let ((*tex-rules* (append (apply rules ruls)
						       *tex-rules*)))
			(let* ((arg1 (capture-argument))
			       (arg2 (capture-braced-expression)))
			  (fprintf *txi-output-port* "\\n@author ")
			  (process-braced-expression))))
		    (rule #\newline ""))
	      *tex-rules*)))
	 (process-full-bracketed #f))
       (fluid-let ((*tex-rules* (append (rule 'eject tex:close) *tex-rules*))
		   (*closer* 'eject))
	 (process-tex-input #\newline)))
      (else
       (fluid-let
	   ((*tex-rules*
	     (append
	      (rule #\newline "")
	      (rule '(begin-center end-center) "")
	      (rule 'begin-tabular
		    (lambda (cmd me)
		      (define numcols
			(string-length
			 (fluid-let ((*tex-rules* (append (rule #\@ "")
							  (rule 'bs-bs "\\n" -1)
							  *tex-rules*)))
			   (capture-braced-expression))))
		      (fprintf *txi-output-port* "@c %s\\n" cmd)
		      (fprintf *txi-output-port* "@quotation\\n")
		      (fprintf *txi-output-port* "@multitable @columnfractions")
		      (do ((i (+ -1 numcols) (+ -1 i)))
			  ((negative? i))
			(fprintf *txi-output-port* " %.2f" (/ numcols)))
		      (fprintf *txi-output-port* "\\n@item ")
		      (fluid-let
			  ((*closer* 'end-tabular)
			   (*tex-rules* (append (rule 'end-tabular tex:close
						      "" "@end multitable"
						      "@end quotation" "")
						(rule #\& "@tab ")
						(rule #\newline "")
						(rule 'bs-bs "\\n@item " -1)
						*tex-rules*)))
			(process-tex-input #t)
			#t)))
	      *tex-rules*)))
	 (process-full-bracketed #f))
       (fluid-let ((*tex-rules* (append (rule 'eject tex:close) *tex-rules*))
		   (*closer* 'eject))
	 (process-tex-input #\newline)))))
    ((item)
     (fprintf *txi-output-port* "@item ")
     (process-full-bracketed #f)
     (cond ((not (eqv? #\newline (pc)))
	    (tex:newline #f #f))))
    (else (tex:error cmd 'does-not-take-optional-arguments)))
  #t)

(define (capture-braced-expression)
  (call-with-output-string
   (lambda (oport)
     (fluid-let ((*txi-output-port* oport))
       (process-braced-expression)))))
(define (process-braced-expression)
  (case (pc)
    ((#\{)
     (read-char *tex-input-port*)
     (fluid-let ((*tex-rules*
		  (append (rule #\} tex:close) *tex-rules*))
		 (*brace-depth* (+ 1 *brace-depth*))
		 (*closer* #\}))
       (process-tex-input #t)
       #t))
    (else (tex:error 'process-braced-expression 'missing #\{))))
(define (capture-argument)
  (call-with-output-string
   (lambda (oport)
     (fluid-let ((*txi-output-port* oport))
       (process-argument)))))
(define (check-brace-depth! bd tok)
  (cond ((eqv? bd *brace-depth*))
	(else (tex:warn (if (< *brace-depth* bd)
			    'brace-over-used-by
			    'brace-under-used-by)
			tok
			*brace-depth* 'not bd))))
;;; Stub for top-level definition.
(define (process-argument)
  (tex:error 'process-argument "called without setup-argument-processing"))

;;;; Backslash commands

;;; TeX is nasty in its treatment of curly braces:
;;; {\rm text}		=> (rm text)
;;; \rm{text}		=> (rm text)
;;; \rm{\var{proc}}	=> (rm (var proc))
;;; \rm{\var proc}	=> (rm (var proc))

;;;	 1 pair: \foo{...}
;;; early-brace: {\foo ...}
;;;	 2 pair: {\foo{...} ...} strip outer pair

(define (read-bs-token)
  (define (char-alphabetic*? chr)
    (or (char-alphabetic? chr)
	(eqv? #\* chr)))
  (do ((chr (pc) (pc))
       (lst '() (cons chr lst)))
      ((or (eof-object? chr)
	   (not (char-alphabetic*? chr)))
       (let ((str (list->string (reverse lst))))
	 (if (equal? "" str)
	     (tex:error 'null-bs-token)
	     (string->symbol str))))
    (read-char *tex-input-port*)))
(define (read-bs-command early-brace?)
  (define bschr (pc))
  (define processed-argument? #f)
  (cond
   ((char-alphabetic? bschr)
    (let* ((tok (read-bs-token))
	   (chr (pc)))
      (cond
       ((eqv? #\[ chr) (process-opt-arg tok))
       (else
	(fluid-let
	    ((process-argument
	      (lambda ()
		(set! processed-argument? #t)
		(let ((chr (pc))
		      (bd *brace-depth*))
		  (case chr
		    ((#\{)
		     (read-char *tex-input-port*)
		     (fluid-let
			 ((*brace-depth* (+ 1 bd)) ;(if early-brace? 2 1)
			  (*tex-rules*
			   (append (rule #\} tex:close) *tex-rules*))
			  (*closer* #\}))
		       (process-tex-input #t)
		       (check-brace-depth! bd tok)
		       #f))
		    (else 			  
		     (do ((ch (pc) (pc)))
			 ((not (char-whitespace? ch)))
		       (read-char *tex-input-port*))
		     (if early-brace?
			 (process-tex-input #t)
			 (if (zero? bd)
			     (fluid-let
				 ((*tex-rules*
				   (append (rule #\newline tex:close)
					   *tex-rules*))
				  (*closer* #\newline))
			       (process-tex-input
				(if (eqv? #\newline chr) #\newline #t))
			       (check-brace-depth! bd tok)
			       (set! processed-argument? #f)
			       #\newline)
			     (process-tex-input #t)))))))))
	  (let ((ans
		 (cond ((and early-brace? (not (eqv? chr #\{)))
			(fluid-let ((*tex-rules*
				     (append (rule #\} tex:close) *tex-rules*))
				    (*brace-depth* (+ 1 *brace-depth*))
				    (*closer* #\}))
			  (process-tex-input tok)
			  #t))
		       ((and early-brace? (eqv? chr #\{))
			(fluid-let ((*tex-rules*
				     (append (rule #\} tex:close) *tex-rules*))
				    (*brace-depth* (+ 1 *brace-depth*))
				    (*closer* #\}))
			  (process-tex-input tok)
			  (process-tex-input #t)
			  #t))
		       (else (process-one tok)))))
	    ;;(print 'processed-argument? processed-argument? 'ans ans)
	    (if processed-argument?
		(if (or (eqv? chr #\{) early-brace?)
		    (if (eqv? ans #f) #t ans)
		    #f)
		ans)))))))
   ((char-numeric? bschr)
    (tex:error 'bs-digit? bschr))
   (else (case bschr
	   ((#\/) (read-char *tex-input-port*) 'italic-space)
	   ((#\=) (read-char *tex-input-port*)
	    (set! *column* (+ -1 *column*))
	    (vector-set! tabs *tab-index* *column*)
	    ;;(print '= *tab-index* *column*)
	    (set! *tab-index* (+ 1 *tab-index*))
	    #t)
	   ((#\>) (read-char *tex-input-port*)
	    (set! *column* (+ -1 *column*))
	    ;;(print '> *tab-index* *column*)
	    (space-to-tab-stop *tab-index*)
	    #t)
	   ((#\\) (read-char *tex-input-port*)
	    (cond ((char-whitespace? (rc))
		   'bs-bs)
		  (else (tex:error 'non-whitespace 'after-bs-bs))))
	   ((#\  #\newline)
	    (case *previous-char*
	      ((#\.) (display "@:" *txi-output-port*)))
	    (display (read-char *tex-input-port*) *txi-output-port*)
	    #t)
	   ((#\#) (read-char *tex-input-port*)
	    (display #\#;;(if (member "scheme" *begin-stack*) #\# "@pounds{}")
		     *txi-output-port*)
	    #t)
	   ((#\{ #\} #\-) (fprintf *txi-output-port* "@%c"
				   (read-char *tex-input-port*)) #t)
	   ((#\_ #\,) (read-char *tex-input-port*)
	    (display #\  *txi-output-port*) #t)
	   ((#\;) (rc) (display "  " *txi-output-port*) #t)
	   ((#\:) (rc) (set! *column* (+ 1 *column*))
	    (display "-->" *txi-output-port*) #t)
	   ((#\` #\| #\' #\" #\$ #\& #\%)
	    (display (read-char *tex-input-port*) *txi-output-port*) #t)
	   (else (tex:warn (string #\\ (pc)))
		 (display (read-char *tex-input-port*) *txi-output-port*)
		 #t)))))
(define (tex:input cmd me)
  (let ((name (capture-argument)))
    (case (string->symbol name)
      ((first)
       (output-text-lines "@c @include{first}" "@titlepage" ""
			  "@c HTML first page" "@title Scheme")
       (fprintf *txi-output-port*
	"@subtitle Revised(%c) Report on the Algorithmic Language Scheme\\n"
	(string-ref *input-basename* 1))
       (fluid-let
	   ((vet-node-name #f)
	    (*html* #t)
	    (*tex-rules*
	     (append (rule 'chapter* node 'unnumbered)
		     (rule `huge
			   (lambda (cmd me) (capture-argument) #f))
		     (rule (string->symbol "Huge") "")
		     (rule 'vskip disappear)
		     (rule '$$ "")
		     *tex-rules*)))
	 (process-rrrs-file name))
       (output-text-lines "@end titlepage" ""
			  "@c INFO first page"
			  "@ifinfo" "")
       (fluid-let
	   ((*tex-rules*
	     (append
	      (rule `huge node 'top
		    collapse-spaces
		    (rule '(bs-bs #\newline #\^ bf) "")
		    (rule 'vskip
			  (lambda (cmd me) (rl) #t)))
	      (rule (string->symbol "Huge") "")
	      (rule 'chapter* node 'majorheading)
	      (rule 'tableofcontents
		    (lambda (cmd me)
		      (define nod (assoc "top" *previous-nodes*))
		      (output-text-lines "@unnumbered Contents")
		      (and nod (emit-menu! nod)) #t))
	      *tex-rules*)))
	 (process-rrrs-file name))
       (output-text-lines "@end ifinfo" ""))
      ((commands)
       (fprintf (current-error-port) "...Skipping \"%s.tex\"\\n" name))
      ((sem)
       (fprintf (current-error-port) "...Skipping \"%s.tex\"\\n" name)
       (emit-node! 'section "Formal semantics")
       (define-label "formalsemanticssection")
       (output-text-lines "" ""
"This section provides a formal denotational semantics for the primitive"
"expressions of Scheme and selected built-in procedures.  The concepts"
"and notation used here are described in @sc{[Stoy77]}."
""
"@quotation"
"@emph{Note:} The formal semantics section was written in La@TeX{} which"
"is incompatible with @TeX{}info.  See the Formal semantics section: "
(sprintf #f "@url{http://swissnet.ai.mit.edu/~jaffer/%s-formal.pdf}"
	 *input-basename*)
"@end quotation" ""
			  ))
      ((index) (output-text-lines "@ifinfo"
				  "@unnumberedsec Concepts"
				  "@end ifinfo"
				  "@printindex cp" "@page"
				  "@ifinfo"
				  "@unnumberedsec Procedures"
				  "@printindex fn"
				  "@end ifinfo"))
      (else
       (fprintf *txi-output-port* "@c @include{%s}\\n" name)
       (process-rrrs-file name))))
  #f)
;;;; Texinfo nodes

(define *new-nodes* '())
(define *previous-nodes* '())
(define *node-stack* '())
(define (node-rank type)
  (case (if (symbol? type) type (string->symbol type))
    ((top) 1)
    ((chapter unnumbered appendix) 3)
    ((majorheading chapheading) 4)
    ((section unnumberedsec appendixsec) 5)
    ((heading) 6)
    ((subsection unnumberedsubsec appendixsubsec) 7)
    ((subheading) 8)
    ((subsubsection unnumberedsubsubsec appendixsubsubsec) 9)
    ((subsubheading) 10)
    (else (tex:error 'unknown-node-type type))))
(define (find-previous-node name rank stack)
  (cond ((null? stack) "(dir)")
	((= (cadar stack) rank)
	 (if (caddar stack) (tex:error 'previous-already-set (car stack)))
	 (set-car! (cddar stack) name)
	 (caar stack))
	((< (cadar stack) rank)
	 (cond ((equal? "top" (caar stack)) (set-car! (cddar stack) name)))
	 (caar stack))
	(else (find-previous-node name rank (cdr stack)))))
(define (find-parent-node name rank stack)
  (cond ((null? stack) "(dir)")
	((< (cadar stack) rank)
	 (set-car! (last-pair (car stack))
		   (append (car (last-pair (car stack))) (list name)))
	 (caar stack))
	(else (find-parent-node name rank (cdr stack)))))
(define (update-stack rank nod stack)
  (cond ((null? stack)
	 (if (not (eqv? 1 rank))
	     (tex:error 'null-stack-with-non-zero-rank? rank nod))
	 (list nod))
	((< rank (cadar stack))
	 (update-stack rank nod (cdr stack)))
	(else (cons nod stack))))
(define (vet-node-name cmd name)
  (if (not (symbol? cmd)) (tex:error 'vet-node-name 'symbol? cmd))
  (cond ((substring? "Appendix: " name)
	 (set! name (substring name (+ (string-length "Appendix: ")
				       (substring? "Appendix: " name))
			       (string-length name))))
	((substring? "index " name) (set! name  "Index"))
	((eq? 'top cmd)
	 (newline *txi-output-port*)
	 (set! *column* 0)
	 (set! name "top")))
  (cond ((not (assoc name *new-nodes*))
	 (let ((rank (node-rank cmd)))
	   (if (odd? rank)
	       (let ((nod (list name
				rank
				#f
				(find-previous-node name rank *node-stack*)
				(find-parent-node name rank *node-stack*)
				'())))
		 (set! *new-nodes* (cons nod *new-nodes*))
		 (set! *node-stack* (update-stack rank nod *node-stack*)))))
	 name)
	((eq? 'top cmd)
	 (tex:error 'multiple-top-nodes? cmd name *name*))
	((eqv? #\s (string-ref name (+ -1 (string-length name))))
	 (vet-node-name cmd (substring name 0 (+ -1 (string-length name)))))
	(else
	 (vet-node-name
	  cmd
	  (string-append
	   name (if (eqv? #\  (string-ref name (+ -2 (string-length name))))
		    "I" " I"))))))
(define (emit-node! cmd name)
  (set! *name* (if vet-node-name (vet-node-name cmd name) name))
  (fprintf (current-error-port) " %s \"%s\"\\n" cmd *name*)
  (if vet-node-name
      (let ((nod (assoc *name* *previous-nodes*)))
	(cond ((not nod)
	       (fprintf *txi-output-port* "@%s %s\\n" cmd name))
	      (else
	       (fprintf *txi-output-port* "@node %s, %s, %s, %s\\n"
			*name*
			(or (caddr nod) " ") (or (cadddr nod) " ")
			(list-ref nod 4))
	       (fprintf *txi-output-port* "@%s %s\\n" cmd name)
	       (cond ((and (not (eqv? 'top cmd)) (not (null? (list-ref nod 5))))
		      (emit-menu! nod))))))
      (fprintf *txi-output-port* "@%s %s\\n" cmd name))
  (set! *index-memory* '())
  (if (not (eqv? 'top cmd)) (make-index-entry name "cindex @w{" "}")))
(define (emit-menu! nod)
  (fprintf *txi-output-port* "\\n@menu\\n")
  (for-each (lambda (menu-line)
	      (fprintf *txi-output-port* "* %-30s\\n"
		       (string-append menu-line "::  ")))
	    (list-ref nod 5))
  (fprintf *txi-output-port* "@end menu\\n"))
(define (node cmd me alias . ruls)
  (define ans #f)
  (let ((name (call-with-output-string
	       (lambda (oport)
		 (fluid-let ((*txi-output-port* oport)
			     (*tex-rules* (append (rule 'tt "")
						  (rule #\, ";")
						  (apply rules ruls)
						  *tex-rules*)))
		   (set! ans (process-argument)))))))
    (cond ((string-ci=? "Contents" name) ans)
	  (else (emit-node! (or alias cmd) name)
		ans))))

(define setboxes (make-vector 10 ""))
(define (setbox cmd me)
  (let* ((idx (string->number (string (read-char *tex-input-port*))))
	 (expn (call-with-output-string
		(lambda (oport)
		  (fluid-let ((*txi-output-port* oport))
		    (process-one #t))))))
    (vector-set! setboxes idx expn)
    #t))
(define (copy-box cmd me)
  (let* ((idx (string->number (string (read-char *tex-input-port*)))))
    (display (vector-ref setboxes idx) *txi-output-port*)
    #t))

;;;; Rule functions

(define (unmatched-close chr me) (tex:error 'unmatched chr))
(define (tex:close chr me . lines)
  (cond ((not (eqv? chr *closer*))
	 (tex:error 'close-mismatch chr 'should-be *closer*)))
  (case chr ((#\})
	     (set! *brace-depth* (+ -1 *brace-depth*))
	     (set! *column* (+ -1 *column*))))
  (if (= 1 (length lines))
      (fprintf *txi-output-port* (car lines))
      (apply output-text-lines lines))
  #f)
(define (scheme-mode cmd me closer . ruls)
  (define post
    (cond (*entry-type*
	   (fprintf *txi-output-port* "\\n@format\\n@t{")
	   (case (pc)
	     ((#\newline)
	      (read-char *tex-input-port*)
	      (set! *column* 0)))
	   "}\\n@end format\\n")
	  (else
	   (fprintf *txi-output-port* "\\n@example\\n")
	   "\\n@end example\\n")))
  (if (member "itemize" *begin-stack*)
      (set-tabs!  5 35)
      (set-tabs! 10 40))
  (fluid-let ((*closer* closer)
	      (*tex-rules* (append (rule closer tex:close post)
				   (apply rules ruls)
				   *tex-rules*)))
    (process-tex-input #t))
  (close-parens)
  #t)

(define (commentize cmd me . ruls)
  (fprintf *txi-output-port* "@c \\\\%s%s\\n" cmd (rl)) #t)
(define (continue-line chr me)
  (case (pc)
    ((#\newline) (rc)
     (do () ((not (or (eqv? (pc) #\ )
		      (eqv? (pc) slib:tab))))
       (read-char *tex-input-port*)))
    (else
     (if (not (= 1 *column*)) (newline *txi-output-port*))
     (fprintf *txi-output-port* "@c %s\\n" (rl))))
  #t)
  
(define (disappear cmd me . ruls)
  (rl)
  #\newline)

(define (postfix cmd me suffix)
  (set! *column* (+ -1 *column*))
  (let ((ans (process-argument)))
    (fprintf *txi-output-port* suffix)
    (set! *column* (+ *column* (string-length suffix)))
    ans))

(define (encapsulate from me to . ruls)
  (set! *column* (+ -1 *column*)) ;for \ already read.
  (fprintf *txi-output-port* "@%s{" to)
  (if (null? ruls)
      (let ((ans (process-argument)))
	(fprintf *txi-output-port* "}")
	ans)
      (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	(let ((ans (process-argument)))
	  (fprintf *txi-output-port* "}")
	  ans))))

(define (process-argument-with-rules cmd me . ruls)
  (define post #f)
  (cond ((and (not (null? ruls)) (not (list? (car ruls))))
	 (fprintf *txi-output-port* (car ruls))
	 (set! *column* (+ -1 *column*)) ;for \ already read.
	 (bump-column (car ruls))
	 (set! ruls (cdr ruls))
	 (cond ((and (not (null? ruls)) (not (list? (car ruls))))
		(set! post (car ruls))
		(set! ruls (cdr ruls))))
	 (let ((ans (if (null? ruls)
			(process-argument)
			(fluid-let ((*tex-rules*
				     (append (apply rules ruls) *tex-rules*)))
			  (process-argument)))))
	   (cond (post (fprintf *txi-output-port* post)
		       (bump-column post)))
	   ans))
	((null? ruls) (process-argument))
	(else (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
		(process-argument)))))

;;;; The Rules

(define collapse-spaces
  (rule (list slib:tab #\ )
	(lambda (chr me)
	  (display #\  *txi-output-port*)
	  (do ((ch (pc) (pc)))
	      ((not (or (eqv? ch #\ )
			(eqv? ch slib:tab)))
	       #t)
	    (read-char *tex-input-port*)))))
(define compress-top-level-spaces
  (rule (list slib:tab #\ )
	(lambda (chr me)
	  (cond ((zero? *paren-depth*)
		 (display #\  *txi-output-port*)
		 (do ((ch (pc) (pc)))
		     ((not (or (eqv? ch #\ )
			       (eqv? ch slib:tab))))
		   (read-char *tex-input-port*)))
		((eqv? slib:tab chr)
		 (space-to-column (+ (- 8 (modulo *column* 8)) *column*)))
		(else (display chr *txi-output-port*)))
	  #t)))
(define *tex-rules*
  (rules
   (rule #\~ #\ )
   (rule #\@ "@@" 1)
   (rule #\% (lambda (chr me)
	       (let* ((col *column*) (line (rl)))
		 (cond ((string-whitespace? line)
			(if (eqv? #\space *previous-char*) ;(= 1 col)
			    #t #\newline))
		       (else
			(if (not (= 1 col)) (newline *txi-output-port*))
			(fprintf *txi-output-port* "@c %s\\n" line)
			#t)))))
   (rule #\{ (lambda (chr me . ruls)
	       (case (pc)
		 ((#\})
		  (read-char *tex-input-port*)
		  (set! *column* (+ -1 *column*))
		  #t)
		 ((#\\)
		  (read-char *tex-input-port*)
		  (read-bs-command #t))
		 (else
		  (fluid-let ((*tex-rules*
			       (append (apply rules ruls) *tex-rules*))
			      (*brace-depth* (+ 1 *brace-depth*))
			      (*closer* #\}))
		    (process-tex-input #t)
		    #t))))
	 (rule #\} tex:close))
   (rule #\} unmatched-close)
   (rule #\] unmatched-close)
   (rule #\\ (lambda (chr me) (read-bs-command #f)))
   (rule #\$ (lambda (chr me)
	       (case (pc)
		 ((#\$) (read-char *tex-input-port*) '$$)
		 (else '$))))
   (rule slib:tab
	 (lambda (chr me)
	   (set! *column* (+ -1 *column*))
	   (space-to-column (+ (- 8 (modulo *column* 8)) *column*))
	   #t))
   (rule #\newline tex:newline)

   (rule '($ $$) (lambda (tok me . ruls)
		   (fluid-let ((*tex-rules* (append (rule tok tex:close)
						    (apply rules ruls)
						    *tex-rules*))
			       (*closer* tok))
		     (case tok
		       (($$)
			(fprintf *txi-output-port* "\\n\\n@center ")
			(process-tex-input #\newline)
			#\newline)
		       (($) (process-tex-input #t)
			#t))))
	 (rule #\newline "")
	 (rule 'sqrt "sqrt")
	 (rule 'sin "sin")
	 (rule 'cos "cos")
	 (rule 'tan "tan")
	 (rule 'log "log")
	 (rule 'pi "pi")
	 (rule 'over "/")
	 (rule 'bf process-argument-with-rules "(" ")"))

   (rule 'char
	 (lambda (cmd me)
	   (let ((chr (read-char *tex-input-port*)))
	     (display
	      (escape-special
	       (integer->char
		(case chr
		  ((#\')
		   (let* ((c1 (read-char *tex-input-port*))
			  (c2 (rc)))
		     (string->number (string c1 c2) 8)))
		  ((#\")
		   (let* ((c1 (read-char *tex-input-port*))
			  (c2 (rc)))
		     (string->number (string c1 c2) 16)))
		  (else
		   (tex:error 'char-argument-not-understood chr)))))
	      *txi-output-port*)
	     #t)))

   (rule 'multicolumn
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (let* ((arg1 (capture-argument))
		    (arg2 (capture-braced-expression)))
	       (fprintf *txi-output-port* "\\n@center ")
	       (process-braced-expression))))
	 (rule #\newline ""))
   (rule 'begin-center
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*))
		       (*closer* 'end-center))
	     (fprintf *txi-output-port* "\\n@center ")
	     (process-tex-input #t)
	     #\newline))
	 collapse-spaces
	 (rule 'end-center tex:close)
	 (rule #\newline "\\n@center "))
   (rule 'extracolsep (lambda (cmd me) (capture-argument) #f))
   (rule 'verb
	 (lambda (cmd me)
	   (let ((closer (read-char *tex-input-port*)))
	     (do ((chr (rc)
		       (read-char *tex-input-port*)))
		 ((or (eqv? chr closer) (eof-object? chr)) #t)
	       (display (escape-special chr) *txi-output-port*)))))

   ;; These commands translate simply.
   (rule (string->symbol "TeX") "@TeX{}" 3)
   (rule 'sharpfoo process-argument-with-rules "@t{#" "}")
   (rule 'cite process-argument-with-rules "[" "]"
	 (rule #\, "], ["))
   (rule 'schfalse "@t{#f}" 2)
   (rule 'schtrue "@t{#t}" 2)
   (rule 'unspecified "@emph{unspecified}" 10)
   (rule 'scherror "@emph{error}" 5)
   (rule 'semantics "@emph{Semantics:}" 10)
   (rule 'syntax "@emph{Syntax:}" 7)
   (rule 'exprtype "syntax")
   (rule 'singlequote "@t{'}" 1)
   (rule 'backquote "@t{`}" 1)
   (rule 'backwhack "\\\\" 1)
   (rule 'atsign "@@" 1)
   (rule 'sharpsign "#")
   (rule 'prime "^")
   (rule '(dots dotsfoo) "@dots{}" 3)
   (rule 'makeindex "")
   ;;(rule 'ae "@ae{}" 2)
   (rule 'ae "ae" 2)
   (rule 'le "<=")
   (rule 'leq "<=")
   (rule 'neq "~=")
   (rule 'langle "<")
   (rule 'rangle ">")
   (rule 'cdot ".")
   (rule 'ldots "@dots{}" 3)
   (rule 'vdots "@dots{}" 3)
   ;;(rule 'ev "@result{}")
   (rule '(goesto evalsto) "@result{}" 3)
   ;;(rule 'lev "@expansion{}")		;never invoked!
   (rule 'break "@*")
   (rule '(hfil hfill vfill par nobreak qquad
		unsection footnotesize tableofcontents) "")

   ;; These come with {} after them.
   (rule (string->symbol "Lambdaexp") "lambda expression")
   (rule 'lambdaexp "lambda expression")
   (rule 'nopagebreak "")
   (rule 'doublequote "\"")
   (rule 'coerce "->")

   ;; These begin lines
   (rule '(newpage eject) "\\n@page\\n" -1)
   (rule 'medskip "@sp 3")
   (rule 'bigskip "@sp 6")
   (rule 'noindent "\\n@noindent\\n" -1)
   (rule 'item
	 (lambda (cmd me)
	   (case (pc)
	     ((#\ )
	      (read-char *tex-input-port*)
	      (fprintf *txi-output-port* "@item\\n")
	      (set! *column* 0))
	     (else
	      (fprintf *txi-output-port* "@item ")
	      (set! *column* 3)))
	   #t))

   ;; These occur as {\tt ...}
   (rule 'tt encapsulate 't)
   (rule 'rm encapsulate 'r)
   (rule 'it encapsulate 'i)
   (rule 'itshape encapsulate 'i)
   (rule 'italic-space "")		;\/
   (rule 'bf encapsulate 'b)
   (rule 'mathbf 'bf)
   (rule 'sc encapsulate 'sc)
   (rule 'authorsc encapsulate 'sc)
   (rule 'em encapsulate 'emph)
   (rule 'cf (lambda (cmd me)
	       (define ans #f)
	       (let ((arg (call-with-output-string
			   (lambda (oport)
			     (fluid-let ((*txi-output-port* oport))
			       (set! ans (process-argument)))))))
		 (fprintf *txi-output-port* "@%s{%s}"
			  (if (substring? "http://" arg) 'url 'samp) arg)
		 ans)))
   (rule 'hbox (lambda (cmd me)
		 (case (pc)
		   ((#\ )
		    (do ((ch (pc) (pc)))
			((eqv? #\\ ch))
		      (read-char *tex-input-port*))
		    (space-to-column (+ 7 *column*))))
		 (fprintf *txi-output-port* "@w{")
		 (let ((ans (process-argument)))
		   (fprintf *txi-output-port* "}")
		   ans)))
   (rule 'wd (lambda (cmd me)
	       (read-char *tex-input-port*)
	       (space-to-column (+ 8 *column*))
	       #t))

   ;; These occur as \vr{...}
   (rule 'vr encapsulate 'var)
   (rule 'var encapsulate 'var)
   (rule 'type encapsulate 'i)
   (rule 'tupe encapsulate 'r)
   ;;(rule 'meta encapsulate 'r)
   (rule 'meta process-argument-with-rules "<" ">")

   (rule 'displaystyle postfix "")

   (rule 'begin-scheme
	 scheme-mode
	 'end-scheme
	 ;;compress-top-level-spaces
	 (rule 'sharpfoo process-argument-with-rules "#")
	 (rule 'schtrue "#t")
	 (rule 'schfalse "#f")
	 (rule 'ev (lambda (cmd me) (close-parens)
			   (space-to-tab-stop 1)
			   (fprintf *txi-output-port* "==>")
			   #t))
	 (rule 'lev (lambda (cmd me) (close-parens)
			    (newline *txi-output-port*)
			    (set! *column* 0)
			    (space-to-tab-stop 0)
			    (fprintf *txi-output-port* "==>")
			    #t))
	 (rule #\# "#")
	 (rule 'ide ""))
   (rule 'begin-schemenoindent		;used for example.tex and derive.tex
	 scheme-mode
	 'end-schemenoindent
	 (rule 'iet " == ")
	 (rule 'schtrue "#t")
	 (rule 'schfalse "#f")
	 (rule 'hyper process-argument-with-rules "<" ">")
	 (rule #\# "#"))
   (rule 'begin-grammar
	 (lambda (cmd me . ruls)
	   (set-tabs! 3 6)
	   (fluid-let ((*closer* 'end-grammar)
		       (*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (fprintf *txi-output-port* "\\n@format\\n@t{")
	     (process-tex-input #t))
	   #t)
	 (rule 'end-grammar tex:close "}" "" "@end format" "")
	 (rule 'sharpfoo process-argument-with-rules "#")
	 (rule 'schtrue "#t")
	 (rule 'schfalse "#f")
	 (rule #\  (lambda (chr me)
		     ;;(print 'space *column*)
		     (set! *column* (+ -1 *column*))
		     (do ((col (+ 1 *column*))
			  (ch (pc) (pc)))
			 ((not (or (eqv? ch #\ )
				   (eqv? ch slib:tab)))
			  (case (pc)
			    ((#\\)
			     (rc)
			     (case (pc)
			       ((#\>) (read-bs-command #f))
			       (else (space-to-column (+ 1 col))
				     (read-bs-command #f))))
			    (else (space-to-column col) #t)))
		       (case (read-char *tex-input-port*)
			 ((#\ ) (set! col (+ 1 col)))
			 (else (tex:warn 'saw-tab)
			       (set! col (+ (- 8 (modulo col 8)) col)))))))
	 (rule #\% continue-line)
	 (rule #\# "#"))
   (rule '(begin-note begin-rationale)
	 (lambda (cmd me)
	   (fprintf *txi-output-port* "\\n@quotation\\n@emph{%s:}"
		    (let ((str (symbol->string cmd)))
		      (set! str (substring str 6 (string-length str)))
		      (string-set! str 0 (char-upcase (string-ref str 0)))
		      str))
	   #t))
   (rule 'begin-tabbing "\\n@format")
   (rule 'bs-bs "\\n" -1)
   (rule 'end-tabbing "@end format\\n")
   (rule '(end-note end-rationale) "@end quotation\\n" -1)
   (rule 'begin-entry "")
   (rule 'end-entry
	 (lambda (tok me)
	   (fprintf *txi-output-port* "@end %s" *entry-type*)
	   (set! *entry-type* #f)
	   #t))
   (rule 'unpenalty
	 (lambda (tok me)
	   (fprintf *txi-output-port* "\\n@end %s" *entry-type*)
	   (set! *entry-type* #f)
	   #t))
   (rule 'begin-itemize
	 (lambda (cmd me . ruls)
	   (fluid-let ((*closer* 'end-itemize)
		       (*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (output-text-lines "" "" "@itemize @bullet")
	     (process-tex-input #t)
	     #t))
	 (rule #\newline
	       (lambda (chr me)
		 (newline *txi-output-port*)
		 (set! *column* 0)
		 (do ((col 0)
		      (ch (pc) (pc)))
		     ((not (or (eqv? ch #\ )
			       (eqv? ch slib:tab)))
		      (case (pc)
			((#\( #\)) (space-to-column col) #t)
			(else #t)))
		   (case (read-char *tex-input-port*)
		     ((#\ ) (set! col (+ 1 col)))
		     (else (set! col (+ (- 8 (modulo col 8)) col)))))))
	 (rule 'bs-bs "\\n@item " -1)
	 (rule 'end-itemize tex:close "\\n@end itemize\\n"))
   (rule 'begin-description "\\n@table @t\\n" -1)
   (rule 'begin-tabular
	 (lambda (cmd me)
	   (define numcols
	     (string-length
	      (fluid-let ((*tex-rules* (append (rule #\@ "")
					       (rule 'bs-bs "\\n" -1)
					       *tex-rules*)))
		(capture-braced-expression))))
	   (fprintf *txi-output-port* "@c %s\\n" cmd)
	   (fprintf *txi-output-port* "@quotation\\n@table @asis\\n@item ")
	   (fluid-let
	       ((*closer* 'end-tabular)
		(*tex-rules* (append (rule 'end-tabular tex:close
					   "" "@end table"
					   "@end quotation" "")
				     (rule #\& "\\n")
				     (rule #\newline "")
				     (rule 'bs-bs "\\n@item " -1)
				     *tex-rules*)))
	     (process-tex-input #t)
	     #t)))
   (rule 'end-description "@end table\\n" -1)
   (rule 'end-thebibliography "@end itemize\\n" -1)
   (rule 'begin-thebibliography
	 (lambda (cmd me)
	   (emit-node! 'unnumbered "Bibliography")
	   (output-text-lines "" "" "@itemize @bullet")
	   (fprintf *txi-output-port* "@c ")
	   (fluid-let ((*tex-rules* (append (rule 'cite encapsulate 'cite)
					    *tex-rules*)))
	     (process-braced-expression)
	     #\newline)))
   (rule 'begin-theindex
	 (lambda (tok me)
	   (fprintf (current-error-port) "...Indexing\\n")
	   (emit-node! 'unnumbered
"Alphabetic index of definitions of concepts, keywords, and procedures")
	   #\newline))
   (rule 'end-theindex
	 "@ifinfo\\n@unnumberedsec References\\n@printindex pg\\n@end ifinfo\\n"
	 )
   (rule 'end-theindex
	 (lambda (tok me)
	   (output-text-lines "@ifinfo"
			      "@unnumberedsec References"
			      "@printindex pg"
			      "@end ifinfo" "")
	   #\newline))
   (rule 'begin-document
	 (lambda (tok me)
	   (output-text-lines ;;"@setchapternewpage on"
			      "@paragraphindent 0"
			      "@c %**end of header"
			      "@iftex"
			      "@syncodeindex fn cp"
			      "@syncodeindex vr cp"
			      "@end iftex" ""
			      "@ifinfo"
			      "@dircategory The Algorithmic Language Scheme"
			      "@direntry")
	   (fprintf *txi-output-port*
		    "* %s: (%s).         The Revised(%c) Report on Scheme.\\n"
		    (string-upcase *input-basename*)
		    *input-basename*
		    (string-ref *input-basename* 1))
	   (output-text-lines "@end direntry" "@end ifinfo")
	   #t))
   (rule 'end-document
	 (lambda (tok me)
	   (output-text-lines "@contents" "@bye")
	   (cond ((not (null? *begin-stack*))
		  (tex:error '*begin-stack* 'not-empty *begin-stack*))
		 ((not (zero? *brace-depth*))
		  (tex:error '*brace-depth* 'not-zero *brace-depth*)))
	   #f))

   ;; \-commands which take arguments
   ;; but aren't simple substitutions.
   (rule 'vest (lambda (cmd me)
		 (case (pc)
		   ((#\ ) (read-char *tex-input-port*)))
		 #t))
   (rule 'begin
	 (lambda (cmd me)
	   (let* ((name (capture-argument))
		  (tok (string->symbol (string-append "begin-" name))))
	     (set! *begin-stack* (cons name *begin-stack*))
	     (cond ((assv tok *tex-rules*) tok)
		   (else (tex:warn tok 'not-recognized) #f)))))
   (rule 'end
	 (lambda (cmd me)
	   (let* ((name (capture-argument))
		  (tok (string->symbol (string-append "end-" name))))
	     (cond ((null? *begin-stack*)
		    (tex:error 'end name 'begin-stack-empty))
		   ((not (equal? name (car *begin-stack*)))
		    (tex:error 'end name 'doesn't-match
			       'begin (car *begin-stack*)))
		   (else (set! *begin-stack* (cdr *begin-stack*))))
	     (cond ((assv tok *tex-rules*) tok)
		   (else (tex:warn tok 'not-recognized) #f)))))
   (rule 'arbno postfix "*")		;was "@r{*}"
   (rule 'atleastone postfix "+")	;was "@r{+}"
   (rule 'hyper process-argument-with-rules "@r{<" ">}")
   (rule '(todo nodomain)
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (cond ((not (eqv? #\newline *previous-char*))
		    (tex:newline #f #f)))
	     (fprintf *txi-output-port* "@ignore %s\\n" cmd)
	     (process-argument)
	     (fprintf *txi-output-port* "\\n@end ignore\\n")
	     #t))
	 (rule #\[ "[")
	 (rule #\] "]")
	 (rule 'begin "\\begin")
	 (rule 'end "\\end")
	 (rule '(subsection subsection*) ""))
   (rule 'domain (lambda (cmd me) (process-argument) #t))

   (rule 'vari process-argument-with-rules "@var{" "1}")
   (rule 'varii process-argument-with-rules "@var{" "2}")
   (rule 'variii process-argument-with-rules "@var{" "3}")
   ;;(rule 'variv process-argument-with-rules "@var{" "4}")

   (rule 'vri process-argument-with-rules "@var{" "1}")
   (rule 'vrii process-argument-with-rules "@var{" "2}")
   (rule 'vriii process-argument-with-rules "@var{" "3}")
   (rule 'vriv process-argument-with-rules "@var{" "4}")

   (rule 'hyperi process-argument-with-rules "@r{<" "1>}")
   (rule 'hyperii process-argument-with-rules "@r{<" "2>}")
   (rule 'hyperiii process-argument-with-rules "@r{<" "3>}")
   (rule 'hyperiv process-argument-with-rules "@r{<" "4>}")
   (rule 'hypern process-argument-with-rules "@r{<" "_n>}")

   (rule 'index rule:index "cindex @w{" "}")
   (rule 'mainindex rule:index "cindex @w{" "}")
   (rule 'mainschindex rule:index "cindex @w{" "}")
   (rule 'schindex rule:index "vindex " "")
   (rule 'sharpindex rule:index "vindex #" "")
   (rule 'ide
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf *txi-output-port* "@code{%s}" name)
	     (make-index-entry name "vindex @w{" "}"))
	   #f))
   (rule 'defining
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf *txi-output-port* "@dfn{%s}" name)
	     (make-index-entry name "cindex @w{" "}"))
	   #f))
   (rule 'bibitem
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf *txi-output-port* "@item [%s]" name)
	     (make-index-entry name "pindex " ""))
	   #f))
   (rule 'foo
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf
	      *txi-output-port*
	      "@var{%s}, @var{%s1}, @dots{} @var{%sj}, @dots{}"
	      name name name))
	   #f))
   (rule 'clearextrapart node 'unnumbered)
   (rule 'extrapart node 'unnumbered)
   (rule 'subsection* node 'unnumberedsec)
   (rule '(chapter section subsection) node #f)
   (rule 'label (lambda (cmd me) (define-label (capture-argument)) #f))
   (rule #\( (lambda (chr me) (set! *paren-depth* (+ 1 *paren-depth*))
		     (display chr *txi-output-port*) #t))
   (rule #\) (lambda (chr me) (set! *paren-depth* (+ -1 *paren-depth*))
		     (display chr *txi-output-port*) #t))
   (rule 'ref (lambda (cmd me)
		(let ((name (label->name (capture-argument))))
		  (cond ((positive? *paren-depth*)
			 (fprintf *txi-output-port* "@pxref{%s}" name))
			(else (fprintf *txi-output-port* "@ref{%s}" name)))
		  #f)))
   (rule '(proto rproto)
	 (lambda (cmd me . ruls)
	   (let* ((proc (capture-argument))
		  (args (fluid-let ((*tex-rules*
				     (append (apply rules ruls) *tex-rules*)))
			  (capture-braced-expression)))
		  (type (capture-braced-expression)))
	     (fprintf *txi-output-port* "@%s {%s} %s %s"
		      (if *entry-type* "deffnx" "deffn")
		      (if (equal? "" type) "syntax" type) proc args)
	     (case cmd ((proto) (define-label proc)))
	     (set! *entry-type* "deffn"))
	   #f)
	 (rule '(vari vri) postfix "1")
	 (rule '(varii vrii) postfix "2")
	 (rule '(variii vriii) postfix "3")
	 (rule 'vriv postfix "4")
	 (rule 'hyperi process-argument-with-rules "<" "1>")
	 (rule 'hyperii process-argument-with-rules "<" "2>")
	 (rule 'hyperiii process-argument-with-rules "<" "3>")
	 (rule 'hyperiv process-argument-with-rules "<" "4>")
	 (rule 'hypern process-argument-with-rules "<" "_n>"))
   (rule 'vproto
	 (lambda (cmd me)
	   (let* ((var (capture-argument))
		  (type (capture-braced-expression)))
	     (fprintf *txi-output-port* "@%s {%s} %s"
		      (if *entry-type* "defvrx" "defvr") type var)
	     (define-label var)
	     (set! *entry-type* "defvr"))
	   #f))
   (rule 'pproto			; like proto without args
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (let* ((proc (capture-argument))
		    (type (capture-braced-expression)))
	       (if *entry-type*
		   (fprintf *txi-output-port* "\\n@deffnx {%s} %s" type proc)
		   (fprintf *txi-output-port* "@deffn {%s} %s" type proc))
	       (extract-do-label proc)
	       (set! *entry-type* "deffn")
	       ;;(process-argument)
	       #\newline
	       )))
	 (rule 'tt ""))
   (rule 'obeyspaces process-argument-with-rules
	 (rule '(bs-bs #\newline #\%) "")
	 collapse-spaces)
   (rule 'vspace
	 (lambda (cmd me)
	   (let ((arg (capture-argument)))
	     (set! arg (string->number (substring arg 0 (substring? "ex" arg))))
	     (fprintf *txi-output-port* "@sp %d" (abs arg))
	     #f)))
   (rule 'vskip
	 (lambda (cmd me)
	   (let* ((line (rl))
		  (linl (string-length line)))
	     (do ((idx 0 (+ 1 idx)))
		 ((or (eqv? idx linl)
		      (not (char-whitespace? (string-ref line idx))))
		  (cond ((or (eqv? idx linl)
			     (not (or (substring? "ex" line)
				      (substring? "pt" line))))
			 (tex:error 'vskip-not-understood: line)))
		  (set! line (substring line idx
					(or (substring? "ex" line)
					    (substring? "pt" line))))))
	     (set! linl (string->number line))
	     (cond (linl (fprintf *txi-output-port* "\\n@sp %d\\n"
				  (inexact->exact linl)))
		   (else (tex:error 'vskip-number-not-understood: line))))
	   #\newline))

   (rule 'input tex:input)
   (rule 'setbox setbox)
   (rule 'copy copy-box)
   ;; R5RS additional symbols.
   (rule 'integerversion "5")
   (rule 'callcc "@t{call-with-current-continuation}" 30)
   ;; \-commands{...} which turn into single-line comments.

   (rule '(newcommand pagestyle thispagestyle clearchapterstar topmargin
		      headsep textheight textwidth columnsep columnseprule
		      parskip parindent topsep oddsidemargin evensidemargin
		      addvspace renewcommand tocshrink def showboxdepth)
	 commentize
	 (rule 'type "\\\\type" 5))

   ;; R7RS additional symbols
   (rule 'appendix "@appendix" 0)
   (rule 'auxiliarytype "auxiliary syntax")
   (rule 'comma "@t{,}" 1)
   (rule 'commaatsign "@t{,@@}" 2)
   (rule 'feature (lambda (cmd me . ruls)
                    (fprintf *txi-output-port* "\\n@table @t\\n@item ")
                    (process-braced-expression)
                    (fprintf *txi-output-port* "\\n")
                    (process-braced-expression)
                    (fprintf *txi-output-port* "\\n@end table\\n")
                    #t))
   (rule 'mathit process-argument-with-rules "@i{" "}")
   (rule 'mathrm process-argument-with-rules "@r{" "}")
   (rule 'rfivers "R@sup{5}RS" 5)
   (rule 'rfourrs "R@sup{4}RS" 5)
   (rule 'rsevenrs "R@sup{7}RS" 5)
   (rule 'rsixrs "R@sup{6}RS" 5)
   (rule 'rthreers "R@sup{3}RS" 5)
   (rule 'sharpbangindex rule:index "vindex #!" "")
   (rule 'sharpfalse "@t{#false}" 6)
   (rule 'sharptrue "@t{#true}" 5)
   (rule 'textbf process-argument-with-rules "@b{" "}")
   (rule 'textit process-argument-with-rules "@i{" "}")
   (rule 'textrm process-argument-with-rules "@r{" "}")
   (rule 'texttt process-argument-with-rules "@t{" "}")
   (rule 'url process-argument-with-rules "@url{" "}")
   (rule 'verticalbar "@t{|}" 5)
   ))

(define t (lambda args (apply tex->txi args) (newline)))

'(begin
  (define r restart)
  (trace-all "rrrs2txi.scm")
  ;;(trace read-char char-whitespace?)
  (untrace rule rules read-bs-token string-whitespace?
	   ;;process-tex-input tex:close process-one
	   tex:warn tex:error tex:errwarn check-brace-depth!))
;;(trace encapsulate process-one process-tex-input tex:close)

(define go
  (let ((me (in-vicinity (program-vicinity) "rrrs2txi")))
    (lambda ()
      (do ((r 7 (+ -1 r))
	   (name "r7rs" (string-append "r" (number->string (+ -1 r)) "rs")))
	  ((or (< r 3) (file-exists? (string-append name ".tex")))
	   (cond ((< r 3)
		  (do ((found? #f)
		       (r 7 (+ -1 r))
		       (base "r7rs" (string-append
				     "r" (number->string (+ -1 r)) "rs")))
		      ((< r 3)
		       (if (not found?) (slib:error 'could-not-find "r?rs.tex"))
		       #t)
		    (let ((vic (sub-vicinity (user-vicinity) base)))
		      (cond ((file-exists?
			      (in-vicinity vic (string-append base ".tex")))
			     (set! found? #t)
			     (load me)
			     (cond ((tex->txi vic base)
				    (txi->info (user-vicinity) base))
				   ((begin (load me) (tex->txi vic base))
				    (txi->info (user-vicinity) base))))))))
		 ((begin (load me) (tex->txi name))
		  (txi->info (user-vicinity) name))
		 ((begin (load me) (tex->txi name))
		  (txi->info (user-vicinity) name))))))))
