
(define-library (local es)
  (import (scheme base))
  (import (scheme file))
  (import (scheme read))
  (import (scheme write))
  (import (chibi system))
  (import (chibi process))
  (import (chibi filesystem))
  (import (srfi 1))
  (import (srfi 33))
  (import (srfi 95))
  (import (srfi 98))
  
  (export 
	
;?	preignition
;?	initialise_interpreter
;?	ctx
;?	env
;?	igor_ctx
;?	igor_env
;?	igor
;?	close_up_shop

;?	eval
;?	load
; display-to-string
; write-to-string
; evaluate-scheme-expression

; term-read history-add history-set-max-length history-save 
; history-load read-from-terminal set-multiline-term-read
	
	;; c routines 
	
	;;sexp-wordexp
	word-expand get-env set-env unset-env

	;; scm-file-stat 
	;; scm-get-stat-dev scm-set-stat-dev! scm-get-stat-ino scm-set-stat-ino!
	;; scm-get-stat-mode scm-set-stat-mode! scm-get-stat-nlink scm-set-stat-nlink!
	;; scm-get-stat-uid scm-set-stat-uid! scm-get-stat-gid scm-set-stat-gid!
	;; scm-get-stat-rdev scm-set-stat-rdev! scm-get-stat-size scm-set-stat-size!
	;; scm-get-stat-blksize scm-set-stat-blksize! scm-get-stat-blocks scm-set-stat-blocks!
	;; scm-get-stat-atime scm-set-stat-atime! scm-get-stat-mtime scm-set-stat-mtime!
	;; scm-get-stat-ctime scm-set-stat-ctime!

;?	file-stat 
	get-stat-dev get-stat-ino
	get-stat-mode get-stat-nlink
	get-stat-uid get-stat-gid
	get-stat-rdev get-stat-size
	get-stat-blksize get-stat-blocks
	get-stat-atime get-stat-mtime
	get-stat-ctime

	;;	argv-to-list

	;; es.scm
	;;; variables
	;;	paren-fence-list
	;;	bracket-fence-list
	;;	brace-fence-list
	;;	quote-fence-list
	;;	igor-token-list

	*igor-report-backgrounding*
	*igor-builtin-list*

	;; functions
	andf orf ++ -- read-all read-all-lines 
	prune-quotes prune-quotes-in-list
	in-range pipe-input show 
	string-index list-head denull denull-and-flatten level depth 
	strspn strcspn
	collapsing-strtok strtok reconstruct-string 
	filter
	*expand-path* expand-path *cross2* *cross*
	string-car string-cdr

	with-input-from-port
	with-output-to-port
	with-error-to-port
	with-io-ports

	acons
	abort

	file-status

	list-ref
	list-set!
	list-set-car!
	list-set-cdr!

	list-tail
	
	strip-fences
	tokenise-string

	*add-builtin*
	;;	*igor-execute-builtin-process*
	;;	*igor-execute-single-process*

	system call cd 

	igor-read-line
	igor-get-command-line

	;; esc
	;; igor-symbol-alist
	;; paren-fence-alist
	;; bracket-fence-alist
	;; brace-fence-alist
	;; quote-fence-alist
	;; igor-token-list
	)

  (include-shared "es")
  (include "es.scm")
  )
