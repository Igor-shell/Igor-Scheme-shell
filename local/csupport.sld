
(define-library (local csupport)
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
  
  eval
  load
;  display-to-string
;  write-to-string
;  evaluate-scheme-expression

;  term-read history-add history-set-max-length history-save 
;  history-load read-from-terminal set-multiline-term-read
	
	;; c routines 
	word-expand get-env set-env unset-env

	file-stat 
	get-stat-dev set-stat-dev! get-stat-ino set-stat-ino!
	get-stat-mode set-stat-mode! get-stat-nlink set-stat-nlink!
	get-stat-uid set-stat-uid! get-stat-gid set-stat-gid!
	get-stat-rdev set-stat-rdev! get-stat-size set-stat-size!
	get-stat-blksize set-stat-blksize! get-stat-blocks set-stat-blocks!
	get-stat-atime set-stat-atime! get-stat-mtime set-stat-mtime!
	get-stat-ctime set-stat-ctime!

	;;	argv-to-list

	;; csupport.scm
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
	filter *expand-path* expand-path *cross2* *cross*
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

  (include-shared "csupport")
  (include "csupport.scm")
  )
