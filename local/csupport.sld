
(define-library (local csupport)
  (export 
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
	paren-fence-list
	bracket-fence-list
	brace-fence-list
	quote-fence-list
	igor-token-list

	*igor-report-backgrounding*
	*igor-builtin-list*


	;; functions
	andf orf ++ -- read-all read-all-lines in-range pipe-input show 
	string-index list-head denull denull-and-flatten level depth 
	strspn strcspn
	collapsing-strtok strtok reconstruct-string 
	filter *expand-path* expand-path *cross2* *cross*
	string-car string-cdr

	with-input-from-port
	with-output-to-port
	with-error-to-port
	with-io-ports

	strip-fences
	tokenise-string

	*add-builtin*
	*igor-execute-builtin-process*
	*igor-execute-single-process*

	system call cd 
	)

  (import (chibi) (chibi filesystem))
  (include-shared "csupport")
  (include "csupport.scm")
  )
