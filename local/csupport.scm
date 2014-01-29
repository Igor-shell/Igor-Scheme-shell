
;;(define verbose-csupport #f)

;;(if verbose-csupport (display "Importing csupport\n"))

;;(define-syntax define-syntax-rule
;;  (syntax-rules ()
;;    ((define-syntax-rule (name . pattern) template)
;;     (define-syntax name
;;       (syntax-rules ()
;;         ((name . pattern) template))))))


(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))


(define esc (make-string 1 #\x1b)) ;; hex character representation works in chibi, gsi * guile

(define (andf . args)
  (cond
   ((null? args) #t)
   ((pair? args) (and (car args) (apply andf (cdr args))))
   (else args)))

(define (orf . args)
  (cond
   ((null? args) #f)
   ((pair? args) (or (car args) (apply orf (cdr args))))
   (else args)))

(define (++ x) (+ x 1))
(define (-- x) (- x 1))

(define (in-range v mn mx)
  (and (<= mn v) (<= v mx)))


;;;(define get-stat-dev #f) ;;/* ID of device containing file */
;;;(define get-stat-ino #f) ;;/* inode number */
;;;(define get-stat-mode #f) ;;/* protection */
;;;(define get-stat-nlink #f) ;;/* number of hard links */
;;;(define get-stat-uid #f) ;;/* user ID of owner */
;;;(define get-stat-gid #f) ;;/* group ID of owner */
;;;(define get-stat-rdev #f) ;;/* device ID (if special file) */
;;;(define get-stat-size #f) ;;/* total size, in bytes */
;;;(define get-stat-blksize #f) ;;/* blocksize for file system I/O */
;;;(define get-stat-blocks #f) ;;/* number of 512B blocks allocated */
;;;(define get-stat-atime #f) ;;/* time of last access */
;;;(define get-stat-mtime #f) ;;/* time of last modification */
;;;(define get-stat-ctime #f) ;;/* time of last status change */

(define file-status 
  (let ((selectorlist (list 
							  (list 'dev get-stat-dev) ;;/* ID of device containing file */
							  (list "dev" get-stat-dev) ;;/* ID of device containing file */
							  (list 'ino get-stat-ino) ;;/* inode number */
							  (list "ino" get-stat-ino) ;;/* inode number */
							  (list 'mode get-stat-mode) ;;/* protection */
							  (list "mode" get-stat-mode) ;;/* protection */
							  (list 'nlink get-stat-nlink) ;;/* number of hard links */
							  (list "nlink" get-stat-nlink) ;;/* number of hard links */
							  (list 'uid get-stat-uid) ;;/* user ID of owner */
							  (list "uid" get-stat-uid) ;;/* user ID of owner */
							  (list 'gid get-stat-gid) ;;/* group ID of owner */
							  (list "gid" get-stat-gid) ;;/* group ID of owner */
							  (list 'rdev get-stat-rdev) ;;/* device ID (if special file) */
							  (list "rdev" get-stat-rdev) ;;/* device ID (if special file) */
							  (list 'size get-stat-size) ;;/* total size, in bytes */
							  (list "size" get-stat-size) ;;/* total size, in bytes */
							  (list 'blksize get-stat-blksize) ;;/* blocksize for file system I/O */
							  (list "blksize" get-stat-blksize) ;;/* blocksize for file system I/O */
							  (list 'blocks get-stat-blocks) ;;/* number of 512B blocks allocated */
							  (list "blocks" get-stat-blocks) ;;/* number of 512B blocks allocated */
							  (list 'atime get-stat-atime) ;;/* time of last access */
							  (list "atime" get-stat-atime) ;;/* time of last access */
							  (list 'mtime get-stat-mtime) ;;/* time of last modification */
							  (list "mtime" get-stat-mtime) ;;/* time of last modification */
							  (list 'ctime get-stat-ctime) ;;/* time of last status change */
							  (list "ctime" get-stat-ctime) ;;/* time of last status change */
							  )))
	 (lambda (file . selector)
		(let ((fs (if (string? file) (file-stat file) file))
				(select 
				 (if (null? selector)
					  (lambda x #f)
					  (map (lambda (x) (if (procedure? x) x (let ((s (assq x selectorlist))) (if s (cdr s) #f)))))))
				)
		  (let ((result  (cond
								((or (not file) (not selector) (null? selector)) #f)
								((null? (cdr selector)) ;; single arg
								 (select fs))
								(map select fs))))
			 (if (string? file) (delete-file-stat fs))
			 result)))))






;;; 							  (
;;;   (if (null? selector) (set! selector '(name ino mode nlink uid gid size mtime ctime atime blksize blocks dev rdev)))

;;;   (let ((fs (file-stat file))
;;; 		  (let* ((dev (get-stat-dev fs)) ;;/* ID of device containing file */
;;; 					(ino (get-stat-ino fs)) ;;/* inode number */
;;; 					(mode (get-stat-mode fs)) ;;/* protection */
;;; 					(nlink (get-stat-nlink fs)) ;;/* number of hard links */
;;; 					(uid (get-stat-uid fs)) ;;/* user ID of owner */
;;; 					(gid (get-stat-gid fs)) ;;/* group ID of owner */
;;; 					(rdev (get-stat-rdev fs)) ;;/* device ID (if special file) */
;;; 					(size (get-stat-size fs)) ;;/* total size, in bytes */
;;; 					(blksize (get-stat-blksize fs)) ;;/* blocksize for file system I/O */
;;; 					(blocks (get-stat-blocks fs)) ;;/* number of 512B blocks allocated */
;;; 					(atime (get-stat-atime fs)) ;;/* time of last access */
;;; 					(mtime (get-stat-mtime fs)) ;;/* time of last modification */
;;; 					(ctime (get-stat-ctime fs)) ;;/* time of last status change */
;;; 					)
;;; 			 (let ((result '()))
;;; 				(if (member 'name selector) (append result (list file)))
;;; 				(if (member 'ino selector) (append result (list ino)))
;;; 				(if (member 'mode selector) (append result (list mode)))
;;; 				(if (member 'nlink selector) (append result (list nlink)))
;;; 				(if (member 'uid selector) (append result (list uid)))
;;; 				(if (member 'gid selector) (append result (list gid)))
;;; 				(if (member 'size selector) (append result (list size)))
;;; 				(if (member 'mtime selector) (append result (list mtime)))
;;; 				(if (member 'ctime selector) (append result (list ctime)))
;;; 				(if (member 'atime selector) (append result (list atime)))
;;; 				(if (member 'blksize selector) (append result (list blksize)))
;;; 				(if (member 'blocks selector) (append result (list blocks)))
;;; 				(if (member 'dev selector) (append result (list dev)))
;;; 				(if (member 'rdev selector) (append result (list rdev)))
;;; 				result)
;;; 			 )
;;; )
		


(define (with-pipe-between lmb1 lmb2)
  (let ((oci (current-input-port))	
		  (oco (current-output-port))
		  )
	 (let* ((pipe (open-pipe))
			  (in (open-input-file-descriptor (car pipe)))
			  (out (open-output-file-descriptor (cadr pipe)))
			  )
		(with-output-to-port out lmb1)
		(close-output-port out)
		(with-input-from-port in lmb2)
		(close-input-port in))
	 (current-input-port oci)
	 (current-output-port oco)))

  ;;(current-input-port (open-input-file-descriptor 0))
  ;;(current-output-port (open-output-file-descriptor 1))


;;(with-pipe-between (lambda () (display '(1 2 3 4 5))) (lambda () (apply + (read-all))))






(define list-ref
  (letrec ((%list-ref list-ref))
    (lambda (l i)
      (if (or (< i 0) (>= i (length l)))
          (abort 'list-ref-index-out-of-bounds)
          (%list-ref l i)))))

(define (list-set! l i v)
   (if (zero? i)
       (set-car! l v)
       (list-set! (cdr l) (- i 1) v)))


 (define list-ref
   (letrec ((%list-ref list-ref)
            )
     (lambda (l i)
       (cond
        ((integer? i) (%list-ref l i))
        ((list? i) (let loop ((rtn '()) (ii i))
                     (if (null? ii)
                         (if (null? rtn) #f (reverse rtn))
                         (loop (cons (%list-ref l (car ii)) rtn) (cdr ii)))))
        ))))

 (define list-set!
   (letrec ((%list-set! list-set!)
            )
     (lambda (l i v)
       (if (list? i)
           (for-each (lambda (x y) (%list-set! l x y)) i v)
           (%list-set! l i v)))))

 (define list-set-car! list-set!)

 (define (list-set-cdr! l i v)
   (if (zero? i)
       (set-cdr! l v)
       (list-set-cdr! (cdr l) (- i 1) v)))
 ;; set the value associated with key in a-list
 ;; (assoc-set! list key value)

  ;;
  ;; return the first k elements of a list (analogous to list-tail)
  ;;
  (define (list-head the-list k)
    (if (and (> k 0) (not (null? the-list)))
        (cons (car the-list) (list-head (cdr the-list) (- k 1)))
        '()))

 (define (acons key value alist)
	(cons (cons key value) alist))

 (define (abort) (exit 1))

 (define (assoc-set! alist key val)
   (let loop ((l alist))
     (if (null? l) 
         (append alist (cons (cons key val) '()))
         (if (equal? (caar l) key)
             (set-cdr! (car l) val)
             (loop (cdr l))))))


 (define (assoc-append alist key value)
   (if (or (null? alist) (not (assoc key alist)))
       (acons key (list value) alist)
       (map (lambda (x)
              (if (and (pair? x) (equal? (car x) key)) 
                  (cons (car x) (append (cdr x) (list value)))
                  x))
            alist
            ) 
       ))

 (define (assoc-delete alist key)
   (reverse (let loop ((a alist)(r '()))
              (if (null? a)
                  r
                  (if (and (pair? a) (pair? (car a)) (equal? (caar a) key))
                      (loop (cdr a) r)
                      (loop (cdr a) (cons (car a) r)))))))



(define (filter pred lst) 
  (cond 
	((null? lst) '()) 
	((not (list? lst)) #f)
	((pred (car lst)) 
	 (cons (car lst) (filter pred (cdr lst)))) 
	(else (filter pred (cdr lst)))))

(define (prune-quotes x)
			(let ((xl (string->list x)))
			  (if (and (> (length xl) 2)
						  (char=? (car xl) #\")
						  (char=? (car (reverse xl))) #\")
					(substring x 1 (- (string-length x) 1))
					x)))

(define (prune-quotes-in-list x)
    (map prune-quotes x))
					
  
(define (read-all)
  (define (*read-all*)
	 (let loop ((lst '())
					(c (read-char))
					)
		(if (eof-object? c) 
			 (strtok (list->string (reverse lst)) " \t\n")
			 (loop (cons c lst) (read-char))))
	 )
  (let ((rtn (filter (lambda (x) (> (string-length x) 0)) (*read-all*))))
	 ;;(close-port (current-output-port))
	 rtn
	 ))
  
(define pipe-input read-all)

(define (read-all-lines)
  (filter pair? 
			 (map 
			  (lambda (x) (collapsing-strtok x " \t"))
			  (let loop ((lst '()))
				 (let ((l (read-line)))
					(if (not (string? l))
						 (reverse lst)
						 (loop (cons (read-line) lst))))))
			 ))





(define (show . args)
  (if (eq? (length args) 1)
		(car args)
		args))

(define (string-index str chr)
  (let ((tail (member chr (string->list str))))
    (if tail
	(- (string-length str) (length tail))
	#f)))

  ;; return the first k elements of a list (analogous to list-tail)
  ;;
  (define (list-head the-list k)
    (if (and (> k 0) (not (null? the-list)))
        (cons (car the-list) (list-head (cdr the-list) (-- k)))
        '()))

 ;; removes completely null strands

 (define (denull l)
   (cond
    ((null? l) '())
    ((not (pair? l)) l)
    (else
     (let ((a (denull (car l)))
           (d (denull (cdr l))))
       (cond
        ((and (null? a) (null? d)) '())
        ((null? a) d)
        (else (cons a d)))))))

 (define (denull-and-flatten l)
   (cond
    ((null? l) '())
    ((not (pair? l)) l)
    (else
     (let ((a (denull (car l)))
           (d (denull (cdr l))))
       (cond
        ((and (null? a) (null? d)) '())
        ((null? a) d)
        ((null? d) a)
        (else (cons a d)))))))



 (define (level the-list n )
   (denull 
    (let loop ((tl the-list) (d 0))
      (if (not (pair? tl))
          (if (eq? d n)
              (list tl)
              '())
          (append (loop (car tl) (+ 1 d)) (loop (cdr tl) d))))))

 ;; (depth the-list) returns the maximum depth of the list

 (define (depth l)
   (let loop ((tl l) (d 0))
     (if (not (pair? tl))
         d
         (max (loop (car tl) (+ 1 d)) (loop (cdr tl) d)))))

(define (string-car str)
  (if (and (string? str) (not (string=? str "")))
		(substring str 0 1)
		""))

(define (string-cdr str)
  (if (and (string? str) (not (string=? str "")))
		(substring str 1 (string-length str))
		""))

 ;;
 ;; (strspn str set) returns index of first char not in set
 ;; (strcspn str set) returns index of first char in set
 ;;

 (define (strspn str set)
   (let loop ((s str))
			(if (zero? (string-length s))
				 (string-length str)
				 (if (let inner-loop ((chset set))
						 (if (zero? (string-length chset))
							  #f
							  (if (eq? (string-ref s 0)
										  (string-ref chset 0))
									#t
									(inner-loop (substring chset 1 (string-length chset))))))
					  (loop (substring s 1 (string-length s)))
					  (- (string-length str) (string-length s))))))

 (define (strcspn str set)
   (let loop ((s str))
     (if (zero? (string-length s))
         (string-length str)
			(if (let inner-loop ((chset set))
					(if (zero? (string-length chset))
						 #t
						 (if (eq? (string-ref s 0)
									 (string-ref chset 0))
							  #f
							  (inner-loop (substring chset 1 (string-length chset))))))
				 (loop (substring s 1 (string-length s)))
				 (- (string-length str) (string-length s))))))

;;;; This silently collapses multiple instances of either spaces or the indicated separator
 (define (collapsing-strtok str . sep)
	(set! sep (if (null? sep) #f (car sep)))
   (if (not sep) (set! sep " "))
   (if (string? str)
       (let loop ((results '())
                  (sstr str))
         (if (zero? (string-length sstr))
             results
             (if (zero? (strspn sstr sep))
                 (loop (append results (list (substring sstr 0 (strcspn sstr sep) )))
                       (substring sstr (strcspn sstr sep) (string-length sstr)))
                 (loop results
                       (substring sstr (strspn sstr sep) (string-length sstr)))))))
   )

;; This does not collapse multiple instances of either spaces or the indicated separator 
 (define (strtok str . sep)
	(set! sep (if (null? sep) #f (car sep)))
   (if (not sep) (set! sep " "))
   (if (string? str)
       (let loop ((results '())
                  (sstr str))
         (if (zero? (string-length sstr))
             results
             (if (zero? (strspn sstr sep))
                 (loop (append results (list (substring sstr 0 (strcspn sstr sep) )))
                       (substring sstr (strcspn sstr sep) (string-length sstr)))
					  (loop (if (and (> (string-length sstr) 1) 
							 (zero? (strspn (substring sstr 1 (string-length sstr)) sep))) 
						    results (append results (list "")))
						(substring sstr 1 (string-length sstr)))))))
   )

(define (strip-fences str)
  (let ((n (string-length str)))
	 (if (< n 2)
		  ""
		  (substring str 1 (- n 1)))))

;;; (define igor-symbol-alist
;;;   '(
;;; 	 ("(" . start_fence)
;;; 	 (")" . end_fence)
;;; 	 ("\\" . escape)
;;; 	 ("'" . squote)
;;; 	 ("\"" . dquote)
;;; 	 ("`" . bquote)
;;; 	 ("'(" . quotedlist)
;;; 	 ("<<" . heredoc)
;;; 	 ("&&" . andsep)
;;; 	 ("||" . orsep)
;;; 	 ("+>>&" . stdouterrapp)
;;; 	 (">>&" . stderrapp)
;;; 	 (">>" . stdoutapp)
;;; 	 ("+|&" . outerrpipe)
;;; 	 ("|&" . errpipe)
;;; 	 ("|" . outpipe)
;;; 	 ("+>&" . stdouterredir)
;;; 	 (">&" . stderredir)
;;; 	 (">" . stdoutredir)
;;; 	 ("<" . stdinredir)
;;; 	 ("&" . makebg)
;;; 	 (";" . nextsep)
;;; 	 ("{" . begblock)
;;; 	 ("}" . endblock)
;;; 	 ("$(" . shellcmd)
;;; 	 ("${" . varexpr)
;;; 	 ("#" . comment)
;;; 	 ("\\" . continuation_str)
;;; ;	 (",@(" . scmunquotesplicelst)
;;; ;	 (",(" . scmunquotelst)
;;; ;	 (",@" . scmunquotesplice)
;;; ;	 ("," . scmunquote)
;;; 	 )
;;; )

;;; (define paren-fence-alist '(("(" . ")") ))
;;; (define bracket-fence-alist '(("(" . ")") ("[" . "]") ))
;;; (define brace-fence-alist '(("(" . ")") ("[" . "]") ("{" . "}")))
;;; (define quote-fence-alist '(("\"") ("'") ("`")))
;;; (define igor-token-list (list-tail (map car igor-symbol-alist) 7))

	;;; ;;; char escape = '\\';
	;;; ;;; char squote = '\'';
	;;; ;;; char dquote = '"';
	;;; ;;; char bquote = '`';
	;;; ;;; char *quotedlist = "'(";
	;;; ;;; char *scmunquotesplicelst = ",@(";
	;;; ;;; char *scmunquotelst = ",(";
	;;; ;;; char *scmunquotesplice = ",@";
	;;; ;;; char *scmunquote = ",";

	;;; 							  "<<" ;; heredoc
	;;; 							  "&&" ;; andsep
	;;; 							  "||" ;; orsep
	;;; 							  "+>>&" ;; stdouterrapp
	;;; 							  ">>&" ;; stderrapp
	;;; 							  ">>" ;; stdoutapp
	;;; 							  "+|&" ;; outerrpipe
	;;; 							  "|&" ;; errpipe
	;;; 							  "|" ;; outpipe
	;;; 							  "+>&" ;; stdouterredir
	;;; 							  ">&" ;; stderredir
	;;; 							  ">" ;; stdoutredir
	;;; 							  "<" ;; stdinredir
	;;; 							  "&" ;; makebg
	;;; 							  ";" ;; nextsep
	;;; 							  "{" ;; begblock
	;;; 							  "}" ;; endblock
	;;; 							  "$(" ;; shellcmd
	;;; 							  "${" ;; varexpr
	;;; 							  "#" ;; comment

	;;; 							  "\\" ;; continuation
	;;; 							  ))


(define (fence-jumper str fence-pair . prune-palings)
  (call-with-current-continuation
	(lambda (abort)
	  ;; (fence-jumper "'This' is a fence" '("'")) => ("'This'" " is a fence")
	  ;; (fence-jumper "'This' is a fence" '("'") #t) => ("This" " is a fence")
	  ;; It expects that the first character is the first fence!
	  
	  (set! prune-palings (if (null? prune-palings) #f (car prune-palings))) ;; The default is to keep fences

	  (if (not (member prune-palings '(#t #f))) (abort 'bad-boolean))

	  (if (not (string=? (string-car str) (car fence-pair)))
			(abort 'bad-string))

	  (let ((q (if (null? (cdr fence-pair)) (car fence-pair) (cdr fence-pair)))
			  )
		 (let loop ((l (if prune-palings "" (car fence-pair)))
						(r (string-cdr str)) ;; this is string-cdr
						)
			(cond
			 ((string=? r "") ;; we have an unterminated string here....
			  (display "Unterminated string: ")(write str)(newline)
			  (list  l r))
			 ((string=? (string-car r) q) ;; we've reached the end!
			  (list (string-append l (if prune-palings "" q)) (string-cdr r)))
			 (else 
			  (loop (string-append l (string-car r)) (string-cdr r)))))))
	))


(define (tokenise-string str symbol-alist fences escape . sep)
  (let ((tokenlist (map car symbol-alist)))
	 ;; This splits a line up according to the indicated tokens, fences, escape char and separator

	 ;; (tokenise-string "echo this is a \\\"test\\\" >> file" '(("(" . ")") ("'") ("\"")) "\\" " ")
	 ;; ==> ("echo" "this" "is" "a" " \\\"test\\\"" " " ">>" "file")

	 ;; a null value for the cdr in an element of the fences list indicates that the closing sym is 
	 ;; the same as the starting sym.

	 (set! sep (if (null? sep) #f (car sep)))
	 (if (not sep) (set! sep " "))

	 (cond
	  ((null? tokenlist)
		(collapsing-strtok str sep))
	  ((not (and (string? str) (list? tokenlist))) #f)
	  ((string=? str "") '())
	  (else 
		(let sloop ((sstr str)
						(results '())
						)
		  (let ((n (if (string? sstr) (string-length sstr) #f)))

			 ;(display "collected ") (write results)(newline)

			 (if (or (not sstr) (string=? sstr ""))
				  (reverse results)
				  (let ((mt (filter (lambda (y) y)
										  (map (lambda (x) (if (and
																		n
																		(string? x)
																		(>= n (string-length x))
																		(string=? (substring sstr 0 (string-length x)) x))
																	  x
																	  #f)) tokenlist)))
						  (cc (if (string=? sstr "") "" (string-car sstr)))
						  )
					 
					 ;(if (pair? mt) (begin (display "MT ")(write mt)(newline)))

					 ;; mt will either be null, or its head will be the first match for the head of the string
					 (cond
					  ((not cc)
						(reverse results))

					  ((string=? cc escape)
						;;(display "*** Still buggered ***\n")
						;;(display "..c1 ")
						;;(write sstr)
						;;(newline)
						(sloop 
						 (substring sstr (+ 1 (string-length escape)) n)
						 (cons (string-append (car results) (substring sstr 0 (+ 1 (string-length escape)))) (cdr results)))
						)

					  ((string=? cc sep) ;; This is a separator, go around again
						;;(display "..c2 ")
						;;(write sstr)
						;;(newline)
						(sloop 
						 (substring sstr (+ (strcspn sstr sep) 1) n)
						 (if (and (pair? results) (string=? (car results) sep))
							  results
							  (cons sep results))
						 ;;results
						 )
						)


					  ((and (null? results) (null? mt)) ;; No match, no separator, no escape.
						;;(display "..c3 ")
						;;(write (substring sstr 1 n))
						;;(newline)
						(sloop 
						 ;;(substr sstr 1 (- (string-length sstr) 1))
						 (string-cdr sstr)
						 ;;(substring sstr 1 n)
						 ;;(cons (substring sstr 0 1) results)
						 (cons cc results)
						 ))

					  ((and (string=? (car results) sep) (null? mt)) ;; No match, no separator, no escape.
						;; the current (head) end of the list is a separator -- replaces it with the beginning of the word
						;;(display "..c4 ")
						;;(write sstr)
						;;(newline)
						(sloop 
						 ;;(substring sstr 1 n)
						 (string-cdr sstr)
						 ;;(cons (substring sstr 0 1) (cdr results))
						 (cons cc (cdr results))
						 ))

					  ((null? mt) ;; No match, no separator, no escape.
						;;(display "..c5 ")
						;;(write sstr)
						;;(newline)
						(sloop 
						 ;;(substring sstr 1 n)
						 (string-cdr sstr)
						 ;;(cons (string-append (car results) (substr sstr 0 1)) (cdr results))
						 (cons (string-append (car results) cc) (cdr results))
						 ))

					  ((and (string=? (car results) sep) (pair? mt))
						;;(display "..c6 ")
						;;(write sstr)
						;;(newline)
						(sloop 
						 (substring sstr (string-length (car mt)) n)
						 (cons 
						  (let ((asym (assoc (car mt) symbol-alist)))
							 (if asym
								  (cdr asym)
								  (car mt)))
						  (cdr results))
						 ))

					  (mt
						;;(display "..c7 ")
						;;(write sstr)
						;;(newline)
						(sloop 
						 (substring sstr (string-length (car mt)) n)
						 (cons 
						  (let ((asym (assoc (car mt) symbol-alist)))
							 (if asym
								  (cdr asym)
								  (car mt)))
						  results)))

					  ((assoc (string-car sstr) fences)
						(let ((l+r (fence-jumper sstr (assoc (string-car sstr) fences) #f)))
						  (sloop 
							(cadr l+r)
							(cons (car l+r) results)
							)))

					  (else
						;;(display "..c* ")
						;;(write sstr)
						;;(newline)
						(sloop (substring sstr (string-length (car mt)) n)
								 (cons 
								  (let ((asym (assoc (car mt) symbol-alist)))
									 (if asym
										  (cdr asym)
										  (car mt)))

								  results)))
					  )
					 )))
		  )
		)
	  )
	)
  )


;; reconstructs the string either with spaces or the indicated separator

 (define (reconstruct-string strarray . sep)
	(set! sep (if (null? sep) #f (car sep)))
   (if (not sep) (set! sep " "))
   (cond
    ((null? strarray) "")
    ((string? strarray) strarray)
    ((pair? strarray)
     (let loop ((sa (cdr strarray))
		(ns (car strarray)))
       (cond
	((null? sa) ns)
	((pair? sa) (loop (cdr sa) (string-append ns sep (car sa))))
	((string? sa) (string-append ns sep sa))
	(else "")
	)))))


;; Chibi's execute is (execute cmdstring arglist)

;; The following with-... are mirrored by wifp, wotp and wetp
(define (with-input-from-port prt lmbda)
  (let ((original-port (current-input-port)))
	 (current-input-port prt)
	 (lmbda)
	 (current-input-port original-port)))

(define (with-output-to-port prt lmbda)
  (let ((original-port (current-output-port)))
	 (current-output-port prt)
	 (lmbda)
	 (current-output-port original-port)))

(define (with-error-to-port prt lmbda)
  (let ((original-port (current-error-port)))
	 (current-error-port prt)
	 (lmbda)
	 (current-error-port original-port)))

(define (with-io-ports in out err lmbda)
  (with-input-from-port in
	 (lambda ()
		(with-output-to-port out
		  (lambda ()
			 (with-error-to-port err lmbda))))))


;; This resolves the programs in the path and allows you to use wildcards in the command 
(define (*expand-path* file)
  (if (and #f (member (car (string->list file)) '( #\( #\" #\')))
		file
		(filter file-exists? 
				  (let ((l 
							(map word-expand 
								  (map (lambda (x) 
											(string-append x "/" file )) 
										 (strtok (list->string (map (lambda (x) 
																				(if (equal? x (car (string->list ":"))) 
																					 (car (string->list " ")) x)) 
																			 (string->list (get-env "PATH"))))
													" ")))))
					 (if (> (length l) 1)
						  (apply append l)
						  l)))))

(define (expand-path file) 
  (let* ((ep (*expand-path* (if (symbol? file) (symbol->string file) file)))
			(result (if (null? ep) #f (car ep)))
			)
	 result))

(define (cd . args) 
  (if (null? args) 
		(change-directory (get-env "HOME")) 
		(change-directory (car args))))


;; return the cross product of two lists (state spaces)
(define (*cross2* a b) 
  (apply append (map (lambda (x) (map (lambda (y) (list x y)) b)) a)))

;; return the cross product of n lists (state spaces)
(define (*cross* . args) 
  (define (*cross2* a b) 
	 (apply append (map (lambda (x) (map (lambda (y) (if (list? y) (cons x y) (list x y))) b)) a))) 
  (cond 
	((not (list? args)) 'bad-argument) 
	((null? args) '()) 
	((= (length args) 1)  (car args))	
	((= (length args) 2)	(apply *cross2* args))	
	(#t (*cross* (car args) (apply *cross* (cdr args))))))

(define (write-to-string sexpr) (let ((out (open-output-string))) (write sexpr out) (get-output-string out)))
(define (display-to-string sexpr) (let ( (out (open-output-string))) (display sexpr out) (get-output-string out)))

(define *eof* (let ((p (open-input-file "/dev/null"))) (let ((e (read p))) (close-port p) e)))


(define (*evaluate-scheme-expression ctx sexpr env inputstring)
  (let* ((stdin-list #f) (stdin #f))
	 (cond 
	  ((not inputstring)
		(set! stdin (lambda x (display "You cannot use (stdin) or (stdin-list) without an input string\n")))
		(set! stdin-list stdin))
	  ((zero? (string-length inputstring))
		(set! stdin (lambda x (display "You cannot use (stdin) or (stdin-list) without an input string\n")))
		(set! stdin-list stdin))
		)
	 (let ((lst (collapsing-strtok inputstring)))
		(set! stdin (lambda () (if (pair? lst) (let ((a (car lst)))(set! lst (cdr lst)) a) *eof*)))
		(set! stdin-list (lambda () (if (pair? lst) (let ((a lst)) (set! lst *eof*) a) *eof*)))


		(let* ((instr (if (null? inputstring) #f (open-input-string inputstring)))
				 )
		  (let loop ((sexpr (read instr))
						 (result #f))
			 (if (not (eof-object sexpr))
				  (loop (read instr) (eval ctx result env))
				  (write-to-string result)
				))))
	 ))



(define (call bg . args)
  (if (or (not (boolean? bg)) (null? args))
		#f
		(let ((in (duplicate-file-descriptor 0))
				(out (duplicate-file-descriptor 1))
				(err (duplicate-file-descriptor 2))
				)
		  
		  (let ((pid (fork)))
			 (cond
			  ((< pid 0) #f)
			  ((> pid 0) ;; parent
				(close-file-descriptor in)
				(close-file-descriptor out)
				(close-file-descriptor err)

				(if (not bg)
					 (waitpid pid 0)
					 )
				0)
			  (#t ;; child
				(let ((sigint (set-signal-action! signal/interrupt #t))
						(sigquit (set-signal-action! signal/quit #t))
						(sigterm (set-signal-action! signal/term #t))

;;;(current-input-port (open-input-file-descriptor 0))
;;;(current-output-port (open-output-file-descriptor 1))
						(r #f)
						)

				  (current-input-port (open-input-file-descriptor in))
				  (current-output-port (open-output-file-descriptor out))
				  (current-error-port (open-output-file-descriptor err))

				  (display (expand-path (car args))) (display ": ") (display args) (newline)

				  (set! r (execute (expand-path (car args)) args))
				  
				  (set-signal-action! signal/interrupt sigint)
				  (set-signal-action! signal/quit sigquit)
				  (set-signal-action! signal/term sigterm)
				  r
				  )
				)
			  )
			 )
		  )
		)
  ) 

(define (system string)
  (let* ((args (collapsing-strtok string))
			(arg1 (word-expand (car args)))	
			)

	 (if (> (length arg1) 1)
		  (begin
			 (display "Ambiguous command name: ")
			 (display arg1)
			 (newline)
			 #f)
		  (begin
			 (set-car! args (car arg1))
			 (apply call (cons #f args))))))


;;(define (make-file-stat)
;;  (make_file_stat))
;;
;;(define (delete-file-stat fs)
;;  (delete_file_stat fs))
;;
;;(define (file-stat filename)
;;  (let* ((fs (make_file_stat))
;;			(n (stat filename fs)))
;;	 (if (zero? n) 
;;		  fs
;;		  #f)))


(define *igor-report-backgrounding* #f)
(define *igor-builtin-list* '())

(define (*add-builtin* name function)
  (if (not (string? name))
		(begin
		  ;(display "The name of the builtin needs to be a string!\n")
		  #f)
		(let ((present (assoc name *igor-builtin-list*)))
		  (if (not present)
				(set! *igor-builtin-list* (cons (cons name function) *igor-builtin-list*)))
		  #t)))
				
(*add-builtin* "add-builtin" *add-builtin*)



;;; (define (*igor-execute-builtin-process* in-the-background func argv-list input-port output-port error-port)
;;;   (if (string? func)
;;; 		(cond
;;; 		 ((procedure? func)
;;; 		  (apply func argv-list))
;;; 		 ((and (string? func) (string=? func "exit"))
;;; 		  'EXIT)
;;; 		 ((string? func)
;;; 		  ;(display func)
;;; 		  ;(display ": ")
;;; 		  ;(display argv-list)
;;; 		  ;(newline)
;;; 		  )
;;; 		 (else #t))
;;; 		(apply (string->symbol func) argv-list)
;;; 		) )


;;; ;; This will execute the command "cmd-string", passing the arguments in argv-list.  Does backgrounding and redirections
;;; (define (*igor-execute-single-process* in-the-background cmd-string argv-list input-port output-port error-port)
;;;   (igor-execute-single-process in-the-background cmd-string argv-list input-port output-port error-port)
;;;   (if (or (not (string? cmd-string)) (zero? (string-length cmd-string)))
;;; 		(begin 
;;; 		  ;(display "Something not quite right.\n" error-port)
;;; 		  -1)

;;; 		(let ((procid #f)
;;; 				)
;;; 		  (cond
;;; 			((> procid 0) ;; parent
;;; 			 (if (not in-the-background)
;;; 				  (waitpid procid 0)
;;; 				  (or #t
;;; 						;(if *igor-report-backgrounding* (display (string-append "[started background process: " cmd-string) (current-error-port)))
;;; 						)
;;; 				  )
;;; 			 (if (not (equal? input-port (current-input-port)))
;;; 				  (close-input-port input-port))
			 
;;; 			 (if (not (equal? output-port (current-output-port)))
;;; 				  (close-output-port output-port))
			 
;;; 			 (if (not (equal? error-port (current-error-port)))
;;; 				  (close-output-port error-port))
;;; 			 'ok
;;; 			 )
;;; 			((zero? procid) ;; child
;;; 			 (let ((sigint (set-signal-action! signal/interrupt #t))
;;; 					 (sigquit (set-signal-action! signal/quit #t))
;;; 					 (sigterm (set-signal-action! signal/term #t)))

;;; 				(with-io-ports input-port output-port error-port 
;;; 									(lambda () 
;;; 									  (execute (expand-path cmd-string) argv-list)))
				
;;; 				(set-signal-action! signal/interrupt sigint)
;;; 				(set-signal-action! signal/quit sigquit)
;;; 				(set-signal-action! signal/term sigterm)
;;; 				)
;;; 			 )
;;; 			(else
;;; 			 ;;(display (string-append "Failed to fork for " cmd-string) (current-error-port))
;;; 			 -4
;;; 			 )
;;; 			)
;;; 		  )	
;;; 		)
;;;   )
		  
			 
;;; ;(define igor-execute-single-process *igor-execute-single-process*)
;;; (define (igor-execute-single-process in-the-background cmd-string argv-list input-port output-port error-port)
;;;   (if #t -1	
;;; 		(begin
;;; 		  ;;(display (string-append "** " (if in-the-background "[bg]" "") " " cmd-string))
;;; 		  (for-each (lambda (x) (display " ") (display x)) argv-list)
;;; 		  (newline)
;;; 		  -1))
;;;   )

;;; (define (process-token-list arglist inp outp errp)
;;;   "Not working yet"
;;;   (let ((set-bg! (lambda (cc x) (list-set! cc x 0)))
;;; 		  (set-inp! (lambda (cc x) (list-set! cc x 1)))
;;; 		  (set-outp! (lambda (cc x) (list-set! cc x 2)))
;;; 		  (set-errp! (lambda (cc x) (list-set! cc x 3)))
;;; 		  (set-cmd! (lambda (cc x) (list-set! cc x 3)))
;;; 		  )
;;; 	 (let loop ((rslt '())
;;; 					(collecting-command '())
;;; 					(argl arglist)
;;; 					)
;;; 		(cond
;;; 		 ((null? argl) rslt)
;;; 		 ((not (pair? argl)) 'process-token-list:bad-element-in-arglist)

;;; 		 ;; a command is a list (append (list bgp inp outp errp) cmdpath-or-name arguments)
;;; 		 ;; where arguments is the argv (which includes the passed cmd)
;;; 		 ((null? collecting-command)
;;; 		  (list #f inp outp errp #f)) ;; the command is #f because it doesn't exist yet

;;; 		 ;; add a literal list
;;; 		 ((and (string? (car argl)) (> (string-length (car argl)) 1) (string=? (substring (car argl) 0 2) "'("))
;;; 		  (loop rslt 
;;; 				  (if (not (list-ref collecting-command 4)) ;; not there yet
;;; 						(append collecting-command (list (car argl) (car argl))) ;; add the literal list as the "command"
;;; 						(append collecting-command (list (car argl)))) ;; just append the list as an argument
;;; 				  (cdr argl)
;;; 				  ))
		 
;;; 		 ((equal? (car argl) 'heredoc)
;;; 		  (display "Here documents are not supported yet\n" errp)
;;; 		  'process-token-list:No-here-documents!
;;; 		  )
;;; 		 ((equal? (car argl) 'stdoutapp)
;;; 		  )
;;; 		 ((equal? (car argl) 'stderrapp)
;;; 		  )
;;; 		 ((equal? (car argl) 'stdouterrapp)
;;; 		  )
;;; 		 ((equal? (car argl) 'stdinredir)
;;; 		  )
;;; 		 ((equal? (car argl) 'stdoutredir)
;;; 		  )
;;; 		 ((equal? (car argl) 'stderredir)
;;; 		  )
;;; 		 ((equal? (car argl) 'stdouterredir)
;;; 		  )
;;; 		 ((equal? (car argl) 'outpipe)
;;; 		  )
;;; 		 ((equal? (car argl) 'errpipe)
;;; 		  )
;;; 		 ((equal? (car argl) 'outerrpipe)
;;; 		  )
;;; 		 ((equal? (car argl) 'makebg)
;;; 		  )
;;; 		 ((equal? (car argl) 'nextsep)
;;; 		  )
;;; 		 ((equal? (car argl) 'begblock)
;;; 		  )
;;; 		 ((equal? (car argl) 'endblock)
;;; 		  )
;;; 		 (else 
;;; 		  (loop rslt 
;;; 				  (let ((bit (word-expand (car argl))))
;;; 					 (if bit
;;; 						  (append collecting-command bit) ;; add the expansion
;;; 						  (append collecting-command (list (car argl)))) ;; else add the work
;;; 					 )
;;; 				  (cdr argl)
;;; 				  ))
		  
;;; 		  )
;;; 		)
;;; 	 )
;;;   )
		
		
		
	  


;;(if verbosec-support (display "csupported\n"))




;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
