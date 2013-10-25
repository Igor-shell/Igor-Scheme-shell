
;;(define verbose-csupport #f)

;;(if verbose-csupport (display "Importing csupport\n"))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((name . pattern) template))))))





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


(define (file-status file . selector)
  (if (null? selector) (set! selector '(name ino mode nlink uid gid size mtime ctime atime blksize blocks dev rdev)))

  (let* ((fs (file-stat file))
		  (dev (get-stat-dev fs)) ;;/* ID of device containing file */
		  (ino (get-stat-ino fs)) ;;/* inode number */
		  (mode (get-stat-mode fs)) ;;/* protection */
		  (nlink (get-stat-nlink fs)) ;;/* number of hard links */
		  (uid (get-stat-uid fs)) ;;/* user ID of owner */
		  (gid (get-stat-gid fs)) ;;/* group ID of owner */
		  (rdev (get-stat-rdev fs)) ;;/* device ID (if special file) */
		  (size (get-stat-size fs)) ;;/* total size, in bytes */
		  (blksize (get-stat-blksize fs)) ;;/* blocksize for file system I/O */
		  (blocks (get-stat-blocks fs)) ;;/* number of 512B blocks allocated */
		  (atime (get-stat-atime fs)) ;;/* time of last access */
		  (mtime (get-stat-mtime fs)) ;;/* time of last modification */
		  (ctime (get-stat-ctime fs)) ;;/* time of last status change */
		  )
	 (let ((result '()))
		(if (member 'name selector) (append result (list file)))
		(if (member 'ino selector) (append result (list ino)))
		(if (member 'mode selector) (append result (list mode)))
		(if (member 'nlink selector) (append result (list nlink)))
		(if (member 'uid selector) (append result (list uid)))
		(if (member 'gid selector) (append result (list gid)))
		(if (member 'size selector) (append result (list size)))
		(if (member 'mtime selector) (append result (list mtime)))
		(if (member 'ctime selector) (append result (list ctime)))
		(if (member 'atime selector) (append result (list atime)))
		(if (member 'blksize selector) (append result (list blksize)))
		(if (member 'blocks selector) (append result (list blocks)))
		(if (member 'dev selector) (append result (list dev)))
		(if (member 'rdev selector) (append result (list rdev)))
		result)
	 )
)
		



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

(define tab (if #t (make-string 1 #\tab) #f)) ;;  controls whether tabs are treated like spaces on the cmd line


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


;; This is a *hack* -- does not handle bad characters (like #\boink)
(define (sexpr-jumper s)
  (let ((is (open-input-string s))
		  (os (open-output-string))
		  )
	 (let ((sexpr #f))
		(let* ((sexpression (read is))
				 (rest (read-line is (string-length s))))
				
		  (write sexpression os)
		  (set! sexpr (get-output-string os))
		  (close-port os)
		  (close-port is)
		  (list sexpr rest))
		)))

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

;; assoc on the cdr
(define (cossa key alist)
  (let ((rev (lambda (x) (cons (cdr x) (car x)))))
	 (let* ((tsila (map rev alist))
			  (r (assoc key tsila))
			  )
		(if r (rev r) r))))

;; assoc on the cdr, but abort if it isn't there
(define (cossa+ key alist)
  (let ((r (cossa key alist)))
	 (if r r (error 'Missing-key-in-alist key alist))))


(define (tokenise-string str symbol-alist escape . sep)
  (define (first-match sstr tokenlist)
	 (if (not (string? sstr)) (error 'first-match:first-arg-not-a-string))
	 (filter (lambda (y) y)
				(map (lambda (x) 
						 (let ((n (if (string? x) (string-length x) #f)))
							;;(display (string-append "testing \"" sstr "\" against \"" x "\"\n"))
							(if (and
								  n
								  (>= n (string-length x))
								  (<= n (string-length sstr))
								  (string=? (substring sstr 0 n) x))
								 x
								 #f)
							)) tokenlist)))

  (set! sep (if (null? sep) " " (car sep)))

  (let ((return-strings-for-symbols #f) ;; 
		  (start-paren (cossa+ 'start-paren symbol-alist))
		  (end-paren (cossa+ 'end-paren symbol-alist))
		  (start-bracket (cossa+ 'start-bracket symbol-alist))
		  (end-bracket (cossa+ 'end-bracket symbol-alist))
		  (start-brace (cossa+ 'start-brace symbol-alist))
		  (end-brace (cossa+ 'end-brace symbol-alist))
		  (squote (cossa+ 'squote symbol-alist))
		  (dquote (cossa+ 'dquote symbol-alist))
		  (bquote (cossa+ 'bquote symbol-alist))
		  (en (string-length escape))
		  )

	 (cond ;; This should bail if we pass a really bad argument
	  ((not (string? str))
		(error 'tokenise-string:bad-string)
		#f)
	  ((and (not (pair? symbol-alist)) (not (null? symbol-alist)))
		(error 'tokenise-string:bad-symbol-alist)
		)
	  )

	 (let loop ((sstr str)
					(result '(""))
					)
		;;(display "looping: ")(write sstr)(newline)
		;;(display "         ")(write result)(newline)
		(let* ((fm (first-match sstr (map car symbol-alist)))
 				 (n (if (null? fm) (string-length sstr) (string-length (car fm))))
				 (cc (if (string=? str "") "" (string-car sstr)))
				 )
		  
		  (if	(and (pair? result) (symbol? (car result)) (not (string=? sstr "")))
				(loop sstr (cons (make-string 0) result))
		  
				(cond
				 ((string=? sstr "") ;; we've reached the end of the string
				  (if (or (null? result) (equal? result '("")))
						'()
						(reverse result)))

				 ((and (string=? (substring sstr 0 en) escape)
						 (> (string-length sstr) (string-length escape)))
				  (let* ((ec (substring sstr en (+ 1 en)))
							)
					 (display "ESCAPE ")(display ec)(newline)
					 (set! sstr (substring sstr en (string-length sstr)))
					 (set! n (string-length sstr))
					 (display "--> ")
					 (if (zero? n)
							 (loop "" (cons (string-append (car result) ec) (cdr result)))
							 (loop (substring sstr 1 n) 
									 (cons (string-append (car result) ec) (cdr result))
									 )
						  )
					 )
				  )

				 ((string=? cc sep)
				  (if (zero? (string-length (car result)))
						(loop (substring sstr 1 n) result) ;; silently consume extra spaces (tabs pass through!)
						(loop (substring sstr 1 n) (cons "" result)))) ;; start next token

				 ((and (string? tab) (string=? cc tab)) ;; treat tabs as spaces
				  (if (zero? (string-length (car result)))
						(loop (substring sstr 1 n) result) ;; silently consume extra tabs
						(loop (substring sstr 1 n) (cons "" result)))) ;; start next token

				 ((or (not fm) (null? fm));; must be a normal part of the argument
				  (loop (substring sstr 1 n) 
						  (cons (string-append (car result) cc) (cdr result))
						  ))

				 (fm ;; this must match one of the operators in the alist.
				  (let ((fme (assoc (car fm) symbol-alist)))
					 (cond 
					  ((member (cdr fme) '(squote dquote bquote))
						(let ((partition (fence-jumper sstr (car fme))))
						  (loop (cadr partition)
								  (cons (string-append (car result) (car partition)) (cdr result)))))
					  
					  ;; Need to deal with the s-expression and s-expression-like things
					  ((member (cdr fme) '(start-paren start-bracket start-brace quotedlist))
						(let ((partition (sexpr-jumper sstr)))
						  (loop (cadr partition)
								  (cons "" (cons (car partition) result)))))

					  ((eq? (cdr fme) 'quotedlist)
						(let ((partition (sexpr-jumper sstr)))
						  (loop (cadr partition)
								  (cons "" (cons (car partition) result)))))

					  (#f
						(let ((partition (sexpr-jumper sstr)))
						  (loop (cadr partition)
								  (cons "" (cons (car partition) result)))))

					  (#t ;; a standard thing -- 
						(if #f
							 (begin
								(display "c T\n")
								(display " fme: ")(write fme)(newline)
								(display "sstr: ")(write sstr)(newline)
								(display "  tr: ")(write (substring sstr (string-length (car fme)) (string-length sstr)))(newline)
								(display "rslt: ")(write (cons (if #t (car fme) (cdr fme)) result))(newline)
								))
						(if (string=? (car result) "")
							 (loop (substring sstr (string-length (car fme)) (string-length sstr))
									 (cons (if return-strings-for-symbols (car fme) (cdr fme)) (cdr result)))
							 (loop (substring sstr (string-length (car fme)) (string-length sstr))
									 (cons (if return-strings-for-symbols (car fme) (cdr fme)) result))
							 )
						)
					  ))
				  )
				 (#t 
				  (display "This should never happen!\n") 'epic-fail)
				 )
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


(define (word-expand s)
  (if (string? s) 
		(word-expand-wrapper s)
		s))

;; This resolves the programs in the path and allows you to use wildcards in the command 
(define (*expand-path* file)
  (filter file-exists? 
			 (let ((l 
					  (map (lambda (s)
								(if (string? s) (word-expand s) s))
							 (map (lambda (x) 
									  (if (string? x) (string-append x "/" file ) x))
									(strtok (list->string (map (lambda (x) 
																		  (if (equal? x (car (string->list ":"))) 
																				(car (string->list " ")) x)) 
																		(string->list (get-env "PATH"))))
											  " ")))))
				(if (> (length l) 1)
					 (apply append l)
					 l))))

(define (expand-path file) 
  (let ((ep (*expand-path* (if (symbol? file) (symbol->string file) file)))) (if (null? ep) #f (car ep))) )


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

;;(if verbosec-support (display "csupported\n"))




;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
