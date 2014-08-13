// -*- outline-regexp: "/\\*-+";  -*-


#define RECURSIVE_DESCENT

/*- Provenance, licensing, patches and discussion  */

/*
  
  igor was written by Randall Gray 
  Copyright 2013
*/

/*-- License */

/*
  This program is licensed under the Free Software Foundation's GPL
  v.3. Derivative works may be licensed under this license or any
  subsequent version of the GPL.

  Small snipppets loosely based on functions/macros from interpreter
  for chibi are scattered through the code, thanks for a great body of
  code to use as an example, Alex.

  Chibi-scheme is availble from the mercurial repository at 
  https://code.google.com/p/chibi-scheme/ (as of 2014-03-19).


  The process handling is loosely based on code from a small shell
  (BD-shell) written by Daniel Graziotin <bodom_lx@task3.cc> as part
  of an operating systems course.  The official page for this is
  http://daniel.graziotin.net/projects/bd-shell (as of 2014-03-19).
  The process handling hasn'tt had much work and is currently not
  functional.



*/



/*-- Patches */

/* 
  Patches are preserved in the "patches" directory.  Contributions
  have been received from:

  Alex Shinn: modifications so the shell compiles under OSX

*/

/*-- Discussion  */

/*

  Igor is written in C and Scheme, and uses the excellent
  "chibi-scheme" by Alex Shinn.

  Igor is intended to be a day-to-day command shell (like bash or
  tcsh) for unix-like operating systems. The emphasis is on the
  interactive usability of the shell.  The basic motivation for
  writing it is to bring scheme to the command-line.  I want to be
  able to write things like

  ls -1 (string-append "/home/randall" (backup-dir)) | \
       (filter executable-file? (read-all stdin))		 \
       | cpio -c > executables.cpio

  [Yes, I *know* that find(1) can do this already ... it's just an
  example, ok?]

  Igor supports (or should eventually support):
     most of the usual sorts of redirection, 
     command-line editing and history via libreadline and libhistory, 
	  globbing, 
	  evaluating s-expressions on the commandline,
	  using s-expressions as arguments to commands, 
	  binding scheme functions to things like 
         globbing
         variable setting
			script execution
         generating prompts
			tasks which occur before and after programs are run
     easy hacking.	  
  

  The name was chosen both because I wanted a shell that I could
  customise radically and which might even be able to customise
  itself.  And of course, in homage to the Igors in the Diskworld
  books by Terry Pratchett, whose Igors are enthusiastic masters of
  self modification and re-creation.








  Discussion:
  
  any set of bare s-exps on the command line are dealt with as though we were running a REPL
  bare scheme variables on a command line will not match a command and will return an error

  X we need to be able to intern programs as functions. (of various return types...)
  X Need to be able to make aliases -- the converse of making a function out of a program.  We do this already with the builtins to a degree.

  Need to be able to pipe the output of a function to programs or redirect it to a file

  X $(...) is a scheme evaluation that allows wordexp expansions with the caveat that all unescaped/quoted parens are masked out.

  X $(...) is probably better aliased with ,@(...) than with `...` -- that is with scheme evaluations
  X rather than command evaluations

  At the lowest level, treat the environment variables and scheme variables as distinct -- if someone wants to "unify" the name-space they can.

*/


/*-  file tag  */

#ifndef __igor_c
#define __igor_c
#endif



/*-  Included files  */

#include "chibi/eval.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pwd.h>
#include <signal.h>

#include <sys/wait.h>
#include <time.h>
#include <assert.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>

#include <wordexp.h>


/*-   Configuration stuff  */

/*--   Compiler tweeking  */

#define _GNU_SOURCE

/*--   Identification defines, and default names  */

#define IGOR_VERSION "(igor 0 2 \"Kibble Pie!\")"
#define IGOR_REPO "https://github.com/Igor-shell/Igor-Scheme-shell"
#define IGOR_HISTORY_DEFAULT "~/.igor-history"

#define IGOR_BOOTSTRAP "/etc/igor-bootstrap"
#define IGOR_SYS_RC_FILE "/etc/igor-rc"
#define IGOR_RC_FILE ".igor-rc"

#define IGOR_COMMAND_PROLOGUE "*igor-command-prologue*"
#define IGOR_COMMAND_EPILOGUE "*igor-command-epilogue*"

#if !defined(IGOR_HISTORY_FILE_VAR)
#define IGOR_HISTORY_FILE_VAR "*igor-history-file*"
#endif

#if !defined(IGOR_VERSION_VAR)
#define IGOR_VERSION_VAR "*igor-version*"
#endif


#if !defined(IGOR_TRACKING_FUNCTION_VAR)
#define IGOR_TRACKING_FUNCTION_VAR "*igor-track-function*"
#endif

#if !defined(IGOR_TRACKING_LIST_VAR)
#define IGOR_TRACKING_LIST_VAR "*igor-track-execution-list*"
#endif

/*- Manifests, macros, and type declarations  */

/*--   scheme environment  stuff  */

#define ENV env    // explicitly use the global environment

#if !defined(ENV)
#define ENV NULL   // use default
#endif

/*--   constants  */

#define BUGGER 233 // This is used as an exit value when a catastrophic, otherwise unhandled error occurs

#define errsize 81 // buffer size for error messages 

/*--- process states */

#define RUNNING 0 // zero in the first bit, not stopped, not backgrounded, not blocked
#define STOPPED 1 // one in the first bit
#define BACKGROUNDED 2 // one in the second bit
#define BLOCKED 4  // blocked on input 




/*--   syntactic sugar and useful macros  */

#define Ok 0
#define is_bad(x) (!x)

#define begin if(1)

#if 0
#define ENTRY(fmt,arg) {fprintf(stderr,"### Entering %s@%s:%d " fmt "\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);}
#define LEAVING(fmt,arg) {fprintf(stderr,"### Leaving %s@%s:%d " fmt "\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);}
#else
#define ENTRY(fmt,arg) {}
#define LEAVING(fmt,arg) {}
#endif

#if 0
#define TRACK {fprintf(stderr,"###     Tracking %s@%s:%d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);}
#else
#define TRACK {}
#endif

#if 0
#define gronk(rv,excmd) {fprintf(stderr,"                                      [%s@%s:%d]    %d    *rv == SEXP_TRUE = %d\n",__PRETTY_FUNCTION__, __FILE__, __LINE__, excmd, *rv == SEXP_TRUE);}
#else
#define gronk(rv,excmd) {}
#endif

#if 0
#define STATUS(s) {fprintf(stderr,"###    STATUS %s@%s:%d = %d; %s\n", __PRETTY_FUNCTION__, __FILE__, __LINE__, s, s?"SEXP_FALSE":"SEXP_TRUE");}
#else
#define STATUS(s) {}
#endif

#define Abort(msg) Abort_i(msg,__FILE__, __LINE__)
#define DAbort(msg) DAbort_i(msg,__FILE__, __LINE__)
#define report_error(err, errormessage, ctx) report_error_i(err, errormessage, ctx, __FILE__, __LINE__) 

#define adjust_fd(which,wnum)	{if (which >= 0 && which != wnum) {	\
				if (dup2(which,wnum) < 0) {perror("Error dup2ing " #which);exit(EBADF);}}}

#define UNSET(var,bit) (var = ((var) & ~(bit)))
#define SET(var,bit) (var = ((var) | (bit)))

#define statq(fname,query) ({struct stat sb; int rslt; rslt = stat(fname,&sb); (!rslt ? ((sb.st_mode & (query)) ? 0 : -1) : errno);}) // returns 0 on a successful query, -1 if no match, and errno otherwise

#define isa_file(fname) (!statq(fname, S_IFREG)) // returns 1 on true ...
#define isa_dir(fname) (!statq(fname, S_IFDIR)) // returns 1 on true ...
#define isa_suidfile(fname) (!statq(fname, S_ISUID) && isa_file(fname)) // returns 1 on true ...
#define isa_sgidfile(fname) (!statq(fname, S_ISGID) && isa_file(fname)) // returns 1 on true ...
#define isa_program(fname) (isa_file(fname) && (!statq(fname, (S_IXUSR|S_IXGRP|S_IXOTH)))) // returns 1 on true ...


/*---   dynamic memory stuff  */

#define Free(A) if (A) {int _i; for (_i = 0; _i < STABSIZE && symbol[_i] && (char *)A != symbol[_i]; _i++); if (_i >= STABSIZE) free(A);	A = 0;}
#if 1
  #define reallocate(p,s) realloc(p, s)
#else
  #define reallocate(p,s) ({void *q = NULL; fprintf(stderr,"REALLOC %s:%d\n",__FUNCTION__, __LINE__); fflush(stderr); q =  realloc(p, s); fprintf(stderr,"REALLOCATED %s:%d\n",__FUNCTION__, __LINE__); fflush(stderr); q;})
#endif

/*---   scheme list construction  */

#define ARG1(a) sexp_cons(ctx,a,SEXP_NULL)
#define ARG2(a,b) sexp_cons(ctx,a,ARG1(b))
#define ARG3(a,b,c) sexp_cons(ctx,a,ARG2(b,c))

/*---   debugging messages  */

//#define NDEBUG 1

//#define Cprintf(format, args...) printf(format, ##args) // execution in command-loop
//#define Iprintf(format, args...) printf(format, ##args) // execution path from the arguments supplied to igor
//#define Dprintf(format, args...) printf(format, ##args) // debugging the tokenising
//#define DPTprintf(format, args...) printf(format, ##args) // processing tokens


#define Note(s)
#if !defined(Note)
#define Note(s) fprintf(stderr,"%s:%d  %s\n", __FILE__, __LINE__, s)
#endif

#if !defined(Cprintf)
#define Cprintf(format, args ...)
#endif

#if !defined(Iprintf)
#define Iprintf(format, args ...)
#endif

#if !defined(Dprintf)
#define Dprintf(format, args ...)
#endif

#if !defined(DPTprintf)
#define DPTprintf(format, args ...)
#endif

/*---   miscellaneous macros  */

#define eat_white_space(p) {while (p && *p && isspace(*p)) p++;}

/*-- type declarations  */

typedef struct PROCESSLIST {
	pid_t pid;  // pid of process
	pid_t pgid;
	int state;
} process_list_t;


// The tokeniser and the parse routines use the same structure; lax, I know.
typedef struct CMD_T {
	int errcond;
	char *cmd;
	int argc;
	char **argv;  // Each string in the argv actually carries an extra byte past the null terminator which indicates what 
	// kind of token it is, namely one of the following types: program, s-expression, unquoting-s-expression, splicing-s-expression, 
	//	function-as-program (builtin), argument, commandline-fragment (like the bit leading up to an s-expression)

//	sexp instringport, outstringport;  // These should be either SEXP_TRUE or SEXP_FALSE and are used for 
                                      // indicating "pipes" associated with s-expressions
//	char **instring, **outstring;  // These are strings used for s-expressions
	int in, out, err;
	int output_to_sexp;
	int input_from_sexp;

	char *inputstring;
	int run_scheme_expression;
	int shuto, shute;
	int bg;
	int NOT; // indicate that the "truth" value returned should be complemented
	int and_next, or_next;  // it is an error if both and_next and or_next are set
	struct CMD_T *next;
} cmd_t;

typedef sexp (*builtin_func)(cmd_t *command, char **argv, int in, int out, char *ins); // the "command" is often NULL

typedef struct BUILTIN {
	char *name;
	builtin_func func;
	struct BUILTIN *left,  *right;
} Builtin;


/*--  function declarations  */

extern sexp argv_to_list(sexp ctx, char **argv, int len);

extern int starts_sexp(char *s);
extern int is_sexp(char *s);
extern int is_unfinished_sexp(char *s);
extern char *jump_sexp(char *s, char escape);
extern char *sexpr_depth(char *buffer, char *str);

extern char *jump_fence_c(char *cp, char *collecting, char lookingfor, char escape, int eat_quotes);
extern char *jump_fence(char *cp, char lookingfor, char escape);


extern char *evaluate_scheme_expression(int emit, char *sexp, char *instring);
extern char *exit_val_evaluate_scheme_expression(int emit, char *sexp, char *instring);
extern char *gets(char *);

extern char *dispatch_scheme_stuff(cmd_t *cmd);

extern char *dispatch_scheme(char **cmd);

void close_up_shop();
void run_command_prologue(char *cmds);
void run_command_epilogue(char *cmds, sexp rv);


/*--  Variables  */

/*---   Scheme environment  */

sexp ctx, env, ERRCON = SEXP_FALSE;
sexp sym;
sexp igor_execute;
sexp current_input, current_output, current_error;
char *error_message = NULL;

Builtin *builtins = NULL;

char **magic_string = NULL;
int n_magic_strings = 0;

int running_script = 0;
int track_execv = 0;
int repl_write = -1; // by default doesn't print things

int SEXP_OK_IN_DQUOTES = 1;
int SEXP_OK_IN_BQUOTES = 1;

int serial_number = 1;

//char start_fence = "([{";
//char end_fence = ")]}";

#if defined(FULLFENCING)
char *start_fence = "{[(";
char *end_fence = ")]}";
#else
char *start_fence = "(";
char *end_fence = ")";
#endif

char sescape = 0;
char escape = '\\';

char squote = '\'';
char dquote = '"';
char bquote = '`';

// These are added to the list of symbols using add_magic in igor()
char *quotedlist = "'(";

char *continuation_str = "\\";

char *scmunquotesplicelst = ",@(";
char *scmunquotelst = ",(";
char *scmunquotesplice = ",@";
char *scmunquote = ",";

char *comment = "#";
char *not = "!";

char *shellcmd = "$(";
char *varexpr = "${";

char *herestr = "<<<";
char *heredoc = "<<";

char *stdouterredir = "+>";
char *stderredir = "->";
char *stdoutredir = ">";
char *stdinredir = "<";
char *stdouterrapp = "+>>";
char *stderrapp = "->>";
char *stdoutapp = ">>";


char *nextsep = ";";
char *makebg = "&";

char *andsep = "&&";
char *outerrpipe = "+|";
char *errpipe = "-|";
char *outpipe = "|";

char *orsep = "||";

char *begblock = "{";
char *endblock = "}";



#define STABSIZE 40
static char *symbol[STABSIZE];

char *history_file = NULL;


FILE *Stdin, *Stdout, *Stderr;
int IN = 0, OUT = 1, ERR = 2;

pid_t igor_pid = 0;
pid_t igor_pgid = 0;

process_list_t  *proclist  = NULL;
int proclist_size = 0;
int n_procs = 0;

/****  the $$ variables need finishing ****/
 

/*----   array of initialisation sexps  */

char *supporting_initialisation[] = {
	//"(import (scheme base))",
	//"(import (scheme read))",
	//"(import (scheme write))",
	//"(import (scheme eval))",
	"(import (scheme file))",

//	"(import (chibi))",
	"(import (chibi process))",     // signal handling, fork, execute...
	"(import (chibi filesystem))",  // chdir, rmdir, dup2, open-pipe....
	"(import (chibi system))",      // user data & session stuff
	"(import (chibi net))",         // for network thingies
	"(import (chibi net server))",  // ... as a server
	
	"(import (srfi 1))",            // list wrangling

   "(import (srfi 6))",  // string ports
   //"(import (srfi 22))", // scheme scripts
   //"(import (srfi 23))", // (error ...)
   //"(import (srfi 62))", // Comment out s-expressions  using #; 
	"(import (srfi 95))", // sorting and merging
	"(import (srfi 98))", // access to environment variables
  

   // ensure that we have filter

	"(import (local es))", // load the extra routines from csupport.stub/external-support.c

	"(define *igor-prompt-for-line-continuation-string* \"\")",
	"(define " IGOR_HISTORY_FILE_VAR " #f)",
	"(define " IGOR_VERSION_VAR "  \'" IGOR_VERSION ")",
	"(define *running-script* 0)",
	"(define *last_igor_eval* \"\")",
	"(define $@ #f)",
	"(define $* #f)",
	"(define $? #f)",
	"(define $- #f)",
	"(define $$ #f)",
	"(define $! #f)",
	"(define $0 #f)",
	"(define semicolon-is-separator #t)",
	"(define " IGOR_TRACKING_LIST_VAR " (list))",
	"(define (" IGOR_TRACKING_FUNCTION_VAR " args) (apply dnl args)))",
	"(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))",

#if 1
	"(define *igor-display-prologue* #f)",
	"(define *igor-display-epilogue* #f)",
	"(define (" IGOR_COMMAND_PROLOGUE " cmd) (if *igor-display-prologue* (dnl \"prologue \" cmd )))",
	"(define (" IGOR_COMMAND_EPILOGUE " cmd rv) (if *igor-display-epilogue* (dnl \"epilogue \" cmd \" --> \" rv)))",
#else
	"(define (" IGOR_COMMAND_PROLOGUE " cmd) #t)",
	"(define (" IGOR_COMMAND_EPILOGUE " cmd rv) rv)",
#endif

	"(define **igor-input-port-stack** '())",
	"(define **igor-output-port-stack** '())",
	"(define **igor-error-port-stack** '())",

	// Functions
	"(define (prompt . args) \"- \")",
	"(define (unfinished-sexp-continuation-prompt . args) \"--      \")",
	"(define (line-continuation-prompt . args) \"--      \")",
	"(define (prompter strn . args) (string-append strn \" \"))",

#if !defined(BOOTSTRAP)
	"(define (*wifp* p l) (let ((pp (current-input-port)))(current-input-port p)(l)(current-input-port pp)))",
	"(define (*wotp* p l) (let ((pp (current-output-port)))(current-output-port p)(l)(current-output-port pp)))",
	"(define (*wetp* p l) (let ((pp (current-error-port)))(current-error-port p)(l)(current-error-port pp)))",
	"(define (*wps* i o e l) (let ((pi (current-input-port))(po (current-output-port))(pe (current-error-port)))(current-input-port i)(current-output-port o)(current-error-port e)(l) (current-input-port pi)(current-output-port po)(current-error-port pe)))",
#endif
	NULL};



/*- Code  */

/*-- For communication with main(),getting the current ports,... */

sexp igor_ctx() { return ctx;};
sexp igor_env() { return env;};
/*
sexp sexp_current_input_port(sexp ctx) {
	return sexp_eval_string(ctx,"(current-input-port)", -1, env);
}
sexp sexp_current_output_port(sexp ctx) {
	return sexp_eval_string(ctx,"(current-output-port)", -1, env);
}
*/

/*-- Error handling */

void Abort_i(char *msg, char *file, int line) {
	fprintf(stderr,"Fatal error in igor: %s at %s:%d\n", msg, file,  line);
	fflush(stderr);
	abort();
}

void DAbort_i(char *msg, char *file, int line) {
	fprintf(stderr,"Development message: ");
	Abort_i(msg, file, line);
}


int report_error_i(int err, char *errormessage, const char *ctx, char *file, int line) {
	char errname[errsize] = "";
	char *context = (char *)ctx;
	
	if (err) {
		strerror_r(err, errname, errsize);

		if (context && errormessage) fprintf(stderr,"igor: %s -- %s [%s]\n", errormessage, errname, context);
		else if (errormessage) fprintf(stderr,"igor: %s  -- %s\n", errormessage, errname);
		else if (context) fprintf(stderr,"igor: %s [%s]\n", errname, context);
		else fprintf(stderr,"igor: %s\n", errname);
	}
	else {
		if (context && errormessage) fprintf(stderr,"igor: %s [%s]\n", errormessage, context);
		else if (errormessage) fprintf(stderr,"igor: %s\n", errormessage);
		else if (context) fprintf(stderr,"igor: Oops. [%s]\n", context);
		else fprintf(stderr,"igor: Oops.\n");
	}
	return 0;
}

sexp check_exception (int emit, sexp ctx, sexp res, char *message, char *subject) { // from  repl.c in the chibi-scheme distribution by AlexShinn@gmail.com
	sexp_gc_var1(err);
	sexp_gc_preserve1(ctx,err);

	err = SEXP_FALSE;

	ERRCON = res;

	if (res && sexp_exceptionp(res)) {

		err = sexp_current_error_port(ctx);
		if (emit) {
			if (! sexp_oportp(err))
				err = sexp_make_output_port(ctx, stderr, SEXP_FALSE);

			if (message) {
				sexp_write(ctx,sexp_c_string(ctx,message,-1), err);
				sexp_newline(ctx,err);
			}
			if (subject) {
				sexp_write(ctx,sexp_c_string(ctx,subject,-1), err);
				sexp_newline(ctx,err);
			}
		 
			sexp_print_exception(ctx, res, err);
			sexp_stack_trace(ctx, err);
		}
      //exit_failure();
		sexp_gc_release1(ctx);
  }
  return res;
}




/*-- memory handling */

/*--- arrays and whatnot */

void free_null_terminated_pointer_array(char **ptr) {
	int i = 0;

	for (i = 0; ptr[i]; i++) {
		if (ptr[i]) {
			Free(ptr[i]);
			ptr[i] = NULL;
		}
	}

	Free(ptr);
}

#if defined(USE_THE_DICKY_ONE)
#warning Using the dicky one
char **duplicate_null_terminated_string_array(char **ptr) {
	char **nptr;
	int i;

	if (!ptr) return NULL;

	for (i = 0; ptr[i];i++);

	nptr = (char **)calloc(i+2, sizeof(char *));
	if (!nptr) {
		fprintf(stderr,"\nOut of memory\n");
		abort();
	}
	nptr[i+1] = NULL;
	i--;

	for (; i >= 0;i--) nptr[i] = (ptr[i] ?  strdup(ptr[i]) : ptr[i]);

	return nptr;
}
#else
char **duplicate_null_terminated_string_array(char **ptr) {
	char **nptr;
	int i;

	if (!ptr) return NULL;

	for (i = 0; ptr[i];i++);

	nptr = (char **)calloc(i+4, sizeof(char *));
	if (!nptr) {
		fprintf(stderr,"\nOut of memory\n");
		abort();
	}
	nptr[i] = NULL;

	for (; i >= 0;i--) nptr[i] = (ptr[i] ?  strdup(ptr[i]) : ptr[i]);

	return nptr;
}
#endif


char **add_to_string_array(char *s, int n, char **array) { 
// s is the string to be added, n is the number of allocated elements counting the null terminator
// the string is strdup'd
	int N = -1;
	char **a;

#warning The "+4" is a symptom of having got things wrong. This works for the moment, but I need to fix it.


	if (array) {
		int i, k = N>n ? N+4 : n+4;
		for (N = 0; array[N]; N++); // N is currently the number of elements actually in the array
		
		a =  (char **)calloc(k+1, sizeof(char *));
		for (i = 0; i < N; i++) a[i] = array[i];
		a[i] = strdup(s);
		a[i+1] = NULL;

		free(array);
		array = a;
	}
	else {
		array = (char **)calloc(n+2, sizeof(char *));
		*array = strdup(s);
		array[1] = NULL;
	}
	return array;
}


// if n < 0 it indicates that s should be treated as a  null terminated array 
void fprintf_string_array(FILE *f, int n, char **s) {
	int i;
	if (!s) fprintf(f,"<nil>");
	else if (!*s) fprintf(f,"<null>");
	for (i = 0; (n < 0 && s[i]) || i < n; i++) {
		if (i) fprintf(f," ");
		
		fprintf(f, "%s", s[i]);
	}
}

/*--- support routines for the command chain */

/*---- Create an empty node */

cmd_t *new_cmd_t() {
	cmd_t *p = (cmd_t *)calloc(1, sizeof(cmd_t));
//	p->instringport = p->outstringport = SEXP_FALSE;
	p->in = 0;
	p->out = 1;
	p->err = 2;
	return p;
}


/*---- free a node and all the dynamic memory it uses, as well as its children */

void free_cmd(cmd_t *p) {
	int i = 0;

	if (!p) return;

	if (p->cmd) Free(p->cmd);
	
	if (p->argv) {
		for (i = 0; i < p->argc; i++)	{
			Free(p->argv[i]);
			p->argv[i] = NULL;
		}
		Free(p->argv);
		p->argv = NULL;
	}
	if (p->in > 2) {close (p->in); p->in = -1;}
	if (p->out > 2) {close (p->out); p->out = -1;}
	if (p->err > 2) {close (p->err); p->err = -1;}

	if (p->next) free_cmd(p->next);
	p->next = NULL;

	Free(p);
}



/*-- String manipulation  */

char *is_symbol(char *s) { 
	int i;
	for (i = 0; i < STABSIZE && symbol[i]; i++) {
		if (s == symbol[i]) return symbol[i];
	}
	return NULL;
}

char *interned_symbol(char *s) {
	int i;
	for (i = 0; i < STABSIZE && symbol[i]; i++) {
		if (s == symbol[i]) return symbol[i];
	}
	return NULL;
}

// s is the enclosing string, p is the start of the excision, n is the length of the excision
char *excise_string(char *s, char *p, int n) {
	strcpy(p, p+n);
	return s;
}


/* this will insert the string pointed to by "insertion" into s in front of the character pointed to by p
	If the total string length (plus the null) is greater than n, it will realloc s
*/
char *insert_string(int n, char *s, char *p, char *insertion) {
	char *tmp;

#if 1 || defined(fussy_debugging) // This should stay here.
	if (p < s) DAbort("Bad buffer pointer passed to insert_string");
#endif
	if (strlen(s) + strlen(insertion) + 1  > n) s = (char *)reallocate(s, strlen(s) + strlen(insertion) + 1);

	tmp = strdup(p);
	strcpy(p, insertion);
	strcat(s, tmp);
	Free(tmp);
	return s;
}

// This will, of course, need freeing....
char *string_array_as_string(int argc, char **argv) {
	int i, n;
	char *s = NULL;

	if (argc > 0) {
		for (i = n = 0; i < argc; i++) {
			n += 1 + strlen(argv[i]);
		}
		s = (char *)malloc(n);

		*s = 0;
		strcat(s, argv[0]);

		for (i = 1; i < argc; i++) {
			strcat(s, " ");
			strcat(s, argv[i]);
		}
	}
	return s;
}




/*-- File handling */

void delete_file_stat(struct stat *fs) {
	if (fs) free(fs);
}



struct stat *file_stat(char *filename) {
	int n;
	struct stat *fs = (struct stat *)calloc(1, sizeof(struct stat));
	if (!fs) return NULL;

	n = stat(filename, fs);
	if  (n) free(fs);
	return (n ? NULL : fs);
}

// The following would really be better done with scheme routines, but
// I cannot get rid of the memory leak associated with them.


unsigned long get_stat_dev(char *filename) { /* device which holds the file */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs)  {
		n = fs->st_dev;
		free(fs);
	}
	else n = 0;
	return n;
}


unsigned long get_stat_ino(char *filename) { /* inode number */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_dev;
		free(fs);
	}
	return n;
}

unsigned short get_stat_mode(char *filename) { /* protection */
	struct stat *fs = NULL;
	unsigned short n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_mode;
		free(fs);
	}
	return n;
}

unsigned long get_stat_nlink(char *filename) { /* number of hard links */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_nlink;
		free(fs);
	}
	return n;
}

unsigned short get_stat_uid(char *filename) { /* user ID of owner */
	struct stat *fs = NULL;
	unsigned short n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_uid;
		free(fs);
	}
	return n;
}

unsigned short get_stat_gid(char *filename) { /* group ID of owner */
	struct stat *fs = NULL;
	unsigned short n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_gid;
		free(fs);
	}
	return n;
}

unsigned long get_stat_rdev(char *filename) { /* device ID (if special file) */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_rdev;
		free(fs);
	}
	return n;
}

unsigned long get_stat_size(char *filename) { /* total size, in bytes */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_size;
		free(fs);
	}
	return n;
}

unsigned long get_stat_blksize(char *filename) { /* blocksize for file system I/O */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_blksize;
		free(fs);
	}
	return n;
}

unsigned long get_stat_blocks(char *filename) { /* number of 512B blocks allocated */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_blocks;
		free(fs);
	}
	return n;
}

unsigned long get_stat_atime(char *filename) { /* time of last access */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_atime;
		free(fs);
	}
	return n;
}

unsigned long get_stat_mtime(char *filename) { /* time of last modification */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_mtime;
		free(fs);
	}
	return n;
}

unsigned long get_stat_ctime(char *filename) { /* time of last status change */
	struct stat *fs = NULL;
	unsigned long n = 0;
	fs = file_stat(filename);
	if (fs) {
		n = fs->st_ctime;
		free(fs);
	}
	return n;
}

int Close(int fd) {
	if (fd >= 0) fd = close(fd);
	else {
#if defined(fussy_debugging)
		abort();
#endif
		return EBADF;
	}
	return 0;
}

char *read_all(int fd) {
	char *inputstring = NULL;
	int i = 0, n = 0, k = 0; // i is the the string length, n is amount read, k is the size of the buffer
	
	if (fd < 0) return NULL;

	inputstring = (char *)malloc(1024);
	if (inputstring) k += 1024;
	else return NULL;
		
	*inputstring = 0;

	for (n = read(fd, inputstring + i, 1023); n > 0; n = read(fd, inputstring + i, 1023)) {
		i += n;
		inputstring[i] = 0;
		inputstring = (char *)realloc(inputstring, k+1024);
		if (inputstring) k += 1024;
		else {
			report_error(0,"Unable to allocate memory for builtin input pipe!", __FUNCTION__);
			return NULL;
		}
	}

	if (n < 0) {
		char *em;

		asprintf(&em,"Unable to read from input file descriptor for %s", inputstring);
		if (em) {
			report_error(errno, em, __FUNCTION__);
			free(em);
		}
		else {
			report_error(errno, "Unable to read from input file descriptor", __FUNCTION__);
		}
		if (inputstring) free(inputstring);
		return NULL;
	}

	close(fd);
	return inputstring;
}


/*-- support routines for builtin commands  */

  void delete_builtin(Builtin *node) {
	if (node) {
		delete_builtin(node->left);
		delete_builtin(node->right);
		Free(node->name);
		Free(node);
	}
}
	

Builtin *insert_builtin(Builtin *tree, char *name, builtin_func func) {
	if (!tree) {
		tree = (Builtin *)malloc(sizeof(Builtin));
		tree->name = strdup(name);
		tree->func = func;
		tree->left = tree->right = NULL;
	}
	else if (strcmp(name, tree->name) < 0) tree->left = insert_builtin(tree->left, name, func);
	else if (strcmp(name, tree->name) > 0) tree->right = insert_builtin(tree->right, name, func);
	else if (strcmp(name, tree->name) == 0) tree->func = func;
	
	return tree;
}

Builtin *member(char *name, Builtin *tree) {
	if (!tree) return NULL;
	else if (strcmp(name, tree->name) < 0) return member(name, tree->left);
	else if (strcmp(name, tree->name) > 0) return member(name, tree->right);
	else if (strcmp(name, tree->name) == 0) return tree;
	return NULL;
}


/*-- support routines associated command line syntax */

/*--- This adds a string into the list of systax elements (like redirection symbols) */

void add_magic(char *str) {
	int i = 0;

	/* NOTE: Order of insertion is important here ... ">>" should go in before ">", for example */

	if (!magic_string) {
		magic_string = (char **)calloc(2, sizeof(char *));
		n_magic_strings = 0; // just to be sure
		add_magic(str);
	}
	else {
		for (i = 0; i < n_magic_strings && strcmp(magic_string[i], str); i++);

		if (i >= n_magic_strings) {
			magic_string = (char **)reallocate(magic_string, (n_magic_strings+2)*sizeof(char *));
			if (is_symbol(str)) magic_string[n_magic_strings++] = interned_symbol(str);
			else magic_string[n_magic_strings++] = strdup(str);
			magic_string[n_magic_strings] = 0;
		}
	}
}

void remove_magic() {
	int i;
	for (i = 0; i < n_magic_strings; i++) {
		Free(magic_string[i]);
	}
	n_magic_strings = 0;
	Free(magic_string);
}


char *is_magic(char *s) {
	int i = 0;

	for (i = 0; i < n_magic_strings && strncmp(s, magic_string[i], strlen(magic_string[i])); i++);
	if (i < n_magic_strings) return magic_string[i];
	else return NULL;
}


/*-- support routines for parsing */


// this returns the expansion *as one string*
char *word_expand_string(char *s) {
	wordexp_t arg;
	int i, n, k;
	int err = 0;
	char *r;

	if (!s) return NULL;
	s = strdup(s);

	if (!*s) return s;
	
	err = wordexp(s, &arg, 0);
	if (err) return s; 

	free(s);

	for (i = 0, n = 0; i < arg.we_wordc; i++) {
		n += strlen(arg.we_wordv[i]);
	}
	
	r = malloc(1 + (n + i)*sizeof(char));
	*r = 0;
	for (k = 0; k < i; k++) {
		if (k && k+1 < i) strcat(r," ");
		strcat(r,arg.we_wordv[k]);
	}
	wordfree(&arg);
	return r;
}


char protect_char(int maskit, char c) {
	char *domain = "()";
	char codomain[] = {'('| 0x80, ')' | 0x80, 0};
	char *from, *to;
	char *s;

	if (maskit) from = domain, to = codomain;
	s = strchr(from,c);
	if (!s) return c;
	else return to[s-from];
}

char *first_non_space_char(char *s) {
	while (s && *s && isspace(*s)) s++;
	return s;
}

char *jump_fence_c(char *cp, char *collecting, char lookingfor, char escape, int eat_quotes) {
	if (!eat_quotes) *collecting++ = *cp;
	cp++; // Skip the initial quote; if it weren't there, we wouldn't be here.

	for (; cp && *cp && *cp != lookingfor; *collecting++ = *cp++) {
		if (*cp == escape &&  cp[1] == lookingfor) {
			cp++;
		}
		collecting[1] = 0;
	}

	if (!eat_quotes) *collecting++ = *cp;
	*collecting = 0;

	cp++; // drop the terminal fence
	return cp;
}


char *jump_fence(char *cp, char lookingfor, char escape) {
	cp++; // Skip the initial quote; if it weren't there, we wouldn't be here.
	for (; cp && *cp && *cp != lookingfor; cp++) {
		if (*cp == escape &&  cp[1] == lookingfor) {
			cp++;
		}
	}
	cp++; // drop the terminal fence
	return cp;
}

/* NOTE: The function below returns a pointer to just past the
	s-expression; if there is a parsing problem, it returns s */

char *paren_protection(char *s, char escape, int mask) { 
	char expecting = 0; // we expect either whitespace or the end of the string
	char fence, unfence;
	
	if (!s) return s;
	
	if (mask) {
		fence = '(';
		unfence = ')';
		if (*s == fence) expecting = unfence;
		else if (*s == '[') expecting = ']';
		else if (*s == '{') expecting = '}';
	}
	else {
		fence = protect_char(1,'(');
		unfence = protect_char(1,')');
		if (*s == fence) expecting = unfence;
		else if (*s == '[') expecting = ']';
		else if (*s == '{') expecting = '}';
	}		

	for (s++; *s && *s != expecting;) {
		if (*s == escape) {
			s+=2;
			if (!*s) return s;
		}
		else if (expecting && *s == expecting) {
			*s = protect_char(mask, *s);
			s++;
			return s;
		}
		else if (*s == '"') {
			s = jump_fence(s, '"', escape);
		}

		else if (*s == '`') {
			s = jump_fence(s, '`', escape);
		}
		
		else if (*s == fence) {
			*s = protect_char(mask, *s);
			s = paren_protection(s, escape, mask);
		}

		else if (!expecting  && (isspace(*s) || !*s)) {
			return s;
		}
		else if (!strncmp(s,"#\\", 2)) {
			s+=3; // handles scheme characters
		}
		else s++;
	}

	if (!expecting && (!*s || isspace(*s))) return s;
	else if (*s == expecting) {
		s++;
		return s;
	}
	else return s;
}	

char *sexpr_depth(char *buffer, char *str) {
	// This returns a dynamically allocated string of the unclosed
	// bits, namely members of {'(', '[', '{' and '"'}.  The string should be 
	// freed by the calling function
	int n = 0;
	int in_quote = 0;
	char *p = 0;

	if (!str) DAbort("Bad 'str' pointer passed to sexpr_depth.");
	
	buffer = (char *) reallocate(buffer, strlen(str) + 3);
	memset(buffer, 0, (strlen(str) + 3)*sizeof(char));

	for (p = str; *p;) {
		//printf("%c <==> %d:%s | %s\n", *p, n,buffer, str);
		if (!strcmp(p, "#\\\"")) p += 3; // skip a scheme literal double quote
		else if (*p == '"') {
			in_quote = !in_quote;
			p++;
		}
		else if (!in_quote) {
			if (strchr(start_fence, *p)) {
				buffer[n++] = *p++;
				buffer[n] = 0; // probably superfluous
				continue; /****** THIS MAY BE WRONG ******/
			}
			if (n <= 0 && strchr(end_fence, *p)) {
				return buffer;
			}
			else if (n > 0 && strchr(end_fence, *p)) {
				if ((buffer[n-1] == '(' && *p == ')') || (buffer[n-1] == '[' && *p == ']') || (buffer[n-1] == '{' && *p == '}')) {
					if (n > 0) n--;
					buffer[n] = 0;
					p++;
				}
				else {
					buffer[n++] = '#';
					buffer[n] = 0; // probably superfluous
					return buffer;
				}
			}
			else p++;
		}
		else if (in_quote) p++;
		else Abort("Problem parsing an s-expression.  Something doesn't believe in the law of the excluded middle.");
	}
	return buffer;
}


int starts_sexp(char *s) {
	char *p = s;
	for (; p && *p && isspace(*p); p++);
	if (*p == '(') return 1;
	if (*p == '[') return 2;
	if (*p == '{') return 3;
	return 0;
}

int is_unfinished_sexp(char *s) {
	return s && starts_sexp(s) && !is_sexp(s);
}

int is_sexp(char *s) {
	char *parens = NULL;
	int n = 0;

	if (s && starts_sexp(s)) {
		parens = sexpr_depth(parens, s);
		if (parens) n = strlen(parens);
		//fprintf(stderr,"--- %s | %s ---\n", s, parens);
		Free(parens);
		n = !n;
	}
	return n;
}


/* NOTE: The function below returns a pointer to just past the
	s-expression; if there is a parsing problem, it returns s */


char *jump_sexp(char *s, char escape) {
	char expecting = 0; // we expect either whitespace or the end of the string

	if (*s == '(') expecting = ')';
	else if (*s == '[') expecting = ']';
	else if (*s == '{') expecting = '}';

	for (s++; *s && *s != expecting;) {
		if (escape && *s == escape) {
			s+=2;
			if (!*s) return s;
		}
		else if (expecting && *s == expecting) {
			s++;
			return s;
		}
		else if (*s == '"') {
			s = jump_fence(s, '"', escape);
		}
		
		else if (strchr(start_fence, *s)) {
			s = jump_sexp(s, escape);
		}
		else if (!expecting  && (isspace(*s) || !*s)) {
			return s;
		}
		else if (!strncmp(s,"#\\", 2)) {
			s+=3; // handles scheme characters
		}
		else s++;
	}

	if (!expecting && (!*s || isspace(*s))) return s;
	else if (*s == expecting) {
		s++;
		return s;
	}
	else return s;
}	





/*--- Support routines for command execution */

/*---- Used to rewrite "define" and "define-syntax" type statements so they are eval'd */

char *guard_definitions(char *s) {
	char *p = NULL, *r = NULL;
	if (!s) return s;

	for (p = s; *p && isspace(*p); p++);

	// *** This ought to catch "define" and "define-syntax", ... 
	if (!strncmp(p,"(define",strlen("(define"))) { 
		asprintf(&r, "(eval '%s)", p);
		return r;
	}
	else return strdup(s);
}



/*-- Routines which call on chibi directly  */

/*--- Utility functions */ 

sexp sexp_get_procedure(sexp ctx, sexp env, char *procname) {
	sexp_gc_var2(proc,tmp);
	sexp_gc_preserve2(ctx,proc,tmp);

	tmp = sexp_intern(ctx,procname,-1);
	proc = sexp_env_ref(ctx,env,tmp,SEXP_FALSE);
	if (sexp_procedurep(proc))	sym = proc;
	else sym = SEXP_FALSE;
	sexp_gc_release2(ctx);
	return sym;
}

int sexp_get_fixnum_value(sexp ctx, sexp env, char *name) {
	int result = 0;
	sexp_gc_var2(obj,tmp);
	sexp_gc_preserve2(ctx,obj,tmp);
	tmp = sexp_intern(ctx,name,-1);
	obj = sexp_env_ref(ctx,env,tmp,SEXP_FALSE);

	if (sexp_fixnump(obj))	result = sexp_unbox_fixnum(obj);
	else result = ~0;

	sexp_gc_release2(ctx);

	return result;
}

long int sexp_get_long_fixnum_value(sexp ctx, sexp env, char *name) {
	long  int result = 0;
	sexp_gc_var2(obj,tmp);
	sexp_gc_preserve2(ctx,obj,tmp);
	tmp = sexp_intern(ctx,name,-1);
	obj = sexp_env_ref(ctx,env,tmp,SEXP_FALSE);

	if (sexp_fixnump(obj))	result = sexp_unbox_fixnum(obj);
	else result = ~0L;

	sexp_gc_release2(ctx);

	return result;
}


char *get_input_string(sexp ctx, sexp instr) {
	char *s = NULL;
	sexp_gc_var3(gos,int1,sstr);
	sexp_gc_preserve3(ctx,gos,int1,sstr);
	
	gos = sexp_get_procedure(ctx,env,"get-output-string");
	int1 = sexp_cons(ctx,instr,SEXP_NULL);
	sstr = sexp_apply(ctx,gos,int1);
	if (sexp_stringp(sstr)) s = strdup(sexp_string_data(sstr));
	else s = NULL;
	sexp_gc_release3(ctx);
	return s;
}

sexp make_input_string(sexp ctx, char *str) {
	sexp isp = SEXP_FALSE;
	char *s = NULL;

	asprintf(&s,"(open-read-string \"%s\")", str);
	if (s && *s) isp = sexp_eval_string(ctx, s, -1, env);
	else isp = SEXP_FALSE;

	free(s);
	return isp;
}


/*---- exotic wordexp support */
 
sexp sexp_wordexp_sexp(sexp ctx, sexp sstr) {
	int i = 0;
	char *s = NULL;
	wordexp_t w;
	sexp_gc_var2(result, str);
	sexp_gc_preserve2(ctx,result,str);

	if (!sexp_stringp(sstr)) {
		report_error(0, "word-exp takes a string argument", __FUNCTION__);
		sexp_gc_release3(ctx);
		return SEXP_FALSE;
	}

	s = strdup(sexp_string_data(sstr));

	i = wordexp(s,&w,0);
	if (!i) {
		result  = SEXP_NULL;
		for (i = w.we_wordc-1; i >= 0; i--) {
			str = sexp_c_string(ctx, w.we_wordv[i], -1);
			result = sexp_cons(ctx,str,result);
		}
	}
	free(s);
	wordfree(&w);

	sexp_gc_release2(ctx);
	return result;
}

sexp sexp_wordexp_ffi(sexp ctx, sexp self, sexp n, sexp sstr) {
	int i = 0;
	char *s  = NULL;
	wordexp_t w;
	sexp_gc_var2(result, str);
	sexp_gc_preserve2(ctx,result,str);

	if (sexp_unbox_fixnum(n) != 1) {
		char *em = NULL;

		asprintf(&em,"word-exp takes exactly one argument, got %ld", sexp_unbox_fixnum(n));
		if (em) {
			report_error(0, em, __FUNCTION__);
			free(em);
		}
		else {
			report_error(0, "word-exp passed the wrong number of arguments", __FUNCTION__);
		}
		sexp_gc_release2(ctx);
		return SEXP_FALSE;
	}


	if (!sexp_stringp(sstr)) {
		report_error(0, "word-exp takes a string argument", __FUNCTION__);
		sexp_gc_release3(ctx);
		return SEXP_FALSE;
	}

	s = strdup(sexp_string_data(sstr));

	i = wordexp(s,&w,0);
	if (!i) {
		result  = SEXP_NULL;
		for (i = w.we_wordc-1; i >= 0; i--) {
			str = sexp_c_string(ctx, w.we_wordv[i], -1);
			result = sexp_cons(ctx,str,result);
		}
	}
	free(s);
	wordfree(&w);

	sexp_gc_release2(ctx);
	return result;
}


char **wordexp_string_array(char *str) {
	wordexp_t w;
	char **arry = NULL;
	int i = wordexp(str,&w,0);
	if (!i) {
		arry = (char **)malloc((1 + w.we_wordc) * sizeof(char *));
		
		for (i = 0; i < w.we_wordc; i++) {
			arry[i] = strdup(w.we_wordv[i]);
		}
		arry[i] = NULL;
		wordfree(&w);
		return arry;
	}
	else {
		//fprintf(stderr,"bad expansion for \"%s\"\n", str);
		return calloc(1,sizeof(char *)); // Returning null just doesn't work.
	}
}

void delete_string_array(char  **wa) {
	int i = 0;
	for (i = 0; wa && wa[i]; i++) {Free(wa[i]); wa[i] = NULL;} // Just in case there is something pointing into wa
	if (wa && wa != symbol) free(wa);
}


/*--- File handling */

sexp open_input_fd(int fd) {
	sexp inp = SEXP_FALSE, int1 = SEXP_FALSE, int2 = SEXP_FALSE;

	sexp_preserve_object(ctx,inp);
	sexp_preserve_object(ctx,int1);
	sexp_preserve_object(ctx,int2);

	int1  = sexp_eval_string(ctx,"open-input-file-descriptor", -1, env);
	int2 = sexp_cons(ctx,sexp_make_integer(ctx, fd), SEXP_NULL);

	inp = sexp_apply(ctx,int1, int2);

	sexp_release_object(ctx,int1);
	sexp_release_object(ctx,int2);

	return inp;
}

sexp open_output_fd(int fd) {
	sexp outp = SEXP_FALSE, int1 = SEXP_FALSE, int2 = SEXP_FALSE;

	sexp_preserve_object(ctx,outp);
	sexp_preserve_object(ctx,int1);
	sexp_preserve_object(ctx,int2);

	int1  = sexp_eval_string(ctx,"open-output-file-descriptor", -1, env);
	int2 = sexp_cons(ctx,sexp_make_integer(ctx, fd), SEXP_NULL);

	outp = sexp_apply(ctx,int1, int2);

	sexp_release_object(ctx,int1);
	sexp_release_object(ctx,int2);

	return outp;
}


/*--- List construction */

sexp sexp_list3(sexp ctx, sexp a, sexp b, sexp c) {
	sexp_gc_var1(res);
	sexp_gc_preserve1(ctx,res);
	res = sexp_cons(ctx, c, SEXP_NULL);
	res = sexp_cons(ctx, b, res);
	res = sexp_cons(ctx, a, res);
	sexp_gc_release1(ctx);
	return res;
}


sexp sexp_list4(sexp ctx, sexp a, sexp b, sexp c, sexp d) {
	sexp_gc_var1(res);
	sexp_gc_preserve1(ctx,res);
	res = sexp_cons(ctx, d, SEXP_NULL);
	res = sexp_cons(ctx, c, res);
	res = sexp_cons(ctx, b, res);
	res = sexp_cons(ctx, a, res);
	sexp_gc_release1(ctx);
	return res;
}

sexp argv_to_list(sexp ctx, char **argv, int n) {
	// if n < 0 it assumes that the array is null terminated, else it must have n args
	
	char **av = NULL;
	int N = 0, i = 0;
	sexp lst = SEXP_NULL;
	//sexp_gc_var1(lst);
	//sexp_gc_preserve1(ctx, lst);

	if (n < 0) {
		for (i = 0; argv[i]; i++);
		N = i;
		av = argv;
	}
	else {
		N = n;
		av = (char **)calloc(N+1, sizeof(char *));
		for (i = 0; i < N; i++) av[i] = argv[i];
		av[i] = NULL;
	}
	
	lst = SEXP_NULL;
	for (i = N-1; i >= 0; --i) lst = sexp_cons(ctx, sexp_c_string(ctx,argv[i],-1), lst); // construct it in reverse

	if (av && av != argv) free(av);
	return lst;
}





/*--- Path/element globbing and expansion of environment variables */


/*---- returns a list of expansions for sstr  */

sexp sexp_wordexp(char *sstr) {
	int i = 0;
	char *s = NULL;
	wordexp_t *w;
	sexp_gc_var2(result, str);
	sexp_gc_preserve2(ctx,result,str);

	w = (wordexp_t *)calloc(1,sizeof(*w));

	if (!w) abort();

	s = strdup(sstr);

	i = wordexp(s,w,0);
	if (!i) {
		result  = SEXP_NULL;
		for (i = w->we_wordc-1; i >= 0; i--) {
			str = sexp_c_string(ctx, w->we_wordv[i], -1);
			result = sexp_cons(ctx,str,result);
		}
	}
	else result = SEXP_FALSE;

	free(s);
	wordfree(w);
	if (w) free(w);

	sexp_gc_release2(ctx);
	return result;
}
	
/*---- calls the scheme routine expand-path to complete the path element in s */

  char *completed_path(char *s) { // Don't free s, the calling routine needs to deal with it
	sexp_gc_var3(expanded,p,r);
	char *sexpr = NULL, *t = NULL;
	sexp_gc_preserve3(ctx,expanded,p,r);

	if (s) {
		if (*s == '/' || !strncmp(s,"./",2) || !strncmp(s,"../",3))  return strdup(s); // it is an absolute or relative reference

		if (is_sexp(s)) { 
			t = evaluate_scheme_expression(0, s,NULL);
		//	Free(s);
		}
		else {
			t = s;
		}

		//expanded = sexp_eval(ctx, sexp_list2(ctx,sexp_eval_string(ctx,"expand-path",-1,ENV),sexp_c_string(ctx,t,-1)), ENV);

		r = SEXP_NULL;
		r = sexp_cons(ctx, sexp_c_string(ctx,s,-1), r);
		p = sexp_get_procedure(ctx,env,"expand-path");
		expanded = sexp_apply(ctx, p, r);		
		
#if 0
		fprintf(stdout,"%s --> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ", s);
		fflush(stdout);

		sexp_write(ctx,expanded,sexp_current_output_port(ctx));
		sexp_newline(ctx,sexp_current_output_port(ctx));
#endif

		sexpr = (sexp_stringp(expanded) ? strdup(sexp_string_data(expanded)) : NULL);

		//fprintf(stderr, ">>>>>> %s\n", sexpr);
		//Free(t);

		if (t && t != s) Free(t);

		sexp_gc_release3(ctx);

		if (sexpr && !strcmp(sexpr,"#f") && access(sexpr,F_OK) == Ok) return NULL;
		else return sexpr;
	}
	return NULL;
}

/*---- Builtins and their close kin  */

/*----- Set an environment variable */

void igor_set(sexp ctx, char *variable, char *value) {
	char *p = NULL;
	char *fmt = "(set! %s  %s)";
	
	p = (char *)malloc((strlen(fmt) + strlen(variable) + strlen(value) + 2) * sizeof(char));
	sprintf(p, fmt, variable, value);
	sexp_eval_string(ctx,p,-1,env);
	setenv(variable, value, 1);
	free(p);
}

/*----- set the prompt for reading a continued line */

void set_prompt_continuation(char *buffer) {
	igor_set(ctx, "*igor-prompt-for-line-continuation-string*", buffer);
}

void free_prompt_continuation() {
	//sexp_eval_string(ctx, "", -1, ENV);
	igor_set(ctx, "*igor-prompt-for-line-continuation-string*", "");
}

/*----- updates the name of the file used by libhistory */

/* NB: it would be easy to save commands history in their cwd  */

void refresh_history_filename() {
	char *s = NULL;
	sexp_gc_var1(igorhistfile);
	
	sexp_gc_preserve1(ctx, igorhistfile);

	Free(history_file);			
	
	igorhistfile = sexp_eval_string(ctx, IGOR_HISTORY_FILE_VAR, -1, ENV);

	if (sexp_procedurep(igorhistfile)) {
		s = evaluate_scheme_expression(1, "(" IGOR_HISTORY_FILE_VAR ")",NULL);
	}
	else if (sexp_booleanp(igorhistfile)) {
		if (sexp_equalp(ctx,igorhistfile,SEXP_FALSE)) s = strdup("#f");
		else  s = strdup("#t");
	}
	else if (sexp_stringp(igorhistfile)) {
		s = evaluate_scheme_expression(1, IGOR_HISTORY_FILE_VAR ,NULL);
	}
	else {
		s = strdup("#f");
	}

	if (s && !strcmp(s,"#t")) {
		Free(s);
		Free(history_file);
		history_file = strdup(IGOR_HISTORY_DEFAULT);
	}
	else if (!s || !strcmp(s,"#f")) {
		Free(s);
		history_file = NULL;
	}
	else {
		Free(history_file);
		history_file = s;
	}
	
}

/*-- command line input routines */

/*--- intermediate level routine for getting the next command line (wraps readline/fgets), resolves prompt, history... */

char *read_line(FILE *f, char *prompt_function) {
	char *cmd = NULL;
	static char *linebuffer = NULL;
	static int n = 0;
	static int n1 = 85, n2 = 171;
	static int k = -1;

	if (!f && !running_script) {
		char *prompt = NULL;
		//char *prompt = strdup("Thur? ");

		if (is_sexp(prompt_function)) {
			prompt = evaluate_scheme_expression(1, prompt_function,NULL);
			if (!prompt) prompt = "? ";

		}
		else {
			if (prompt) prompt = strdup(prompt_function);
			else prompt = strdup("> ");
		}

		rl_filename_quote_characters = "\"\t ()<>$\\,[]{}*&#|;'?";
		rl_completer_quote_characters = "\"'";
		rl_filename_quoting_desired = 1;

		cmd = readline(prompt);

		if (cmd && !linebuffer && !strncmp(cmd,"#!/",3)) { // this is likely to be the hash-bang at the start of a script
			linebuffer = strdup("");
			if (prompt) Free(prompt); // Added 2013-12-27-16:21:25 
			return read_line(f, prompt_function);
		}

		if (!f) {
			refresh_history_filename();
		}
		if (cmd && *cmd) {
			add_history(cmd);
			if (history_file) append_history(1,history_file);
		}
		Free(prompt);
	}
	else {
		//if (f == stdin) fprintf(stderr,"** The file seems to be stdin! (%s)\n", __PRETTY_FUNCTION__);

		if (k < 0) k = n1 + n2;
		cmd = NULL;

		if (!linebuffer && !(linebuffer = malloc(k))) Abort("Out of memory");
		*linebuffer = 0;
		
		for (n = strlen(linebuffer); !feof(f) && (*linebuffer == 0 || linebuffer[n-1] != '\n'); n = strlen(linebuffer)) {
			if (k - n < n1) {
				int t = 0;

				linebuffer = (char *)reallocate(linebuffer, k+n2);

				if (!linebuffer) Abort("Out of memory");
				t = k+n2;
				n1 = n2;
				n2 = k;
				k = t;
			}
			fgets(linebuffer+n, k-n, f);
		}

		n = strlen(linebuffer);
		if (n > 0 && linebuffer[n-1] == '\n') {
			linebuffer[n-1] = 0;
			cmd = strdup(linebuffer);
		}
		else if (n > 0) cmd = strdup(linebuffer); // Must have hit the end of the file without a newline
		else cmd = NULL;
	}

	if (f && feof(f) && linebuffer && !*linebuffer) {
		if (cmd) Free(cmd);
		return NULL;
	}

	return cmd;
}

/*--- high level routine to get next command. handles continuations, selects prompts, handles ^d, & eof */

// Note that s-expressions which span lines are treated quite differently to "normal" commandlines.

char *get_commandline(FILE *f, char *buffer) {
	char *prompt = NULL;
	char *current_line = NULL;
	char *sdepth = NULL;
	int depth = 0;
	int collecting_s_exp = 0;
	int ctrld = 0;

	char *stdprompt = "(prompt)";
	char *contprompt = "(prompt-for-line-continuation)";
	char *sexpcontprompt = "(prompt-for-sexp-continuation)";
	char *remindprompt = "(prompter)";

	//if (f == stdin) fprintf(stderr,"** The file seems to be stdin! (%s)\n", __PRETTY_FUNCTION__);

	if (!buffer) {
		prompt = stdprompt;
		buffer = (char *)malloc(1);
		*buffer = 0;
	}
	else {
		prompt = contprompt;
		collecting_s_exp = is_sexp(buffer);
		sdepth = sexpr_depth(sdepth, buffer);
		depth = strlen(sdepth);
	}

	for (;;) {
		int current_line_len = 0;

		if (buffer && *buffer && is_unfinished_sexp(buffer)) {
			prompt = sexpcontprompt;
		}

		current_line = read_line(f, prompt);
		if (current_line) current_line_len = strlen(current_line);
		
		if ((current_line_len == 1 && *current_line == '\\') ||
			(current_line_len > 1
				&& current_line[current_line_len-1] == '\\'  // the last char is a backslash
				&& current_line[current_line_len-2] != '\\') // and the second to last *isn't*
			) {
			prompt = contprompt;
		}
		
		

		//fprintf(stderr,"[%s: %s]  cse=%d  sdepth = %s depth = %d\n", prompt, current_line, collecting_s_exp, sdepth, depth);


		Note("Top of loop: I've read the line and set the prompt");

		if (current_line && !*buffer) {
			Note("First entry in buffer");
			collecting_s_exp = (starts_sexp(current_line) || is_unfinished_sexp(current_line));
			sdepth = sexpr_depth(sdepth, current_line);
			if (sdepth) depth = strlen(sdepth);
			else depth = 0;
		}
		
		if (!current_line && ctrld) { // must be a continuation some such
			Note("control-d");
			*buffer = 0;
			Free(buffer);
			buffer = NULL;
			if (sdepth) Free(sdepth);
			sdepth = 0;
			Note("...exiting 1");
			return NULL;
		}

		if (!current_line) { // "control-d" ...
			if (!buffer || !*buffer) { // exit shell
				Note("Exit");
				*buffer = 0;
				Free(buffer);
				buffer = NULL;
				if (sdepth) Free(sdepth);
				sdepth = 0;
				Note("...exiting 2");
				return NULL;
			}
			else { // print buffer as prompt and continue editing
				Note("print accumulated  buffer and continue");
				ctrld++;
				set_prompt_continuation(buffer);
				prompt = remindprompt;
				continue;
			}
		}
		else if (!collecting_s_exp && !strcmp(current_line, continuation_str)) { // blank continuation line in a non-sexp
			Note("Blank  continuation  line");
			ctrld = 0;
			Free(current_line);
			current_line = 0;
			set_prompt_continuation(buffer);
			prompt = contprompt;
			continue;
		}
		else if (!collecting_s_exp && !*current_line) { // early exit -- we know we don't have to adjust "buffer"
			Note("Early exit");
			Free(current_line);
			current_line = 0;
			if (sdepth) Free(sdepth);
			sdepth = 0;
			free_prompt_continuation();
			Note("...exiting");
			return buffer;
		}
		else if (collecting_s_exp && !*current_line && is_unfinished_sexp(buffer)) { // return to the top of the loop -- we know we don't have to adjust "buffer"
			Note("Still collecting an unfinished s-expression");
			ctrld = 0;
			Free(current_line);
			current_line = 0;
			prompt = contprompt;
			continue;
		}
		else if (collecting_s_exp && *current_line && is_unfinished_sexp(buffer)) { // return to the top of the loop
			ctrld = 0;
			buffer = (char *)reallocate(buffer, strlen(buffer) + strlen(current_line) + 2);
			strcat(buffer, " ");
			strcat(buffer, current_line);
			Free(current_line);
			current_line = 0;

			if (is_unfinished_sexp(buffer)) {
				prompt = sexpcontprompt;
			}
			else {
				prompt = prompt;
				collecting_s_exp = 0;
				if (sdepth) Free(sdepth);
				sdepth = 0;
				if (current_line) Free(current_line);
				current_line = 0;
				free_prompt_continuation();
				Note("...exiting");
				return buffer;
				
			}

			continue;
		}
		else {
			Note("... just do stuff");
			ctrld = 0;
			buffer = (char *)reallocate(buffer, strlen(buffer) + strlen(current_line) + 1);
			strcat(buffer, current_line);
			Free(current_line);
			current_line = 0;
		}

		if (!collecting_s_exp) {
			Note("not collecting s-expression");
			if (strcmp(buffer + strlen(buffer)-strlen(continuation_str), continuation_str)) {
				Note("... continuation ...");
				if (sdepth) Free(sdepth);
				sdepth = 0;
				if (current_line) Free(current_line);
				current_line = 0;
				free_prompt_continuation();
				Note("...exiting");
				return buffer;
		      // because it doesn't end with the continuation string....
			}
			else {
				buffer[strlen(buffer)-strlen(continuation_str)] = 0;
			}
		}
		else { // we are collecting an s-expression, and we wont stop till we are done.
			Note("collecting s-expression");
			sdepth = sexpr_depth(sdepth, buffer);
			depth = strlen(sdepth);
			if (depth == 0) {
				if (sdepth) Free(sdepth); 
				if (current_line) Free(current_line);
				sdepth = 0;
				current_line = 0;
				free_prompt_continuation();
				Note("...exiting");
				return buffer;
			}
		}

		ctrld = 0;
	}

	if (sdepth) Free(sdepth);
	if (current_line) Free(current_line);
	Note("...exiting");
	return NULL;
}


/*-- Command execution calls  */

/*--- straight execution of  a command array  */

sexp execute_command_array(char **argv) { // DOES NOT FREE ANYTHING
	char *cmds = NULL;
	char **tokenise_cmdline(char *cmdline);
	cmd_t *process_token_list(char **Argv, int in, int out,int err);
	void run_command_prologue(char *cmds);
	void run_command_epilogue(char *cmds, sexp rv);
	sexp run_commands(cmd_t *cmd);

	cmd_t *C = 0;
	int i  = 0, k = 0;

#if 0
	for (i = 0; argv && argv[i]; i++) {
		fprintf(stderr,"in eca[%d] %s\n", i, argv[i]);
	}
#endif

	if (!argv || !*argv) return SEXP_NEG_ONE;

	for (i = 0, k = 0; argv[i]; i++) k += strlen(argv[i])+1;

	cmds = malloc(k);
	*cmds = 0;

	if (*argv) strcat(cmds,argv[0]);
	
	for (i = 1; argv[i]; i++) {
		strcat(cmds," ");
		strcat(cmds, argv[i]);
	}

//	C = process_token_list(argv,0,1,2);
//	C->cmd = cmds;

	// Call to run-command-prologue
	run_command_prologue(cmds);

	if (C) {
		sexp rtv = SEXP_FALSE;
		sexp_preserve_object(ctx, rtv);

		rtv =  run_commands(C);

	   // Call to run-command-epilogue (with return value)
		run_command_epilogue(cmds, rtv);

		free_cmd(C);

		return rtv;
	}
	else {
		return SEXP_NEG_ONE;
	}
}

/*--- straight execution of  a commandline  */

#if defined(RECURSIVE_DESCENT)
sexp execute_command_string(char *cmds) { // The string cmds is freed!
	sexp rv = SEXP_ZERO;
	char **tokenise_cmdline(char *cmdline);
	char **argv = 0;

	//fprintf(stderr,"ecs %s\n", cmds);

	argv = tokenise_cmdline(cmds);

/*
	begin {
		int i;
		for (i = 0; argv && argv[i]; i++) {
			fprintf(stderr,"ecs[%d] %s\n", i, argv[i]);
		}
	}
*/
	rv = execute_command_array(argv);
	Free(argv);
	return rv;
}
	

#else
sexp execute_command_string(char *cmds) { // The string cmds is freed!
	char **tokenise_cmdline(char *cmdline);
	cmd_t *process_token_list(char **Argv, int in, int out,int err);
	void run_command_prologue(char *cmds);
	void run_command_epilogue(char *cmds, sexp rv);
	sexp run_commands(cmd_t *cmd);

	char **argv = 0;
	cmd_t *C = 0;

	argv = tokenise_cmdline(cmds);

	C = process_token_list(argv,0,1,2);
	//fprintf(stderr,">%s<\n", cmds);
	C->cmd = strdup(cmds);
			
	// Call to run-command-prologue
	run_command_prologue(cmds);

	if (C) {
		sexp rtv = SEXP_FALSE;
		sexp_preserve_object(ctx, rtv);

		rtv =  run_commands(C);

	   // Call to run-command-epilogue (with return value)
		run_command_epilogue(cmds, rtv);
		

		free_cmd(C); // This will  free the strings for argv
		Free(argv);

		return rtv;
	}
	else {
		free_null_terminated_pointer_array(argv);
		//Free(argv);
		return SEXP_NEG_ONE;
	}
	
}
#endif

/*--- Run a command for a backquote substitution, à la     head `calc $LEN - $DELTA`  */

char *backquote_system_call(char *str) {
	char *fname = NULL;
	char *bqbuff = NULL;
	struct stat stbuf[1];
	sexp n = SEXP_FALSE;


	asprintf(&fname, "/tmp/igor.%d.%d", igor_pid, serial_number++);
	asprintf(&bqbuff,"%s > %s", str, fname);

	n = execute_command_string(bqbuff);

	if (sexp_equalp(ctx, n, SEXP_TRUE) && stat(fname, stbuf) == Ok) {
		int fd = open(fname,O_RDONLY);
		if (fd >= 0) {
			char *cursor;
			char *insertionbuf;

			insertionbuf = (char *)calloc(stbuf->st_size + 1, 0);
			if (!insertionbuf) Abort("Out of memory in backquote expansion");
						
			if (read(fd, insertionbuf, stbuf->st_size) != stbuf->st_size) Abort("Bad read in backquote expansion");
			close(fd);
						
			insertionbuf[stbuf->st_size] = 0; // null terminate the beastie
			while (insertionbuf[strlen(insertionbuf)-1] == '\n') insertionbuf[strlen(insertionbuf)-1] = 0; // Eat the trailing space because its the Right Thing To Do.

			for (cursor = insertionbuf; *cursor; cursor++) {
				if (strchr("\n\r\f\a\b", *cursor)) *cursor = ' ';
			}
			unlink(fname);
			return insertionbuf;
		}
	}
	return NULL;
}

/*--- Write an s-expression to the current output or error port */

void write_sexp(sexp ctx, sexp bit, int err) {
	sexp_gc_var1(out);
	sexp_gc_preserve1(ctx,out);
	
	if (err) out = sexp_current_error_port(ctx);
	else out = sexp_current_output_port(ctx);

	if (! sexp_oportp(out))	{
		if (err) out = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
		else out = sexp_make_output_port(ctx, stdout, SEXP_FALSE);
	}
	sexp_write(ctx, bit, out);
	sexp_newline(ctx,out);
}


/*--- Evaluate a string containing an s-expression (possibly with an input string) and return the result as a string  */
char *evaluate_scheme_expression(int emit, char *Sexpr,  char *inputstring) { // This returns an allocated string which ought to be freed (ultimately)
	char *sexpr = NULL;
	begin {
		char *psexpr = NULL;
		int run_word_expand = 0;
		char *wsexpr = NULL, *rstr = NULL;
		sexp_gc_var1(result);
		sexp_gc_preserve1(ctx, result);

#if 0
		if ((run_word_expand = (*sexpr == *shellcmd))) {
			char *tp;
			tp = strdup(Sexpr + 1);
			psexpr = word_expand_string(tp);
			Free(tp);
			paren_protection(psexpr, escape, 0);
		}
#endif			
		//sexpr_s=tarts = "([{";
		//sexpr_starts = "([";
		const char *sexpr_starts = "(";

		if (strchr(sexpr_starts, *Sexpr)) {
			char *t = jump_sexp(Sexpr, '\\');
			if (t == Sexpr) {
				report_error(0,"Bad s-expression",Sexpr);
				return strdup(Sexpr);
			}
		}

		//fprintf(stderr,"## %s ##  %d  %d\n", Sexpr, is_sexp(Sexpr), is_unfinished_sexp(Sexpr));
		if (is_unfinished_sexp(Sexpr) || !is_sexp(Sexpr)) return strdup(Sexpr);

		sexpr = guard_definitions(((run_word_expand && psexpr) ? psexpr : Sexpr));


//#define START_EVAL_BLOCK "   (let ((rslt #f)) (set! rslt (display-to-string %s))" 
#define START_EVAL_BLOCK " " "  (let ((rslt (display-to-string %s)))" 

#define END_EVAL_BLOCK ")"

/*
		if (!inputstring) {


			asprintf(&wsexpr, 
				"(let* ((stdin-list (lambda () (display \"You cannot use stdin or stdin-list without input!\n\") *eof*)) (stdin stdin-list))"
				START_EVAL_BLOCK " rslt" END_EVAL_BLOCK ")",
				);
		}
		else if (!*inputstring) {
			asprintf(&wsexpr, 
				"(let* ((stdin-list (lambda () *eof*)) (stdin stdin-list))"
				START_EVAL_BLOCK " rslt" END_EVAL_BLOCK ")",
				);
		}
		else {
			asprintf(&wsexpr, 
				"(let* ((stdin-list #f) (stdin #f))"
				"  (let ((lst (collapsing-strtok \"%s\")))"
				"    (set! stdin (lambda () (if (pair? lst) (let ((a (car lst)))(set! lst (cdr lst)) a) *eof*)))"
				"    (set! stdin-list (lambda () (if (pair? lst) (let ((a lst)) (set! lst *eof*) a) *eof*)))"
				START_EVAL_BLOCK " rslt" END_EVAL_BLOCK ")",
				inputstring, sexpr);
		}

*/

		asprintf(&wsexpr, 
			START_EVAL_BLOCK " rslt" END_EVAL_BLOCK ")",
			sexpr
			);




		//fprintf(stderr,"@@ %s @@\n", wsexpr);

		result = check_exception(emit, ctx, sexp_eval_string(ctx, wsexpr, -1, ENV), "There was an error in:", sexpr);

		begin{
			char *p =  (sexp_stringp(result) ? sexp_string_data(result) : NULL);

			if (result == SEXP_VOID || result == SEXP_UNDEF 
				|| (p && (!*p || !strcmp(p, "#<undef>") || !strcmp(p, "#<void>")))) {
				rstr = strdup("");
			}
			else if (sexp_stringp(result)) rstr = strdup(sexp_string_data(result));
			else {
				asprintf(&rstr,"Failed to evaluate [%s]", sexpr);
			}
			
		}
		Free(wsexpr);
		Free(psexpr);
		Free(sexpr);

		sexp_gc_release1(ctx);
		
		return rstr;

/*
		sexp_string_data(sexp)
		sexp_sint_value(sexp)
		sexp_uint_value(sexp)
*/

	}
}

/*---  char *exit_val_evaluate_scheme_expression(int emit, char *sexpr, char *instring)   */
/* This returns the exit value of the scheme expression */

char *exit_val_evaluate_scheme_expression(int emit, char *sexpr, char *instring) { // This returns an allocated string which ought to be freed (ultimately)
	begin {
		return strdup(sexp_string_data(sexp_write_to_string(ctx, sexp_eval_string(ctx, sexpr, -1, ENV))));
	}
	return NULL;
}


	/* 
		These will take the following forms:
	  
		* a bare s-expression which is just treated as a "repl" type thing
		* an expression of the form ",(s-expr)" or ",atom" which is treated like a macro thing
		* an expression of the form ",@(s-expr)" or ",@lst" which is treated like a macro thing and spliced in

		All s-expressions are evaluated in order and inserted (or not) appropriately into the string before it is passed off to 
		wordexp(3)

		See the notes under "discussion" above....
	*/



/*-- Parsing the command line  */
/*---  Tokenise the command line and return an array of elements */

// This returns a null terminated array of strings (like argv)
char **tokenise_cmdline(char *cmdline) {
	int argc = 0;
	char **argv = 0;
	char *cp = NULL;
	char *collecting = NULL;
	int i = 0;
	int CSIZE = 0;

	if (!cmdline || !*cmdline) {
		return NULL;
	}

	Dprintf("Tokenising %s\n", cmdline);

	cp = cmdline; // readline allocates this,

	CSIZE = strlen(cmdline)+1;
	collecting = (char *)calloc(CSIZE,1);
	argv = (char **)calloc(1, sizeof(char *));

	for (*collecting = 0, i = 0;; )  {
		int dbgi = 0;
		// We've reached the end of the line

		Dprintf("----------------------\n");
		Dprintf("collecting = %s\n",collecting);
		Dprintf("cp = %s\n",cp);
		Dprintf("argc = %d, argv = %p\n",argc, argv);

		if (argv && argc) {
			for (dbgi = 0; argv && dbgi < argc+1; dbgi++) {
				Dprintf("  argv[%d] = %s\n",dbgi, argv[dbgi]);
			}
		}
		else Dprintf("  no argv\n");

		if (cp && (*cp == '\n' || *cp == '\r')) {
			cp++;
		}

		if (!cp || !*cp) { // either there is nothing there, or we have reached the end of the cmdline
			if (i > 0) { // we are still collecting a string, so
				// add another bit to the token array
				argv = (char **)reallocate(argv, (argc + 3)*sizeof(char **));
				argv[argc] = strdup(collecting);
				*collecting = 0;
				argc++;
				argv[argc] = 0;
				argv[argc+1] = 0;
			}
			else { // we *aren't* collecting, so just add the terminating null
				argv = (char **)reallocate(argv, (argc + 1)*sizeof(char **));
				argv[argc] = 0;
			}

			Free(collecting);

			return argv;
		}

		else if (i == 0  && isspace(*cp)) {
			cp++;
			collecting[i] = 0;
		}

		else if (cp != collecting && isspace(*cp)) {
			// We have reached a gap between arguments
			i = 0;
			argv = (char **)reallocate(argv, (argc + 2)*sizeof(char **));
			argv[argc++] = strdup(collecting);
			argv[argc] = 0; // for safety's sake
			cp++;
		}

		else if (*cp == escape) { // this may be wrong ...
			//cp++;
			collecting[i++] = *cp++;
			collecting[i] = 0;
		}

		else if (!strncmp(cp, quotedlist, strlen(quotedlist)) || !strncmp(cp, shellcmd, strlen(shellcmd))) { // this is a quoted list or "special" scheme expression
			char *tcp = jump_sexp(cp+1, sescape);
			int n = collecting?strlen(collecting):1;

			if (!collecting || n+tcp-cp > CSIZE) collecting = (char *)reallocate(collecting, (n+tcp-cp + CSIZE)*sizeof(char));
			strncpy(collecting+i, cp, tcp - cp);
			collecting[i+tcp - cp] = 0;

			i = strlen(collecting);
			cp = tcp;
		}
		else if (*cp == squote) {
			cp = jump_fence_c(cp, collecting, squote, escape, 0);  /* do not eat the single quotes -- wordexp will deal with it */
			//cp = jump_fence_c(cp, collecting, squote, escape, 1);
			//printf("Collecting = %s\n", collecting);
			i = strlen(collecting);
		}

		else if (*cp == dquote) {
			cp = jump_fence_c(cp, collecting, dquote, escape, 0); /* do not eat the double quotes -- wordexp will deal with it */
			//cp = jump_fence_c(cp, collecting, dquote, escape, 1);
			//printf("Collecting = %s\n", collecting);
			i = strlen(collecting);
		}

		else if (!strncmp(cp, varexpr, strlen(varexpr))) {
#if 0
			cp = jump_fence_c(cp, collecting, '}', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
			i = strlen(collecting);
#else
			char *ts = NULL;
			cp = jump_fence_c(cp, collecting, '}', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
			
			i = strlen(collecting);

			if (i <= 3) strcpy(collecting,"${}");
			else {
				collecting[i-1] = 0;
				ts = backquote_system_call(collecting+2);
				strcpy(collecting, "$");
				strcat(collecting, ts);
			}
			i = strlen(collecting);
			Free(ts);
#endif
		}
		else if (!strncmp(cp, varexpr, strlen(shellcmd))) {
#if 0
			cp = jump_fence_c(cp, collecting, ')', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
			i = strlen(collecting);
#else
			char *ts = NULL;
			cp = jump_fence_c(cp, collecting, ')', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
			
			i = strlen(collecting);

			if (i <= 3) strcpy(collecting,"");
			else {
				collecting[i-2] = 0;
				ts = backquote_system_call(collecting+2);
				strcat(collecting, ts);
			}
			i = strlen(collecting);
			Free(ts);
#endif
		}
		else if (*cp == bquote) { /* This refers to an explicit substitution  using a program with arguments */
			cp = jump_fence_c(cp, collecting, bquote, escape, 0); /* do not eat the backquotes -- wordexp will deal with it */
			// cp = jump_fence_c(cp, collecting, bquote, escape, 1);
			i = strlen(collecting);
		}

		else if (strchr(start_fence, *cp)) { /* This is a straight s-expression  */
			// make sure we start a new argv here
			char *tcp = jump_sexp(cp, sescape); 
			int n = collecting?strlen(collecting):1;

			if (!collecting || n+tcp-cp > CSIZE) collecting = (char *)reallocate(collecting, (n+tcp-cp + CSIZE)*sizeof(char));
			strncpy(collecting+i, cp, tcp - cp);
			collecting[i+tcp - cp] = 0;

			i = strlen(collecting);
			cp = tcp;
			
		}


/***** THESE NEED TO BE SET AS SCHEME VARIABLES AND HANDLED IN "set" TOO *****/

		// must come after the s-expressions have been jumped so that the scheme variables are used.in s-expressions

		else if (!strncmp(cp, "$#", 2)) { // number of args in commandline of script (apart from cmd)
		   cp += 2;                       // this needs to be explicit  so the hash isn't interpreted as a comment
			
			strcpy(collecting, "$#");
			i = strlen(collecting);
		}

		else if (!strncmp(cp, "$?", 2)) { // Nothing, not replaced ** [bash uses this for status of last pipeline]
			cp += 2;
			
			strcpy(collecting, "$?");
			i = strlen(collecting);
		}

		else if (!strncmp(cp, "$-", 2)) {  // Nothing, not replaced ** [bash uses this for current  option  flags]
			cp += 2;
			
			strcpy(collecting, "$-");
			i = strlen(collecting);
		}

		else if (!strncmp(cp, "$!", 2)) { // pid of last backgrounded command (do the $!-3 thing?)
			cp += 2;
			
			strcpy(collecting, "$!");
			i = strlen(collecting);
		}

#if 1
		else if (!strncmp(cp, "$@", 2)) { // command line args $1 ... arguably as array of strings
			cp += 2;
			
			strcpy(collecting, "$@");
			i = strlen(collecting);
		}

		else if (!strncmp(cp, "$*", 2)) { // command line args $1 ... arguably as one string with spaces
			cp += 2;
			
			strcpy(collecting, "$*");
			i = strlen(collecting);
		}

		else if (!strncmp(cp, "$0", 2)) { // name/path of shell/script
			cp += 2;
			
			strcpy(collecting, "$0");
			i = strlen(collecting);
		}

		else if (!strncmp(cp, "$$", 2)) { // pid of this process
			cp += 2;
			
			strcpy(collecting, "$$");
			i = strlen(collecting);
		}
#endif

		else {
			char *ims = is_magic(cp);
			if (!ims) {
				collecting[i++] = *cp;
				collecting[i] = 0;
				cp++;

				//printf("(%c:%d) ", *cp, i);
			}
			else { // this is a special symbol like | or >> ... tokenise it separately
				if (i > 0) {
					char *sym;
					// finish the last token
					argv = (char **)reallocate(argv, (argc + 3)*sizeof(char **));
					argv[argc+2] = 0;
					sym = is_symbol(collecting);
					if (sym) argv[argc++] = sym;
					else argv[argc++] = strdup(collecting);
				}
				else argv = (char **)reallocate(argv, (argc + 2)*sizeof(char **));
				argv[argc+1] = 0;

				if ((*ims == *scmunquote)) { // we have one of the unquoting rules
					char *sch = cp;
					char *ssch = NULL;
					int n = strlen(collecting);

			      // make sure we start a new argv here
					
					if (collecting && *collecting) {
						argv[argc++] = strdup(collecting);
						argv[argc] = 0; // for safety's sake
						cp += strlen(ims);
						i = 0;
					}

					collecting[i++] = *cp;
					collecting[i] = 0;
					cp++; 

					if (*cp == scmunquotesplice[1]) {
						collecting[i++] = *cp;
						collecting[i] = 0;
						cp++; 
					}

					ssch = cp;
					DAbort("Not splicing yet");
					cp = jump_sexp(cp,sescape);
					if (ssch == cp) {
						DAbort("Whut?");
					}
					else {
						strncat(collecting, sch, cp - sch);
						collecting[n+cp-sch] = 0;

						argv[argc++] = strdup(collecting);
						argv[argc] = 0; // for safety's sake
						cp = collecting + strlen(collecting);
						i = 0;
					}

				}
				else if ((!strncmp(cp,"#",1) && strncmp(cp,"#\\",2) && strncmp(cp,"#:",2)) || !strncmp(cp,";;",2)) {
					// This is a comment
					*cp = 0;
				}

				else if (!strncmp(cp, varexpr, strlen(varexpr))) {
#if 0
					cp = jump_fence_c(cp, collecting, '}', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
					i = strlen(collecting);
#else
					cp = jump_fence_c(cp, collecting, '}', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
					i = strlen(collecting);
			
					printf("#### collecting = [%s]\n", collecting);
#endif
				}
				else {
					//argv[argc++] = strdup(ims);
					argv[argc++] = ims;
					argv[argc] = 0; // for safety's sake
					cp += strlen(ims);
					i = 0;
				}
			}
		}

		collecting[i] = 0;
	}

	Free(collecting);

	




	return argv;
}

/*--- handle filename globbing */

char *handle_filename(char *s) {
	wordexp_t arg;
	char *t = NULL;
	int n = 0;

	if (*s == '(') { // Kludge!
		if (is_sexp(s)) {
			t = evaluate_scheme_expression(0, s,NULL);
			if (!*t) {
				Free(t);
				return NULL;
			}
		}
		else {
			t = strdup(s);
		}
	}
	else {
		n = wordexp(s, &arg, 0);
		
		if (n != 0) {
			// We need to tease this out a bit
			t = strdup(s);
		}
		else {
			if (arg.we_wordc <= 0) {
				wordfree(&arg);
				return NULL;
			}
			else if (arg.we_wordc > 1) {
				int i = 0;

				fprintf(stderr,"igor: Multiple filenames in redirection!: %s", arg.we_wordv[0]);
				for (i = 1; i < arg.we_wordc; i++) fprintf(stderr,", %s", arg.we_wordv[i]);
				fprintf(stderr,"\n");
			}

			else {
				t = strdup(arg.we_wordv[0]);
				wordfree(&arg);
			}
		}
	}
	return t;
}

/*--- Expand words in an array of strings  */

char **word_expansion(char **argv, int *argc, int argix) {
	wordexp_t arg;
	int n =  0;
	int err = 0;

	if (!argv || argix >= *argc || !argv[argix] || !*argv[argix]) return argv;

	err = wordexp(argv[argix], &arg, 0);

	if (err) return NULL;

	n = arg.we_wordc + *argc + 1;
	
	if (arg.we_wordc == 0) {
		int j = 0;
		//char *s = argv[argix];
		for (j = argix; j < *argc; j++) argv[j] = argv[j+1];
		*argc = *argc-1;
		//Free(s); // I hope this is ok! .. maybe not?
	}
	else if (arg.we_wordc == 1) {
		int sn = strlen(arg.we_wordv[0]);
		if (strlen(argv[argix]) < sn) argv[argix] = (char *)reallocate(argv[argix], (sn+2) * sizeof(char));
		strcpy(argv[argix], arg.we_wordv[0]);
	}
	else {
		int i = 0;
		argv = (char **)reallocate(argv,sizeof(char **) * (n+1));
		for (i = *argc; i <= n && argv; i++) argv[i] = 0;
		if (!argv) Abort("Out of memory: no argv!");

		memmove(argv + argix + 1, argv + arg.we_wordc - 1, (*argc-argix)* sizeof(char *)); // ought to move the pointers appropriately
		Free(argv[argix]);

		for (i = 0; i < arg.we_wordc; i++) {
			argv[i+argix] = strdup(arg.we_wordv[i]);

			//argv[i+argix] = (char *)reallocate(argv[i+argix], (strlen(arg.we_wordv[i])+1) * sizeof(char));
			//strcpy(argv[i+argix], arg.we_wordv[i]);
		}

		*argc = *argc + arg.we_wordc - 1;
	}
		
	wordfree(&arg);
	return argv;
}


/*--  process_token_list -- Convert the array of tokens into a command chain.  Sets up the redirections ... */

cmd_t *process_token_list(char **Argv, int in, int out,int err) {
	int Argc = 0;

	int i = 0;
	cmd_t *C = new_cmd_t();
	char *fname = NULL;

	for (Argc = 0; Argv[Argc]; Argc++) {
		//char *s = Argv[Argc];
		//if (is_magic(s) || strchr("(),|&;<>{}", *s)) continue;

		//DPTprintf("DPT Argv[%d] = \"%s\"\n",Argc, Argv[Argc]);
	}

	C->argv = (char **)calloc(Argc+1, sizeof(char **)); // argv *must* be null terminated

	C->in = in;
	C->out = out;
	C->err = err;
	
//	C->inport = sexp_input_port(C->in);
//	C->outport = sexp_input_port(C->out);
//	C->errport = sexp_input_port(C->err);

//	C->inport = inport;
//	C->outport = outport;
//	C->errport = errport;
	
	
/*
#warning ---------------------------------------------------------------
#warning I need to convert bare scheme expressions to a fork and print that
#warning we can use to pipe into other things or ... stuff.
#warning ---------------------------------------------------------------
*/

	for (i = 0; Argv && i < Argc; i++) {
		if (!Argv[i] || !Argv[i][0]) continue;

		if (is_magic(Argv[i])) {
			if (0) {}
			//if (strcmp("(,", *Argv[i])) { // Catch the scheme stuff early
			//}
			else if (!strncmp(Argv[i], quotedlist, strlen(quotedlist)) || !strncmp(Argv[i], shellcmd, strlen(shellcmd))) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", 
					__FUNCTION__, 
					__LINE__, 
					Argv[i], 
					(C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				C->run_scheme_expression = 1;
				C->argv[C->argc++] = Argv[i];
				C->argv[C->argc] = 0;
			}
			else if (!strcmp(Argv[i], heredoc)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				report_error(0,"here documents not supported yet", NULL);
				C->errcond = 255;
				return C;
			}
			else if (!strcmp(Argv[i], stdoutapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to append stdout to!", NULL);
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					C->out = open(fname, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->out < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], stderrapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to append stderr to!", NULL);
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					C->err = open(fname, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->err < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], stdouterrapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to append stdout and stderr to!", NULL);
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					C->out = C->err = open(fname, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->out < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], stdinredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to read as stdin!", NULL);

					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					C->in = open(fname, O_RDONLY);
					if (C->in < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
				
			}
			else if (!strcmp(Argv[i], stdoutredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to write stdout to!", NULL);
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					if (access(fname,F_OK) == Ok) unlink(fname);
					C->out = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->out < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], stderredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to write stderr to!", NULL);
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					if (access(Argv[i], F_OK) == Ok) unlink(fname);
					C->err = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->err < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], stdouterredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to write stderr to!", NULL);
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					if (access(fname, F_OK) == Ok) unlink(fname);
					C->out = C->err = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->out < 0) {
						C->errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], outpipe)) {
				i++;
				/*** NEED TO MAKE THIS WORK, NEED TO MAKE THE REMAINING TWO CASES WORK  ***/

				if (0) {;}
#if 0
				else if (C->argv[0] && is_sexp(C->argv[0]) && is_sexp(Argv[i])) { // an s-expression piping into another s-expression
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);

					C->input_from_sexp = C->output_to_sexp = 1;
					C->next = process_token_list(Argv + i, in, out, err);
				}
				else if (C->argv[0] && !is_sexp(C->argv[0]) && is_sexp(Argv[i])) { // a program piping into an s-expression
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
					report_error(0, "'cmd | (sexp)' is not working yet", NULL);

					C->input_from_sexp = 0; C->output_to_sexp = 1;
					C->next = process_token_list(Argv + i, in, out, err);

					return C;
				}
// This one works normally, 
//				else if (C->argv[0] && is_sexp(C->argv[0]) && !is_sexp(Argv[i])) { // an s-expression piping into a program
//					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
//					report_error(0, "'(sexp) | cmd' is not working yet", NULL);
//
//					C->next = process_token_list(Argv + i, in, out, err);
//					C->next->input_from_sexp = 1; C->output_to_sexp = 0;
//					return C;
//				}
#endif
				else {
					int pipefd[2] = {-1, -1}; // you read from pipefd[0] and write to pipefd[1]
					//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");

					if (i >= Argc) {
						report_error(0, "Nothing specified to write stdout to!", NULL);
						C->errcond = 1;
						return C;
					}

					C->input_from_sexp = 0; C->output_to_sexp = 0;
					C->argv[C->argc] = 0;
					
					pipe(pipefd);
					C->out = pipefd[1];
					C->shuto = 1;
					C->next = process_token_list(Argv + i, pipefd[0], out, err);
					return C;
				}
			}
			else if (!strcmp(Argv[i], errpipe)) {
				i++;
				if (C->argv[0] && is_sexp(C->argv[0]) && is_sexp(Argv[i])) { // both the command that we've been collecting and the next are s-exprs
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);

				}
				else if (C->argv[0] && !is_sexp(C->argv[0]) && is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
					
				}
				else if (C->argv[0] && is_sexp(C->argv[0]) && !is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
					
				}
				else {
					int pipefd[2] = {-1, -1}; // you read from pipefd[0] and write to pipefd[1]
					DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
					if (i >= Argc) {
						report_error(0, "Nothing specified to write stderr to!", NULL);
						C->errcond = 1;
						return C;
					}
					C->argv[C->argc] = 0;
				
					pipe(pipefd);

					C->err = pipefd[1];
					C->shute = 1;
					C->next = process_token_list(Argv + i, pipefd[0], out, err);
					return C;
				}
			}
			else if (!strcmp(Argv[i], outerrpipe)) {
				if (C->argv[0] && is_sexp(C->argv[0]) && is_sexp(Argv[i])) { // both the command that we've been collecting and the next are s-exprs
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
					
				}
				else if (C->argv[0] && !is_sexp(C->argv[0]) && is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
					
				}
				else if (C->argv[0] && is_sexp(C->argv[0]) && !is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)", Argv[i]);
					
					
				}
				else {
					int pipefd[2] = {-1, -1}; // you read from pipefd[0] and write to pipefd[1]
					DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
					i++;
					if (i >= Argc) {
						report_error(0,"Nothing specified to write stdout and stderr to!", NULL);
						C->errcond = 1;
						return C;
					}
					C->argv[C->argc] = 0;
				
					pipe(pipefd);

					C->out = C->err = pipefd[1];
					C->shuto = C->shute = 1;
					C->next = process_token_list(Argv + i, pipefd[0], out, err);
					return C;
				}
			}
			else if (!strcmp(Argv[i], makebg)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				C->argv[C->argc+1] = 0;
				C->bg = 1;
				C->argv[C->argc] = 0;
				
				C->next = process_token_list(Argv + i, in, out, err);
				return C;
			}
			else if (!strcmp(Argv[i], nextsep)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				C->argv[C->argc+1] = 0;
				C->next = process_token_list(Argv + i, in, out, err);
				return C;
			}
			else if (!strcmp(Argv[i], andsep)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				if (C->or_next) {
					report_error(0, "Both || and && were used to separate this command from the previous one!", NULL);
					C->errcond = 1;
					return C;
				}
				C->argv[C->argc+1] = 0;
				C->and_next = 1;
				C->next = process_token_list(Argv + i, in, out, err);
				return C;
			}
			else if (!strcmp(Argv[i], orsep)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				if (C->and_next) {
					report_error(0, "Both && and '' were used to separate this command from the previous one!", NULL);
					C->errcond = 1;
					return C;
				}
				C->argv[C->argc+1] = 0;
				C->or_next = 1;
				C->and_next = 0;
				C->next = process_token_list(Argv + i, in, out, err);
				return C;
			}
			else if (!strcmp(Argv[i], begblock)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				report_error(0, "No blocks yet, sorry", NULL);
				C->errcond = 3;
				return C;
			}
			else if (!strcmp(Argv[i], endblock)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				report_error(0,"Missing begin block!", NULL);
				C->errcond = 4;
				
				return C;
			}
		}
		else { // normal string or special ($@, $*, $#, $?, $-, $$, $!, $0)
			char **tmp = NULL; 

//			DPTprintf("Assigning \"%s\" to C->argv[%d]\n", Argv[i], C->argc);
			C->argv[C->argc++] = Argv[i];
			C->argv[C->argc] = 0;
			tmp = word_expansion(C->argv, &(C->argc), C->argc-1);
			if (tmp) C->argv = tmp;
		}

//		DPTprintf("{Argv[%d] = \"%s\"  &  ", i, Argv[i]);
//		DPTprintf("C->argv[%d] = \"%s\"  is_sexp() = %d //  ", 0, C->argv[0], is_sexp(C->argv[0]));
//		DPTprintf("Argv[%d] = \"%s\" %p}\n", i+1, Argv[i+1], Argv[i+1]);

		// if the first thing in the list is an s-expression and the *next thing in the list is an s-expression
		if (is_sexp(C->argv[0]) && C->argc == 1 && (!Argv[i+1] || (Argv[i] && is_sexp(Argv[i+1])))) { 
//			int k = 0;
//			DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
			i++; // We *do not* increment here, because i already points to the right spot
//			if (Argv[i] && *Argv[i]) {
//				DPTprintf("%s:%d -- which will be followed by %s\n", __FUNCTION__, __LINE__, Argv[i]);
//			}
			
//			for (k = 0; k < C->argc; k++) {
//				DPTprintf("  [%d] %s\n", k, C->argv[k]);
//			}

			C->next = process_token_list(Argv + i, in, out, err);
			return C;
		}

		if (0) {
			int j = 0;
			if (C->argc > 0) {
				printf("command = %s", C->argv[0]);
				for (j = 1; C->argv[j]; j++) printf(" %s", C->argv[j]);
				printf("\n");
			}
		}
	}

	return C;
}
	
/*--  char **process_command_in_token_list(int execute, int emit, char **Argv, int In, int Out,int Err, char **inputstring, char **outputstring)   */
// inputstring and outputstring are used when we are dealing with s-expressions
/*
#warning ---------------------------------------------------------------
#warning I need to convert bare scheme expressions to a fork and print that
#warning we can use to pipe into other things or ... stuff.
#warning ---------------------------------------------------------------
*/

#if defined(NOT_EVEN_CLOSE_TO_WORKING_YET)


char **process_command_in_token_list(int *ret, int execute, int emit, char **Argv, int In, int Out,int Err, char **inputstring, char **outputstring) { 
/*--- ISSUES: Passing back return values, managing the way Argv steps through the array of tokens across nested invocations
 */
	int Argc = 0;

	char **argv = NULL;
	int argc = 0, run_scheme_expression = 0, errcond = 0;
	int next = 0;

	int in  = -1, out = -1, err = -1, bg = 0;
	int i = 0;
	cmd_t *C = new_cmd_t();
	char *fname = NULL;

	for (Argc = 0; Argv[Argc]; Argc++) {
		//char *s = Argv[Argc];
		//if (is_magic(s) || strchr("(),|&;<>{}", *s)) continue;

		//DPTprintf("DPT Argv[%d] = \"%s\"\n",Argc, Argv[Argc]);
	}

	argv = (char **)calloc(Argc+1, sizeof(char **));

	in = In;
	out = Out;
	err = Err;

	
/*---  loop over tokens in list till we hit the end of a command/s-expression   */

	for (i = 0; Argv && i < Argc; i++) {
		if (!Argv[i] || !Argv[i][0]) continue;

/*----  We've hit a symbol or the beginning of a literal list....*/
  if (is_magic(Argv[i])) {
			if (0) {}
			//else if (!strcmp("(,", *Argv[i])) { // Catch the scheme stuff early
			//}

/*----- handle a quoted list, send it to c_execute */
  			else if (!strncmp(Argv[i], quotedlist, strlen(quotedlist)) || !strncmp(Argv[i], shellcmd, strlen(shellcmd))) {
				int kq;
			DPTprintf("%s:%d -- processing %s as a part of command: %s\n", 
					__FUNCTION__, 
					__LINE__, 
					Argv[i], 
					(C && argv && *argv[0]) ? argv[0] : "(none)");

				argv[argc++] = Argv[i];
				argv[argc] = 0;
				if (execute) kq = c_execute(1, 0, Argv, in, out, err, inputstring ? *inputstring : NULL);
				else kq = 0;

				STATUS(kq);
			}

/*----- heredoc -- not implemented */
			else if (!strcmp(Argv[i], heredoc)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				report_error(0,"here documents not supported yet", NULL);
				*ret = errcond = 255;
				return Argv+i;
			}

/*----- open a file for standard out to append to */
			else if (!strcmp(Argv[i], stdoutapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to append stdout to!", NULL);
					*ret = errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					out = open(fname, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (out < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
			}

/*----- open a file for stderr to append to */
			else if (!strcmp(Argv[i], stderrapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to append stderr to!", NULL);
					*ret =  errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					err = open(fname, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (err < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
			}

/*----- open a file for both stdout and stderr to append to */
			else if (!strcmp(Argv[i], stdouterrapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to append stdout and stderr to!", NULL);
					*ret  = errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					out = err = open(fname, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (out < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
			}
/*----- open a file to take stdin from */
			else if (!strcmp(Argv[i], stdinredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to read as stdin!", NULL);

					*ret = errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					in = open(fname, O_RDONLY);
					if (in < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
				
			}

/*----- open a file to write stdout to */
  
			else if (!strcmp(Argv[i], stdoutredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to write stdout to!", NULL);
					*ret = errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					if (access(fname,F_OK) == Ok) unlink(fname);
					out = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (out < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
			}

/*----- open a file to write stderr to */
			else if (!strcmp(Argv[i], stderredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to write stderr to!", NULL);
					*ret = errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					if (access(Argv[i], F_OK) == Ok) unlink(fname);
					err = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (err < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
			}

/*----- open a file to write both stdout and stderr to */
			else if (!strcmp(Argv[i], stdouterredir)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (i >= Argc) {
					report_error(0,"No filename specified to write stderr to!", NULL);
					*ret = errcond = 1;
					return Argv+i;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				if (fname && *fname) {
					if (access(fname, F_OK) == Ok) unlink(fname);
					out = err = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (out < 0) {
						errcond = 1;
						report_error(errno, "Unable to open file", fname);
					}
					free(fname);
				}
				else errcond = 1;
			}

			
/********************  The following bits may execute commands ********************/
/*----- we have a thing that is piping stdout to another thing */
			else if (!strcmp(Argv[i], outpipe)) {
				i++;
				/*** NEED TO MAKE THIS WORK, NEED TO MAKE THE REMAINING TWO CASES WORK  ***/

				if (0) {;}

/*------ this s-expression is piping into another s-expression  */
				else if (argv[0] && is_sexp(argv[0]) && is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);

					if (execute) *outputstring = evaluate_scheme_expression(emit, argv[0], *inputstring);
					if (inputstring && *inputstring) {Free(*inputstring); inputstring = NULL;} // consume input string

					Argv = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);
### HERE ###					Argc
				}

/*------ this program is piping into an s-expression  */
				else if (argv[0] && !is_sexp(argv[0]) && is_sexp(Argv[i])) { 
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					report_error(0, "'cmd | (sexp)' is not working yet", NULL);

					if (inputstring && *inputstring) {
						if (in != In) {
							// there is some error here!
							errcond = 1;
							report_error(255, "Huh? A program seemingly has a redirected input file as well as an input string:", inputstring);
						}
					}

					if (execute) *outputstring = c_execute(0, , Argv, in, out, err, inputstring ? *inputstring : NULL);
					if (inputstring && *inputstring) {Free(*inputstring); inputstring = NULL;} // consume input string

					next = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);

					return errcond;
				}
/*------ this s-expression is piping into a program  */
#if 0 // Seems to work without this....
				else if (argv[0] && is_sexp(argv[0]) && !is_sexp(Argv[i])) { 
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					report_error(0, "'(sexp) | cmd' is not working yet", NULL);

					next = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);
					return errcond;
				}
#endif
/*------ this program is piping to another program  */
				else {
					int pipefd[2] = {-1, -1}; // you read from pipefd[0] and write to pipefd[1]
					//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");

					if (i >= Argc) {
						report_error(0, "Nothing specified to write stdout to!", NULL);
						errcond = 1;
						return errcond;
					}
					if (inputstring && *inputstring) {
						if (in != In) {
							// there is some error here!
							errcond = 1;
							report_error(255, "Huh? A program seemingly has a redirected input file as well as an input string:", inputstring);
						}
					}

					argv[argc] = 0;
					
					pipe(pipefd);
					out = pipefd[1];

					errcond = c_execute(execute, 0, Argv, in, out, err, NULL);
					STATUS(errcond);
					
					next = process_command_in_token_list(&ret, execute, emit, Argv + i, pipefd[0], out, err, outputstring, NULL);
					return errcond;
				}
			}

/*----- we have a thing that is piping stderr to another thing */
			else if (!strcmp(Argv[i], errpipe)) {
				i++;
				if (argv[0] && is_sexp(argv[0]) && is_sexp(Argv[i])) { // both the command that we've been collecting and the next are s-exprs
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					fprintf(stderr,"\nBOINK\n");

				}
				else if (argv[0] && !is_sexp(argv[0]) && is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					fprintf(stderr,"\nBOINK\n");

				}
				else if (argv[0] && is_sexp(argv[0]) && !is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					fprintf(stderr,"\nBOINK\n");

				}
				else {
					int pipefd[2] = {-1, -1}; // you read from pipefd[0] and write to pipefd[1]
					DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
					if (i >= Argc) {
						report_error(0, "Nothing specified to write stderr to!", NULL);
						errcond = 1;
						return errcond;
					}
					argv[argc] = 0;
				
					pipe(pipefd);

					err = pipefd[1];
					next = process_command_in_token_list(&ret, execute, emit, Argv + i, pipefd[0], out, err, outputstring, NULL);
					return errcond;
				}
			}

/*----- we have a thing that is piping both stdout and stderr to another thing */
			else if (!strcmp(Argv[i], outerrpipe)) {
				if (argv[0] && is_sexp(argv[0]) && is_sexp(Argv[i])) { // both the command that we've been collecting and the next are s-exprs
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					fprintf(stderr,"\nBOINK\n");
				
				}
				else if (argv[0] && !is_sexp(argv[0]) && is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					fprintf(stderr,"\nBOINK\n");
					
				}
				else if (argv[0] && is_sexp(argv[0]) && !is_sexp(Argv[i])) {
					DPTprintf("%s:%d -- processing %s as a part of command: %s, piping into s-expression %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)", Argv[i]);
					fprintf(stderr,"\nBOINK\n");
					
				}
				else {
					int pipefd[2] = {-1, -1}; // you read from pipefd[0] and write to pipefd[1]
					DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
					i++;
					if (i >= Argc) {
						report_error(0,"Nothing specified to write stdout and stderr to!", NULL);
						errcond = 1;
						return errcond;
					}
					argv[argc] = 0;
				
					pipe(pipefd);

					out = err = pipefd[1];
					next = process_command_in_token_list(&ret, execute, emit, Argv + i, pipefd[0], out, err, outputstring, NULL);
					return errcond;
				}
			}

/*----- make the command run in the background */
			else if (!strcmp(Argv[i], makebg)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				argv[argc+1] = 0;
				bg = 1;
				argv[argc] = 0;
				//next = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);
				return errcond;
			}

/*----- Finished with the command, execute it begin processing the next one */
			else if (!strcmp(Argv[i], nextsep)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				argv[argc+1] = 0;
				next = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);
				return errcond;
			}

/*----- NEED A NEGATION FLAG FOR THE STATE OF THE COMMAND */

/*----- Finished with the command, execute it, if this one failed set the execute flag to false, begin processing the next one  */
			else if (!strcmp(Argv[i], andsep)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				if (or_next) {
					report_error(0, "Both || and && were used to separate this command from the previous one!", NULL);
					errcond = 1;
					return errcond;
				}
				argv[argc+1] = 0;
				and_next = 1;
				next = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);
				return errcond;
			}
/*----- Finished with the command, execute it, if this one succeeded set the execute flag to false, begin processing the next one  */
			else if (!strcmp(Argv[i], orsep)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				if (and_next) {
					report_error(0, "Both && and '' were used to separate this command from the previous one!", NULL);
					errcond = 1;
					return errcond;
				}
				argv[argc+1] = 0;
				or_next = 1;
				and_next = 0;
				next = process_command_in_token_list(&ret, execute, emit, Argv + i, in, out, err, outputstring, NULL);
				return errcond;
			}

/*----- Start a command block */
			else if (!strcmp(Argv[i], begblock)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				report_error(0, "No blocks yet, sorry", NULL);
				errcond = 3;
				return errcond;
			}

/*----- Finish a command block */
			else if (!strcmp(Argv[i], endblock)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
				i++;
				report_error(0,"Missing begin block!", NULL);
				errcond = 4;
				
				return errcond;
			}

/********************  The preceding bits may execute commands ********************/

		}
		else {
			char **tmp = NULL; 
//			DPTprintf("Assigning \"%s\" to argv[%d]\n", Argv[i], argc);
			argv[argc++] = Argv[i];
			argv[argc] = 0;
			tmp = word_expansion(argv, &(argc), argc-1);
			if (tmp) argv = tmp;
		}

//		DPTprintf("{Argv[%d] = \"%s\"  &  ", i, Argv[i]);
//		DPTprintf("argv[%d] = \"%s\"  is_sexp() = %d //  ", 0, argv[0], is_sexp(argv[0]));
//		DPTprintf("Argv[%d] = \"%s\" %p}\n", i+1, Argv[i+1], Argv[i+1]);

		// if the first thing in the list is an s-expression and the *next thing in the list is an s-expression
		if (is_sexp(argv[0]) && argc == 1 && (!Argv[i+1] || (Argv[i] && is_sexp(Argv[i+1])))) { 
			int k = 0;
			DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && argv && *argv[0]) ? argv[0] : "(none)");
			i++; // We *do not* increment here, because i already points to the right spot
			if (Argv[i] && *Argv[i]) {
				DPTprintf("%s:%d -- which will be followed by %s\n", __FUNCTION__, __LINE__, Argv[i]);
			}
			
//			for (k = 0; k < argc; k++) {
//				DPTprintf("  [%d] %s\n", k, argv[k]);
//			}

			// EXECUTE THE COMMAND IF THE FLAG PERMITS

			return errcond;
		}

		if (0) {
			int j = 0;
			if (argc > 0) {
				printf("command = %s", argv[0]);
				for (j = 1; argv[j]; j++) printf(" %s", argv[j]);
				printf("\n");
			}
		}
	}

	return errcond;
}

#endif

#define adjust_fd(which,wnum)	{if (which >= 0 && which != wnum) {	\
				if (dup2(which,wnum) < 0) {perror("Error dup2ing " #which);exit(EBADF);}}}


/*-- Execute a builtin function */

sexp execute_builtin(cmd_t *command, Builtin *op, char **argv, int input, int output) {
	char *inputstring = NULL;
	sexp r = SEXP_FALSE;
	sexp_gc_var2(oin, oout);

	sexp_gc_preserve2(ctx, oin, oout);
	sexp_preserve_object(ctx,r);



	if (input > 2) {
		oin = sexp_eval_string(ctx,"(current-input-port)", -1, ENV);
//		adjust_fd(input, 0);
//		sexp_eval_string(ctx,"(current-input-port (open-input-file-descriptor 0))", -1, ENV);
	}
	
	if (output > 2) {
		oout = sexp_eval_string(ctx,"(current-output-port)", -1, ENV);
		adjust_fd(output, 1);
		sexp_eval_string(ctx,"(current-output-port (open-output-file-descriptor 1))", -1, ENV);
	}

	if (input > 2) {
		inputstring = read_all(input);
		if (!inputstring) return SEXP_FALSE;
	}

	//DPTprintf("about to run %s\n", *argv);
	r = (op->func)(command, argv, 0, 1, inputstring);

	if (output > 2) close(output);

	sexp_apply(ctx, sexp_eval_string(ctx,"current-input-port",-1,ENV), sexp_cons(ctx,oin, SEXP_NULL));
	sexp_apply(ctx, sexp_eval_string(ctx,"current-output-port",-1,ENV), sexp_cons(ctx,oout, SEXP_NULL));

	Free(inputstring);

	sexp_gc_release3(ctx);
	return r;
}

/*--- Non-functional scheme dispatch routine for commands */

int scm_execute_single_process(int in_the_background, char *cmd, char **argv, int input, int output, int error) {
//	int i, o, e;
//	FILE *I, *O, *E;

	sexp_gc_var2(exec_stuff,result);
	sexp_gc_preserve2(ctx,exec_stuff,result);

	exec_stuff = SEXP_NULL;
	//exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,shute), exec_stuff);
	//exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,shuto), exec_stuff);
	exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,error), exec_stuff);
	exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,output), exec_stuff);
	exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,input), exec_stuff);
	exec_stuff = sexp_cons(ctx, argv_to_list(ctx, argv, -1), exec_stuff);
#if 1 // use cmd
	exec_stuff = sexp_cons(ctx, sexp_c_string(ctx, cmd, -1), exec_stuff);
#else // use *argv
	exec_stuff = sexp_cons(ctx, sexp_c_string(ctx, *argv, -1), exec_stuff);
#endif
	if (in_the_background) exec_stuff = sexp_cons(ctx, SEXP_TRUE, exec_stuff);
	else exec_stuff = sexp_cons(ctx, SEXP_FALSE, exec_stuff);

#if 0
	exec_stuff = sexp_cons(ctx, sexp_eval_string(ctx, "*igor-execute-single-process*", -1, env), exec_stuff);
	sexp_eval(ctx,exec_stuff, env);
#else
	result = sexp_apply(ctx,sexp_eval_string(ctx, "*igor-execute-single-process*", -1, env), exec_stuff);
#endif
	//result = sexp_apply(ctx, igor_dispatch_exec, exec_stuff)

	sexp_gc_release2(ctx);
	
	return -1;
}

/*--- bg/fg process control stuff -- not functional **/
#if 1
int process_index(pid_t pid) {
	if (1 || n_procs < 16)  {
		int i = -1;
		for (i = 0; i < n_procs; i++) {
			if (proclist[i].pid == pid) return i;
		}
		return -1;
	}
	else {
		// use a binary search
	}
}

void register_death(int ix, int exitval) {
	abort();
}

void record_process(pid_t pid, int bg) {
	abort();
}

void wait_for(int ix)
{
	int pstat = -1;

	if (ix < 0 || ix >= n_procs) return;

   for (;!waitpid(proclist[ix].pid, &pstat, WNOHANG);) {   // Loops till the child has been stopped or it has exited
		if (0) {;}
		else if (WIFEXITED(pstat)) { // normal exit  for proclist[ix]
			int exitval =  WEXITSTATUS(pstat);
			register_death(ix, exitval);
		}
		else if (WIFSIGNALED(pstat)) { // abnormal exit  for proclist[ix]
			int sigval =  WTERMSIG(pstat);
			fprintf(stderr,"\nProcess %d was killed by %d",proclist[ix].pid,sigval);
#if  defined(WCOREDUMP)
			if (WCOREDUMP(sigval)) fprintf(stderr," ... core dumped\n");
			else  fprintf(stderr,"\n");
#else
			fprintf(stderr,"\n");
#endif
			register_death(ix,-sigval);
		}
		else if (WIFSTOPPED(pstat)) { // process has been stopped, just return
			SET(proclist[ix].state, STOPPED);
			return;
		}
		else if (WIFCONTINUED(pstat)) { // process has been signaled with SIGCONT
			UNSET(proclist[ix].state, STOPPED);// clear the STOPPED bit
		}
	}
}


void move_proc_to_foreground(int ix) {
	if (ix < 0 || ix >= n_procs) abort();
	
	UNSET(proclist[ix].state,BACKGROUNDED);
	tcsetpgrp(igor_pgid, proclist[ix].pgid);  // let the child play with the powertools

	if (kill(- proclist[ix].pgid, SIGCONT) < 0) report_error(errno,"Process unable to resume!", NULL);

	wait_for(ix);  // wait till the child is finished

	tcsetpgrp(0, igor_pgid); // pick up the circular saw and mop up the blood
}

void background_proc(int ix) {
	if (ix < 0 || ix >= n_procs) abort();
		  
	SET(proclist[ix].state, BACKGROUNDED);
		  // restore the terminal to the parent shell
	tcsetpgrp(0, igor_pid);
}

void background_running_proc(int ix)
{
	if (ix < 0 || ix >= n_procs) abort();

	SET(proclist[ix].state, BACKGROUNDED);
	if (proclist[ix].state != BLOCKED) SET(proclist[ix].state, BLOCKED);
	if (kill(-proclist[ix].pgid, SIGCONT) < 0) report_error(errno,"unable to raise SIGCONT", NULL);

	tcsetpgrp(0, igor_pid);
}
#endif

/*--- Signal handlers  */

void catch_sigchld(int signum) {
	int child_status = 0;
	
	waitpid(0, &child_status, WNOHANG);
	// This basically just keeps the innards clear
	return;
}

void catch_sigint(int signum) {
	//int child_status = 0;
	
	//waitpid(0, &child_status, WNOHANG);
	//// This basically just keeps the innards clear
	return;
}


void catch_sigterm(int signum) {
	//int child_status = 0;
	
	//waitpid(0, &child_status, WNOHANG);
	//// This basically just keeps the innards clear
	return;
}


void catch_sigquit(int signum) {
	//int child_status = 0;
	
	//waitpid(0, &child_status, WNOHANG);
	//// This basically just keeps the innards clear
	return;
}

void signalhandler(int p) {
	int exitval = -1;
	pid_t pid = -1;
	int ix = -1;

	pid = waitpid(WAIT_ANY, &exitval, WUNTRACED | WNOHANG); // intercept the process that sends the signal

	if (pid > 0) {                                                                          // if there are information about it
		ix = process_index(pid);                      // get the job from the list
		if (ix < 0)	return;

		if (WIFSTOPPED(exitval)) { // SIGSTOP
			if (!(proclist[ix].state & BACKGROUNDED)) { // we are suspending things
				tcsetpgrp(0, proclist[ix].pgid);
				SET(proclist[ix].state, STOPPED);
			} 
			else { // mark it as blocked
				tcsetpgrp(0, igor_pgid);
				SET(proclist[ix].state,BLOCKED);
			}
			return;
		}
		else if (WIFCONTINUED(exitval)) { // SIGCONT
				if (proclist[ix].state & BACKGROUNDED) UNSET(proclist[ix].state, STOPPED);
				
			//else  jobsList = delJob(ix);
		} 
		else if (WIFSIGNALED(exitval)) { // killed
			register_death(ix,exitval);
		} 
		else if (WIFEXITED(exitval)) { // Normal exit
			if (proclist[ix].state & BACKGROUNDED) register_death(ix,exitval);
			//else register_death(ix,exitval);
		} 
		else { // Something must have happened, just remove it from the array; we won't be able to do anything with it.
			if (proclist[ix].state & BACKGROUNDED) register_death(ix,exitval);
		}
		tcsetpgrp(0, igor_pgid);
	}
}


/*--- Execute a single command -- C dispatch function ... emits a string*/

int c_emit_string_in_process(int in_the_background, char *cmd, char **argv, char *inputstring, int input, int output, int error) {
	int procid = -1;
	int cstate = 0;

   #define ifewsxz if  // Westley, the Dread Pirate Rabbits added the "ewsxz".  
	                    // If one can't have input into the C standard just because one
                       // is a rabbit, then there is something dreadfully unfair....
                       
	ifewsxz (!cmd) return 0; // it's ok to try and run an empty process ... it just doesn't do anything

	if (!cmd) return 0; // it's ok to try and run an empty process ... it just doesn't do anything
	//time(&now);
	procid = fork();

	if (procid > 0) { // This is the parent
		int cid = 0;

		record_process(procid, in_the_background);

		if (!in_the_background) {
			cid = waitpid(procid, &cstate, 0);
			//waitpid(procid, &cstate, 0);
		}
		else if (in_the_background) {
			fprintf(stderr,"[started background process: %s]\n", cmd);
		}

		if (input > 2 && close(input) == -1) perror("cannot close stdin");
		if (output > 2 && close(output) == -1) perror("cannot close stdout");

		if (cid == -1 && errno != ECHILD) perror("wait error");

		if (error > 2 && close(error) == -1) perror("cannot close stderr");

		return 0;
	}
	else if (!procid) { // this is the child
		int n = 0;
		fflush(stderr);

		adjust_fd(input,0);
		adjust_fd(output,1);
		adjust_fd(error,2);

		Dprintf("Child (%s) about to exec\n",cmd);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		signal(SIGTERM, SIG_DFL);
		signal(SIGTSTP, SIG_DFL);
		signal(SIGCHLD, &signalhandler);
		signal(SIGTTIN, SIG_DFL);
		

		usleep(25000);
		setpgrp();

/*
		if (executionMode == FOREGROUND)
			tcsetpgrp(BDSH_TERMINAL, igor_pid);                                // if we want the process to be in foreground
		if (executionMode == BACKGROUND)
			printf("[%d] %d\n", ++numActiveJobs, (int) getpid());              // inform the user about the new job in bg
*/


		if (1) {
			int i = 0;
			for (i = 1; argv[i]; i++) {
				if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
					char *ss = evaluate_scheme_expression(1, argv[i], NULL);  // These get cleaned up when the argv[] are freed
					if (ss) {
						Free(argv[i]);
						argv[i] = ss;
					}
				}
			}
		}

		begin {
			char *tcmd = completed_path(cmd);
			//char *tcmd = strdup(cmd);
			//if (tcmd) fprintf(stderr,"%s (%d)\n", tcmd, access(tcmd, X_OK));

			if (tcmd && access(tcmd, X_OK) == Ok) {
				int q = 0;
				
				for (q = 0; track_execv && argv && argv[q]; q++) {
					if (!q) fprintf(stderr,"Executable = %s\n", tcmd);
					fprintf(stderr,"arg[%d] = %s\n", q, argv[q]);
				}

				if ((n = execv(tcmd, argv)) == -1) {
		            //time(&then);
					report_error(0, "Failed to run; the program probably lacked permissions or does not exist", cmd);
				}
				else {
					//fprintf(stderr,"Goodo.\n");
			         //time(&then);
			         //proc_finished(procid, then);
				}
				free_null_terminated_pointer_array(argv);
				Free(tcmd);

				close_up_shop();
				exit(n); // if the process doesn't go, we need to dispatch it
			}
			else {
				report_error(0,"Unable to execute program", argv[0]);
				if (!strcmp(cmd,"#f"))	report_error(0,"The program was not found", cmd);
				else {
					if (tcmd) report_error(0,"The file was not found in your 'path'", tcmd);
					else report_error(0,"The file was not found", cmd);
				}
				free_null_terminated_pointer_array(argv);
				Free(tcmd);

				close_up_shop();
				exit(ENOENT);
			}
		}
	}
			
	else {
		report_error(errno, "Failed to fork", NULL);
//		sexp_destroy_context(ctx);
		return -1;
	}
}


/*--- Execute a command -- C dispatch function */


int c_execute(int run_scheme_expression, int in_the_background, char **argv, int input, int output, int error, char *inputstring) {
	int procid = -1;
	char *cmd = *argv;
	char *returnstr = NULL;

#if 0
	char **ss = NULL;
	fprintf(stderr,"EXECUTING %s [", cmd);
	for (ss = argv; *ss; ss++) {
		if (ss == argv) fprintf(stderr,"%s", *ss);
		else  fprintf(stderr,", %s", *ss);
	}
	fprintf(stderr,"] ");
	
	fprintf(stderr,"<%d,%d> &%d", input, output, error);
	fprintf(stderr, " %s\n", (in_the_background ? "&" : ""));
#endif

	ENTRY("",NULL);

   #define ifewsxz if  // Westley, the Dread Pirate Rabbits added the "ewsxz".  
	                    // If one can't have input into the C standard just because one
                       // is a rabbit, then there is something dreadfully unfair....
                       
	ifewsxz (!cmd) return 0; // it's ok to try and run an empty process ... it just doesn't do anything


	if (run_scheme_expression) {
		TRACK;
		returnstr = dispatch_scheme(argv);
	}
	else {
		TRACK;

		procid = fork();

		if (procid > 0) { // This is the parent
			int cid = 0;
			int cstate= -1;

			
			if (!in_the_background) {
				TRACK;
				cid = waitpid(procid, &cstate, 0);
				TRACK;
			}
			else if (in_the_background) {
				char *bgpid;
				TRACK;
				asprintf(&bgpid,"%d",procid);
				if (bgpid) {
					igor_set(ctx,"$!", bgpid);
					Free(bgpid);
				}
			
				fprintf(stderr,"[started background process: %s]\n", cmd);
				TRACK;
			}
			TRACK;

			if (input > 2 && close(input) == -1) perror("cannot close stdin");
			if (output > 2 && close(output) == -1) perror("cannot close stdout");

			if (cid == -1 && errno != ECHILD) perror("wait error");

			if (error > 2 && close(error) == -1) perror("cannot close stderr");

			LEAVING("",NULL);
			return cstate;
		}
		else if (!procid) { // this is the child
			int n = 0;
			fflush(stderr);

			adjust_fd(input,0);
			adjust_fd(output,1);
			adjust_fd(error,2);

			Dprintf("Child (%s) about to exec\n",cmd);
			signal(SIGINT, SIG_DFL);
			signal(SIGQUIT, SIG_DFL);
			signal(SIGTERM, SIG_DFL);
			signal(SIGTSTP, SIG_DFL);
			signal(SIGCHLD, &signalhandler);
			signal(SIGTTIN, SIG_DFL);
		
			if (!run_scheme_expression) {
				if (1) {
					int i = 0;
					for (i = 1; argv[i]; i++) {
						if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
							char *ss = evaluate_scheme_expression(1, argv[i], NULL);  // These get cleaned up when the argv[] are freed
							if (ss) {
								Free(argv[i]);
								argv[i] = ss;
							}
						}
					}
				}

				begin {
					char *tcmd = completed_path(cmd);
			//char *tcmd = strdup(cmd);
			//if (tcmd) fprintf(stderr,"%s (%d)\n", tcmd, access(tcmd, X_OK));

					if (tcmd && access(tcmd, X_OK) == Ok) {
						int q = 0;
				
						for (q = 0; track_execv && argv && argv[q]; q++) {
							if (!q) fprintf(stderr,"Executable = %s\n", tcmd);
							fprintf(stderr,"arg[%d] = %s\n", q, argv[q]);
						}

						if ((n = execv(tcmd, argv)) == -1) {
		            //time(&then);
							report_error(0, "Failed to run; the program probably lacked permissions or does not exist", cmd);
						}
						else {
					//fprintf(stderr,"Goodo.\n");
			         //time(&then);
			         //proc_finished(procid, then);
						}
						free_null_terminated_pointer_array(argv);
						Free(tcmd);

						close_up_shop();
						exit(n); // if the process doesn't go, we need to dispatch it
					}
					else {
						report_error(0,"Unable to execute program", argv[0]);
						if (!strcmp(cmd,"#f"))	report_error(0,"The program was not found", cmd);
						else {
							if (tcmd) report_error(0,"The file was not found in your 'path'", tcmd);
							else report_error(0,"The file was not found", cmd);
						}
						free_null_terminated_pointer_array(argv);
						Free(tcmd);

						close_up_shop();
						exit(ENOENT);
					}
				}
			}
			else {

				if (returnstr){
					write(output, returnstr, strlen(returnstr));
					write(output, "\n", 1);
				}
				free_null_terminated_pointer_array(argv);
				close_up_shop();
				exit(0);
			}
		}	
		else {
			report_error(errno, "Failed to fork", NULL);
//		sexp_destroy_context(ctx);
			return -1;
		}
	}
	LEAVING("",NULL);
	return 0;
}


/*--- Execute a single command -- C dispatch function */


//int c_execute_single_process(int in_the_background, char *cmd, char **argv, char *inputstring, int input, int output, int error) 
int c_execute_single_process(cmd_t *command) {
	return c_execute(command->run_scheme_expression, command->bg, command->argv, command->in, command->out, command->err, command->inputstring);
}



/*--- Dispatch a scheme command */

char *dispatch_scheme(char **argv) {// argv must be a null terminated array
	// iterate through the argv[i] and execute them in order, capturing the output and appendiing it to the string which will be returned.
	int i = 0;
	char *returns = NULL;
	

	for  (i = 0; argv && argv[i]; i++) {
		char *component = evaluate_scheme_expression(1, argv[i], NULL);
		if (!returns) {
			returns = component;
		}
		else if (component) {
			returns = (char *)realloc(returns, strlen(returns) + strlen(component) + 2);
			strcat(returns, " ");
			strcat(returns, component);
			Free(component);
		}
		else Free(component);
	}
	return returns;		
}

char *dispatch_scheme_stuff(cmd_t *command) {
	// iterate through the argv[i] and execute them in order, capturing the output and appendiing it to the string which will be returned.
	int i = 0;
	char *returns = NULL;
	
	//int procid = -1;
	//char *ss = NULL;
	//char *cmd = *command->argv;
	//int in_the_background = command->bg;
	//char **argv = command->argv;
	//char *inputstring = command->inputstring;
	//int input = command->in;
	//int output = command->out;
	//int error = command->err;



	//fprintf(stderr,"In dispatch_scheme_stuff: %s\n",command->argv[0]);

/*
  if (command->input != 0) {
  instring = read-all-input;
  Close(input);
  }
*/

	for  (i = 0; i < command->argc; i++) {
		char *component = evaluate_scheme_expression(1, command->argv[i], NULL);
		if (!returns) {
			returns = component;
		}
		else if (component) {
			returns = (char *)realloc(returns, strlen(returns) + strlen(component) + 2);
			strcat(returns, " ");
			strcat(returns, component);
			Free(component);
		}
		else Free(component);
	}
	return returns;		
}

		

/*--- Run a chain of commands -- doesn't support && ||  */

sexp run_commands(cmd_t *cmd) {
	//static sexp inp = SEXP_FALSE, outp = SEXP_FALSE, errp = SEXP_FALSE;
	int n = 0;
	sexp sn = SEXP_TRUE; // This function takes s-expressions that are constant, or referenced elsewhere and passes them back up the chain
	//int pass_numbers = 1;

	char *dp = NULL;
	char *lp = NULL;

	for (;n == 0 && cmd;cmd=cmd->next) {
		Builtin *op = NULL;

//		printf("%s is a program ==== %d\n", cmd->argv[0], isa_program(cmd->argv[0]));
//		printf("%s is a file ======= %d\n", cmd->argv[0], isa_dir(cmd->argv[0]));
//		printf("%s is a suid prog == %d\n", cmd->argv[0], isa_suidfile(cmd->argv[0]));
//		printf("%s is a sgid prog == %d\n", cmd->argv[0], isa_sgidfile(cmd->argv[0]));

		if (!cmd->argv[0] || !cmd->argv[0][0]) continue;

		strtod(cmd->argv[0], &dp);
		strtol(cmd->argv[0], &lp, 10);

#define dispatch_path
		
		// So what I need to do here is set up the current-*-ports for indicated scheme expressions (the underlying fds ought to be ok)

/*****  Set the run_scheme_expression flag  appropriately  *****/ 
		if (0);
		else if (is_sexp(cmd->argv[0]) || (strchr("$'", cmd->argv[0][0]) && cmd->argv[0][1] == '('))  {
#if defined(dispatch_path)
			cmd->run_scheme_expression = 1;
			op  = NULL;
#else
			op = member("scm", builtins);
#endif
			Dprintf("Scheme function: %s\n", cmd->argv[0]);
		}
		else if (is_sexp(cmd->argv[0]) || (cmd->argv[0][0] == '\'' && cmd->argv[0][1] == '('))  {
#if defined(dispatch_path)
			cmd->run_scheme_expression = 1;
			op  = NULL;
#else
			op = member("scm", builtins);
#endif
			Dprintf("Scheme function: %s\n", cmd->argv[0]);
		}
		else {
			op = member(*cmd->argv, builtins);
			Dprintf("Builtin function: %s\n", cmd->argv[0]);
		}
/*****  Now run the command  *****/


		char *tcmd = completed_path(cmd->argv[0]);

		if (!op && ((tcmd && isa_program(tcmd)) || cmd->run_scheme_expression)) {
			if (!cmd->cmd) cmd->cmd = string_array_as_string(cmd->argc, cmd->argv);
			Free(tcmd);

			run_command_prologue(cmd->cmd);
			n = c_execute_single_process(cmd);
			STATUS(n);
			run_command_epilogue(cmd->cmd, sexp_make_integer(ctx,n));
			
			if (!n) sn = SEXP_TRUE; // unix commands return 0 on success, we map this to "true"
			else {
				sexp tn;
				sexp_preserve_object(ctx,tn);
				sn = tn = sexp_make_integer(ctx,n);
			}
		}
#if !defined(dispatch_path)
		else if (cmd->run_scheme_expression) { // This is a scheme expression or set of expressions to be run in a separate 
			if (tcmd) Free(tcmd);
			if (!cmd->cmd) cmd->cmd = string_array_as_string(cmd->argc, cmd->argv);
			
			begin {
				char *s = NULL;
				
				printf("Scheme expression: %s ...\n", cmd->argv[0]);
				run_command_prologue(cmd->cmd);
				s = dispatch_scheme_stuff(cmd);
				run_command_epilogue(cmd->cmd, sexp_make_string(ctx,s,-1));
				printf("  ==> %s\n",s);
				Free(s);
			}
		}
#endif


#if defined(ALLOW_NUMBERS_AS_NUMBERS) // Not really useful
		else if (pass_numbers && *cmd->argv[0] && (!*dp || !*lp) && !isa_program(tcmd)) { // it is a number of some sort, but not an executable file name
			if (tcmd) Free(tcmd);

			printf("NUMBER: %s\n", cmd->argv[0]);
		}
#endif
		else if (op) {
			if (tcmd) Free(tcmd);

			sn = execute_builtin(cmd, op, cmd->argv, cmd->in, cmd->out);
			return sn;
		}
		else if ((n = scm_execute_single_process(cmd->bg, *cmd->argv, cmd->argv, cmd->in, cmd->out, cmd->err)) >= 0) {
			// fine;
			if (tcmd) Free(tcmd);

			if (!n) sn = SEXP_TRUE; // unix commands return 0 on success, we map this to "true"
			else {
				sexp tn = SEXP_FALSE;
				sexp_preserve_object(ctx,tn);
				sn = tn = sexp_make_integer(ctx,n);
			}
		}
		else {
			report_error(0,"You cannot run that, is it really a program or command?", cmd->argv[0]);
		}

		if (cmd->shuto) Close(cmd->out);
		if (cmd->shute) Close(cmd->err);
		cmd->shuto = cmd->shute = 0;
		cmd->out = cmd->err = -1;
		

		/*
		  if (command was piping into something) {
		  close the previous input port
		  }
		*/

		Free(tcmd);
	}

	return sn;
}


/*--- run_recursive_descent routines */

char **simple_sexpression(char **argv, sexp *rv, int excmd) { // terminated by the end of the s-expression
	// Treat a set of s-expressions as a single group (so we only emit on the last)
	char **c = NULL;

	if (!argv || !*argv) return argv;

	c = (char **)calloc(2, sizeof(char  *));
	*c = *argv;
	c[1] = NULL;

	*rv = execute_command_array(c); 
	
	return argv+1;
}

char **simple_exec_command(char **argv, sexp *rv, int excmd) { // terminated  by ";" "&&" "||" "&", or "}" (there should be no matching "${")
	char **a = NULL, **c = NULL;
	int N = 0;

	// I am sure this is wrong, but we are just trying to get things going to start with
	// NB: You can have an s-expression as an argument to a command!

	if (!argv || !*argv) return argv;

	for (a = argv; a && *a && !(!strcmp(*a, nextsep) || !strcmp(*a, andsep) || !strcmp(*a, orsep) || !strcmp(*a, makebg) || !strcmp(*a, begblock) || !strcmp(*a, endblock) || !strcmp(*a, outerrpipe) || !strcmp(*a, errpipe) || !strcmp(*a, outpipe)); a++);
	N = a-argv;

	if (excmd) {
		c = duplicate_null_terminated_string_array(argv);
		*rv = execute_command_array(c);
	}
	
	return argv+N;
}
	



// ****!! runs the same program over and over forever!


char **simple_command(char **argv, sexp *rv, int excmd, int in, int out, int err, int force_bg) {
	int done = 0;
	int i, fN = 0;
	int infd = -1, outfd = -1, errfd = -1;
	int pipefd[2];
	char *infn = NULL, *outfn = NULL, *errfn = NULL;
	int make_outpipe = 0, make_errpipe = 0, make_background = 0;
	int piping = 0;
	int N = 0;
	char **cmd = 0, **av = argv;

	static int LIMIT = 20;
	if (LIMIT < 0) exit(0);
	else LIMIT--;
	
	if (!argv) return NULL;
	// N is the number of non-null args, the number of elements is actually N+1

	if (force_bg) make_background = 1;
	
	if (in >= 0) infd = in;
	if (out >= 0) outfd = out;
	if (err >= 0) errfd = err;
	
	ENTRY("", NULL);

	for (i = 0; !done && argv && *argv; i++) {
		TRACK;
		static int LIMIT = 20;
		if (LIMIT < 0) exit(0);
		else LIMIT--;

		if (!cmd) {
			cmd = (char **)calloc(1, sizeof(*cmd)); 
		}

		if (!cmd)  abort();

#if 0
		if (cmd) {
			fprintf(stderr,"(%d:%d)  Current cmd = ", i, N);
			fprintf_string_array(stderr,-1,cmd);
			fprintf(stderr,"\n");
		}
#endif

		// There *is* an argument or symbol to process
		if (*argv) {
			TRACK;
		   // if we have a redirection, process it appropriately

			// stdin redirection
			if (*argv == stdinredir || !strcmp(*argv, stdinredir)) { 
				TRACK;
				if (infn || infd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Input is already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing filename for redirection in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				infn = handle_filename(argv[1]);
				in = open(infn, O_RDONLY);
				argv+=2;
				continue;
			}

			// stdout + stderr redirection
			else if (*argv == stdouterredir || *argv == stdouterrapp || !strcmp(*argv, stdouterredir) || !strcmp(*argv, stdouterrapp)) {
				TRACK;
				if (outfn || outfd > -1 || errfn || errfd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Output and Error are already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing filename for redirection in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				outfn = errfn = handle_filename(argv[1]);
				if (*argv == stdouterrapp) {
					err = out = open(outfn, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
				}
				else {
					err = out = open(outfn, O_TRUNC|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
				}
				argv+=2;
				continue;
			}	

			// stdout redirection
			else if (*argv  == stdoutredir || *argv == stdoutapp || !strcmp(*argv, stdoutredir) || !strcmp(*argv, stdoutapp)) {
				TRACK;
				if (outfn || outfd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Output is already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing filename for redirection in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				outfn = handle_filename(argv[1]);
				if (*argv == stdoutapp) {
					out = open(outfn, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
				}
				else {
					out = open(outfn, O_TRUNC|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
				}
				argv+=2;
				continue;
			}

			// stderr redirection
			else if (*argv == stderredir || *argv == stderrapp || !strcmp(*argv, stderredir) || !strcmp(*argv, stderrapp)) {
				TRACK;
				if (errfn || errfd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Error output is already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing filename for redirection in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				errfn = handle_filename(argv[1]);
				if (*argv == stderrapp) {
					err = open(errfn, O_APPEND|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
				}
				else {
					err = open(errfn, O_TRUNC|O_CREAT|O_WRONLY, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
				}
				argv+=2;
				continue;
			}	

			// pipe stdout to next program
			else if (*argv == outpipe || !strcmp(*argv, outpipe)) {
				TRACK;
				if (outfn || outfd > -1 || errfn || errfd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Output is already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing command for pipe in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				make_outpipe = 1;
				piping = 1;
				argv++;
				continue;
			}

			// pipe stderr to next program
			else if (*argv == errpipe || !strcmp(*argv, errpipe)) {
				TRACK;
				if (outfn || outfd > -1 || errfn || errfd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Error output is already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing command for pipe in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				make_errpipe = 1;
				piping = 1;
				argv++;
				continue;
			}

			// pipe both stdout and stderr to next program
			else if (*argv == outerrpipe || !strcmp(*argv, outerrpipe)) {
				TRACK;
				if (outfn || outfd > -1 || errfn || errfd > -1) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Output or Error is already redirected in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				if (!argv[1]) {
					report_error(0, "Command redirection error", NULL);
					fprintf(stderr,"Missing command for pipe in '");
					fprintf_string_array(stderr,-1,av);
					fprintf(stderr,"'\n");
					LEAVING("",NULL);
					return NULL;
				}
				make_outpipe = make_errpipe = 1;
				piping = 1;
				argv++;
				continue;
			}
			else if (*argv == makebg || !strcmp(*argv, makebg)) {
				TRACK;
				make_background = 1;
				argv++;
				continue;
			}
		}
//		else fprintf(stderr,"-------------- Done with argv --------------\n");
		TRACK;

		
		// If it isn't a pipe of some sort, execute the command we have collected and fall back to linear_chain for the next bit 
		if (!*argv || *argv  == nextsep  || *argv  == andsep  || *argv == orsep || *argv == begblock || *argv == endblock
			|| !strcmp(*argv, nextsep)  	|| !strcmp(*argv, andsep)  || !strcmp(*argv, orsep) || !strcmp(*argv, begblock) || !strcmp(*argv, endblock)) { 
			int r = -1;
			TRACK;

			if (excmd) {
				r = c_execute(0,make_background,cmd, in, out, err, NULL);
				STATUS(r);
			}

			if (r) { // non-zero 
				*rv = sexp_make_fixnum(r);
			}
			else {
				*rv = SEXP_TRUE;
			}

			gronk(rv,excmd);

			delete_string_array(cmd);
			cmd = NULL;

			return argv;
		}
			
/*
		so if we are piping, we call linear_chain *here* rather than unrolling with a return;
		if it is a nextsep or other terminator, we just return and let the previous routines continue;
		everything from here is likely to need extensive ... *adjustment*;

		mind the makebg flag....

*/
		// these things mark the end of a command
		else if (piping || done) { 
			// cmd should hold all the bits of the command now... dispatch as appropriate

			TRACK;
			if (piping) {
				i++; // skip the pipe so that the i++ in the for-loop will skip the filename
				// Set up pipefd
				fprintf(stderr,"[Pipes]\n");
				pipe(pipefd);
				
				if (make_outpipe && make_errpipe) {
					fprintf(stderr,"[out+err]\n");
					out = err = pipefd[1];
				}
				else if (make_outpipe) {
					fprintf(stderr,"[out]\n");
					out = pipefd[1];
				}
				else if (make_errpipe) {
					fprintf(stderr,"[err]\n");
					err = pipefd[1];
				}
				else {
					fprintf(stderr,"Whut?");
					abort();
				}
				
				// Now fire off the leading program and the following program in the bg
				begin {
					sexp qv;
					int r;
					// This is the program being piped into
					argv = simple_command(argv, &qv, excmd, in, out, err, 1);
				
					if (excmd) r = c_execute(0,make_background,cmd, in, out, err, NULL);
					STATUS(r);

					if (r) { // non-zero 
						*rv = sexp_make_fixnum(r);
					}
					else {
						*rv = SEXP_TRUE;
					}
					done = 0; // the semicolon implies that there may be something after...

					
					if (*rv == SEXP_TRUE) {
						if (qv != SEXP_TRUE) *rv = qv;
					}
				}
			}
		}

		else {
			int q;
			piping = 0;
			char **ncmd = 0;

			TRACK;
#if 0
			fprintf(stderr,"Adding '%s' to array of %d elements: ", *argv, N);
			fprintf_string_array(stderr,-1,cmd);
			fprintf(stderr,"\n");
#endif
			cmd = add_to_string_array(*argv, -1, cmd);
#if 0
			fprintf(stderr,"becomes ");
			fprintf_string_array(stderr,-1,cmd);
			fprintf(stderr,"\n");
#endif
			argv++;
		}
	}

	// Execute command/sexpression here

	if (!excmd) {
		
	}
	else {
#if 0
		fprintf(stderr,"Executing: ");
		fprintf_string_array(stderr,-1,cmd);
		fprintf(stderr,"\n");
#endif			
		if (excmd) i = c_execute(0,make_background,cmd, in, out, err, NULL);
		STATUS(i);

		if (i) { // non-zero 
			*rv = sexp_make_fixnum(i);
		}
		else {
			*rv = SEXP_TRUE;
		}
	}

	delete_string_array(cmd);
	cmd = NULL;

	LEAVING("", NULL);
	return argv; // j handles the file redirection bits
}


/*
linear-chain ---> returns the last exit value, continues till end of chain if possible

   __ linear-chain _______________________________________________________
                        \ __ ; _________ linear-chain __/
								 
*/

char **linear_chain(char **argv, sexp *rv, int excmd, int in, int out, int err) {
	int oin = in, oout = out, oerr = err;
	ENTRY("",NULL);
	static int LIMIT = 20;
	if (LIMIT < 0) exit(0);
	else LIMIT--;
	
	if (argv && argv[0]) {
		argv = simple_command(argv, rv, excmd, in, out, err, 0); // in, out, and err may be modified by simple_command to take piping into account. 
		
		if (argv && argv[0]) { 
//			fprintf(stderr,"%p %p: %s\n", argv, *argv, *argv);
			if (*argv == nextsep || !strcmp(*argv, nextsep)) {
				TRACK;
				argv++; // consume ;
				gronk(rv,excmd);
				argv = linear_chain(argv, rv, excmd, in, out, err);
			}
			else if (*argv == andsep || *argv == orsep || *argv == begblock || *argv == endblock || !strcmp(*argv, andsep)  || !strcmp(*argv, orsep) || !strcmp(*argv, begblock) || !strcmp(*argv, endblock)) {
				TRACK;
			}
		}
		//if (rv && excmd && *rv != SEXP_TRUE) excmd = 0;
		gronk(rv,excmd);
	}

	LEAVING("", NULL);
	return argv;
}


/*
and-chain ---> returns the last exit value, stops on failure

   __ simple-chain _______________________________________________________
                        \ __ && __ and-chain __/
*/

char **and_chain(char **argv, sexp *rv, int excmd, int in, int out, int err) {
	int done = 0;
	
	ENTRY("",NULL);
	while (!done && argv && argv[0] ) {
		TRACK;
		if (argv && argv[0]) argv = linear_chain(argv, rv, excmd, in, out, err);

		if (argv && argv[0]) { 
			if (*argv == andsep || !strcmp(*argv,andsep)) {
				TRACK;
				argv++; // consume &&
		
				if (*rv != SEXP_TRUE) excmd = 0;
		
				gronk(rv,excmd);
				argv = linear_chain(argv, rv, excmd, in, out, err);
				if (*rv == SEXP_FALSE) excmd = 0;
				gronk(rv,excmd);
			}
			else if (*argv == orsep || *argv == begblock || *argv == endblock || !strcmp(*argv, orsep) || !strcmp(*argv, begblock) || !strcmp(*argv, endblock)) {
				TRACK;
				done = 1;
			}
		}
		if (rv && excmd &&  *rv == SEXP_FALSE) excmd = 0;
		gronk(rv,excmd);
	}

	LEAVING("",NULL);
	return argv;
}


/*
or-chain  ---> returns the last exit value, stops on success

  ___ and-chain  _________________________________________________________
                    \ __ || __ or-chain__/
*/

char **or_chain(char **argv, sexp *rv, int excmd, int in, int out, int err) {
	int done = 0;
	ENTRY("",NULL)
	while (!done && argv && *argv) {
		TRACK;
		if (argv && *argv) argv = and_chain(argv, rv, excmd, in, out, err);

		if (argv && *argv) {
			if (*argv == orsep || !strcmp(*argv, orsep)) {
				TRACK;
				argv++; // consume ||

				if (*rv == SEXP_TRUE) excmd = 0;
				gronk(rv,excmd);


				argv = and_chain(argv, rv, excmd, in, out, err);

				if (*rv == SEXP_TRUE) excmd = 0;
				gronk(rv,excmd);
			}
			else if (*argv == begblock || *argv == endblock || !strcmp(*argv,begblock) || !strcmp(*argv,endblock)) {
				TRACK;
				done = 1;
			}
		}
		if (rv && excmd && *rv != SEXP_TRUE) excmd = 0;
		gronk(rv,excmd);

	}

	LEAVING("",NULL);
	return argv;
}





/*
command-block --> returns whatever comes out

  ___ or-chain ___________________________________________________________
    \__ { _______ command-block _______ } _________________________/


*/

char **command_block(char **argv, sexp *rv, int excmd, int in, int out, int err) { 
//	char **av = argv;

	ENTRY("",NULL);
	if (!argv || !*argv) return NULL;

	if (strcmp(*argv, begblock)) {
		TRACK;	
	if (rv) argv = or_chain(argv, rv, excmd, in, out, err);
		else argv = or_chain(argv, NULL, excmd, in, out, err);
	}
	else {
		TRACK;
		argv++; // consume beginning delimiter
		argv = or_chain(argv, rv,excmd, in, out, err);

		//if (rv && excmd && sexp_equalp(ctx, *rv, SEXP_FALSE)) excmd = 0;

		if (!argv || !*argv || strcmp(*argv, endblock)) {
			report_error(0,"Missing end of block", NULL);
			return NULL;
		}
		else argv++; // consume final delimiter
	}
	LEAVING("",NULL);
	return argv;
}


/*--- Routines that have the potential to support *amazing* customisation ... if they work */

void run_command_prologue(char *cmds) {
	sexp_gc_var2(prologue, str);
	sexp_gc_preserve2(ctx,prologue, str);
	str = SEXP_NULL;
	if (cmds) {
		str = sexp_cons(ctx, sexp_c_string(ctx,cmds, -1), str);
		prologue = sexp_get_procedure(ctx, env, IGOR_COMMAND_PROLOGUE);
		if (sexp_procedurep(prologue)) sexp_apply(ctx, prologue, str);
	}
	sexp_gc_release2(ctx);
}

void run_command_epilogue(char *cmds, sexp rv) {
	sexp_gc_var2(epilogue, str);
	sexp_gc_preserve2(ctx,epilogue, str);
	str = SEXP_NULL;
	if (cmds) {
		str = sexp_cons(ctx, rv, str);
		str = sexp_cons(ctx, sexp_c_string(ctx,cmds, -1), str);
		epilogue = sexp_get_procedure(ctx, env, IGOR_COMMAND_EPILOGUE);
		if (sexp_procedurep(epilogue)) sexp_apply(ctx, epilogue, str);
	}
	sexp_gc_release2(ctx);
}


void update_internal_c_variables() {
	char *s = NULL;
	s = getenv("TRACK_EXECV");
	if (s) track_execv = atoi(s);
}


/*- Command loop */
	
#if defined(RECURSIVE_DESCENT)
void command_loop(FILE *f) {
	char *cmd = NULL;
	char **argv = NULL;
	int line_num = 0, N = 0;
	sexp rv = SEXP_FALSE;

	//if (f == stdin) fprintf(stderr,"** The file seems to be stdin! (%s)\n", __PRETTY_FUNCTION__);


#if 0
	cmd = strdup("a b c d");
	argv = tokenise_cmdline(cmd);
	if (1) {
		int i;
		for (i = 0; argv && argv[i]; i++) {
			printf("[%d] == %s @ %p\n", i, argv[i], argv[i]);
		}
		return;
	}
#endif


	while((cmd = get_commandline(f,NULL))) {
		int i;

		if (!line_num && !strncmp(cmd, "#!/", 3)) {
			Cprintf("Passing over the magic number\n");
			continue;
		}
		line_num++;

		Cprintf("about to run the command [%s]\n", cmd);
		umask((mode_t)(S_IWGRP|S_IWOTH));
		if (!f && !running_script) { // reset the file descriptors
			dup2(IN, 0);
			dup2(OUT,1);
			dup2(ERR,2);
		}

		update_internal_c_variables();

		if (*cmd != '\n' && *cmd != '\r' && *cmd) {
			char **a = NULL;
			int n = 0;
			if (!strcmp(cmd,"exit")) return;

			argv = tokenise_cmdline(cmd);
			if (!argv) {
				//fprintf(stderr,"DEBUGGING: finishing command loop\n");
				return;
			}
			a = argv;
			for (N = 0; argv && argv[N]; N++);

#if 0
			for (i = 0; i <= N; i++) {
				if (a[i]) printf("%p %p %p %d) %s\n", a, a+i, a[i],i, a[i]);
				else printf("%p %p %p %d) (NULL)) \n", a, a+i, a[i],i);
			}
#endif

			while (a && *a) {
				a = command_block(a, &rv, 1, -1, -1, -1);
#if 0
				if (a - argv >= N) fprintf(stderr,"At end of command line\n");
				for (n = 0; n < N && argv[n]; n++) {
					fprintf(stderr,"CL before[%d] = %s\n", n, argv[n]);
				}
				for (n = 0; a[n]; n++) {
					fprintf(stderr,".. executed[%d] = %s\n", n, argv[n]);
				}
#endif				

				// *** EAT ANYTHING THAT  NEEDS EATING
			}

			Free(argv);
		}
		if (cmd) Free(cmd);
		cmd = NULL;
	}
}

#else
void command_loop(FILE *f) {
	char *cmd = NULL;
	cmd_t *C = NULL;
	char **argv  = NULL;
	int i = 0;
	int line_num = 0;

	//if (f == stdin) fprintf(stderr,"** The file seems to be stdin! (%s)\n", __PRETTY_FUNCTION__);


	while((cmd = get_commandline(f,NULL))) {
		if (!line_num && !strncmp(cmd, "#!/", 3)) {
			Cprintf("Passing over the magic number\n");
			continue;
		}
		line_num++;

		Cprintf("about to run the command [%s]\n", cmd);
		umask((mode_t)(S_IWGRP|S_IWOTH));
		if (!f && !running_script) { // reset the file descriptors
			dup2(IN, 0);
			dup2(OUT,1);
			dup2(ERR,2);
		}

		update_internal_c_variables();

		if (*cmd != '\n' && *cmd != '\r' && *cmd) {
			if (!strcmp(cmd,"exit")) return;

			argv = tokenise_cmdline(cmd);

			for (i = 0; argv &&  argv[i]; i++) {
				Dprintf("** argv[%d] = %s\n",i, argv[i]);
			}

			C = process_token_list(argv,0,1,2);

			//write(1,C->argv[0], strlen(C->argv[0]));
			//write(1,"\n",1);

			if (!C) printf("NO C -- I don't think this ought to happen.\n");
			else run_commands(C);

			if (C) free_cmd(C);
			C = NULL;
				//if (argv) free_null_terminated_pointer_array(argv);
			Free(argv);
			argv = NULL;
		}
		if (cmd) Free(cmd);
		cmd = NULL;
	}
}
#endif


sexp run_source_file(char *filename) {
	FILE *f = NULL;

	if (access(filename, R_OK) == Ok) {
		fprintf(stderr,"Found %s\n", filename);
		
		f = fopen(filename, "r");

		//if (f == stdin) fprintf(stderr,"** The file seems to be stdin! (%s)\n", __PRETTY_FUNCTION__);

		if (f) {
			fprintf(stderr,"Executing commands from %s\n", filename);
			command_loop(f);
			fclose(f);
			f = NULL;
		}
		else {
			report_error(errno, "Failed to open source file", filename);
			return SEXP_FALSE;
		}
	}
	else {
		report_error(errno, "Failed to open source file; Either it doesn't exist or it is not readable", filename);
		return SEXP_FALSE;
	}
	return SEXP_TRUE;
}


sexp load_igor_rc(char *urc) {
	static char *igoretcrc = IGOR_SYS_RC_FILE;
	const int use_load = 1;
	char userrc[512] = "";
	const struct passwd* pwen;
	int id = getuid();
	const char *hdir;
	
	sexp_gc_var2(fname,rtn);
	sexp_gc_preserve2(ctx, fname,rtn);
	
	pwen = getpwuid(id);
	hdir = pwen ? pwen->pw_dir : "/";

	if (access(igoretcrc, R_OK) == Ok) { 
		fname = sexp_c_string(ctx, igoretcrc, -1);
		if (use_load) {
			rtn = sexp_load(ctx, fname, ENV);
		}
		else {
			if (sexp_equalp(ctx, SEXP_TRUE, run_source_file(sexp_string_data(fname)))) rtn = SEXP_TRUE;
			else rtn = SEXP_FALSE;
		}
	}
	
	*userrc = 0; // superfluous
	if (urc) strcpy(userrc, urc);
	else if (hdir) sprintf(userrc, "%s/" IGOR_RC_FILE, hdir);
	else *userrc = 0;

	if (*userrc && access(userrc, R_OK) == Ok) {
		fname = sexp_c_string(ctx, userrc, -1);
		if (use_load) {
			rtn = sexp_load(ctx, fname, NULL);
		}
		else {
			if (sexp_equalp(ctx,SEXP_TRUE, run_source_file(sexp_string_data(fname)))) rtn = SEXP_TRUE;
			else rtn = SEXP_FALSE;
		}
	}
	sexp_gc_release2(ctx);

	return rtn;
}


/*-------   Builtins   -------------------------------*/


sexp exit_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring) {
	int i = 0;
	char *message = "\nBad argument to exit: %s\n";
	
	if (inputstring) {report_error(0,"'exit' takes no input!",NULL);}

	Cprintf("exit_func\n");
	close_up_shop();

	if (argv == NULL || *argv == NULL || **argv == 0)	exit(0);
	else if (sscanf(argv[1], "%d",&i) >= 1) exit(i);
	else {
		int n = strlen(message) + strlen(*argv);
		char *msg = (char *)malloc(n);
		sprintf(msg,message, *argv);
		if (*msg) write(2, msg, strlen(msg));
		Free(msg);
		exit(BUGGER);
		//return 1;
	}
	return SEXP_FALSE;
}

sexp set_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring) {
	Cprintf("set_func\n");
	if (argv[1]) {
		char *cmd = NULL;
		int i = 0;

		if (inputstring) {report_error(0,"'set' takes no input!",NULL);}

		for (i = 1; argv[i]; i++) {
			if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
				char *ss = evaluate_scheme_expression(1, argv[i], NULL); // These get cleaned up when argv[i] is freed
				if (ss) {
					Free(argv[i]);
					argv[i] = ss;
				}
			}
		}


		if (argv[2]) {
			char *ts = NULL;
			setenv(argv[1], argv[2], 1);
			asprintf(&cmd, "(define %s \"%s\")", argv[1], argv[2]);
			if (cmd) ts = evaluate_scheme_expression(1, cmd, NULL);
			Free(ts)
			Free(cmd);
		}
		else {
			char *ts = NULL;
			setenv(argv[1], "", 1);
			asprintf(&cmd, "(define %s '())", argv[1]);
			if (cmd) ts = evaluate_scheme_expression(1, cmd, NULL);
			Free(ts)
			Free(cmd);
		}
	}
	else {
		char *message = "\nbad call to builtin 'set'\n";
		if (*message) write(2, message, strlen(message));
		return SEXP_FALSE;
	}
	return SEXP_TRUE;
}

sexp repl_output(cmd_t *cmnd, char **argv, int in, int out, char *inputstring) {
	char *value = argv[1];

	if (inputstring) {report_error(0,"'repl_output' takes no input!",NULL);}

	Cprintf("repl_output\n");
	if (!value || !*value) repl_write = -1;
	else if (!strcmp(value, "none")) repl_write = -1;
	else if (!strcmp(value, "quiet")) repl_write = -1;
	else if (!strcmp(value, "stdout")) repl_write = 0;
	else if (!strcmp(value, "stderr")) repl_write = 1;

	setenv("REPL_WRITE", "-1", repl_write);
	return sexp_make_integer(ctx,repl_write);
}


sexp unset_func(cmd_t *cmnd, char **argv, int in, int out, char *inputstring) {
	int i = 0;
	char *cmd = 0;
	char *ts = NULL;

	if (inputstring) {report_error(0,"'unset' takes no input!",NULL);}

	Cprintf("unset_func\n");
	for (i = 1; argv[i]; i++) {
		unsetenv(argv[i]);
		asprintf(&cmd, "(define %s '())", argv[i]);
		if (cmd) ts = evaluate_scheme_expression(1, cmd, NULL);
		Free(ts);
		Free(cmd);
	}
	return SEXP_TRUE;
}

sexp exec_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring) {
	Cprintf("exec_func\n");

	if (inputstring) {report_error(0,"'exec' takes no input!",NULL);}

	argv = argv+1;
	if (argv == NULL || *argv == NULL || **argv == 0)	exit(0);
	if (execvp(*argv, argv)) {
		report_error(errno, "Failed to execute the program",*argv);
	}
	return SEXP_FALSE;
}

sexp cd_func(cmd_t *cmnd, char **argv, int in, int out, char *inputstring) {
	/*** change this so it can be replaced by a scheme routine ***/
	int i = 0;

	if (inputstring) {report_error(0,"'cd' takes no input!",NULL); return SEXP_FALSE;}

 	Cprintf("cd_func\n");

	for (i = 1; argv[i]; i++) {
		if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
			char *ss = evaluate_scheme_expression(1, argv[i], NULL); // Freed when argv[i] is freed
			if (ss) {
				Free(argv[i]);
				argv[i] = ss;
			}
		}
	}

	if (argv && *argv) {
#if 1
		sexp_gc_var1(rslt);
		sexp_gc_preserve1(ctx, rslt);

		if (!argv[1]) rslt = sexp_eval_string(ctx,"(cd)", -1, ENV);
		else {
			char *cd;
			asprintf(&cd,"(cd \"%s\")", argv[1]);
			rslt = sexp_eval_string(ctx,cd, -1, ENV);
			Free(cd);
		}

		if (rslt == SEXP_FALSE) {
			if (!argv[1]) printf("Unable to change to the home directory! (%s)\n", getenv("HOME"));
			else printf("Failed to change to %s\n", argv[1]);

			sexp_gc_release1(ctx);
			return SEXP_FALSE;
		}
		
		sexp_gc_release1(ctx);
		return SEXP_TRUE;

#else
		if (!argv[1]) {
			if (!chdir(getenv("HOME"))) return 0;
			else {
				printf("Unable to change to the home directory! (%s)\n", strerror(errno));
				return SEXP_FALSE;
			}
		}
		else if (!chdir(argv[1])) return 0;
		else {
			printf("Failed to change to %s (%s)\n", argv[1], strerror(errno));
			return SEXP_FALSE;
		}
#endif
	}
	else abort();
	return SEXP_TRUE;
}

// Process a "sourced" file
sexp source_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring) {
	int i = 0;

	for (i = 1; argv[i]; i++) {
		if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
			char *ss = evaluate_scheme_expression(1, argv[i], NULL);
			if (ss) {
				Free(argv[i]);
				argv[i] = ss;
			}
		}
	}


	for (i = 1; argv[i]; i++) {
		if (run_source_file(argv[i])) {
			// Failed
			report_error(0, "Terminating source command", argv[i]);
			return SEXP_FALSE;
		}
	}
	return SEXP_TRUE;
}




sexp scm_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring) {
	// Need to collect *all* the arguments  and process them as input....
	int k = 0;
	char *p = NULL, *q = NULL, *r = NULL, *s = NULL, *t = NULL;
	char *sline = NULL;
	char **Sexp = NULL;
	sexp_gc_var3(result, inexpr, outexpr);
	sexp_gc_preserve3(ctx, result, inexpr, outexpr);

   //fprintf(stderr,"Entering scm_func %s\n", *argv);

	k = 0;
	
	if (!argv || !argv[0] || !*argv[0]) {
		return SEXP_FALSE;
	}
	
	sline = strdup("");

	// Stick all the s-expressions in one spot
	for (Sexp = argv; *Sexp; Sexp++) {
		k += strlen(*Sexp);
		sline = (char *)reallocate(sline, (strlen(sline) + strlen(*Sexp) + k + 3) * sizeof(char));
		
		if (Sexp != argv) strcat(sline, " "); // insert a space if it isn't the first one
		strcat(sline,*Sexp);
	}

	r = NULL; 
	// over all the arguments, 
	k = 0;
	for (s = sline; *s; ) {
		t = jump_sexp(s,0);
		if (t == s && *t) {
			// Parsing problem

			report_error(0,"Error parsing s-expression", s);
			Free(sline);
			return SEXP_FALSE;
		}

		p = (char *)malloc(t - s + 2);
		strncpy(p, s, t-s);
		p[t-s] = 0;

		s = t;
		t = NULL;

		if (!strcmp(p,"()")) {
			char *ts = NULL;
		// empty function application -- there really ought to be something good we could do...
			ts = evaluate_scheme_expression(1, "#t", inputstring); // Sets the ERRCON state appropriately
			Free(ts);
			q = strdup("()"); // 
		}
		else {
			q = evaluate_scheme_expression(1, p, inputstring); // Sets the ERRCON state appropriately
		}
		
		//fprintf(stderr,"*** loop %d: %s ## %s\n", k, q, s);

		//result = sexp_eval_string(ctx,"*last_igor_eval*",-1,ENV);

		if (ERRCON != SEXP_UNDEF && ERRCON != SEXP_VOID && !sexp_exceptionp(ERRCON)) {
			if (q && *q) {
				int nq = strlen(q);
				write(out,q,nq);
				if (!r) {
					r = (char *)realloc(r,(nq + 1)*sizeof(char));
					strcpy(r,q);
				}
				else {
					r = (char *)realloc(r,(strlen(r) + nq + 1)*sizeof(char));
					strcat(r, q);
				}

				k++;
			}
			Free(q);
		}
		else if (sexp_exceptionp(ERRCON)) {
			report_error(0,"Exception raised in scm_func", p);
			ERRCON = SEXP_TRUE;
		}
		Free(p);
	}

	if (ERRCON != SEXP_UNDEF && ERRCON != SEXP_VOID && !sexp_exceptionp(ERRCON)) {
		fprintf(stderr,"*** loop %d: %s\n", k, q);
		if (repl_write >= 0 && q) {
			write(out,q,strlen(q));
			k++;
		}
	}

	if (k > 0) write(out, "\n", 1);

	//fprintf(stderr,"scm_func finished evaluating %s\n", sline);
	
	Free(sline);

	sexp_gc_release3(ctx);
	return SEXP_TRUE;
}

/*- system initialisation and shutdown */

void install_builtins() {
	builtins = insert_builtin(builtins, "exec", exec_func);
	builtins = insert_builtin(builtins, "source", source_func);
	builtins = insert_builtin(builtins, ".", source_func); // alias
	builtins = insert_builtin(builtins, "repl_output", repl_output);
	builtins = insert_builtin(builtins, "scm", scm_func);
	builtins = insert_builtin(builtins, "exit", exit_func);
	builtins = insert_builtin(builtins, "cd", cd_func);
	builtins = insert_builtin(builtins, "set", set_func);
	builtins = insert_builtin(builtins, "setenv", set_func); // alias
	builtins = insert_builtin(builtins, "set!", set_func);   // alias
	builtins = insert_builtin(builtins, "unset", unset_func);
	builtins = insert_builtin(builtins, "unsetenv", unset_func); // alias
	builtins = insert_builtin(builtins, "unset!", unset_func);   // alias
}

int exit_value(sexp rtnv, int but_continue) {
	if (sexp_equalp(ctx,rtnv,SEXP_TRUE) && !but_continue) exit(0);
	else if (sexp_numberp(rtnv)) exit(sexp_unbox_fixnum(rtnv));
	else exit(BUGGER);
}

void close_up_shop() {
	int i = 0;

	for (i = 0; magic_string && i < n_magic_strings; i++) Free(magic_string[i]);
	Free(magic_string);
	Free(history_file);

	delete_builtin(builtins);

	sexp_release_object(ctx,ERRCON);
	sexp_release_object(ctx,sym);
	sexp_release_object(ctx,igor_execute);
	sexp_release_object(ctx,current_input);
	sexp_release_object(ctx,current_output);
	sexp_release_object(ctx,current_error);

	sexp_destroy_context(ctx);
}


void preignition() {
  igor_pid = getpid();
  igor_pgid = getpgid(igor_pid);

  // These are the stdin, stdout and stderr on entry
  IN = dup(0); 
  OUT = dup(1);
  ERR = dup(2);
}


void set_special_vars(int argc, char **argv, int pid) {
	char *str = NULL;

   asprintf(&str, "%d", pid);
	igor_set(ctx,"$$", str);
	Free(str);

   asprintf(&str, "%d", argc-1);
	igor_set(ctx,"$#", str);
	Free(str);

	if (argc > 0) igor_set(ctx,"$0", argv[0]);
	else igor_set(ctx,"$0", "");
	
	igor_set(ctx,"$!", "-1"); // no "last backgrounded job"
	igor_set(ctx,"$-", "#f"); // no last pipline status

	if (pid >= 0) igor_set(ctx,"$?", "0");  // last pipeline is fine
	

	/**** FIX THIS ****/
	igor_set(ctx,"$@", ""); // no "last backgrounded job"
	igor_set(ctx,"*", ""); // no "last backgrounded job"
}


void initialise_symbol_table() {
	int i = 0;

	symbol[i++] =  start_fence;
	symbol[i++] =  end_fence;

	// These are characters
//	symbol[i++] =  sescape;
//	symbol[i++] =  escape;
//	symbol[i++] =  squote;
//	symbol[i++] =  dquote;
//	symbol[i++] =  bquote;

	symbol[i++] =  quotedlist;
	symbol[i++] =  continuation_str;
	symbol[i++] =  scmunquotesplicelst;
	symbol[i++] =  scmunquotelst;
	symbol[i++] =  scmunquotesplice;
	symbol[i++] =  scmunquote;

	symbol[i++] =  comment;
	symbol[i++] =  not;

	symbol[i++] =  shellcmd;
	symbol[i++] =  varexpr;
	symbol[i++] =  herestr;
	symbol[i++] =  heredoc;
	symbol[i++] =  stdouterrapp;
	symbol[i++] =  stderrapp;
	symbol[i++] =  stdoutapp;
	symbol[i++] =  stdouterredir;
	symbol[i++] =  stderredir;
	symbol[i++] =  stdoutredir;
	symbol[i++] =  stdinredir;

	symbol[i++] =  nextsep;
	symbol[i++] =  makebg;

	symbol[i++] =  andsep;
	symbol[i++] =  outerrpipe;
	symbol[i++] =  errpipe;
	symbol[i++] =  outpipe;

	symbol[i++] =  orsep;

	symbol[i++] =  begblock;
	symbol[i++] =  endblock;

	for (; i < STABSIZE; symbol[i++] = NULL);
}





void initialise_interpreter(int argc, char **argv) {
	sexp res = SEXP_FALSE;
	char **ss = NULL;

    // ctx is global
  sexp_scheme_init();

  ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  env = sexp_context_env(ctx);
  //env = NULL;

  sexp_preserve_object(ctx,ERRCON);
  sexp_preserve_object(ctx,sym);
  sexp_preserve_object(ctx,igor_execute);
  sexp_preserve_object(ctx,current_input);
  sexp_preserve_object(ctx,current_output);
  sexp_preserve_object(ctx,current_error);
  sexp_preserve_object(ctx,res);

#if defined(TRACK_LOADING)
  printf("LOADING STANDARD ENVIRONMENT\n");
#endif
  sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 0);

#if defined(TRACK_LOADING)
  printf("INTERNS\n");
#endif
  sexp_intern(ctx,IGOR_HISTORY_FILE_VAR, -1);
  sexp_intern(ctx,"*igor-input-source-port*", -1);
  sexp_intern(ctx,"*igor-output-capture-port*", -1);
  sexp_intern(ctx,"*igor-swap-input-source-port*", -1);
  sexp_intern(ctx,"*igor-swap-output-capture-port*", -1);
  
#if defined(TRACK_LOADING)
  printf("DEFINES\n");
#endif
  
  sexp_eval_string(ctx,"(define *eof* (let ((p (open-input-file \"/dev/null\"))) (let ((e (read p))) (close-port p) e)))", -1, env);
  sexp_eval_string(ctx,"(define (display-to-string sexpr) (let ( (out (open-output-string))) (display sexpr out) (get-output-string out))))", -1, env);
  sexp_eval_string(ctx,"(define (write-to-string sexpr) (let ((out (open-output-string))) (write sexpr out) (get-output-string out))))", -1, env);

#if defined(TRACK_LOADING)
  printf("CHIBI MODULES\n");
#endif
  
  for (ss = supporting_initialisation; ss && *ss; ss++) {
#if defined(TRACK_LOADING)
	  printf("-- %s\n", *ss);
#endif

    res = sexp_eval_string(ctx,*ss, -1, env);
    if (sexp_exceptionp(res)) sexp_print_exception(ctx, res, SEXP_FALSE);
#if defined(TRACK_LOADING)
	  printf("   DONE\n");
#endif
  }

  set_special_vars(argc, argv, getpid());

#if defined(BOOTSTRAP)
  begin {
	  if (access(BOOTSTRAP, F_OK) == Ok) {
		  if (access(BOOTSTRAP, R_OK) != Ok) {
				  report_error(0,"Unable to load bootstrap file, aborting", BOOTSTRAP);
		  }
		  sexp_load(ctx,sexp_c_string(ctx,BOOTSTRAP,-1),ENV);
	  }
  }
#endif


#if defined(extra_load_file)
  begin {
	  sexp_var1(elf);

	  sexp_preserve_object(ctx,elf);
	  elf = sexp_c_string(ctx,extra_load_file,-1);
	  sexp_load(ctx,elf,ENV);
	  sexp_release_object(ctx,elf);
  }
#endif

  /* Incorporate local extensions */
  //  sexp_define_foreign(ctx,env,"word-expand",1,sexp_wordexp);

}





/*- Running things!  */


int scripting(int i) {
	char b[100] = "";
	int l = running_script;
	running_script = i;
	sprintf(b,"(define *running-script* %d)", i);
	sexp_eval_string(ctx,b,-1,ENV);
	return l;
}


int igor(int argc, char **argv) {
	int i = 1;
	int run_interactive_shell = 1;
	int run_rc = 1;
	int run_builtins = 1;
	int just_exit = 0;
	sexp_gc_var1(rtnv);
	sexp_gc_preserve1(ctx,rtnv);
	
	add_magic(quotedlist);
	add_magic(heredoc);
   add_magic(not);
	add_magic(andsep);
	add_magic(orsep);
	add_magic(outerrpipe);
	add_magic(errpipe);
	add_magic(outpipe);
	add_magic(stdouterrapp);
	add_magic(stdoutapp);
	add_magic(stderrapp);
	add_magic(stdouterredir);
	add_magic(stdoutredir);
	add_magic(stderredir);
	add_magic(stdinredir);
	add_magic(makebg);
	add_magic(nextsep);
	add_magic(begblock);
	add_magic(endblock);
	add_magic(shellcmd);
	add_magic(varexpr);
	add_magic(comment);

	add_magic(scmunquotesplicelst);
	add_magic(scmunquotelst);
	add_magic(scmunquotesplice);
	add_magic(scmunquote);


#if 0	
	{
		int q;
		for (q = 0; q < n_magic_strings; q++) {
			printf("%d) %p %s\n", q, magic_string[q], magic_string[q]);
		}
	}
#endif

   /* check if this is an interactive shell */
	if (isatty(0) && isatty(1)) {
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGTERM, SIG_IGN);
		signal(SIGCHLD, &catch_sigchld);
	}
#define MAX_CMD 2048

	// set the array that the function "command-line" returns as the list of the arguments here

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i],"--no-builtins")) run_builtins = 0;
		else if (!strcmp(argv[i], "-q") || !strcmp(argv[i], "--no-rc")) run_rc = 0;
		else if (!strcmp(argv[i],"-h") || !strcmp(argv[i],"--help")) {

		}
		else if (!strcmp(argv[i],"-V") || !strcmp(argv[i],"--verbose-version")) {
			printf("Igor, the lithping thell (with thanks to Terry Pratchett)\n");
			printf("For more information and source code visit\n   %s\n",IGOR_REPO);
			printf("version list: %s\n",IGOR_VERSION);
		}
		else if (!strcmp(argv[i],"-v") || !strcmp(argv[i],"--version")) fprintf(stdout,"%s\n",IGOR_VERSION);
	}

	if (just_exit) exit(0);
	if (run_builtins) install_builtins();
	if (run_rc) load_igor_rc(NULL); // first from /etc/igor-rc then ~/.igor-rc
	

	refresh_history_filename();

	for (i = 1; i < argc; i++) {
		
   /* exec a command if -c */
		if (argv[ 1 ] != NULL) {
			if (!strcmp(argv[ 1 ], "-c") == 0) {
				int k = 0;
				char *cmds = 0;

				Iprintf("Processing -c\n");

				run_interactive_shell = 0;
				Free(history_file);
				history_file = NULL; // we don't want to add script things to the history

				int i = 0;
				for (i = 2; i < argc; i++) k += strlen(argv[i]) + 2;
				cmds = (char*)calloc(k,1);

				for (i = 2; i < argc; i++) {
					strcat(cmds, argv[i]);
					if (i+1 < argc) strcat(cmds, " ");
				}
				Iprintf("About to execute [%s]\n", cmds);
				rtnv = execute_command_string(cmds);
				
				exit_value(rtnv, 0);
			}

			else if (!strcmp(argv[ 1 ], "-i") == 0) {
				int k = 0;
				char *cmds = 0;

				Iprintf("Processing -i\n");

				run_interactive_shell = 0;
				Free(history_file);
				history_file = NULL; // we don't want to add script things to the history

				int i = 0;
				for (i = 2; i < argc; i++) k += strlen(argv[i]) + 2;
				cmds = (char*)calloc(k,1);

				for (i = 2; i < argc; i++) {
					strcat(cmds, argv[i]);
					if (i+1 < argc) strcat(cmds, " ");
				}
				Iprintf("About to execute [%s]\n", cmds);
				exit_value(execute_command_string(cmds), 1); // continue if it is ok

				run_interactive_shell = 1;
			}

			else if (!strcmp(argv[ 1 ], "--") == 0) { // This is a shell command
				sexp ev;
				Iprintf("Processing --\n");

				run_interactive_shell = 0;

				Free(history_file);
				history_file = NULL; // we don't want to add script things to the history
				
				Iprintf("running [%s]\n", argv[2]);
				ev = run_source_file(argv[2]);
				if (sexp_numberp(ev)) exit(sexp_unbox_fixnum(ev));
				else if (sexp_equalp(ctx,ev,SEXP_TRUE)) exit(0);
				else exit(BUGGER);
			}
			else if (!strcmp(argv[ 1 ], "-e") == 0) { // This is a shell command
				Iprintf("Processing -e\n");

				run_interactive_shell = 0;

				Free(history_file);
				history_file = NULL; // we don't want to add script things to the history

				Iprintf("opening [%s]\n", argv[2]);
				FILE *f = fopen(argv[2],"r");
				if (f) {
					scripting(1);
					command_loop(f);
					fclose(f);
					return 0;
				}
				report_error(errno, "Cannot open file", argv[2]);

				remove_magic();

				return 1;
			}
		}
	}

	if (run_interactive_shell) {
		Iprintf("Running interactive shell\n");
		Free(history_file);

		refresh_history_filename();

		if (history_file && *history_file) read_history(history_file);

		command_loop(NULL);

		if (history_file) write_history(history_file);
	}


	remove_magic();

	return 0;
}





/*
char *sexp_get_env_var(char *str) {
	char *s = getenv(str);
	if (!s) return SEXP_FALSE;
	return sexp_c_string(ctx,s, -1);
}
	

sexp sexp_set_env_var(sexp ctx, char *str, char *val) {
	int r = setenv(str, val, 1); //overwrite, always
	if (r) return SEXP_FALSE;
	else return SEXP_TRUE;
}
	
sexp sexp_unset_env_var(sexp ctx, char *str) {
	int r = unsetenv(str);
	if (r) return SEXP_FALSE;
	else return SEXP_TRUE;
}
	
*/


/*-  The End  */

