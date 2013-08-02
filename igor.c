// -*- outline-regexp: "/\\*-+";  -*-
/*-  Identification and Changes  */

/*
  igor.c -- Written by Randall Gray
*/

// gcc -o igor igor.c -lreadline -lhistory


/*-  Discussion  */
/*
  Ok, the plan is thus:
  
  * a bash-like backquote works like it does in bash

  * something in unquoted and unescaped parens is evaluated as a bit of scheme code.
  if it *isn't* the first thing in the command, the result is treated in a manner similar to 
  backquotes

  * things that are syntactically "programs" but without a corresponding executable are treated as 
  scheme input:  "path" might return the value of the appropriate scheme variable or an "undefined" error.

  * an s-expression that *does* begin a command sends its output to stdout (which may then be redirected)

  * we can define "program" like things using '(program (fname arg...) body)' which will be run as though
  they were programs w.r.t. stdin, stdout and stderr, but backgrounding is Just Too Hard at the moment.

*/

#define IGOR_VERSION "(igor 0 1 \"dogfoodable\")"
#define IGOR_REPO "https://github.com/Igor-shell/Igor-Scheme-shell"


#define USES_CHIBI
//#define USES_GAMBIT
//#define USES_GUILE


//#define Cprintf(format, args...) printf(format, ##args) // execution in command-loop
//#define Iprintf(format, args...) printf(format, ##args) // execution path from the arguments supplied to igor
//#define Dprintf(format, args...) printf(format, ##args) // debugging the tokenising
//#define DPTprintf(format, args...) printf(format, ##args) // processing tokens

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

/*-  Configuration stuff  */

#ifndef __igor_c
#define __igor_c
#endif

/*-  Included files  */

#define _GNU_SOURCE

#define begin if(1)

//#include "igor.h"
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

#include <wordexp.h>

extern char *gets(char *);

#if defined(USES_GUILE)
#error Guile support not yet implemented
#include <libguile.h>
#elif defined(USES_GAMBIT)
#error Gambit-C support not yet implemented
#elif defined(USES_CHIBI)
#include <chibi/eval.h>
#endif

#define eat_white_space(p) {while (p && *p && isspace(*p)) p++;}
#define Free(A) if (A) {free(A);	A = 0;}

#define Ok 0
#define is_bad(x) (!x)

//#define NDEBUG 1

/******************************/
// Scheme environment.
/******************************/

#define ARG1(a) sexp_cons(ctx,a,SEXP_NULL)
#define ARG2(a,b) sexp_cons(ctx,a,ARG1(b))
#define ARG3(a,b,c) sexp_cons(ctx,a,ARG2(b,c))

extern sexp argv_to_list(sexp ctx, char **argv, int len);

sexp ctx, env, ERRCON = SEXP_FALSE;
sexp igor_execute;
char *error_message = NULL;

//#define ENV env    // explicitly use the global environment

#if !defined(ENV)
#define ENV NULL   // use default
#endif



char *supporting_initialisation[] = {
	"(import (scheme base))",
	"(import (chibi system))",      // user data & session stuff
	"(import (chibi process))",     // signal handling, fork, execute...
	"(import (chibi filesystem))",  // chdir, rmdir, dup2, open-pipe....
	"(import (srfi 1))",            // list wrangling

   //  "(import (srfi 6))",  // string ports
   //  "(import (srfi 22))", // scheme scripts
   //  "(import (srfi 23))", // (error ...)
   //  "(import (srfi 62))", // Comment out s-expressions  using #; 
	"(import (srfi 95))", // sorting and merging
	"(import (srfi 98))", // access to environment variables
  
   // ensure that we have filter

	"(import (local csupport))", // load the extra routines from csupport.stub/external-support.c

	"(define *igor-prompt-for-continuation-string* \"\")",
	"(define *igor-history-file*  #f)",
	"(define *igor-version*  \'" IGOR_VERSION ")",
	"(define *running-script* 0)",
	"(define *last_igor_eval* \"\")",


	// Functions
	"(define (prompt . args) \"> \")",
	"(define (continuation-prompt . args) \">       \")",
	"(define (prompter strn . args) (string-append strn \" \"))",

	"(define (*wifp* p l) (let ((pp (current-input-port)))(current-input-port p)(l)(current-input-port pp)))",
	"(define (*wotp* p l) (let ((pp (current-output-port)))(current-output-port p)(l)(current-output-port pp)))",
	"(define (*wetp* p l) (let ((pp (current-error-port)))(current-error-port p)(l)(current-error-port pp)))",
	"(define (*wps* i o e l) (let ((pi (current-input-port))(po (current-output-port))(pe (current-error-port)))(current-input-port i)(current-output-port o)(current-error-port e)(l) (current-input-port pi)(current-output-port po)(current-error-port pe)))",

	NULL};








/******************************/


int running_script = 0;
int track_execv = 0;
int repl_write = -1; // by default doesn't print things

int SEXP_OK_IN_DQUOTES = 1;
int SEXP_OK_IN_BQUOTES = 1;

int serial_number = 1;

//char start_fence = "([{";
//char end_fence = ")]}";

char *start_fence = "(";
char *end_fence = ")";

char escape = '\\';

char squote = '\'';
char dquote = '"';
char bquote = '`';

char *quotedlist = "'(";
char *heredoc = "<<";
char *andsep = "&&";
char *orsep = "||";
char *stdouterrapp = "+>>&";
char *stderrapp = ">>&";
char *stdoutapp = ">>";
char *outerrpipe = "+|&";
char *errpipe = "|&";
char *outpipe = "|";
char *stdouterredir = "+>&";
char *stderredir = ">&";
char *stdoutredir = ">";
char *stdinredir = "<";
char *makebg = "&";
char *nextsep = ";";
char *begblock = "{";
char *endblock = "}";
char *shellcmd = "$(";
char *varexpr = "${";
char *comment = "#";

char *continuation_str = "\\";

char *scmunquotesplicelst = ",@(";
char *scmunquotelst = ",(";
char *scmunquotesplice = ",@";
char *scmunquote = ",";

char *history_file = NULL;


FILE *Stdin, *Stdout, *Stderr;
int IN = 0, OUT = 1, ERR = 2;


char *evaluate_scheme_expression(char *sexp);
char *exit_val_evaluate_scheme_expression(char *sexp);

void Abort(char *msg) {
	fprintf(stderr,"Fatal error in igor: %s\n", msg);
	fflush(stderr);
	abort();
}


typedef int (*builtin_func)(char **argv, int in, int out, int err, int shuto, int shute);

typedef struct BUILTIN {
	char *name;
	builtin_func func;
	struct BUILTIN *left,  *right;
} Builtin;

Builtin *builtins = NULL;


// The tokeniser and the parse routines use the same structure; lax, I know.
typedef struct CMD_T {
	int errcond;
	char *cmd;
	int argc;
	char **argv;  // Each string in the argv actually carries an extra byte past the null terminator which indicates what 
	// kind of token it is, namely one ofthe following types: program, s-expression, unquoting-s-expression, splicing-s-expression, 
	//	function-as-program (builtin), argument, commandline-fragment (like the bit leading up to an s-expression)
	int in, out, err;
	int shuto, shute;
	int bg;
	struct CMD_T *next;
} cmd_t;


char **magic_string = NULL;
int n_magic_strings = 0;


char *completed_path(char *s) {
	char *sexpr, *t;

	if (s) {
		if (*s == '/' || !strncmp(s,"./",2) || !strncmp(s,"../",3))  return strdup(s); // it is an absolute or relative reference
		asprintf(&sexpr,"(expand-path \"%s\")", s);
		t = evaluate_scheme_expression(sexpr);
		if (!strcmp(t,"#f") && access(t,F_OK)) return NULL;
		else return t;
	}
	return NULL;
}



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

void close_up_shop() {
	int i;

	for (i = 0; i < n_magic_strings; i++) Free(magic_string[i]);
	Free(magic_string);
	Free(history_file);

	delete_builtin(builtins);
	sexp_destroy_context(ctx);
}

void add_magic(char *str) {
	int i = 0;

	/* NOTE: Order of insertion is important here ... ">>" should go in before ">", for example */

	if (!magic_string) {
		magic_string = (char **)calloc(2, sizeof(char *));
		add_magic(str);
	}
	else {
		for (i = 0; i < n_magic_strings && strcmp(magic_string[i], str); i++);

		if (i >= n_magic_strings) {
			magic_string = (char **)realloc(magic_string, (n_magic_strings+2)*sizeof(char *));
			magic_string[n_magic_strings++] = strdup(str);
			magic_string[n_magic_strings] = 0;
		}
	}
}

char *is_magic(char *s) {
	int i = 0;

	for (i = 0; i < n_magic_strings && strncmp(s, magic_string[i], strlen(magic_string[i])); i++);
	if (i < n_magic_strings) return magic_string[i];
	else return NULL;
}




cmd_t *new_cmd_t() {
	cmd_t *p = (cmd_t *)calloc(1, sizeof(cmd_t));
	p->in = 0;
	p->out = 1;
	p->err = 2;
	return p;
}

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

void free_null_terminated_pointer_array(char **ptr) {
	int i;

	for (i = 0; ptr[i]; i++) {
		if (ptr[i]) {
			Free(ptr[i]);
			ptr[i] = NULL;
		}
	}

	Free(ptr);
}

sexp argv_to_list(sexp ctx, char **argv, int n) {
	// if n < 0 it assumes that the array is null terminated, else it must have n args
	
	char **av;
	int N, i;
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

sexp sexp_current_output_port(sexp ctx) {
	return sexp_eval_string(ctx,"(current-output-port)", -1, env);
}

char *guard_definitions(char *s) {
	char *p, *r;
	if (!s) return s;

	for (p = s; *p && isspace(*p); p++);

#warning This needs to be a lot better
	if (!strncmp(p,"(define",strlen("(define"))) { 
		asprintf(&r, "(eval '%s)", p);
		return r;
	}
	else return strdup(s);
}
		
void igor_set(sexp ctx, char *variable, char *value) {
	char *p;
	char *fmt = "(set! %s  %s)";
	
	p = (char *)malloc((strlen(fmt) + strlen(variable) + strlen(value) + 2) * sizeof(char));
	sprintf(p, fmt, variable, value);
	sexp_eval_string(ctx,p,-1,env);
	setenv(variable, value, 1);
	free(p);
}

#if 0 // This does not work for some reason
void igor_define_var(sexp ctx,char *name, char *value) {
	sexp_intern(ctx, name, -1);	
	igor_set(ctx, name, value);
}

void igor_define_func(sexp ctx, char *name, char *arguments, char *body) {
	char *def = "(define %s '*uninitialised*)";
	char *fmt = "(lambda %s %s)";
	char *defn = NULL;
	char *value = NULL;

	asprintf(&defn, def, name);
	asprintf(&value,fmt,arguments,body);

	if (defn && value) {
		sexp_intern(ctx,name,-1);
		sexp_eval_string(ctx,defn,-1,env);
		igor_define_var(ctx, name, value);
	}
	Free(defn);
	Free(value);
}
#endif

void set_prompt_continuation(char *buffer) {
	igor_set(ctx, "*igor-prompt-for-continuation-string*", buffer);
}

void free_prompt_continuation() {
	//sexp_eval_string(ctx, "", -1, ENV);
	igor_set(ctx, "*igor-prompt-for-continuation-string*", "");
}

char *read_line(FILE *f, char *prompt_function) {
	char *cmd;
	static char *linebuffer = NULL;
	static int n = 0;
	static int n1 = 85, n2 = 171;
	static int k = -1;

	
	if (!f && !running_script) {
		//char *prompt = strdup("Thur? ");
		char *prompt = evaluate_scheme_expression(prompt_function);
		if (!prompt) prompt = strdup("> ");

#if 0
		if (prompt_function) {
			fprintf(stderr,"prompt function: [");
			write(1, prompt_function, strlen(prompt_function));
			fprintf(stderr,"]\nprompt:          [");
			write(1, prompt, strlen(prompt));
			fprintf(stderr,"]\n");
		}
		else {
			fprintf(stderr,"no prompt function!");
		}
#endif			

#if 1
		rl_filename_quote_characters = "\"\t ()<>$\\,[]{}*&#|;'?";
		rl_completer_quote_characters = "\"'";
		rl_filename_quoting_desired = 1;

		cmd = readline(prompt);

		if (cmd && !linebuffer && !strncmp(cmd,"#!/",3)) { // this is likely to be the hash-bang at the start of a script
			linebuffer = strdup("");
			return read_line(f, prompt_function);
		}


#else // Exclude readline from the mix
		printf("%s", prompt);
		begin {
			char *s;
			char buffer[2048];
			s = gets(buffer);
			fprintf(stdout,"buffer: %p %s\n", s, buffer);
			if (!s) cmd = NULL;
			else cmd = strdup(buffer);

			fprintf(stderr,"cmd: %s\n", cmd);
		}
#endif

		if (!f) {
			Free(history_file);			
			history_file = evaluate_scheme_expression("*igor-history-file*");
			if (!history_file || !strcmp(history_file,"#f")) {
				Free(history_file);
				Free(f);
			}
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
				int t;
				linebuffer = (char *)realloc(linebuffer, k+n2);
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
	if (p < s) abort();
	if (strlen(s) + strlen(insertion) + 1  > n) s = (char *)realloc(s, strlen(s) + strlen(insertion) + 1);

	tmp = strdup(p);
	strcpy(p, insertion);
	strcat(s, tmp);
	Free(tmp);
	return s;
}

extern int execute_command_string(char *cmds);

char *backquote_system_call(char *str) {
	char *fname = NULL;
	char *bqbuff = NULL;
	struct stat stbuf[1];


	asprintf(&fname, "/tmp/igor.%d.%d", getpid(), serial_number++);
	asprintf(&bqbuff,"%s > %s", str, fname);

	execute_command_string(bqbuff);
				
	if (stat(fname, stbuf) == Ok) {
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

sexp check_exception (sexp ctx, sexp res, char *message, char *subject) { // from  repl.c in the chibi-scheme distribution by AlexShinn@gmail.com
	
	sexp_gc_var1(err);
	sexp_gc_preserve1(ctx,err);
  
	ERRCON = res;

	if (res && sexp_exceptionp(res)) {

		err = sexp_current_error_port(ctx);
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
      //exit_failure();
		sexp_gc_release1(ctx);
  }
  return res;
}

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


/*
  Discussion:
  
  any set of bare s-exps on the command line are dealt with as though we were running a REPL
  bare scheme variables on a command line will not match a command and will return an error

  we need to be able to intern programs as functions. (of various return types...)
  Need to be able to make aliases -- the converse of making a function out of a program.  We do this already with the builtins to a degree.

  Need to be able to pipe the output of a function to programs or redirect it to a file

  $(...) is a scheme evaluation that allows wordexp expansions with the caveat that all unescaped/quoted parens are masked out.

  $(...) is probably better aliased with ,@(...) than with `...` -- that is with scheme evaluations
  rather than command evaluations

  At the lowest level, treat the environment variables and scheme variables as distinct -- if someone wants to "unify" the name-space they can.

  
*/



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
	
	r = malloc((n + i)*sizeof(char));
	*r = 0;
	for (k = 0; k < i; k++) {
		if (k && k+1 < i) strcat(r," ");
		strcat(r,arg.we_wordv[k]);
	}
	wordfree(&arg);
	return r;
}

/* This returns the output of the scheme expression */
char *evaluate_scheme_expression(char *Sexpr) {
	char *sexpr = NULL;
#if !defined(EXTERNAL_SCHEME)
#if defined(USES_GUILE)
#error Not implemented yet
#elif defined(USES_GAMBIT)
#error Not implemented yet
#elif defined(USES_CHIBI)

	begin {
		char *psexpr = NULL;
		int run_word_expand = 0;
		char *wsexpr, *rstr;
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

		sexpr = guard_definitions(((run_word_expand && psexpr) ? psexpr : Sexpr));

		asprintf(&wsexpr, "(let ((output (open-output-string)) (rslt #f))" 
			"  (set! *last_igor_eval* %s)"
			"  (display *last_igor_eval* output)"
			"  (set! rslt (get-output-string output))"
			"  (close-output-port output) rslt)", sexpr);

		result = check_exception(ctx, sexp_eval_string(ctx, wsexpr, -1, ENV), "There was an error in:", sexpr);

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
		Dprintf("[%s]\n", rstr);
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
#else
#error No scheme implementation selected
#endif


#else  // Execute with a new scheme process
	begin {
		char *prefix = NULL;
#if defined(USES_GUILE)
		char *prefixfmt = "guile %s -c ";
#elif defined(USES_GAMBIT)
		char *prefixfmt = "gambit %s -e ";
#elif defined(USES_CHIBI)
		char *prefixfmt = "chibi-scheme %s -c ";
#else
#error No scheme implementation selected
#endif
		char *scheme_exec_flags = getenv("SCHEME_EXEC_FLAGS");
		char *fname = NULL;
		char *bqbuff = NULL;
		struct stat stbuf[1];

		asprintf(&prefix, prefixfmt, scheme_exec_flags);

		abort();

		if (!scheme_exec_flags) scheme_exec_flags = "";

		asprintf(&fname, "/tmp/igor.%d.%d", getpid(), serial_number++);
		asprintf(&bqbuff,"%s %s > %s", prefix, sexpr, fname);

		execute_command_string(bqbuff);
				
		if (stat(fname, stbuf) == Ok) {
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
	}
#endif	
	return NULL;
}



/* This returns the output of the scheme expression */
char *exit_val_evaluate_scheme_expression(char *sexpr) {
#if defined(USES_GUILE)
#error Not implemented yet
#elif defined(USES_GAMBIT)
#error Not implemented yet
#elif defined(USES_CHIBI)
	begin {
		return strdup(sexp_string_data(sexp_write_to_string(ctx, sexp_eval_string(ctx, sexpr, -1, ENV))));
	}
#else
#error No scheme implementation selected
#endif
	return NULL;
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


int is_sexp(char *s) {
	char *p = s;
	for (; p && *p && isspace(*p); p++);
	if (*p == '(') return 1;
	if (*p == '[') return 2;
	if (*p == '{') return 3;
	return 0;
}


char *sexpr_depth(char *buffer, char *str) {
	// This returns a dynamically allocated string of the unclosed
	// bits, namely members of {'(', '[', '{' and '"'}.  The string should be 
	// freed by the calling function
	int n = 0;
	int in_quote = 0;
	char *p = 0;

	if (!str) abort();

	buffer = (char *) realloc(buffer, strlen(str) + 3);
	memset(buffer, 0, (strlen(str) + 3)*sizeof(char));

	for (p = str; *p;) {
		//printf("%c <==> %d:%s\n", *p, n,buffer);
		if (!strcmp(p, "#\\\"")) p += 3; // skip a scheme literal double quote
		else if (*p == '"') {
			in_quote = !in_quote;
			p++;
		}
		else if (!in_quote) {
			if (strchr(start_fence, *p)) {
				buffer[n++] = *p++;
				buffer[n] = 0; // probably superfluous
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




	/* 
		These will take the following forms:
	  
		* a bare s-expression which is just treated as a "repl" type thing
		* an expression of the form ",(s-expr)" or ",atom" which is treated like a macro thing
		* an expression of the form ",@(s-expr)" or ",@lst" which is treated like a macro thing and spliced in

		All s-expressions are evaluated in order and inserted (or not) appropriately into the string before it is passed off to 
		wordexp(3)

		See the notes under "discussion" above....
	*/


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

		if (cp && (*cp == '\n' || *cp == '\r')) cp++;

		if (!cp || !*cp) { // either there is nothing there, or we have reached the end of the cmdline
			if (i > 0) { // we are still collecting a string, so
				// add another bit to the token array
				argv = (char **)realloc(argv, (argc + 2)*sizeof(char **));
				argv[argc] = strdup(collecting);
				*collecting = 0;
				argc++;
				argv[argc] = 0;
			}
			else { // we *aren't* collecting, so just add the terminating null
				argv = (char **)realloc(argv, (argc + 1)*sizeof(char **));
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
			argv = (char **)realloc(argv, (argc + 2)*sizeof(char **));
			argv[argc++] = strdup(collecting);
			argv[argc] = 0; // for safety's sake
			cp++;
		}

		else if (*cp == escape) {
			cp++;
			collecting[i++] = *cp++;
			collecting[i] = 0;
		}

		else if (!strncmp(cp, quotedlist, strlen(quotedlist)) || !strncmp(cp, shellcmd, strlen(shellcmd))) { // this is a quoted list or "special" scheme expression
			char *tcp = jump_sexp(cp+1, escape);
			int n = collecting?strlen(collecting):1;

			if (!collecting || n+tcp-cp > CSIZE) collecting = (char *)realloc(collecting, (n+tcp-cp + CSIZE)*sizeof(char));
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
			cp = jump_fence_c(cp, collecting, '}', escape, 0); /* This refers to an explicit substitution  using a program with arguments */
			i = strlen(collecting);
		}

		else if (*cp == bquote) { /* This refers to an explicit substitution  using a program with arguments */
			cp = jump_fence_c(cp, collecting, bquote, escape, 0); /* do not eat the backquotes -- wordexp will deal with it */
			// cp = jump_fence_c(cp, collecting, bquote, escape, 1);
			i = strlen(collecting);
		}

		else if (strchr(start_fence, *cp)) { /* This is a straight s-expression  */
			// make sure we start a new argv here
			char *tcp = jump_sexp(cp, escape);
			int n = collecting?strlen(collecting):1;

			if (!collecting || n+tcp-cp > CSIZE) collecting = (char *)realloc(collecting, (n+tcp-cp + CSIZE)*sizeof(char));
			strncpy(collecting+i, cp, tcp - cp);
			collecting[i+tcp - cp] = 0;

			i = strlen(collecting);
			cp = tcp;
			
		}

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
					// finish the last token
					argv = (char **)realloc(argv, (argc + 3)*sizeof(char **));
					argv[argc++] = strdup(collecting);
				}
				else argv = (char **)realloc(argv, (argc + 2)*sizeof(char **));


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
					abort();
					cp = jump_sexp(cp,escape); 
					if (ssch == cp) {
						abort();
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


char *handle_filename(char *s) {
	wordexp_t arg;
	char *t = NULL;
	int n;

	if (*s == '(') { // Kludge!
		if (is_sexp(s)) {
			t = evaluate_scheme_expression(s);
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
			t = strdup(s);
		}
		else {
			if (arg.we_wordc <= 0) {
				wordfree(&arg);
				return NULL;
			}
			else if (arg.we_wordc > 1) {
				int i;
				fprintf(stderr,"Multiple filenames in redirection!: %s", arg.we_wordv[0]);
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

char **word_expansion(char **argv, int *argc, int argix) {
	wordexp_t arg;
	int n;
	int err = 0;

	if (!argv || argix >= *argc || !argv[argix] || !*argv[argix]) return argv;

	err = wordexp(argv[argix], &arg, 0);

	if (err) return NULL;

	n = arg.we_wordc + *argc;
	
	if (arg.we_wordc == 0) {
		int j;
		char *s = argv[argix];
		for (j = argix; j < *argc; j++) argv[j] = argv[j+1];
		*argc = *argc-1;
		Free(s); // I hope this is ok!
	}
	else if (arg.we_wordc == 1) {
		int sn = strlen(arg.we_wordv[0]);
		if (strlen(argv[argix]) < sn) argv[argix] = (char *)realloc(argv[argix], (sn+2) * sizeof(char));
		strcpy(argv[argix], arg.we_wordv[0]);
	}
	else {
		int i;
		argv = (char **)realloc(argv,sizeof(char **) * (n+1));
		for (i = *argc; i <= n; i++) argv[i] = 0;
		if (!argv) abort();

		memmove(argv + argix + 1, argv + arg.we_wordc - 1, (*argc-argix)* sizeof(char *)); // ought to move the pointers appropriately
		Free(argv[argix]);

		for (i = 0; i < arg.we_wordc; i++) {
			
			argv[i+argix] = strdup(arg.we_wordv[i]);

			//argv[i+argix] = (char *)realloc(argv[i+argix], (strlen(arg.we_wordv[i])+1) * sizeof(char));
			//strcpy(argv[i+argix], arg.we_wordv[i]);
		}

		*argc = *argc + arg.we_wordc - 1;
	}
		
	wordfree(&arg);
	return argv;
}

cmd_t *process_token_list(char **Argv, int in, int out,int err) {
	int Argc;
	int i = 0;
	cmd_t *C = new_cmd_t();
	char *fname = NULL;

	for (Argc = 0; Argv[Argc]; Argc++) {
		//char *s = Argv[Argc];
		//if (is_magic(s) || strchr("(),|&;<>{}", *s)) continue;
	}

	C->argv = (char **)calloc(Argc+1, sizeof(char **));

	C->in = in;
	C->out = out;
	C->err = err;


	for (i = 0; i < Argc; i++) {
		if (!Argv[i] || !Argv[i][0]) continue;

		if (is_magic(Argv[i])) {
			if (0) {}
			//if (strcmp("(,", *Argv[i])) { // Catch the scheme stuff early
			//}
			else if (!strncmp(Argv[i], quotedlist, strlen(quotedlist)) || !strncmp(Argv[i], shellcmd, strlen(shellcmd))) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				C->argv[C->argc++] = Argv[i];
				C->argv[C->argc] = 0;
			}
			else if (!strcmp(Argv[i], heredoc)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				fprintf(stderr,"here documents not supported yet\n");
				C->errcond = 255;
				return C;
			}
			else if (!strcmp(Argv[i], stdoutapp)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (i >= Argc) {
					fprintf(stderr,"No filename specified to append stdout to!");
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
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
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
					fprintf(stderr,"No filename specified to append stderr to!");
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
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
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
					fprintf(stderr,"No filename specified to append stdout and stderr to!");
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
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
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
					fprintf(stderr,"No filename specified to read stdin from!");
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
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
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
					fprintf(stderr,"No filename specified to write stdout to!");
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					if (!access(fname,F_OK)) unlink(fname);
					C->out = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->out < 0) {
						C->errcond = 1;
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
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
					fprintf(stderr,"No filename specified to write stderr to!");
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					if (!access(Argv[i], F_OK)) unlink(fname);
					C->err = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->err < 0) {
						C->errcond = 1;
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
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
					fprintf(stderr,"No filename specified to write stderr to!");
					C->errcond = 1;
					return C;
				}

				fname = handle_filename(Argv[i]);
				Free(Argv[i]);

				//i++;
				//DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				if (fname && *fname) {
					if (!access(fname, F_OK)) unlink(fname);
					C->out = C->err = open(fname, O_CREAT|O_WRONLY|O_TRUNC, (mode_t)(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH));
					if (C->out < 0) {
						C->errcond = 1;
						fprintf(stderr,"Unable to open file: %s (%s)", fname, strerror(errno));
					}
					free(fname);
				}
				else C->errcond = 1;
			}
			else if (!strcmp(Argv[i], outpipe)) {
				int pipefd[2]; // you read from pipefd[0] and write to pipefd[1]
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				if (i >= Argc) {
					fprintf(stderr,"Nothing specified to write stdout to!");
					C->errcond = 1;
					return C;
				}

				C->argv[C->argc] = 0;
				
				pipe(pipefd);
				C->out = pipefd[1];
				C->shuto = 1;
				C->next = process_token_list(Argv + i, pipefd[0], out, err);
				return C;
			}
			else if (!strcmp(Argv[i], errpipe)) {
				int pipefd[2]; // you read from pipefd[0] and write to pipefd[1]
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				if (i >= Argc) {
					fprintf(stderr,"Nothing specified to write stderr to!");
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
			else if (!strcmp(Argv[i], outerrpipe)) {
				int pipefd[2]; // you read from pipefd[0] and write to pipefd[1]
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				if (i >= Argc) {
					fprintf(stderr,"Nothing specified to write stdout and stderr to!");
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
			else if (!strcmp(Argv[i], begblock)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				fprintf(stderr,"No blocks yet, sorry\n");
				C->errcond = 3;
				return C;
			}
			else if (!strcmp(Argv[i], endblock)) {
				DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
				i++;
				fprintf(stderr,"Missing begin block!\n");
				C->errcond = 4;
				
				return C;
			}
		}
		else {
			char **tmp; 
			C->argv[C->argc++] = Argv[i];
			C->argv[C->argc] = 0;
			tmp = word_expansion(C->argv, &(C->argc), C->argc-1);
			if (tmp) C->argv = tmp;
		}

		// if the first thing in the list is an s-expression and the *next thing in the list is an s-expression
		if (is_sexp(C->argv[0]) && C->argc == 1 && Argv[i] && is_sexp(Argv[i])) { 
			DPTprintf("%s:%d -- processing %s as a part of command: %s\n", __FUNCTION__, __LINE__, Argv[i], (C && C->argv && *C->argv[0]) ? C->argv[0] : "(none)");
			i++; // We *do not* increment here, because i already points to the right spot
			C->argv[C->argc+1] = 0;
			C->next = process_token_list(Argv + i, in, out, err);
			return C;
		}

		if (0) {
			int j;
			if (C->argc > 0) {
				printf("command = %s", C->argv[0]);
				for (j = 1; C->argv[j]; j++) printf(" %s", C->argv[j]);
				printf("\n");
			}
		}
	}

	return C;
}


int execute_builtin(Builtin *op, char **argv, int input, int output, int error, int shuto, int shute) {
	int r;
	int in = -2, out = -3, err = -4;
	
#if 0
	char *cmd = *argv;
	char **ss;
	fprintf(stderr,"execute_builtin %s [", cmd);
	for (ss = argv; *ss; ss++) {
		if (ss == argv) fprintf(stderr,"%s", *ss);
		else  fprintf(stderr,", %s", *ss);
	}
	fprintf(stderr,"] ");
	
	fprintf(stderr,"<%d,%d> &%d", input, output, error);
	return 0;
#endif

#if 1 // This was a bad idea, I think.
	if (input != 0) {
		in = dup(0);
		if (in >= 3) {
			close(0);
			dup2(input, 0);
		}
	}

	if (output != 1) {
		out = dup(1);
		if (out >= 3) {
			close(1);
			dup2(output, 1);
		}
	}

	if (error != 2) {
		err = dup(2);
		if (err >= 3) {
			close(2);
			dup2(error, 2);
		}
	}

#endif

	r = (op->func)(argv, input, output, error, shuto, shute);

#if 1 // This was a bad idea, I think.

	if (input != 0 && in >= 0 ) {
		close(0);
		dup2(in, 0);
		close(in);
	}
		
	if (output != 1 && out >= 0 ) {
		close(1);
		dup2(out, 1);
		close(out);
	}

	if (error != 3 && err >= 0) {
		close(2);
		dup2(err, 2);
		close(err);
	}

#endif
	return r;
}


int scm_execute_single_process(int in_the_background, char *cmd, char **argv, int input, int output, int error) {
//	int i, o, e;
//	FILE *I, *O, *E;

	sexp_gc_var2(exec_stuff,result);
	sexp_gc_preserve2(ctx,exec_stuff,result);

	exec_stuff = sexp_list1(ctx,sexp_make_integer(ctx,error));
	exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,output), exec_stuff);
	exec_stuff = sexp_cons(ctx, sexp_make_integer(ctx,input), exec_stuff);
	exec_stuff = sexp_cons(ctx, argv_to_list(ctx, argv, -1), exec_stuff);
	exec_stuff = sexp_cons(ctx, sexp_c_string(ctx, *argv, -1), exec_stuff);
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




int c_execute_single_process(int in_the_background, char *cmd, char **argv, int input, int output, int error) {
	int procid = -1;

#if 0
	char **ss;
	fprintf(stderr,"C %s [", cmd);
	for (ss = argv; *ss; ss++) {
		if (ss == argv) fprintf(stderr,"%s", *ss);
		else  fprintf(stderr,", %s", *ss);
	}
	fprintf(stderr,"] ");
	
	fprintf(stderr,"<%d,%d> &%d", input, output, error);
	fprintf(stderr, " %s\n", (in_the_background ? "&" : ""));
	return 0;
#endif

	if (!cmd) return 0; // it's ok to try and run an empty process ... it just doesn't do anything

	//time(&now);
	procid = fork();

	if (procid > 0) { // This is the parent
		int cid = 0;
		int cstate;

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
		int n;
		fflush(stderr);

#define adjust_fd(which,wnum)	{if (which >= 0 && which != wnum) {	\
				if (dup2(which,wnum) < 0) {perror("Error dup2ing " #which);exit(1);}}}

		adjust_fd(input,0);
		adjust_fd(output,1);
		adjust_fd(error,2);


		if (1) {
			int i;
			for (i = 1; argv[i]; i++) {
				if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
					char *ss = evaluate_scheme_expression(argv[i]);
					if (ss) {
						free(argv[i]);
						argv[i] = ss;
					}
				}
			}
		}

		Dprintf("Child (%s) about to exec\n",cmd);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		signal(SIGTERM, SIG_DFL);
		
		begin {
			char *tcmd = completed_path(cmd);
			//char *tcmd = strdup(cmd);
			//if (tcmd) fprintf(stderr,"%s (%d)\n", tcmd, access(tcmd, X_OK));

			if (tcmd && access(tcmd, X_OK) == Ok) {
				int q;
				
				for (q = 0; track_execv && argv && argv[q]; q++) {
					if (!q) fprintf(stderr,"Executable = %s\n", tcmd);
					fprintf(stderr,"arg[%d] = %s\n", q, argv[q]);
				}

				if ((n = execv(tcmd, argv)) == -1) {
		            //time(&then);
					fprintf(stderr,"'%s' failed to run: possibly not found or bad permissions\n", cmd);
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
				if (!strcmp(tcmd,"#f"))	fprintf(stderr,"File not found: %s (%s)\n", cmd, strerror(errno));
				else fprintf(stderr,"File not found: %s (%s)\n", tcmd, strerror(errno));
				free_null_terminated_pointer_array(argv);
				Free(tcmd);

				close_up_shop();
				exit(255);
			}
		}
	}
			
	else {
		perror("Failed to fork");
//		sexp_destroy_context(ctx);
		return -1;
	}
}





int run_commands(cmd_t *cmd) {
	int n = 0;
	for (;n == 0 && cmd;cmd=cmd->next) {
		Builtin *op = NULL;
		if (!cmd->argv[0] || !cmd->argv[0][0]) continue;

		if (is_sexp(cmd->argv[0]) || (strchr("$'", cmd->argv[0][0]) && cmd->argv[0][1] == '('))  {
			op = member("scm", builtins);
			Dprintf("Scheme function: %s\n", cmd->argv[0]);
		}
		else if (is_sexp(cmd->argv[0]) || (cmd->argv[0][0] == '\'' && cmd->argv[0][1] == '('))  {
			op = member("scm", builtins);
			Dprintf("Scheme function: %s\n", cmd->argv[0]);
		}
		else {
			op = member(*cmd->argv, builtins);
			Dprintf("Builtin function: %s\n", cmd->argv[0]);
		}
		
		if (op) n = execute_builtin(op, cmd->argv, cmd->in, cmd->out, cmd->err, cmd->shuto, cmd->shute);
		else if ((n = scm_execute_single_process(cmd->bg, *cmd->argv, cmd->argv, cmd->in, cmd->out, cmd->err)) >= 0) {
			// fine;
		}
		else n = c_execute_single_process(cmd->bg, *cmd->argv, cmd->argv, cmd->in, cmd->out, cmd->err);
	}
	return n;
}

char *first_non_space_char(char *s) {
	while (s && *s && isspace(*s)) s++;
	return s;
}

char *get_commandline(FILE *f, char *buffer) {
	char *prompt;
	char *cl;
	char *sdepth = NULL;
	int depth = 0;
	int collecting_s_exp = 0;
	int ctrld = 0;

	char *stdprompt = "(prompt)";
	char *contprompt = "(prompt-for-continuation)";
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
		int cln = 0;

		cl = read_line(f, prompt);
		if (cl) cln = strlen(cl);
		
		/****** NOT WORKING ******/
		if (0) {
			char *fnsc = NULL;
			fnsc = first_non_space_char(cl);

			if (*fnsc == ';' || *fnsc == '#') {
			// treat line as a comment
				continue;
			}
		}

		if (cln > 1 && cl[cln-1] == '\\' && cl[cln-2] != '\\') {
			prompt = contprompt;
		}

		if (cl && !*buffer) {
			collecting_s_exp = is_sexp(cl);
			sdepth = sexpr_depth(sdepth, cl);
			if (sdepth) depth = strlen(sdepth);
			else depth = 0;
		}
		
		if (!cl && ctrld) {
			*buffer = 0;
			Free(buffer);
			buffer = NULL;
			if (sdepth) Free(sdepth);
			sdepth = 0;
			return NULL;
		}

		if (!cl) { // "control-d"
			if (!buffer || !*buffer) { // exit shell
				*buffer = 0;
				Free(buffer);
				buffer = NULL;
				if (sdepth) Free(sdepth);
				sdepth = 0;
				return NULL;
			}
			else { // print buffer as prompt and continue editing
				ctrld++;
				set_prompt_continuation(buffer);
				prompt = remindprompt;
				continue;
			}
		}
		else if (!collecting_s_exp && !strcmp(cl, continuation_str)) { // blank continuation line
			ctrld = 0;
			Free(cl);
			cl = 0;
			set_prompt_continuation(buffer);
			prompt = contprompt;
			continue;
		}
		else if (!collecting_s_exp && !*cl) { // early exit -- we know we don't have to adjust "buffer"
			Free(cl);
			cl = 0;
			if (sdepth) Free(sdepth);
			sdepth = 0;
			free_prompt_continuation();
			return buffer;
		}
		else if (collecting_s_exp && !*cl) { // return to the top of the loop -- we know we don't have to adjust "buffer"
			ctrld = 0;
			Free(cl);
			cl = 0;
			prompt = contprompt;
			continue;
		}
		else {
			ctrld = 0;
			buffer = (char *)realloc(buffer, strlen(buffer) + strlen(cl) + 1);
			strcat(buffer, cl);
			Free(cl);
			cl = 0;
		}

		if (!collecting_s_exp) {
			if (strcmp(buffer + strlen(buffer)-strlen(continuation_str), continuation_str)) {
				if (sdepth) Free(sdepth);
				sdepth = 0;
				if (cl) Free(cl);
				cl = 0;
				free_prompt_continuation();
				return buffer;
		      // because it doesn't end with the continuation string....
			}
			else {
				buffer[strlen(buffer)-strlen(continuation_str)] = 0;
			}
		}
		else { // we are collecting an s-expression, and we wont stop till we are done.
			sdepth = sexpr_depth(sdepth, buffer);
			depth = strlen(sdepth);
			if (depth == 0) {
				if (sdepth) Free(sdepth); 
				if (cl) Free(cl);
				sdepth = 0;
				cl = 0;
				free_prompt_continuation();
				return buffer;
			}
		}

		ctrld = 0;
	}

	if (sdepth) Free(sdepth);
	if (cl) Free(cl);
	return NULL;
}

int execute_command_string(char *cmds) {
	char **argv = 0;
	cmd_t *C = 0;

	argv = tokenise_cmdline(cmds);

	C = process_token_list(argv,0,1,2);
			
	if (C) {
		int n =  run_commands(C);
		free_cmd(C);
		Free(argv);

		return n;
	}
	else return -1;
}

void update_internal_c_variables() {
	char *s;
	s = getenv("TRACK_EXECV");
	if (s) track_execv = atoi(s);
}

	
void command_loop(FILE *f) {
	char *cmd = NULL;
	cmd_t *C = NULL;
	char **argv;
	int i;
	int line_num = 0;

	//if (f == stdin) fprintf(stderr,"** The file seems to be stdin! (%s)\n", __PRETTY_FUNCTION__);


	while((cmd = get_commandline(f,NULL))) {
		if (!line_num && !strncmp(cmd, "#!", 2)) {
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

extern int run_source_file(char *);

int load_igor_rc(char *urc) {
	static char *igoretcrc = "/etc/igor.rc";
	const int use_load = 1;
	char userrc[512];
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
			rtn = sexp_load(ctx, fname, NULL);
		}
		else {
			if (run_source_file(sexp_string_data(fname))) rtn = SEXP_FALSE;
			else rtn = SEXP_TRUE;
		}
	}
	
	*userrc = 0; // superfluous
	if (urc) strcpy(userrc, urc);
	else if (hdir) sprintf(userrc, "%s/.igor.rc", hdir);
	else *userrc = 0;

	if (*userrc && access(userrc, R_OK) == Ok) {
		fname = sexp_c_string(ctx, userrc, -1);
		if (use_load) {
			rtn = sexp_load(ctx, fname, NULL);
		}
		else {
			if (run_source_file(sexp_string_data(fname))) rtn = SEXP_FALSE;
			else rtn = SEXP_TRUE;
		}
	}
	sexp_gc_release2(ctx);

	return Ok;
}


/*-------------------------------   Builtins   -------------------------------*/


int exit_func(char **argv, int in, int out, int err, int shuto, int shute) {
	int i;
	char *message = "\nBad argument to exit: %s\n";
	
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
		exit(1);
		//return 1;
	}
	return 0;
}

int set_func(char **argv, int in, int out, int err, int shuto, int shute) {
	Cprintf("set_func\n");
	if (argv[1]) {
		char *cmd;
		int i;

		for (i = 1; argv[i]; i++) {
			if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
				char *ss = evaluate_scheme_expression(argv[i]);
				if (ss) {
					free(argv[i]);
					argv[i] = ss;
				}
			}
		}


		if (argv[2]) {
			setenv(argv[1], argv[2], 1);
			asprintf(&cmd, "(define %s \"%s\")", argv[1], argv[2]);
			if (cmd) evaluate_scheme_expression(cmd);
			Free(cmd);
		}
		else {
			setenv(argv[1], "", 1);
			asprintf(&cmd, "(define %s '())", argv[1]);
			if (cmd) evaluate_scheme_expression(cmd);
			Free(cmd);
		}
	}
	else {
		char *message = "\nbad call to builtin 'set'\n";
		if (*message) write(2, message, strlen(message));
		return 1;
	}
	return 0;
}

int repl_output(char **argv, int in, int out, int err, int shuto, int shute) {
	char *value = argv[1];
	Cprintf("repl_output\n");
	if (!value || !*value) repl_write = -1;
	else if (!strcmp(value, "none")) repl_write = -1;
	else if (!strcmp(value, "quiet")) repl_write = -1;
	else if (!strcmp(value, "stdout")) repl_write = 0;
	else if (!strcmp(value, "stderr")) repl_write = 1;

	setenv("REPL_WRITE", "-1", repl_write);
	return repl_write;
}


int unset_func(char **argv, int in, int out, int err, int shuto, int shute) {
	int i;
	char *cmd = 0;
	Cprintf("unset_func\n");
	for (i = 1; argv[i]; i++) {
		unsetenv(argv[i]);
		asprintf(&cmd, "(define %s '())", argv[i]);
		if (cmd) evaluate_scheme_expression(cmd);
		Free(cmd);
	}
	return 0;
}

int exec_func(char **argv, int in, int out, int err, int shuto, int shute) {
	Cprintf("exec_func\n");
	argv = argv+1;
	if (argv == NULL || *argv == NULL || **argv == 0)	exit(0);
	if (execvp(*argv, argv)) {
		fprintf(stderr,"Failed to execute %s (%s)\n", *argv, strerror(errno));
	}
	return -1;
}



int cd_func(char **argv, int in, int out, int err, int shuto, int shute) {
	/*** change this so it can be replaced by a scheme routine ***/
	int i;

 	Cprintf("cd_func\n");

	for (i = 1; argv[i]; i++) {
		if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
			char *ss = evaluate_scheme_expression(argv[i]);
			if (ss) {
				free(argv[i]);
				argv[i] = ss;
			}
		}
	}

	if (argv && *argv) {
#if 1
		sexp_gc_var1(rslt);
		sexp_gc_preserve1(ctx, rslt);

		if (!argv[1]) rslt = sexp_eval_string(ctx,"(cd)", -1, NULL);
		else {
			char *cd;
			asprintf(&cd,"(cd \"%s\")", argv[1]);
			rslt = sexp_eval_string(ctx,cd, -1, NULL);
		}

		if (rslt == SEXP_FALSE) {
			if (!argv[1]) printf("Unable to change to the home directory! (%s)\n", getenv("HOME"));
			else printf("Failed to change to %s\n", argv[1]);

			sexp_gc_release1(ctx);
			return -1;
		}
		
		sexp_gc_release1(ctx);
		return 0;

#else
		if (!argv[1]) {
			if (!chdir(getenv("HOME"))) return 0;
			else {
				printf("Unable to change to the home directory! (%s)\n", strerror(errno));
				return -1;
			}
		}
		else if (!chdir(argv[1])) return 0;
		else {
			printf("Failed to change to %s (%s)\n", argv[1], strerror(errno));
			return -1;
		}
#endif
	}
	else abort();
	return 0;
}

int run_source_file(char *filename) {
	FILE *f = NULL;

	if (!access(filename, R_OK)) {
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
			fprintf(stderr,"igor: Failed to open \"%s\" to execute its contents: %s\n", filename, strerror(errno));
			return 1;
		}
	}
	else {
		fprintf(stderr,"igor: Failed to open \"%s\": Either the file doesn't exist or it's not readable\n", filename);
		return 1;
	}
	return 0;
}

// Process a "sourced" file
int source_func(char **argv, int in, int out, int err, int shut_output, int shut_error) {
	int i;

	for (i = 1; argv[i]; i++) {
		if (is_sexp(argv[i]) || (argv[i][0] == '$' && argv[i][1] == '(')) {
			char *ss = evaluate_scheme_expression(argv[i]);
			if (ss) {
				free(argv[i]);
				argv[i] = ss;
			}
		}
	}


	for (i = 1; argv[i]; i++) {
		if (run_source_file(argv[i])) {
			// Failed
			fprintf(stderr,"igor: Terminating source command.\n");
			return 1;
		}
	}
	return 0;
}


int scm_func(char **argv, int in, int out, int err, int shut_output, int shut_error) {
#if 0
	char *cmd = *argv;
	char **ss;
	fprintf(stderr,"scheme %s [", cmd);
	for (ss = argv; *ss; ss++) {
		if (ss == argv) fprintf(stderr,"%s", *ss);
		else  fprintf(stderr,", %s", *ss);
	}
	fprintf(stderr,"] ");
	
	fprintf(stderr,"<%d,%d> &%d", in, out, err);
//	fprintf(stderr, " %s\n", (in_the_background ? "&" : ""));
	return 0;
#endif

	// Need to collect *all* the arguments  and process them as input....
	int k;
	char *p, *q, *s, *t;
	char *sline = NULL;
	char **Sexp = NULL;
	sexp_gc_var1(result);
	sexp_gc_preserve1(ctx, result);

	k = 0;
	
	if (!argv || !argv[0] || !*argv[0]) {
		return -1;
	}
	
	sline = strdup("");

	// Stick all the s-expressions in one spot
	for (Sexp = argv; *Sexp; Sexp++) {
		k += strlen(*Sexp);
		sline = (char *)realloc(sline, (strlen(sline) + strlen(*Sexp) + k + 3) * sizeof(char));
		
		if (Sexp != argv) strcat(sline, " "); // insert a space if it isn't the first one
		strcat(sline,*Sexp);
	}

	// over all the arguments, 
	k = 0;
	for (s = sline; *s; ) {
		t = jump_sexp(s,0);
		if (t == s && *t) {
			// Parsing problem

			fprintf(stderr,"igor: Error parsing s-expression: %s\n", s);
			Free(sline);
			return -1;
		}

		p = (char *)malloc(t - s + 2);
		strncpy(p, s, t-s);
		p[t-s] = 0;

		s = t;

		if (!strcmp(p,"()")) {
		// empty function application -- there really ought to be something good we could do...
			evaluate_scheme_expression("#t"); // Sets the ERRCON state appropriately
			q = strdup("()"); // 
		}
		else {
			q = evaluate_scheme_expression(p);
		}
		
		result = sexp_eval_string(ctx,"*last_igor_eval*",-1,NULL);

		if (ERRCON != SEXP_UNDEF && ERRCON != SEXP_VOID && !sexp_exceptionp(ERRCON)) {
			if (q && *q) {
				write(out,q,strlen(q));
				k++;
			}
			Free(q);
		}
		else if (sexp_exceptionp(ERRCON)) {
			fprintf(stderr,"Exception raised by %s\n", p);
			ERRCON = SEXP_TRUE;
		}
		Free(p);
	}

	if (result != SEXP_UNDEF && result != SEXP_VOID && !sexp_exceptionp(result)) {
		if (repl_write >= 0) {
			write_sexp(ctx, result, repl_write);
			k++;
		}
	}

	if (k > 0) write(out, "\n", 1);

	Free(sline);

	if (shut_output) close(out);
	if (shut_error) close(err);

	sexp_gc_release1(ctx);
	return 0;
}


int scm1_func(char **argv, int in, int out, int err, int shut_output, int shut_error) {
	// This is wrong, really.  It ought to be incorporated in the structure sent to run_commands
	int r = 0;

	
#if 1
	char *cmd = *argv;
	char **ss;
	fprintf(stderr,"scheme %s [", cmd);
	for (ss = argv; *ss; ss++) {
		if (ss == argv) fprintf(stderr,"%s", *ss);
		else  fprintf(stderr,", %s", *ss);
	}
	fprintf(stderr,"] ");
	
	fprintf(stderr,"<%d,%d> &%d", in, out, err);
//	fprintf(stderr, " %s\n", (in_the_background ? "&" : ""));
	return 0;
#endif

#if defined(USES_CHIBI) || defined(USES_GAMBIT) || defined(USES_GUILE)
	// Need to collect *all* the arguments  and process them as input....
	int i, n, k;
	char *line = NULL, *segment = NULL;

	sexp_gc_var2(result, val);
	sexp_gc_preserve2(ctx, result, val);

	i = n = k = 0;
	
	if (!argv || !argv[0] || !*argv[0]) {
		sexp_gc_release2(ctx);
		return -1;
	}
	
	if (!strncmp(argv[0],"()", 2)) {
		// empty function application -- there really ought to be something good we could do...
		
		sexp_gc_release2(ctx);
		return -1;
	}


	i = 0;

#if 1
	segment = evaluate_scheme_expression(argv[i]);
	if (segment) {
		k = strlen(segment);
		line = (char *)realloc(line, n + k + 3); // we do this rather than strdup so we have room for space and newline
		strcpy(line, segment);
		n = strlen(line);
		
		Free(segment);
	}
	//SEXP_UNDEF SEXP_VOID !sexp_exceptionp(ERRCON)

	if (sexp_exceptionp(ERRCON)) {
		// handle the exception

	}
	else if (ERRCON != SEXP_UNDEF && ERRCON != SEXP_VOID) {
		for (i = 1; argv[i]; i++) {
		
			segment = evaluate_scheme_expression(argv[i]);
			if (segment) {
				k = strlen(segment);

//			fprintf(stderr,"argv[%d] = %s ==> [%s]\n",i, argv[i],segment);

				if (ERRCON != SEXP_UNDEF && ERRCON != SEXP_VOID && !sexp_exceptionp(ERRCON) && k > 0) {
					strcat(line," ");

					line = (char *)realloc(line, n + k + 3);
					strcat(line, segment);
					n = strlen(line);
					Free(segment);
				}
				else if (sexp_exceptionp(ERRCON)) {
					fprintf(stderr,"Exception raised by %s\n", argv[i]);
					ERRCON = SEXP_TRUE;
				}
			}
		}

	}
//	if (in != IN) close(in); /*****/

#else
	for (i = 0; argv[i]; i++) {
		int n = line ? strlen(line) : 0;
		int k = 0;
		segment = evaluate_scheme_expression(argv[i]);
		k = segment?strlen(segment):0;

		if (ERRCON != SEXP_UNDEF && ERRCON != SEXP_VOID && !sexp_exceptionp(ERRCON) && k > 0) {
			line = (char *)realloc(line, (n + k + 3)*sizeof(char)); // the extra three leaves room for ' ', '\n' and \0
			if (n > 0) strcat(line," ");
			strcat(line, segment);
			Free(segment);
		}
		
	}
#endif				
	

/*
	result = sexp_eval_string(ctx, "(get-output-string (current-output-port))", -1, ENV);
	sexp_eval_string(ctx,"(close-output-port (current-output-port))", -1, ENV);
	

	sexp_eval_string(ctx, "(begin (current-output-port *igor-output-capture-port*) "")", -1, ENV);
	//sexp_eval_string(ctx,"(begin (close-output-port *igor-output-capture-port*) "")", -1, ENV);

	s = sexp_string_data(result);
	if (*s) write(out, s, strlen(s));
*/

	if (line && strlen(line) > 0) strcat(line, "\n"); // the "3" in n + k + 3 leaves enough room for a space and a newline
	write(out, line, strlen(line));
	Free(line);

	if (shut_output) close(out);
	if (shut_error) close(err);

#else
	char *cmdprime = 0;
	for (i = 0; !r && argv[i]; i++) {
		asprintf(&cmdprime, "guile -c '(begin (display %s)(newline))'", argv[i]);
		r = system(cmdprime);
		Free(cmdprime);
	}
#endif

	sexp_gc_release2(ctx);
	return r;
}


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

int scripting(int i) {
	char b[100];
	int l = running_script;
	running_script = i;
	sprintf(b,"(define *running-script* %d)", i);
	sexp_eval_string(ctx,b,-1,NULL);
	return l;
}


int igor(int argc, char **argv) {
	int i = 1;
	int run_interactive_shell = 1;
	int run_rc = 1;
	int run_builtins = 1;
	int just_exit = 0;

	add_magic(quotedlist);
	add_magic(heredoc);
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
	if (run_rc) load_igor_rc(NULL); // first from /etc/igor.rc then ~/.igor.rc
	
	
	for (i = 1; i < argc; i++) {
		
   /* exec a command if -c */
		if (argv[ 1 ] != NULL) {
			if (strcmp(argv[ 1 ], "-c") == 0) {
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
				i = execute_command_string(cmds);
				exit(i);
			}

			else if (strcmp(argv[ 1 ], "--") == 0) { // This is a shell command
				Iprintf("Processing --\n");

				run_interactive_shell = 0;

				Free(history_file);
				history_file = NULL; // we don't want to add script things to the history
				
				Iprintf("running [%s]\n", argv[2]);
				return run_source_file(argv[2]);
			}
			else if (strcmp(argv[ 1 ], "-e") == 0) { // This is a shell command
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
				fprintf(stderr,"Cannot open '%s': %s\n", argv[2], strerror(errno));
				return 1;
			}
		}
	}
	if (run_interactive_shell) {
		Iprintf("Running interactive shell\n");
		Free(history_file);
		history_file = evaluate_scheme_expression("*igor-history-file*");

		if (history_file && *history_file && ERRCON == SEXP_FALSE) read_history(history_file);

		command_loop(NULL);

		if (history_file) write_history(history_file);
	}


	return 0;
}

int main(int argc, char **argv) {
  int code = 0;
  char **ss;
  sexp res;

  // These are the stdin, stdout and stderr on entry
  IN = dup(0); 
  OUT = dup(1);
  ERR = dup(2);

  // ctx is global
  sexp_scheme_init();

  ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  //env = sexp_context_env(ctx);
  env = NULL;

  sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 0);

  sexp_intern(ctx,"*igor-history-file*", -1);
  sexp_intern(ctx,"*igor-input-source-port*", -1);
  sexp_intern(ctx,"*igor-output-capture-port*", -1);
  sexp_intern(ctx,"*igor-swap-input-source-port*", -1);
  sexp_intern(ctx,"*igor-swap-output-capture-port*", -1);

  for (ss = supporting_initialisation; ss && *ss; ss++) {
    res = sexp_eval_string(ctx,*ss, -1, env);
    if (sexp_exceptionp(res)) sexp_print_exception(ctx, res, SEXP_FALSE);
  }


#if defined(extra_load_file)
  begin {
	  sexp_var1(elf);

	  sexp_preserve_object(ctx,elf);
	  elf = sexp_c_string(ctx,extra_load_file,-1);
	  sexp_load(ctx,elf,NULL);
	  sexp_release_object(ctx,elf);
  }
#endif


  code = igor(argc, argv);

  close_up_shop();
  return code;
}



/*-  The End  */
