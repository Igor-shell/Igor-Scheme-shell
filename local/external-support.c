// -*- outline-regexp: "/\\*-+";  -*-


#include <wordexp.h>
#include <readline/readline.h>
#include <readline/history.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
//#include "linenoise.c"


#define Free(s) {if(s) free(s); s = NULL;}

//struct stat *make_file_stat(void) {
//	struct stat *fs;
//	fs = (struct stat *)calloc(1, sizeof(struct stat));
//	if (!fs) return NULL;
//}


int sexp_fd_val(sexp ob) {
	sexp_fileno_fd(ob);

}


void delete_file_stat(struct stat *fs) {
	if (fs) free(fs);
}

struct stat *file_stat(char *filename) {
	int n;
	struct stat *fs = (struct stat *)calloc(1, sizeof(struct stat));
	if (!fs) return NULL;

	n = stat(filename, fs);
	return (n ? NULL : fs);
}


char **wordexp_wrapper(char *str) {
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


char *igor_read_line(int isfile, FILE *f, char *prompt, char *history_file) {
	char *cmd;
	static char *linebuffer = NULL;
	static int n = 0;
	static int n1 = 85, n2 = 171;
	static int k = -1;

	
	if (!f || !isfile) {

		rl_filename_quote_characters = "\"\t ()<>$\\,[]{}*&#|;'?";
		rl_completer_quote_characters = "\"'";
		rl_filename_quoting_desired = 1;

		if (prompt) cmd = readline(prompt);
		else {
			if (geteuid()) cmd = readline("$ ");
			else cmd = readline("# ");
		}
				

		if (cmd && !linebuffer && !strncmp(cmd,"#!/",3)) { // this is likely to be the hash-bang at the start of a script
			linebuffer = strdup("");
			return igor_read_line(isfile, f, prompt, history_file);
		}

		if (cmd && *cmd) {
			add_history(cmd);
			if (history_file && *history_file && *history_file != '#') append_history(1,history_file);
		}
	}
	else {
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




/* NOTE: The function below returns a pointer to just past the
	s-expression; if there is a parsing problem, it returns s */

static char *start_fence = "{[(";
static char *end_fence = "}])";
static char *continuation_str = "\\";


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




char *igor_get_commandline(int isfile, FILE *f, char *stdprompt, char *contprompt, char *remindprompt, char *history_file) {
	static char *buffer = NULL;
	char *prompt;
	char *cl;
	char *sdepth = NULL;
	int depth = 0;
	int collecting_s_exp = 0;
	int ctrld = 0;

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

		cl = igor_read_line(isfile,f, prompt, history_file);
		if (cl) cln = strlen(cl);
		
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
			return strdup(buffer);
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
				return strdup(buffer);
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
				return strdup(buffer);
			}
		}

		ctrld = 0;
	}

	if (sdepth) Free(sdepth);
	if (cl) Free(cl);
	return NULL;
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
