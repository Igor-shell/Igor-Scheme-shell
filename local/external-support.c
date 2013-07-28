// -*- outline-regexp: "/\\*-+";  -*-


#include <wordexp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
//#include "linenoise.c"


//struct stat *make_file_stat(void) {
//	struct stat *fs;
//	fs = (struct stat *)calloc(1, sizeof(struct stat));
//	if (!fs) return NULL;
//}


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
	char **arry;
	int i = wordexp(str,&w,0);
	if (!i) {
		arry = (char **)malloc((1 + w.we_wordc) * sizeof(char *));
		
		for (i = 0; i < w.we_wordc; i++) {
			arry[i] = strdup(w.we_wordv[i]);
		}
		arry[i] = NULL;
	}
	wordfree(&w);

	return arry;
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
