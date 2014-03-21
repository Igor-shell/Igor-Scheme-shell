// -*- outline-regexp: "/\\*-+";  -*-

#include "chibi/eval.h"
#include <wordexp.h>
#include <readline/readline.h>
#include <readline/history.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>


#define IGOR_VERSION "(igor 0 1 \"dogfoodable\")"
#define IGOR_REPO "https://github.com/Igor-shell/Igor-Scheme-shell"
#define IGOR_HISTORY_DEFAULT "~/.igor-history"

#define USES_CHIBI

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
#include <stdarg.h>

#include <wordexp.h>

typedef struct PROCESSLIST {
	pid_t pid;  // pid of process
	pid_t pgid;
	int state;
} process_list_t;


typedef struct CMD_T {
	int errcond;
	char *cmd;
	int argc;
	char **argv;  // Each string in the argv actually carries an extra byte past the null terminator which indicates what 
	// kind of token it is, namely one ofthe following types: program, s-expression, unquoting-s-expression, splicing-s-expression, 
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
	struct CMD_T *next;
} cmd_t;


typedef sexp (*builtin_func)(cmd_t *command, char **argv, int in, int out, char *ins); // the "command" is often NULL

typedef struct BUILTIN {
	char *name;
	builtin_func func;
	struct BUILTIN *left,  *right;
} Builtin;

extern char *gets(char *);
extern sexp argv_to_list(sexp ctx, char **argv, int len);
extern sexp ctx, env, ERRCON;
extern sexp sym;
extern sexp igor_execute;
extern sexp current_input, current_output, current_error;
extern char *error_message;
extern pid_t igor_pid;
extern pid_t igor_pgid;
extern int starts_sexp(char *s);
extern int is_sexp(char *s);
extern int is_unfinished_sexp(char *s);
extern char *jump_sexp(char *s, char escape);
extern char *sexpr_depth(char *buffer, char *str);
extern char *jump_fence_c(char *cp, char *collecting, char lookingfor, char escape, int eat_quotes);
extern char *jump_fence(char *cp, char lookingfor, char escape);

extern char *evaluate_scheme_expression(int emit, char *sexp, char *instring);
extern char *exit_val_evaluate_scheme_expression(int emit, char *sexp, char *instring);
extern sexp igor_ctx() { return ctx;};
extern sexp igor_env() { return env;};
extern int report_error_i(int err, char *errormessage, const char *ctx, char *file, int line);
extern char *read_all(int fd);

extern sexp sexp_wordexp(char *sstr);
	
extern char *completed_path(char *s);

extern void delete_builtin(Builtin *node);
	
extern Builtin *insert_builtin(Builtin *tree, char *name, builtin_func func);
extern Builtin *member(char *name, Builtin *tree);
extern void close_up_shop();
extern void add_magic(char *str);
extern char *is_magic(char *s);


extern cmd_t *new_cmd_t();
extern void free_cmd(cmd_t *p);
extern void free_null_terminated_pointer_array(char **ptr);
extern sexp argv_to_list(sexp ctx, char **argv, int n);
extern sexp sexp_current_input_port(sexp ctx);
extern sexp sexp_current_output_port(sexp ctx);
extern char *guard_definitions(char *s);
		
extern void igor_set(sexp ctx, char *variable, char *value);
extern void set_prompt_continuation(char *buffer);
extern void free_prompt_continuation();
extern void refresh_history_filename();

extern char *read_line(FILE *f, char *prompt_function);

extern char *excise_string(char *s, char *p, int n);

extern char *insert_string(int n, char *s, char *p, char *insertion);
extern sexp execute_command_string(char *cmds);
extern char *backquote_system_call(char *str);
extern sexp check_exception (int emit, sexp ctx, sexp res, char *message, char *subject);
extern void write_sexp(sexp ctx, sexp bit, int err);
extern char protect_char(int maskit, char c);

extern char *jump_fence_c(char *cp, char *collecting, char lookingfor, char escape, int eat_quotes);
extern char *paren_protection(char *s, char escape, int mask);	
extern char *word_expand_string(char *s);
extern sexp sexp_list3(sexp ctx, sexp a, sexp b, sexp c);

extern sexp sexp_list4(sexp ctx, sexp a, sexp b, sexp c, sexp d);

extern char *evaluate_scheme_expression(int emit, char *Sexpr,  char *inputstring);
extern char *exit_val_evaluate_scheme_expression(int emit, char *sexpr, char *instring);
extern char *jump_sexp(char *s, char escape);	

extern char *sexpr_depth(char *buffer, char *str);

extern int starts_sexp(char *s);
extern int is_unfinished_sexp(char *s);
extern int is_sexp(char *s);
extern char **tokenise_cmdline(char *cmdline);

extern char *handle_filename(char *s);
extern char **word_expansion(char **argv, int *argc, int argix);
extern cmd_t *process_token_list(char **Argv, int in, int out,int err);
extern sexp execute_builtin(cmd_t *command, Builtin *op, char **argv, int input, int output);

extern int scm_execute_single_process(int in_the_background, char *cmd, char **argv, int input, int output, int error);

extern int process_index(pid_t pid);
extern void register_death(int ix, int exitval);
extern void record_process(pid_t pid, int bg);
extern void wait_for(int ix);

extern void move_proc_to_foreground(int ix);
extern void background_proc(int ix);
extern void background_running_proc(int ix);
extern void signalhandler(int p);

extern int c_emit_string_in_process(int in_the_background, char *cmd, char **argv, char *inputstring, int input, int output, int error);

extern char *dispatch_scheme_stuff(cmd_t *cmd);
//int c_execute_single_process(int in_the_background, char *cmd, char **argv, char *inputstring, int input, int output, int error) 
int c_execute_single_process(cmd_t *command);

extern char *dispatch_scheme_stuff(cmd_t *command);
		
extern sexp run_commands(cmd_t *cmd);
extern char *first_non_space_char(char *s);
extern char *get_commandline(FILE *f, char *buffer);
extern void run_command_prologue(char *cmds, cmd_t *C);
extern void run_command_epilogue(char *cmds, cmd_t *C, sexp rv);

extern void update_internal_c_variables();
extern sexp open_input_fd(int fd);
extern sexp open_output_fd(int fd);
	
extern void command_loop(FILE *f);
extern void catch_sigchld(int signum);
extern void catch_sigint(int signum);

extern void catch_sigterm(int signum);

extern void catch_sigquit(int signum);
extern sexp run_source_file(char *filename);

extern sexp load_igor_rc(char *urc);

extern sexp exit_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring);
extern sexp set_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring);
extern sexp repl_output(cmd_t *cmnd, char **argv, int in, int out, char *inputstring);

extern sexp unset_func(cmd_t *cmnd, char **argv, int in, int out, char *inputstring);
extern sexp exec_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring);
extern sexp cd_func(cmd_t *cmnd, char **argv, int in, int out, char *inputstring);
extern sexp source_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring);

extern sexp sexp_get_procedure(sexp ctx, sexp env, char *procname);
extern char *get_input_string(sexp ctx, sexp instr);
extern sexp make_input_string(sexp ctx, char *str);

extern sexp scm_func(cmd_t *cmd, char **argv, int in, int out, char *inputstring);

extern void install_builtins();
extern int exit_value(sexp rtnv, int but_continue);
extern int scripting(int i);

extern int igor(int argc, char **argv);

extern void preignition();
extern int initialise_interpreter();

extern void delete_file_stat(struct stat *fs);
extern struct stat *file_stat(char *filename);

extern sexp sexp_wordexp_sexp(sexp ctx, sexp sstr);
extern sexp sexp_wordexp_ffi(sexp ctx, sexp self, sexp n, sexp sstr);

extern char **wordexp_wrapper(char *str);
extern void delete_wordexp_array(char  **wa);

extern char *igor_read_line(int isfile, FILE *f, char *prompt, char *history_file);

extern char *jump_fence(char *cp, char lookingfor, char escape);
extern char *igor_get_commandline(int isfile, FILE *f, char *stdprompt, char *contprompt, char *remindprompt, char *history_file);

