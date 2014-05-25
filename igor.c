// -*- outline-regetxp: "/\\*-+";  -*-
/*-  Identification and Changes  */

/*
  igor.c -- Written by Randall Gray
*/

// gcc -o igor igor.c -lreadline -lhistory


/*-  Discussion  */
/*
  Ok, the plan is thus:
  
  * a bash-like backquote works like it does in bash

  * something in unquoted and unescaped parens is evaluated as a bit
  of scheme code.  if it *isn't* the first thing in the command, the
  result is treated in a manner similar to backquotes

  * things that are syntactically "programs" but without a
  corresponding executable are treated as scheme input: "path" might
  return the value of the appropriate scheme variable or an
  "undefined" error.

  * an s-expression that *does* begin a command sends its output to
  * stdout (which may then be redirected)

  * we can define "program" like things using '(program (fname arg...)
  body)' which will be run as though they were programs w.r.t. stdin,
  stdout and stderr, but backgrounding is Just Too Hard at the moment.

  My initial impression concerning how we handle return values of
  scheme expressions versus output values is that as a part of a
  command, the s-expression is evaluated and its result replaces the
  s-expression (like an argument in an s-expression, or a backquoted
  command), but as a part of the pipeline, it is the output that gets
  used, and the return value is treated like a return value from a
  command (#f indicates a failure, anything else is ok).

  Pipes between scheme expressions and other things need to be handled
  idiosyncratically:
  
  The problem is that we construct a pipe between two program which
  are in separate address spaces: we can dup the input and output and
  prevent two processes trying to read/write at the same time. With
  expressions which are evaluated we have the constraint that they may
  need to modify the state of the interpreter {(set!....) and (define
  ...)}; this gives rise to the necessity of keeping it in the current
  shell.  Pipes into and out of a scheme expression then need to be
  mediated by reading from and writing to string ports which are then
  shunted appropriately.  This is a bugger of a problem.

*/

#include "chibi/eval.h"
#include <stdio.h>

extern sexp ctx, env; 

extern void preignition();
extern void initialise_interpreter();
sexp igor_ctx();
sexp igor_env();

extern int igor(int argc, char **argv);
extern void close_up_shop();

int main(int argc, char **argv) {
  int code = 0;

#if defined(TRACK_LOADING)
  printf("IGOR\n");
#endif

  preignition();

  initialise_interpreter(argc, argv);

  ctx = igor_ctx();
  env = igor_env(ctx);

  /* run shell */
  code = igor(argc, argv);

  close_up_shop();
  return code;
}



/*-  The End  */
