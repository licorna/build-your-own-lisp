/* Interactive Prompt */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "mpc.h"

enum { LVAL_NUM, LVAL_ERR };
enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM };

/* Lisp Value struct definition. */
typedef struct {
  int type;
  long num;
  int err;
} lval;

lval lval_num(long x) {
  lval v;
  v.type = LVAL_NUM;
  v.num = x;
  return v;
}

lval lval_err(int x) {
  lval v;
  v.type = LVAL_ERR;
  v.err = x;
  return v;
}

void lval_print(lval v) {
  switch(v.type) {
  case LVAL_NUM: printf("%li", v.num); break;

  case LVAL_ERR:
    switch(v.err) {
    case LERR_DIV_ZERO: printf("Error: Division By Zero!"); break;
    case LERR_BAD_OP: printf("Error: Invalid Operation!"); break;
    case LERR_BAD_NUM: printf("Error: Invalid Number!"); break;
    }
    break;
  }
}

void lval_println(lval v) { lval_print(v); putchar('\n'); }

lval eval_op(lval x, char *op, lval y) {

  /* If we have errors already, return it. */
  if (x.type == LVAL_ERR) { return x; }
  if (y.type == LVAL_ERR) { return y; }

  if (strcmp(op, "+") == 0) { return lval_num(x.num + y.num); }
  if (strcmp(op, "-") == 0) { return lval_num(x.num - y.num); }
  if (strcmp(op, "*") == 0) { return lval_num(x.num * y.num); }
  if (strcmp(op, "^") == 0) { return lval_num(pow(x.num, y.num)); }

  /* Check we are not dividing by 0. */
  if (strcmp(op, "/") == 0) {
    if (y.num == 0) {
      return lval_err(LERR_DIV_ZERO);
    } else {
      return lval_num(x.num / y.num);
    }
  }

  /* Op: min. Returns the minimum of the list */
  if (strcmp(op, "min") == 0) {
    if (x.num < y.num) { return x; }
    return y;
  }

  return lval_err(LERR_BAD_OP);
}

lval eval(mpc_ast_t *t) {

  /* If tagged as number, return it directly. */
  if (strstr(t->tag, "number")) {
    errno = 0;
    long x = strtol(t->contents, NULL, 10);
    return errno != ERANGE? lval_num(x) : lval_err(LERR_BAD_NUM);
  }

  /* Operator is always second child */
  char *op = t->children[1]->contents;

  /* We store the third child in `x` */
  lval x = eval(t->children[2]);

  /* Iterate remaining children an combine. */
  int i = 3;
  while (strstr(t->children[i]->tag, "expr")) {
    x = eval_op(x, op, eval(t->children[i]));
    i++;
  }

  return x;
}

/* Returns amount of leaves. */
int ast_size(mpc_ast_t *t) {
  if (t->children_num == 0) { return 1; }
  if (t->children_num > 0) {
    int total = 1;
    for (int i = 0; i < t->children_num; i++) {
      total += ast_size(t->children[i]);
    }
    return total;
  }
  return 0;
}

int main(int argc, char **argv) {
  /* mpc_parser_t *Integer = mpc_new("integer"); */
  /* mpc_parser_t *Decimal = mpc_new("decimal"); */
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Operator = mpc_new("operator");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "                                                  \
             number   : /-?[0-9]+/ ;                           \
             op_unary : '^' ;                                  \
             operator : '+' | '-' | '*' | '/' | '^' | \"min\" ;\
             expr     : <number> | '(' <operator> <expr>+ ')' ;\
             lispy    : /^/ <operator> <expr>+ /$/ ;           \
            ", Number, Operator, Expr, Lispy );

  puts("Lispy version 0.0.1\n");
  puts("Press Ctrl+C to exit\n");

  while(1) {

    char *input = readline("lispy> ");

    add_history(input);

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      printf("[LOG] AST size: %d\n", ast_size(r.output));
      lval result = eval(r.output);
      lval_println(result);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
    free(input);
  }


  mpc_cleanup(4, Number, Operator, Expr, Lispy);
  return 0;
}
