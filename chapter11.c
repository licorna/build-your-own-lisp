#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "mpc.h"


/* Forward declarations. */
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

enum { LVAL_NUM, LVAL_ERR, LVAL_FUN, LVAL_SYM, LVAL_SEXPR, LVAL_QEXPR };
enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM };

typedef lval*(*lbuiltin)(lenv*, lval*);

struct lval {
  int type;

  long num;
  char* err;
  char* sym;
  lbuiltin fun;

  int count;
  lval** cell;
};

struct lenv {
  int count;
  char** syms;
  lval** vals;
};

void lval_print(lval*);
char* lval_print_message(lval*);
lval* lval_eval_sexpr(lenv*, lval*);
void lval_del(lval*);
lval* lval_copy(lval*);

#define UNUSED(x) (void)(x)

#define LASSERT(args, cond, fmt, ...)               \
  if (!(cond)) {                                    \
    lval* err = lval_err(fmt, ##__VA_ARGS__);       \
    lval_del(args);                                 \
    return err;                                     \
}

char* ltype_name(int t) {
  switch(t) {
  case LVAL_FUN: return "Function";
  case LVAL_NUM: return "Number";
  case LVAL_ERR: return "Error";
  case LVAL_SYM: return "Symbol";
  case LVAL_SEXPR: return "S-Expression";
  case LVAL_QEXPR: return "Q-Expression";
  }
  return "Unknown";
}

lval* lval_err(char* fmt, ...) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;

  /* Create va list and initialize it. */
  va_list va;
  va_start(va, fmt);

  /* Allocate some space. */
  v->err = malloc(512);

  /* Print error string with maximum of 511 chars. */
  vsnprintf(v->err, 511, fmt, va);
  v->err = realloc(v->err, strlen(v->err) + 1);

  /* Cleanup va list. */
  va_end(va);

  return v;
}

lenv* lenv_new(void) {
  lenv* e = malloc(sizeof(lenv));
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

void lenv_del(lenv *e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    lval_del(e->vals[i]);
  }
  free(e->syms);
  free(e->vals);
  free(e);
}

lval* lenv_get(lenv* e, lval* k) {
  /* Iterate over the keys. */
  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) {
      /* Found the symbol, return it. */
      char *msg = lval_print_message(e->vals[i]);
      fprintf(stderr, "Found pair: %s -> %s\n", k->sym, msg);
      free(msg);
      return lval_copy(e->vals[i]);
    }
  }

  return lval_err("Unbound symbol '%s'", k->sym);
}

void lenv_put(lenv* e, lval* k, lval* v) {
  /* Iterate over all items in environment. */
  for (int i = 0; i < e->count; i++) {
    /* If found, delete in that position */
    /* and put new value. */
    if (strcmp(e->syms[i], k->sym) == 0) {
      lval_del(e->vals[i]);
      e->vals[i] = lval_copy(v);
      return;
    }
  }

  /* If non existing value... */
  e->count++;
  e->vals = realloc(e->vals, sizeof(lval*) * e->count);
  e->syms = realloc(e->syms, sizeof(char*) * e->count);

  /* Copy contents. */
  e->vals[e->count-1] = lval_copy(v);
  e->syms[e->count-1] = malloc(strlen(k->sym) + 1);
  strcpy(e->syms[e->count-1], k->sym);
}

lval* lval_eval(lenv* e, lval* v) {
  if (v->type == LVAL_SYM) {
    lval* x = lenv_get(e, v);
    lval_del(v);
    return x;
  }
  if (v->type == LVAL_SEXPR) {
    return lval_eval_sexpr(e, v);
  }
  return v;
}

lval* lval_fun(lbuiltin func) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->fun = func;
  return v;
}

lval* lval_num(long x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval* lval_sym(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lval* lval_sexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval* lval_qexpr(void) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_del(lval* v) {
  switch(v->type) {
  case LVAL_NUM: break;
  case LVAL_FUN: break;
  case LVAL_ERR: free(v->err); break;
  case LVAL_SYM: free(v->sym); break;

  case LVAL_QEXPR:
  case LVAL_SEXPR:
    /* Free memory of every element in the sexpr. */
    for (int i = 0; i < v->count; i++) {
      lval_del(v->cell[i]);
    }
    /* Memory allocated to contain the pointers */
    free(v->cell);
  }
}

lval* lval_read_num(mpc_ast_t* t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE?
    lval_num(x) : lval_err("invalid number");
}

lval* lval_add(lval* v, lval* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  v->cell[v->count - 1] = x;
  return v;
}

lval* lval_read(mpc_ast_t* t) {
  /* If Symbol or Number, return conversion to that type. */
  if (strstr(t->tag, "number")) { return lval_read_num(t); }
  if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }

  /* If root (>) or sexpr then create empty list. */
  lval* x = NULL;
  if (strcmp(t->tag, ">") == 0 ) { x = lval_sexpr(); }
  if (strstr(t->tag, "sexpr")) { x = lval_sexpr(); }
  if (strstr(t->tag, "qexpr")) { x = lval_qexpr(); }

  /* Fill this list with any valid expression contained within. */
  for (int i = 0; i < t->children_num; i++) {
    /* Continue on start or end of Sexpr and Qexpr. Why? */
    if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "}") == 0) { continue; }

    if (strcmp(t->children[i]->tag, "regex") == 0) { continue; }
    x = lval_add(x, lval_read(t->children[i]));
  }
  return x;
}

char* lval_expr_print(lval* v, char open, char close) {
  char *msg = malloc(1024);
  char *tmp = malloc(1024);

  /* Fill memory with 0s in case of an empty lval. */
  memset(tmp, 0, 1024);

  int offset = 0;
  for (int i = 0; i < v->count; i++) {
    /* Print value contained within. */
    char* current = lval_print_message(v->cell[i]);

    /* Don't print trailing space if last element. */
    if (i != (v->count-1)) {
      /* Is there a better way to add a space at the end of the string? */
      int len = strlen(current);
      current[len + 1] = '\0';
      current[len] = ' ';
    }

    /* Keep building our string. */
    memcpy(tmp + offset, current, strlen(current) + 1);
    offset = strlen(tmp);
    free(current);
  }
  snprintf(msg, 1024, "%c%s%c", open, tmp, close);
  msg = realloc(msg, strlen(msg) + 1);
  free(tmp);
  return msg;
}

char* lval_print_message(lval* v) {
  char *msg = malloc(1024);
  switch(v->type) {
  case LVAL_NUM: snprintf(msg, 1024, "%li", v->num); break;
  case LVAL_FUN: snprintf(msg, 1024, "<function>"); break;
  case LVAL_ERR: snprintf(msg, 1024, "Error: %s", v->err); break;
  case LVAL_SYM: snprintf(msg, 1024, "%s", v->sym); break;
  case LVAL_SEXPR:
    free(msg);
    return lval_expr_print(v, '(', ')');
  case LVAL_QEXPR:
    free(msg);
    return lval_expr_print(v, '{', '}');
  }

  msg = realloc(msg, strlen(msg) + 1);
  return msg;
}

void lval_print(lval* v) {
  char *msg = lval_print_message(v);
  printf("%s", msg);
  free(msg);
}

void lval_println(lval* v) { lval_print(v); putchar('\n'); }

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

lval* lval_copy(lval* v) {
  lval* x = malloc(sizeof(lval));
  x->type = v->type;

  switch(x->type) {
    /* Functions and numbers are copied directly. */
  case LVAL_FUN: x->fun = v->fun; break;
  case LVAL_NUM: x->num = v->num; break;

    /* Strings are copied allocating memory and copying every byte. */
  case LVAL_ERR:
    x->err = malloc(strlen(v->err) + 1);
    strcpy(x->err, v->err); break;

    /* Copy lists by copying each subexpression. */
  case LVAL_SEXPR:
  case LVAL_QEXPR:
    x->count = v->count;
    x->cell = malloc(sizeof(lval*) * x->count);
    for (int i = 0; i < x->count; i++) {
      x->cell[i] = lval_copy(v->cell[i]);
    }
    break;
  }

  return x;
}

lval* lval_pop(lval* v, int i) {
  /* Find item at "i". */
  lval* x = v->cell[i];

  /* Shift memory after the item at "i" over the top. */
  memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));

  /* Decrease the count of items in the list. */
  v->count--;

  /* Reallocate memory used. */
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  return x;
}

lval* lval_take(lval* v, int i) {
  lval *x = lval_pop(v, i);
  lval_del(v);
  return x;
}

lval* builtin_def(lenv* e, lval* a) {
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
          "Function 'def' passed incorrect type. Got '%s'; Expected '%s'",
          ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));

  /* First argument is symbol list. */
  lval* syms = a->cell[0];

  /* Ensure all elements of first list are symbols. */
  for (int i = 0; i < syms->count; i++) {
    LASSERT(a, syms->cell[i]->type == LVAL_SYM,
            "Function 'def' cannot define non-symbol");
  }

  /* Check correct number of symbols and values. */
  LASSERT(a, syms->count == a->count-1,
          "Function 'def' cannot define incorrect"
          "number of values to symbols.");

  /* Assign copies of values to symbols. */
  for (int i = 0; i < syms->count; i++) {
    lenv_put(e, syms->cell[i], a->cell[i+1]);
  }

  lval_del(a);
  return lval_sexpr();
}

lval* builtin_list(lenv* e, lval* a) {
  UNUSED(e);
  a->type = LVAL_QEXPR;
  return a;
}

lval* lval_join(lval* x, lval* y) {
  /* For each cell in 'y' add it to 'x'. */
  while (y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }

  lval_del(y);
  return x;
}

lval* builtin_join(lenv* e, lval* a) {
  UNUSED(e);
  for (int i = 0; i < a->count; i++) {
    LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
            "Function 'join' passed incorrect type.");
  }

  lval* x = lval_pop(a, 0);

  while (a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }

  lval_del(a);
  return x;
}

lval* builtin_eval(lenv* e, lval* a) {
  LASSERT(a, a->count == 1,
          "Function 'eval' passed too many arguments!");
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
          "Function 'eval' passed incorrect type!");

  lval* x = lval_take(a, 0);
  x->type = LVAL_SEXPR;
  return lval_eval(e, x);
}

lval* builtin_head(lenv* e, lval* a) {
  UNUSED(e);
  LASSERT(a, a->count == 1,
          "Function 'head' passed too many arguments!");
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
          "Function 'head' passed incorrect types!");
  LASSERT(a, a->cell[0]->count != 0,
          "Function 'head' passed {}!");

  lval* v = lval_take(a, 0);
  while(v->count > 1) { lval_del(lval_pop(v, 1)); }
  return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  UNUSED(e);
  LASSERT(a, a->count == 1,
          "Function 'tail' passed too many arguments!");
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
          "Function 'tail' passed incorrect types!");
  LASSERT(a, a->cell[0]->count != 0,
          "Function 'tail' passed {}!");

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_op(lenv* e, lval* a, char* op) {
  UNUSED(e);
  /* Ensure all arguments are numbers. */
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != LVAL_NUM) {
      lval_del(a);
      return lval_err("Cannot operate on non-number!");
    }
  }

  /* Pop the first element. */
  lval* x = lval_pop(a, 0);

  /* If not arguments and sub the perform unary negation. */
  if ((strcmp(op, "-") == 0) && a->count == 0) {
    x->num = -x->num;
  }

  /* While there are still elements remaining. */
  while (a->count > 0) {
    /* Pop the next element. */
    lval* y = lval_pop(a, 0);

    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division By Zero!"); break;
      }
      x->num /= y->num;
    }

    lval_del(y);
  }

  lval_del(a);
  return x;
}

lval* builtin_add(lenv* e, lval* a) {
  return builtin_op(e, a, "+");
}

lval* builtin_sub(lenv* e, lval* a) {
  return builtin_op(e, a, "-");
}

lval* builtin_mul(lenv* e, lval* a) {
  return builtin_op(e, a, "*");
}

lval* builtin_div(lenv* e, lval* a) {
  return builtin_op(e, a, "/");
}

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(func);
  lenv_put(e, k, v);
  lval_del(k); lval_del(v);
}

void lenv_add_builtins(lenv* e) {
  /* List Functions. */
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "join", builtin_join);

  /* Math Functions. */
  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);

  /* Variable Functions. */
  lenv_add_builtin(e, "def", builtin_def);
}

lval* lval_eval_sexpr(lenv* e, lval* v) {
  /* Evaluate children. */
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(e, v->cell[i]);
  }

  /* Error checking. */
  for (int i = 0; i < v->count; i++) {
    if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
  }

  /* Empty Expression. */
  if (v->count == 0) { return v; }

  /* Single Expression. */
  if (v->count == 1) { return lval_take(v, 0); }

  /* Ensure first element is Symbol. */
  lval* f = lval_pop(v, 0);
  if (f->type != LVAL_FUN) {
    lval_del(v); lval_del(f);
    return lval_err("first element is not a function");
  }

  /* Call builtin with operator. */
  lval* result = f->fun(e, v);
  lval_del(f);
  return result;
}

int main(int argc, char **argv) {
  UNUSED(argc); UNUSED(argv);

  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Symbol = mpc_new("symbol");
  mpc_parser_t *Sexpr = mpc_new("sexpr");
  mpc_parser_t *Qexpr = mpc_new("qexpr");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "                                                     \
             number   : /-?[0-9]+/ ;                              \
             symbol   : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;        \
             sexpr    : '(' <expr>* ')' ;                         \
             qexpr    : '{' <expr>* '}' ;                         \
             expr     : <number> | <symbol> | <sexpr> | <qexpr> ; \
             lispy    : /^/ <expr>* /$/ ;                         \
            ", Number, Symbol, Sexpr, Qexpr, Expr, Lispy );

  lenv* env = lenv_new();
  lenv_add_builtins(env);

  puts("Lispy version 0.0.1\n");
  puts("Press Ctrl+C to exit\n");

  while(1) {

    char *input = readline("lispy> ");

    add_history(input);

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval* x = lval_eval(env, lval_read(r.output));
      lval_println(x);
      lval_del(x);

      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
    free(input);
  }

  lenv_del(env);
  mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  return 0;
}
