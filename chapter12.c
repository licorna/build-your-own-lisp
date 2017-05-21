/* -*- compile-command: "cc -std=c99 chapter12.c mpc.c -l edit -o chapter12" -*- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <editline/readline.h>
#if defined(__linux__)
#include <editline/history.h>
#endif

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

    /* Basic */
    long num;
    char* err;
    char* sym;

    /* Function */
    lbuiltin builtin;
    lenv* env;
    lval* formals;
    lval* body;

    /* Expression */
    int count;
    lval** cell;
};

struct lenv {
    lenv* par; /* Parent Environment. */

    /* Local environment. */
    int count;
    char** syms;
    lval** vals;
};

void lval_print(lval*);
char* lval_print_message(lval*);
lval* lval_eval_sexpr(lenv*, lval*);
void lval_del(lval*);
lval* lval_copy(lval*);
lval* lval_lambda(lval*, lval*);
lval* builtin_eval(lenv*, lval*);


#define UNUSED(x) (void)(x)

#define LASSERT(args, cond, fmt, ...)             \
    if (!(cond)) {                                \
        lval* err = lval_err(fmt, ##__VA_ARGS__); \
        lval_del(args);                           \
        return err;                               \
    }

#define LASSERT_NUM(func, args, c)                                      \
    LASSERT(args, args->count == c, "Function '%s' expected "           \
            "%i parameters, got %i", func, c, args->count);

#define LASSERT_TYPE(func, args, c, t)                                  \
    LASSERT(args, args->cell[c]->type == t, "Function '%s' expected ",  \
            "type %s at cell %i, got %s instead.",                      \
            func, ltype_name(t), c, ltype_name(args->cell[c]->type));

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
    e->par = NULL;
    e->count = 0;
    e->syms = NULL;
    e->vals = NULL;
    return e;
}

lenv* lenv_copy(lenv* e) {
    lenv* n = malloc(sizeof(lenv));

    n->par = e->par;
    n->count = e->count;
    n->syms = malloc(sizeof(char*) * n->count);
    n->vals = malloc(sizeof(lval*) * n->count);

    /* Copy symbols and values. Chars* and lvals*. */
    for (int i = 0; i < e->count; i++) {
        n->syms[i] = malloc(strlen(e->syms[i]) + 1);
        strcpy(n->syms[i], e->syms[i]);
        n->vals[i] = lval_copy(e->vals[i]);
    }

    return n;
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
            return lval_copy(e->vals[i]);
        }
    }

    /* Key not found in environment, check parent. */
    if (e->par) {
        return lenv_get(e->par, k);
    } else {
        return lval_err("Unbound symbol '%s'", k->sym);
    }
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

void lenv_def(lenv* e, lval* k, lval* v) {
    /* Iterate until no parent. */
    while (e->par) { e = e->par; }

    lenv_put(e, k, v);
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

lval* lval_builtin(lbuiltin func) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;
    v->builtin = func;
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
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_QEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}

void lval_del(lval* v) {
    switch(v->type) {
    case LVAL_NUM: break;
    case LVAL_FUN:
        if (!v->builtin) {
            lenv_del(v->env);
            lval_del(v->formals);
            lval_del(v->body);
        }
        break;
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
    case LVAL_FUN:
        if (v->builtin) {
            return "<builtin>";
        } else {
            snprintf(msg, 1024, "(\\ %s %s)",
                     lval_print_message(v->formals),
                     lval_print_message(v->body));
        }
        break;
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

    switch(v->type) {
    case LVAL_FUN:
        if (v->builtin) {
            x->builtin = v->builtin;
        } else {
            x->builtin = NULL;
            x->env = lenv_copy(v->env);
            x->formals = lval_copy(v->formals);
            x->body = lval_copy(v->body);
        }
        break;
    case LVAL_NUM: x->num = v->num; break;

        /* Strings are copied allocating memory and copying every byte. */
    case LVAL_ERR:
        x->err = malloc(strlen(v->err) + 1);
        strcpy(x->err, v->err); break;

    case LVAL_SYM:
        x->sym = malloc(strlen(v->sym) + 1);
        strcpy(x->sym, v->sym);
        break;

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

lval* builtin_lambda(lenv* e, lval* a) {
    UNUSED(e);
    LASSERT_NUM("\\", a, 2);

    /* Check first Q-Expression contains only Symbols. */
    for (int i = 0; i < a->cell[0]->count; i++) {
        LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
                "Cannot define non-symbol. Got %s, Expected %s.",
                ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));
    }

    /* Pop first two arguments and pass them to lval_lambda. */
    lval* formals = lval_pop(a, 0);
    lval* body = lval_pop(a, 0);
    lval_del(a);

    return lval_lambda(formals, body);
}

lval* builtin_var(lenv* e, lval* a, char* func) {
    LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

    lval* syms = a->cell[0];
    for (int i = 0; i < syms->count; i++) {
        LASSERT(a, (syms->cell[i]->type == LVAL_SYM),
                "Function '%s' cannot define non-symbol. "
                "Got %s, Expected %s.", func,
                ltype_name(syms->cell[i]->type),
                ltype_name(LVAL_SYM));
    }

    LASSERT(a, (syms->count == a->count - 1),
            "Function '%s' passed too many arguments for symbols. "
            "Got %i, Expected %i.", func, syms->count, a->count-1);

    for (int i = 0; i < syms->count; i++) {
        /* If 'def' define in globally. If 'put' define it locally. */
        if (strcmp(func, "def") == 0) {
            lenv_def(e, syms->cell[i], a->cell[i+1]);
        }

        if (strcmp(func, "=") == 0) {
            lenv_put(e, syms->cell[i], a->cell[i+1]);
        }
    }

    lval_del(a);
    return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) {
    return builtin_var(e, a, "def");
}

lval* builtin_put(lenv* e, lval* a) {
    return builtin_var(e, a, "=");
}

lval* builtin_list(lenv* e, lval* a) {
    UNUSED(e);
    a->type = LVAL_QEXPR;
    return a;
}

lval* lval_join(lval* x, lval* y) {
    /* For each cell in 'y' add it to 'x'. */
    for (int i = 0; i < y->count; i++) {
        x = lval_add(x, y->cell[i]);
    }

    free(y->cell);
    free(y);
    return x;
}

lval* lval_lambda(lval* formals, lval* body) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;

    /* Set builtin to NULL. */
    v->builtin = NULL;

    /* Build new Environment. */
    v->env = lenv_new();

    /* Set formals and body. */
    v->formals = formals;
    v->body = body;
    return v;
}

lval* lval_call(lenv* e, lval* f, lval* a) {
    /* If builtin, call it. */
    if (f->builtin) {
        return f->builtin(e, a);
    }

    /* Record Argument Counts. */
    int given = a->count;
    int total = f->formals->count;

    /* While argumentd still remain to be processed */
    while (a->count) {
        /* If we ran out of formal arguments to bind */
        if (f->formals->count == 0) {
            lval_del(a);
            return lval_err(
                "Function passed too many arguments. "
                "Got %i, Expected %i", given, total);
        }

        /* Pop the first symbol from the formals. */
        lval* sym = lval_pop(f->formals, 0);
        /* Special case to deal with '&' */
        if (strcmp(sym->sym, "&") == 0) {
            /* Ensure '&' is followed by another symbol. */
            if (f->formals->count != 1) {
                lval_del(a);
                return lval_err("Function format invalid."
                                "Symbol '&' not followed by single symbol.");
            }

            /* Next formal should be bound to remaining arguments. */
            lval *nsym = lval_pop(f->formals, 0);
            lenv_put(f->env, nsym, builtin_list(e, a));
            lval_del(sym); lval_del(nsym);
            break;
        }

        /* Pop next argument from the list. */
        lval* val = lval_pop(a, 0);

        /* Bind a copy into the function's environment. */
        lenv_put(f->env, sym, val);

        /* Delete symbol and value. */
        lval_del(sym); lval_del(val);

    }

    lval_del(a);

    /* If '&' remains in formal list bind to empty list. */
    if (f->formals->count > 0 &&
        strcmp(f->formals->cell[0]->sym, "&") == 0) {
        if (f->formals->count != 2) {
            return lval_err("Function format invalid. "
                            "Symbol '&' not followed by single symbol.");
        }

        /* Pop and delete '&' Symbol. */
        lval_del(lval_pop(f->formals, 0));

        /* Pop next symbol and create empty list. */
        lval* sym = lval_pop(f->formals, 0);
        lval* val = lval_qexpr();

        /* Bind to environment and delete. */
        lenv_put(f->env, sym, val);
        lval_del(sym); lval_del(val);
    }

    /* If all formats have been evaluated. */
    if (f->formals->count == 0) {
        /* Set environment parent to evaluation environment. */
        f->env->par = e;

        /* Evaluate and Return. */
        return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
    } else {
        /* Otherwise return partially evaluated function. */
        return lval_copy(f);
    }
}

lval* builtin_join(lenv* e, lval* a) {
    UNUSED(e);
    for (int i = 0; i < a->count; i++) {
        LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
                "Function 'join' passed incorrect type.");
    }

    lval* x = lval_pop(a, 0);

    while (a->count) {
        lval* y = lval_pop(a, 0);
        x = lval_join(x, y);
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
                x = lval_err("Division By Zero!");
                break;
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
    lval* v = lval_builtin(func);
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
    lenv_add_builtin(e, "\\",  builtin_lambda);
    lenv_add_builtin(e, "def", builtin_def);
    lenv_add_builtin(e, "=",   builtin_put);
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
        lval* err = lval_err(
            "S-Expression starts with incorrect type. "
            "Got %s, Expected %s.",
            ltype_name(f->type), ltype_name(LVAL_FUN));
        lval_del(f); lval_del(v);
        return err;

    }

    /* Call builtin with operator. */
    lval* result = lval_call(e, f, v);
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
        if (!input) { printf("\n"); return 0; }; // Ctrl-D

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
