#include <stdio.h>
#include <stdlib.h>

#define VALS_SIZE 1000000 //10^6
#define VAL_LIST_SIZE 1000000 //10^6
#define TERMS_SIZE 10000 //10^4

struct term;

struct value;

struct val_list;

struct term_array {
  size_t buff_size;
  struct term * addr;
};

struct val_array {
  size_t buff_size;
  struct value * addr;
};

struct val_list_array {
  size_t buff_size;
  struct val_list * addr;
};

//big ol' globals for memory
struct term_array mem_term;
struct val_array mem_val;
struct val_list_array mem_val_list;

enum term_tag {
  VAR,
  NUM,
  APP,
  LAM,
  PLUS,
  ITE
};

union term_arg {
  //Var of
  int db_index;
  //Num of
  int num_val;
  //App of
  struct {
    struct term *arg1;
    struct term *arg2;
  } app_args;
  //App of
  struct {
    struct term *arg1;
    struct term *arg2;
  } plus_args;
  //Lam of
  struct term *body;
  //ITE of
  struct {
    struct term *cond;
    struct term *true_branch;
    struct term *false_branch;
  } ite_args;
};

struct term {
  enum term_tag tag;
  union term_arg arg;
};

//The dumbest allocator one can imagine:
//grab the next heap of space of size `size` in the
// mem buffer, and crashes if you can't.
struct term *alloc_term() {
  if (mem_term.buff_size <= 0) {
    perror("alloc_term: out of space");
    exit(1);
  }
  struct term * out = mem_term.addr;
  mem_term.addr++;
  mem_term.buff_size--;
  return out;
}

struct term * mk_var(int db_index) {
  struct term * out = alloc_term();
  out->tag = VAR;
  out->arg.db_index = db_index;
  return out;
}

struct term * mk_num(int num_val) {
  struct term * out = alloc_term();
  out->tag = NUM;
  out->arg.num_val = num_val;
  return out;
}

struct term * mk_app(struct term *arg1, struct term *arg2) {
  struct term * out = alloc_term();
  out->tag = APP;
  out->arg.app_args.arg1 = arg1;
  out->arg.app_args.arg2 = arg2;
  return out;
}

struct term * mk_lam(struct term *body) {
  struct term * out = alloc_term();
  out->tag = LAM;
  out->arg.body = body;
  return out;
}

struct term * mk_plus(struct term *arg1, struct term *arg2) {
  struct term * out = alloc_term();
  out->tag = PLUS;
  out->arg.plus_args.arg1 = arg1;
  out->arg.plus_args.arg2 = arg2;
  return out;
}

struct term * mk_ite(struct term *arg1, struct term *arg2, struct term *arg3) {
  struct term * out = alloc_term();
  out->tag = ITE;
  out->arg.ite_args.cond = arg1;
  out->arg.ite_args.true_branch = arg2;
  out->arg.ite_args.false_branch = arg3;
  return out;
}

void pretty_term(struct term* t) {
  switch (t->tag) {
  case VAR: {
    printf("$%d", t->arg.db_index);
    break;
  }
  case NUM: {
    printf("%d", t->arg.num_val);
    break;
  }
  case APP: {
    printf("@ ");
    pretty_term(t->arg.app_args.arg1);
    printf(" ");
    pretty_term(t->arg.app_args.arg2);
    break;
  }
  case LAM: {
    printf("\\ ");
    pretty_term(t->arg.body);
    break;
  }
  case PLUS: {
    printf("+ ");
    pretty_term(t->arg.plus_args.arg1);
    printf(" ");
    pretty_term(t->arg.plus_args.arg2);
    break;
  }
  case ITE: {
    printf("? ");
    pretty_term(t->arg.ite_args.cond);
    printf(" ");
    pretty_term(t->arg.ite_args.true_branch);
    printf(" ");
    pretty_term(t->arg.ite_args.false_branch);
    break;
  }
  default:
    fprintf(stderr, "pretty_term: unhandled case");
    break;
  }
  return;
}

//global input
char *glob;

char peek(void) {
  return *glob;
}

int eof(void) {
  return peek() == '\0';
}

char pop(void) {
  if (eof()) {
    fprintf(stderr, "Unexpected end of input\n");
    exit(1);
  }
  char out = *glob;
  glob++;
  return out;
}

int is_digit(char c) {
  return '0' <= c && c <= '9';
}

int to_digit(void) {
  char c = pop();
  return (int)(c - '0');
}

void space(void) {
  char d = pop();
  if (d == ' ') {
    return;
  }
  fprintf(stderr, "space: Unexpected char %c\n", d);
  exit(1);
}

int parse_int(void) {
  int value = 0;
  while (!eof() && is_digit(peek())) {
    value *= 10;
    value += to_digit();
  }
  return value;
}

struct term *parse_num(void) {
  int i = parse_int();
  return mk_num(i);
}

struct term *parse_neg(void) {
  pop();
  int i = parse_int();
  return mk_num(-i);
}

struct term *parse_var(void) {
  pop();
  int i = parse_int();
  return mk_var(i);
}

struct term *parse_term(void);

struct term *parse_lam(void) {
  pop();
  space();
  struct term *body = parse_term();
  return mk_lam(body);
}

struct term *parse_app(void) {
  pop();
  space();
  struct term* arg1 = parse_term();
  space();
  struct term* arg2 = parse_term();
  return mk_app(arg1, arg2);
}

struct term *parse_plus(void) {
  pop();
  space();
  struct term* arg1 = parse_term();
  space();
  struct term* arg2 = parse_term();
  return mk_plus(arg1, arg2);
}

struct term *parse_ite(void) {
  pop();
  space();
  struct term* arg1 = parse_term();
  space();
  struct term* arg2 = parse_term();
  space();
  struct term* arg3 = parse_term();
  return mk_ite(arg1, arg2, arg3);
}

struct term *parse_term(void) {
  char c = peek();
  switch (c) {
  case '$': {
    return parse_var();
  }
  case '@': {
    return parse_app();
  }
  case '\\': {
    return parse_lam();
  }
  case '?': {
    return parse_ite();
  }
  case '+': {
    return parse_plus();
  }
  case '-': {
    return parse_neg();
  }
  default:
    if (is_digit(c)) {
      return parse_num();
    }
    fprintf(stderr, "parse_term: unexpected char %c\n", c);
    exit(1);
  }
  return NULL;
}

enum val_tag {
  VNUM,
  VCLOS
};

/* FIXME: this should really just be a `value **`, or even better, a
   `value *` */
struct val_list {
  struct value *hd;
  struct val_list *tl;
};

struct value {
  //We wouldn't need this if we had types!
  enum val_tag vtag;
  union {
    int vnum;
    struct {
      struct term *clos_body;
      struct val_list *clos_env;
    } clos;
  } arg;
};

struct value *alloc_val() {
  if (mem_val.buff_size <= 0) {
    perror("alloc_val: out of space");
    exit(1);
  }
  struct value * out = mem_val.addr;
  mem_val.addr++;
  mem_val.buff_size--;
  return out;
}


struct val_list *alloc_val_list() {
  if (mem_val_list.buff_size <= 0) {
    perror("alloc_val_list: out of space");
    exit(1);
  }
  struct val_list * out = mem_val_list.addr;
  mem_val_list.addr++;
  mem_val_list.buff_size--;
  return out;
}

struct value *mk_vnum(int i) {
  struct value *v = alloc_val();
  v->vtag = VNUM;
  v->arg.vnum = i;
  return v;
}

struct value *mk_vclos(struct term *clos_body, struct val_list *clos_env) {
  struct value *v = alloc_val();
  v->vtag = VCLOS;
  v->arg.clos.clos_body = clos_body;
  v->arg.clos.clos_env = clos_env;
  return v;
}

struct val_list *cons_val_list(struct value *hd, struct val_list *tl) {
  struct val_list *l = alloc_val_list();
  l->hd = hd;
  l->tl = tl;
  return l;
}

struct value *get_value(struct val_list *l, int pos) {
  struct value *out = NULL;
  while (0 <= pos) {
    if (!l) {
      fprintf(stderr, "get_value: list too small");
      exit(1);
    }
    out = l->hd;
    l = l->tl;
    pos--;
  }
  return out;
}

void pretty_val(struct value *v);

void pretty_val_list(struct val_list *vs) {
  if (!vs) {
    return;
  }
  pretty_val(vs->hd);
  printf(", ");
  pretty_val_list(vs->tl);
}

void pretty_val(struct value *v) {
  switch (v->vtag) {
  case VNUM: {
    printf("%i", v->arg.vnum);
    break;
  }
  case VCLOS: {
    printf("\\ ");
    pretty_term(v->arg.clos.clos_body);
    printf("[");
    pretty_val_list(v->arg.clos.clos_env);
    break;
  }
  default:
    break;
  }
}

struct value *eval(struct term *t, struct val_list *env) {
  switch (t->tag) {
  case VAR: {
    return get_value(env, t->arg.db_index);
  }
  case NUM: {
    return mk_vnum(t->arg.num_val);
  }
  case PLUS: {
    struct value *v1 = eval(t->arg.plus_args.arg1, env);
    struct value *v2 = eval(t->arg.plus_args.arg2, env);
    if ((v1->vtag != VNUM) || (v2->vtag != VNUM)) {
          fprintf(stderr, "eval, case PLUS: Expected int * int");
          exit(1);
    }
    return mk_vnum(v1->arg.vnum + v2->arg.vnum);
  }
  case ITE: {
    struct value *vcond = eval(t->arg.ite_args.cond, env);
    if (vcond->vtag != VNUM) {
      fprintf(stderr, "eval, case ITE: Expected int");
      exit(1);
    }
    if (vcond->arg.vnum) {
      return eval(t->arg.ite_args.true_branch, env);
    }
    return eval(t->arg.ite_args.false_branch, env);
  }
  case LAM: {
    return mk_vclos(t->arg.body, env);
  }
  case APP: {
    struct value *varg1 = eval(t->arg.app_args.arg1, env);
    if (varg1->vtag != VCLOS) {
      fprintf(stderr, "eval, case APP: expected lambda");
      exit(1);
    }
    struct value *varg2 = eval(t->arg.app_args.arg2, env);
    struct val_list *env1 = cons_val_list(varg2, varg1->arg.clos.clos_env);
    return eval(varg1->arg.clos.clos_body, env1);
  }
  default:
    fprintf(stderr, "eval: unhandled case");
    exit(1);
  }
}

//run with:
//gcc -Wall -Wpedantic main.c && ./a.out
int main(int argc, char** argv) {
  struct term *term_buff = (struct term *)malloc(sizeof(struct term) * TERMS_SIZE);
  struct value *val_buff = (struct value *)malloc(sizeof(struct value) * VALS_SIZE);
  struct val_list *val_list_buff =
    (struct val_list *)malloc(sizeof(struct val_list) * VAL_LIST_SIZE);
  if (!term_buff || !val_buff || !val_list_buff) {
    fprintf(stderr, "main: malloc failure");
    exit(1);
  }
  mem_term.buff_size = TERMS_SIZE;
  mem_term.addr = term_buff;
  mem_val.buff_size = VALS_SIZE;
  mem_val.addr = val_buff;
  mem_val_list.buff_size = VAL_LIST_SIZE;
  mem_val_list.addr = val_list_buff;

  printf("hello, world of lambda!\n");
  char *test = "@ @ @ \\ @ \\ @ $1 \\ @ @ $1 $1 $0 \\ @ $1 \\ @ @ $1 $1 $0 \\ \\ \\ ? $1 + $0 @ @ $2 + $1 -1 $0 0 1000 1000";
  glob = test;
  struct term *parsed = parse_term();
  printf("\nparsed:\n");
  pretty_term(parsed);
  printf("\nevaled:\n");
  pretty_val(eval(parsed, NULL));
  printf("\n");
  return 0;
}
