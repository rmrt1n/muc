#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define ST_BASE_SIZE 1

typedef struct Token {
    enum {
        TOKEN_ID, TOKEN_INT, TOKEN_SEMICOLON,
        TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_LBRACE, TOKEN_RBRACE,
        TOKEN_MINUS, TOKEN_TILDE, TOKEN_BANG,
        TOKEN_PLUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT,
        TOKEN_AMPAMP, TOKEN_PIPEPIPE, TOKEN_EQEQ, TOKEN_NEQ,
        TOKEN_LT, TOKEN_LTE, TOKEN_GT, TOKEN_GTE,
        TOKEN_AMP, TOKEN_PIPE, TOKEN_CARET, TOKEN_LSHIFT, TOKEN_RSHIFT,
        TOKEN_PLUSPLUS, TOKEN_MINUSMINUS,
        TOKEN_EQ, TOKEN_PLUSEQ, TOKEN_MINUSEQ,
        TOKEN_STAREQ, TOKEN_SLASHEQ, TOKEN_PERCENTEQ,
        TOKEN_AMPEQ, TOKEN_PIPEEQ, TOKEN_CARETEQ,
        TOKEN_LSHIFTEQ, TOKEN_RSHIFTEQ, TOKEN_QUESTION, TOKEN_COLON,
        TOKEN_RETURN, TOKEN_IF, TOKEN_ELSE,
        TOKEN_FOR, TOKEN_DO, TOKEN_WHILE, TOKEN_BREAK, TOKEN_CONTINUE
    } type;
    char *value;
} Token;

enum op_type {
    NEG, BITW_NOT, LOGIC_NOT, PRE_INC, PRE_DEC,
    ADD, SUB, MUL, DIV, MOD,
    LT, LTE, GT, GTE, EQ, NEQ,
    LOGIC_AND, LOGIC_OR,
    BITW_AND, BITW_OR, BITW_XOR, LSHIFT, RSHIFT,
    AS, AS_ADD, AS_SUB, AS_MUL, AS_DIV, AS_MOD,
    AS_LSHIFT, AS_RSHIFT, AS_BW_AND, AS_BW_OR, AS_BW_XOR
};

typedef struct AST {
    enum {
        AST_INT, AST_ID, AST_FUNC, AST_RET,
        AST_UNOP, AST_BINOP, AST_ASSIGN, AST_DECLARE, AST_COND,
        AST_FOR, AST_WHILE, AST_DOWHILE, AST_BREAK, AST_CONTINUE,
        AST_BLOCK, AST_NOOP
    } type;
    union {
        struct {int value;} integer;
        struct {char *value;} id;
        struct {struct AST **blk_items;} block;
        struct {
            // int ret_type;
            char *id;
            struct AST *body;
        } function;
        struct {struct AST *expr;} ret_stmt;
        struct {
            int type;
            struct AST *expr;
        } unop;
        struct {
            int type;
            struct AST *left;
            struct AST *right;
        } binop;
        struct {
            int type;
            char *id;
            struct AST *expr;
        } assign;
        struct {
            // int type;
            char *id;
            struct AST *expr;
        } declare;
        struct {
            struct AST *cond;
            struct AST *then_branch;
            struct AST *else_branch;
        } cond;
        struct {
            struct AST *start;
            struct AST *stop;
            struct AST *step;
            struct AST *body;
        } for_loop;
        struct {
            struct AST *expr;
            struct AST *body;
        } while_loop;
        struct {
            struct AST *body;
            struct AST *expr;
        } dowhile_loop;
    } node;
} AST;

// parser struct to keep track of current tokens
typedef struct Parser {
    FILE *fp;
    Token *cur_token;
} Parser;

// symbol table (implemented as dynamic array)
typedef struct VarMap {
    char *id;
    int stack_offset;
} VarMap;

typedef struct SymTable {
    int size;
    int used;
    VarMap **arr;
    struct SymTable *parent;
} SymTable;

// for saving states during code gen
// (function stack index, break & continue labels, all purpose flag)
typedef struct State {
    int *stack_index;
    int cont_label;
    int break_label;
    bool flag;
} State;

// grammar
// program -> func
// func -> "int" "main" "(" ")" block
// block -> "{" {block-item} "}"
// block-item -> stmt | declaration
// stmt -> expr-stmt
//       | block
//       | "return" expr ";"
//       | "if" "(" expr ")" stmt ["else" stmt]
//       | "for" "(" expr-stmt expr-stmt expr ")" stmt
//       | "for" "(" declaration expr-stmt expr ")" stmt
//       | "while" "(" expr ")" stmt
//       | "do" stmt "while" "(" expr ")" ";"
//       | "break" ";"
//       | "continue" ";"
// expr-stmt -> expr? ";"
// declaration -> "int" id ["=" expr] ";"
// expr -> id asop expr
//       | ternary
// ternary -> logic-or ["?" expr ":" ternary]
// logic-or -> logic-and {"||" logic-and}
// logic-and -> bitw-or {"&&" bitw-or}
// bitw-or -> bitw-xor {"|" bitw-xor}
// bitw-xor -> bitw-and {"^" bitw-and}
// bitw-and -> equality {"&" equality}
// equality -> relational {("==" | "!=") relational}
// relational -> shift {("<" | "<=" | ">" | ">=") shift}
// shift -> addition {("<<" | ">>") addition}
// addition -> multiplication {("+" | "-") multiplication}
// multiplication -> unary {("*" | "/" | "%") unary}
// unary -> unop unary
// primary -> "(" exp ")" | int | id | EMPTY
// unop -> "-" | "~" | "!" | "++" | "--"
// asop -> "=" | "+=" | "-=" | "*=" | "/=" | "%="
//       | "&=" | "|=" | "^=" | "<<=" | ">>="
// int -> [0-9]+
// id -> [a-zA-Z_][a-zA-Z0-9_]*

// helper funcs
void *dmalloc(size_t size);
void *drealloc(void *p, size_t size);
int strtoi(char *str);
void print_spaces(int n);
bool is_unop(Token *token);
bool is_asop(Token *token);
bool last_is_ret(AST *ast);
// token && lexer funcs
Token *token_init(int type, char *value);
void token_free(Token *token);
void eat_whitespace(FILE *fp);
char peek(FILE *fp);
Token *get_next_token(FILE *fp);
// ast && parser funcs
AST *ast_init(int type);
void ast_free(AST *ast);
void expect_id(Token *token, char *id);
Parser *parser_init(FILE *fp);
void parser_free(Parser *parser);
void parser_eat(Parser *parser, int token_type);
AST *parse_primary(Parser *parser);
AST *parse_unary(Parser *parser);
AST *parse_multiplication(Parser *parser);
AST *parse_addition(Parser *parser);
AST *parse_shift(Parser *parser);
AST *parse_relational(Parser *parser);
AST *parse_equality(Parser *parser);
AST *parse_bitw_and(Parser *parser);
AST *parse_bitw_xor(Parser *parser);
AST *parse_bitw_or(Parser *parser);
AST *parse_logic_and(Parser *parser);
AST *parse_logic_or(Parser *parser);
AST *parse_ternary(Parser *parser);
AST *parse_assignment(Parser *parser, AST *id);
AST *parse_expr(Parser *parser);
AST *parse_return(Parser *parser);
AST *parse_if(Parser *parser);
AST *parse_for(Parser *parser);
AST *parse_do(Parser *parser);
AST *parse_while(Parser *parser);
AST *parse_declare(Parser *parser);
AST *parse_block_item(Parser *parser);
AST *parse_block(Parser *parser);
AST *parse_expr_stmt(Parser *parser);
AST *parse_stmt(Parser *parser);
AST *parse_func(Parser *parser);
AST *parse(Parser *parser);
// debugging funcs
void debug_print_token(Token *token);
void debug_print_op(int op_type);
void debug_print_ast(AST *ast, int n);
// code gen funcs
void setup(FILE *fp);
void write_id(FILE *fp, AST *ast, SymTable *env);
void write_unop(FILE *fp, AST *ast, SymTable *env, State state);
void write_binop_operands(FILE *fp, AST *src, AST *dst, SymTable *env, State state);
void write_binop(FILE *fp, AST *ast, SymTable *env, State state);
void write_assignment(FILE *fp, AST *ast, SymTable *env, State state);
void write_declare(FILE *fp, AST *ast, SymTable *env, State state);
void write_cond(FILE *fp, AST *ast, SymTable *env, State state);
void write_block(FILE *fp, AST *ast, SymTable *env, State state);
void write_func(FILE *fp, AST *ast, bool flag);
void write_for(FILE *fp, AST *ast, SymTable *env, State state);
void write_do(FILE *fp, AST *ast, SymTable *env, State state);
void write_while(FILE *fp, AST *ast, SymTable *env, State state);
void write_ast(FILE *fp, AST *ast, SymTable *env, State state);
// symtable funcs
VarMap *varmap_init(char *id, int offset);
void varmap_free(VarMap *var);
SymTable *symtable_init(int size, SymTable *parent);
void symtable_free(SymTable *st);
void symtable_resize(SymTable *st);
void symtable_insert(SymTable *st, char *id, int offset);
bool symtable_contains(SymTable *st, char *id);
bool symtable_cur_contains(SymTable *st, char *id);
VarMap *symtable_get(SymTable *st, char *id);
VarMap *symtable_cur_get(SymTable *st, char *id);

// global symtable (temporary)
// SymTable *global_st;
// int global_stack_index = 8;
int GLOBAL_LABEL = 0;

int main(int argc, char **argv) {
    // global_st = symtable_init(1);
    if (argc > 2) {
        fputs("usage: exec.out [file name]", stderr);
        exit(1);
    }
    // lex
    FILE *fp = argc == 1 ? stdin : fopen(argv[1], "r");
    // Token *token;
    // while ((token = get_next_token(fp)) != NULL) debug_print_token(token);
    
    // parse
    Parser *parser = parser_init(fp);
    AST *ast = parse(parser);
    if (argc == 1) debug_print_ast(ast, 0);

    // debug_print_op(LOGIC_NEG);
    // asm gen
    FILE *target; //= fopen("tmp.s", "w");
    if (argc == 1) {
        target = fopen("tmp.s", "w");
    } else {
        char *filename = argv[1], *p = &filename[strlen(filename)-1];
        int n = strlen(filename);
        while (n--) {
            if (*p-- == '/') {
                filename = p + 2;
                break;
            }
        }
        filename[strlen(filename)-1] = 's';
        target = fopen(filename, "w");
    }
    setup(target);
    // int tmp = 8;
    State state;
    write_ast(target, ast, NULL, state);

    // cleanup
    fclose(target);
    fclose(fp);
    ast_free(ast);

    return 0;
}

void *dmalloc(size_t size) {
    void *p = malloc(size);
    if (p == NULL) {
        fputs("can't allocate memory", stderr);
        exit(1);
    }
    return p;
}

void *drealloc(void *p, size_t size) {
    if (size == 0) return NULL;
    void *np = realloc(p, size);
    if (np == NULL) {
        fputs("can't allocate memory", stderr);
        exit(1);
    }
    return np;
}

Token *token_init(int type, char *value) {
    Token *token = dmalloc(sizeof(Token));
    token->type = type;
    token->value = value;
    return token;
}

void token_free(Token *token) {
    free(token->value);
    free(token);
}

void eat_whitespace(FILE *fp) {
    char c;
    while ((c = getc(fp)) != EOF) {
        if (isspace(c)) continue;
        ungetc(c, fp);
        break;
    }
}

char peek(FILE *fp) {
    char c = getc(fp);
    ungetc(c, fp);
    return c;
}

Token *get_next_token(FILE *fp) {
    eat_whitespace(fp);
    char c = getc(fp);
    // get identifier
    if (isalpha(c) || c == '_') {
        int size = 1;
        char *id = dmalloc(size);
        *id = '\0';
        while (isalnum(c) || c == '_') {
            id = drealloc(id, ++size);
            // char *tmp = id;
            // sprintf(id, "%s%c", tmp, c);
            strncat(id, &c, 1);
            c = getc(fp);
        }
        ungetc(c, fp);
        id[size-1] = '\0';

        if (strcmp(id, "return") == 0) return token_init(TOKEN_RETURN, NULL);
        if (strcmp(id, "if") == 0) return token_init(TOKEN_IF, NULL);
        if (strcmp(id, "else") == 0) return token_init(TOKEN_ELSE, NULL);
        if (strcmp(id, "for") == 0) return token_init(TOKEN_FOR, NULL);
        if (strcmp(id, "do") == 0) return token_init(TOKEN_DO, NULL);
        if (strcmp(id, "while") == 0) return token_init(TOKEN_WHILE, NULL);
        if (strcmp(id, "break") == 0) return token_init(TOKEN_BREAK, NULL);
        if (strcmp(id, "continue") == 0) return token_init(TOKEN_CONTINUE, NULL);

        return token_init(TOKEN_ID, id);
    }
    // get integer literal, no neg int for now
    // if (isdigit(c) || (c == '-' && isdigit(peek(fp)))) {
    if (isdigit(c)) {
        ungetc(c, fp);
        int size = 1;
        char *num = dmalloc(size);
        *num = '\0';
        while (isdigit((c = getc(fp)))) {
            num = drealloc(num, ++size);
            strncat(num, &c, 1);
        }
        ungetc(c, fp);
        num[size-1] = '\0';
        return token_init(TOKEN_INT, num);
    }
    // get symbols
    switch (c) {
        case '(': return token_init(TOKEN_LPAREN, NULL);
        case ')': return token_init(TOKEN_RPAREN, NULL);
        case '{': return token_init(TOKEN_LBRACE, NULL);
        case '}': return token_init(TOKEN_RBRACE, NULL);
        case ';': return token_init(TOKEN_SEMICOLON, NULL);
        case '~': return token_init(TOKEN_TILDE, NULL);
        case '?': return token_init(TOKEN_QUESTION, NULL);
        case ':': return token_init(TOKEN_COLON, NULL);
        case '!':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_NEQ, NULL);
            }
            return token_init(TOKEN_BANG, NULL);
        case '+':
            if (peek(fp) == '+') {
                getc(fp);
                return token_init(TOKEN_PLUSPLUS, NULL);
            }
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_PLUSEQ, NULL);
            }
            return token_init(TOKEN_PLUS, NULL);
        case '-':
            if (peek(fp) == '-') {
                getc(fp);
                return token_init(TOKEN_MINUSMINUS, NULL);
            }
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_MINUSEQ, NULL);
            }
            return token_init(TOKEN_MINUS, NULL);
        case '*':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_STAREQ, NULL);
            }
            return token_init(TOKEN_STAR, NULL);
        case '/':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_SLASHEQ, NULL);
            }
            return token_init(TOKEN_SLASH, NULL);
        case '%':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_STAREQ, NULL);
            }
            return token_init(TOKEN_PERCENT, NULL);
        case '&':
            if (peek(fp) == '&') {
                getc(fp);
                return token_init(TOKEN_AMPAMP, NULL);
            }
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_AMPEQ, NULL);
            }
            return token_init(TOKEN_AMP, NULL);
        case '|':
            if (peek(fp) == '|') {
                getc(fp);
                return token_init(TOKEN_PIPEPIPE, NULL);
            }
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_PIPEEQ, NULL);
            }
            return token_init(TOKEN_PIPE, NULL);
        case '^':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_CARETEQ, NULL);
            }
            return token_init(TOKEN_CARET, NULL);
        case '<':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_LTE, NULL);
            }
            if (peek(fp) == '<') {
                getc(fp);
                if (peek(fp) == '=') {
                    getc(fp);
                    return token_init(TOKEN_LSHIFTEQ, NULL);
                }
                return token_init(TOKEN_LSHIFT, NULL);
            }
            return token_init(TOKEN_LT, NULL);
        case '>':
            if (peek(fp) == '=') {
                getc(fp);
                if (peek(fp) == '=') {
                    getc(fp);
                    return token_init(TOKEN_RSHIFTEQ, NULL);
                }
                return token_init(TOKEN_GTE, NULL);
            }
            if (peek(fp) == '>') {
                getc(fp);
                return token_init(TOKEN_RSHIFT, NULL);
            }
            return token_init(TOKEN_GT, NULL);
        case '=':
            if (peek(fp) == '=') {
                getc(fp);
                return token_init(TOKEN_EQEQ, NULL);
            }
            return token_init(TOKEN_EQ, NULL);
    }
    return NULL;
}

void err_unexpected_token(Token *token) {
    fprintf(stderr, "unexpected token: ");
    debug_print_token(token);
    exit(1);
}

AST *ast_init(int type) {
    AST *ast = dmalloc(sizeof(AST));
    ast->type = type;
    return ast;
}

void ast_free(AST *ast) {
    switch (ast->type) {
        case AST_INT:
            break;
        case AST_ID:
            free(ast->node.id.value);
            break;
        case AST_RET:
            ast_free(ast->node.ret_stmt.expr);
            break;
        case AST_FUNC:
            ast_free(ast->node.function.body);
            // for (; *body != NULL; body++) ast_free(*body);
            free(ast->node.function.id);
            break;
        case AST_UNOP:
            ast_free(ast->node.unop.expr);
            break;
        case AST_BINOP:
            ast_free(ast->node.binop.left);
            ast_free(ast->node.binop.right);
            break;
        case AST_ASSIGN:
            ast_free(ast->node.assign.expr);
            free(ast->node.assign.id);
            break;
        case AST_DECLARE:
            AST *expr = ast->node.declare.expr;
            if (expr != NULL) ast_free(expr);
            free(ast->node.declare.id);
            break;
        case AST_COND:
            ast_free(ast->node.cond.cond);
            ast_free(ast->node.cond.then_branch);
            ast_free(ast->node.cond.else_branch);
            break;
        case AST_BLOCK:
            AST **items = ast->node.block.blk_items;
            for (; *items != NULL; items++) ast_free(*items);
            free(ast->node.block.blk_items);
            break;
        case AST_FOR:
            ast_free(ast->node.for_loop.start);
            ast_free(ast->node.for_loop.stop);
            ast_free(ast->node.for_loop.step);
            ast_free(ast->node.for_loop.body);
            break;
        case AST_DOWHILE:
            ast_free(ast->node.dowhile_loop.expr);
            ast_free(ast->node.dowhile_loop.body);
            break;
        case AST_WHILE:
            ast_free(ast->node.dowhile_loop.expr);
            ast_free(ast->node.dowhile_loop.body);
            break;
        default:
    }
    free(ast);
}

void expect_id(Token *token, char *id) {
    if (strcmp(token->value, id) != 0) {
        err_unexpected_token(token);
    }
}

int strtoi(char *str) {
    int sign = 1, num = 0;
    if (*str == '-') {
        sign *= -1;
        str++;
    }
    while (*str != '\0') num = num * 10 + (*str++ - '0');
    num *= sign;
    return num;
}

Parser *parser_init(FILE *fp) {
    Parser *parser = dmalloc(sizeof(Parser));
    parser->fp = fp;
    parser->cur_token = get_next_token(fp);
    return parser;
}

void parser_free(Parser *parser) {
    token_free(parser->cur_token);
    free(parser);
}

void parser_eat(Parser *parser, int token_type) {
    if (parser->cur_token->type == token_type) {
        parser->cur_token = get_next_token(parser->fp);
    } else {
        err_unexpected_token(parser->cur_token);
    }
}

AST *parse_primary(Parser *parser) {
    AST *node;
    Token *token = parser->cur_token;
    switch (token->type) {
        case TOKEN_INT:
            node = ast_init(AST_INT);
            node->node.integer.value = strtoi(token->value);
            parser_eat(parser, TOKEN_INT);
            break;
        case TOKEN_ID:
            node = ast_init(AST_ID);
            node->node.id.value = strdup(token->value);
            parser_eat(parser, TOKEN_ID);
            break;
        case TOKEN_LPAREN:
            parser_eat(parser, TOKEN_LPAREN);
            node = parse_expr(parser);
            parser_eat(parser, TOKEN_RPAREN);
            break;
        default:
            node = ast_init(AST_NOOP);
            // err_unexpected_token(token);
    }
    return node;
}

bool is_unop(Token *token) {
    int type = token->type;
    return type == TOKEN_MINUS    ||
           type == TOKEN_TILDE    ||
           type == TOKEN_BANG     ||
           type == TOKEN_PLUSPLUS ||
           type == TOKEN_MINUSMINUS;
}

AST *parse_unary(Parser *parser) {
    AST *node;
    Token *token = parser->cur_token;
    if (is_unop(token)) {
        node = ast_init(AST_UNOP);
        switch (token->type) {
            case TOKEN_MINUS: node->node.unop.type = NEG; break;
            case TOKEN_TILDE: node->node.unop.type = BITW_NOT; break;
            case TOKEN_BANG: node->node.unop.type = LOGIC_NOT; break;
            case TOKEN_PLUSPLUS: node->node.unop.type = PRE_INC; break;
            case TOKEN_MINUSMINUS: node->node.unop.type = PRE_DEC; break;
            default: err_unexpected_token(token);
        }
        parser_eat(parser, token->type);
        node->node.unop.expr = parse_unary(parser);
    } else {
        node = parse_primary(parser);
    }
    return node;
}

AST *parse_multiplication(Parser *parser) {
    AST *node = parse_unary(parser); 
    Token *token = parser->cur_token;
    while (token->type == TOKEN_STAR ||
           token->type == TOKEN_SLASH ||
           token->type == TOKEN_PERCENT) {
        AST *unary = node;
        node = ast_init(AST_BINOP);
        switch (token->type) {
            case TOKEN_STAR: node->node.binop.type = MUL; break;
            case TOKEN_SLASH: node->node.binop.type = DIV; break;
            case TOKEN_PERCENT: node->node.binop.type = MOD; break;
            default: err_unexpected_token(token);
        }
        parser_eat(parser, token->type);
        node->node.binop.left = unary;
        node->node.binop.right = parse_unary(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_addition(Parser *parser) {
    AST *node = parse_multiplication(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_PLUS || token->type == TOKEN_MINUS) {
        AST *factor = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = token->type == TOKEN_PLUS ? ADD : SUB;
        parser_eat(parser, token->type);
        node->node.binop.left = factor;
        node->node.binop.right = parse_multiplication(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_shift(Parser *parser) {
    AST *node = parse_addition(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_LSHIFT || token->type == TOKEN_RSHIFT) {
        AST *addition = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = token->type == TOKEN_LSHIFT ? LSHIFT : RSHIFT;
        parser_eat(parser, token->type);
        node->node.binop.left = addition;
        node->node.binop.right = parse_addition(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_relational(Parser *parser) {
    AST *node = parse_shift(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_LT  ||
           token->type == TOKEN_LTE ||
           token->type == TOKEN_GT  ||
           token->type == TOKEN_GTE) {
        AST *shift = node;
        node = ast_init(AST_BINOP);
        switch (token->type) {
            case TOKEN_LT: node->node.binop.type = LT; break;
            case TOKEN_LTE: node->node.binop.type = LTE; break;
            case TOKEN_GT: node->node.binop.type = GT; break;
            case TOKEN_GTE: node->node.binop.type = GTE; break;
            default: err_unexpected_token(token);
        }
        parser_eat(parser, token->type);
        node->node.binop.left = shift;
        node->node.binop.right = parse_shift(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_equality(Parser *parser) {
    AST *node = parse_relational(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_EQEQ || token->type == TOKEN_NEQ) {
        AST *comparison = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = token->type == TOKEN_EQEQ ? EQ : NEQ;
        parser_eat(parser, token->type);
        node->node.binop.left = comparison;
        node->node.binop.right = parse_relational(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_bitw_and(Parser *parser) {
    AST *node = parse_equality(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_AMP) {
        AST *equality = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = BITW_AND;
        parser_eat(parser, token->type);
        node->node.binop.left = equality;
        node->node.binop.right = parse_equality(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_bitw_xor(Parser *parser) {
    AST *node = parse_bitw_and(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_CARET) {
        AST *bitw_and = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = BITW_XOR;
        parser_eat(parser, token->type);
        node->node.binop.left = bitw_and;
        node->node.binop.right = parse_bitw_and(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_bitw_or(Parser *parser) {
    AST *node = parse_bitw_xor(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_PIPE) {
        AST *bitw_xor = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = BITW_OR;
        parser_eat(parser, token->type);
        node->node.binop.left = bitw_xor;
        node->node.binop.right = parse_bitw_xor(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_logic_and(Parser *parser) {
    AST *node = parse_bitw_or(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_AMPAMP) {
        AST *equality = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = LOGIC_AND;
        parser_eat(parser, token->type);
        node->node.binop.left = equality;
        node->node.binop.right = parse_bitw_or(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_logic_or(Parser *parser) {
    AST *node = parse_logic_and(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_PIPEPIPE) {
        AST *logic_and = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = LOGIC_OR;
        parser_eat(parser, token->type);
        node->node.binop.left = logic_and;
        node->node.binop.right = parse_logic_and(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_ternary(Parser *parser) {
    AST *node = parse_logic_or(parser);
    Token *token = parser->cur_token;
    if (token->type == TOKEN_QUESTION) {
        AST *cond = node;
        node = ast_init(AST_COND);
        node->node.cond.cond = cond;
        parser_eat(parser, token->type);
        node->node.cond.then_branch = parse_expr(parser);

        token = parser->cur_token;
        if (token->type != TOKEN_COLON) {
            fprintf(stderr, "expected ':' before ");
            debug_print_token(token);
            exit(1);
        }
        parser_eat(parser, token->type);
        node->node.cond.else_branch = parse_ternary(parser);
    }
    return node;
}

bool is_asop(Token *token) {
    int type = token->type;
    return type == TOKEN_EQ ||
           type == TOKEN_PLUSEQ    || type == TOKEN_MINUSEQ  ||
           type == TOKEN_STAREQ    || type == TOKEN_SLASHEQ  ||
           type == TOKEN_PERCENTEQ || type == TOKEN_AMPEQ    ||
           type == TOKEN_PIPEEQ    || type == TOKEN_CARETEQ  ||
           type == TOKEN_LSHIFTEQ  || type == TOKEN_RSHIFTEQ;
}

AST *parse_assignment(Parser *parser, AST *id) {
    AST *node = id;
    Token *token = parser->cur_token;
    if (is_asop(token)) {
        node = ast_init(AST_ASSIGN);
        switch (token->type) {
            case TOKEN_EQ: node->node.assign.type = AS; break;
            case TOKEN_PLUSEQ: node->node.assign.type = AS_ADD; break;
            case TOKEN_MINUSEQ: node->node.assign.type = AS_SUB; break;
            case TOKEN_STAREQ: node->node.assign.type = AS_MUL; break;
            case TOKEN_SLASHEQ: node->node.assign.type = AS_DIV; break;
            case TOKEN_PERCENTEQ: node->node.assign.type = AS_MOD; break;
            case TOKEN_AMPEQ: node->node.assign.type = AS_BW_AND; break;
            case TOKEN_PIPEEQ: node->node.assign.type = AS_BW_OR; break;
            case TOKEN_CARETEQ: node->node.assign.type = AS_BW_XOR; break;
            case TOKEN_LSHIFTEQ: node->node.assign.type = AS_LSHIFT; break;
            case TOKEN_RSHIFTEQ: node->node.assign.type = AS_RSHIFT; break;
            default: err_unexpected_token(token);
        }
        parser_eat(parser, token->type);
        node->node.assign.id = strdup(id->node.id.value);
        node->node.assign.expr = parse_expr(parser);
    }
    return node;
}

AST *parse_expr(Parser *parser) {
    AST *node = parse_ternary(parser);
    if (node->type == AST_ID) return parse_assignment(parser, node);
    return node;
}

AST *parse_return(Parser *parser) {
    AST *node = ast_init(AST_RET);
    parser_eat(parser, TOKEN_RETURN);
    node->node.ret_stmt.expr = parse_expr(parser);
    parser_eat(parser, TOKEN_SEMICOLON);
    return node;
}

AST *parse_if(Parser *parser) {
    AST *node = ast_init(AST_COND);
    parser_eat(parser, TOKEN_IF);

    parser_eat(parser, TOKEN_LPAREN);
    node->node.cond.cond = parse_expr(parser);
    parser_eat(parser, TOKEN_RPAREN);

    node->node.cond.then_branch = parse_stmt(parser);

    Token *token = parser->cur_token;
    if (token->type == TOKEN_ELSE) {
        parser_eat(parser, TOKEN_ELSE);
        node->node.cond.else_branch = parse_stmt(parser);
    } else {
        node->node.cond.else_branch = ast_init(AST_NOOP);
    }

    return node;
}

AST *parse_for(Parser *parser) {
    AST *node = ast_init(AST_FOR);
    parser_eat(parser, TOKEN_FOR);
    parser_eat(parser, TOKEN_LPAREN);
    Token *token = parser->cur_token;
    if (token->type == TOKEN_ID && strcmp(token->value, "int") == 0) {
        node->node.for_loop.start = parse_declare(parser);
    } else {
        node->node.for_loop.start = parse_expr_stmt(parser);
    }
    node->node.for_loop.stop = parse_expr_stmt(parser);
    node->node.for_loop.step = parse_expr(parser);
    parser_eat(parser, TOKEN_RPAREN);

    node->node.for_loop.body = parse_stmt(parser);
    return node;
}

AST *parse_do(Parser *parser) {
    AST *node = ast_init(AST_DOWHILE);
    parser_eat(parser, TOKEN_DO);
    node->node.dowhile_loop.body = parse_stmt(parser);
    parser_eat(parser, TOKEN_WHILE);
    parser_eat(parser, TOKEN_LPAREN);
    node->node.dowhile_loop.expr = parse_expr(parser);
    parser_eat(parser, TOKEN_RPAREN);
    return node;
}

AST *parse_while(Parser *parser) {
    AST *node = ast_init(AST_WHILE);
    parser_eat(parser, TOKEN_WHILE);
    parser_eat(parser, TOKEN_LPAREN);
    node->node.while_loop.expr = parse_expr(parser);
    parser_eat(parser, TOKEN_RPAREN);
    node->node.while_loop.body = parse_stmt(parser);
    return node;
}

AST *parse_declare(Parser *parser) {
    AST *node = ast_init(AST_DECLARE);
    // eat type specifier
    parser_eat(parser, TOKEN_ID);

    Token *id = parser->cur_token;
    node->node.declare.id = strdup(id->value);
    parser_eat(parser, TOKEN_ID);

    Token *token = parser->cur_token;
    if (token->type == TOKEN_EQ) {
        parser_eat(parser, TOKEN_EQ);
        node->node.declare.expr = parse_expr(parser);
    } else {
        node->node.declare.expr = ast_init(AST_NOOP);
    }

    parser_eat(parser, TOKEN_SEMICOLON);
    return node;
}

AST *parse_block(Parser *parser) {
    AST *node = ast_init(AST_BLOCK);
    parser_eat(parser, TOKEN_LBRACE);
    
    AST **blk_items = dmalloc(sizeof(AST *));
    int size = 1;
    Token *token = parser->cur_token;
    while (token->type != TOKEN_RBRACE) {
        AST *item = parse_block_item(parser);
        blk_items = drealloc(blk_items, (size + 1) * sizeof(AST *));
        blk_items[size++ - 1] = item;
        token = parser->cur_token;
    }
    blk_items[size-1] = NULL;
    parser_eat(parser, TOKEN_RBRACE);
    node->node.block.blk_items = blk_items;

    return node;
}

AST *parse_block_item(Parser *parser) {
    AST *node;
    Token *token = parser->cur_token;
    if (token->type == TOKEN_ID && strcmp(token->value, "int") == 0) {
        node = parse_declare(parser);
    } else {
        node = parse_stmt(parser);
    }
    return node;
}

AST *parse_expr_stmt(Parser *parser) {
    AST *node = parse_expr(parser);
    parser_eat(parser, TOKEN_SEMICOLON);
    return node;
}

AST *parse_stmt(Parser *parser) {
    AST *node;
    Token *token = parser->cur_token;
    switch (token->type) {
        case TOKEN_RETURN: node = parse_return(parser); break;
        case TOKEN_IF: node = parse_if(parser); break;
        case TOKEN_FOR: node = parse_for(parser); break;
        case TOKEN_DO: node = parse_do(parser); break;
        case TOKEN_WHILE: node = parse_while(parser); break;
        case TOKEN_BREAK:
            node = ast_init(AST_BREAK);
            parser_eat(parser, TOKEN_BREAK);
            parser_eat(parser, TOKEN_SEMICOLON);
            break;
        case TOKEN_CONTINUE:
            node = ast_init(AST_CONTINUE);
            parser_eat(parser, TOKEN_CONTINUE);
            parser_eat(parser, TOKEN_SEMICOLON);
            break;
        case TOKEN_LBRACE: node = parse_block(parser); break;
        default: node = parse_expr_stmt(parser);
    }
    return node;
}

AST *parse_func(Parser *parser) {
    // TODO: multiple statements allowed in func
    AST *node = ast_init(AST_FUNC);
    // parse func ret type
    expect_id(parser->cur_token, "int");
    parser_eat(parser, TOKEN_ID);
    // parse func name, for now only main is supported
    expect_id(parser->cur_token, "main");
    node->node.function.id = strdup(parser->cur_token->value);
    parser_eat(parser, TOKEN_ID);
    // parse func params
    parser_eat(parser, TOKEN_LPAREN);
    parser_eat(parser, TOKEN_RPAREN);

    // parse func body
    node->node.function.body = parse_block(parser);

    return node;
}

AST *parse(Parser *parser) {
    return parse_func(parser);
}

void print_spaces(int n) {
    while (n--) printf(" ");
}

void debug_print_token(Token *token) {
    int type = token->type;
    switch (type) {
        case TOKEN_INT: printf("TOKEN_INT: %s\n", token->value); break;
        case TOKEN_ID: printf("TOKEN_ID: %s\n", token->value); break;
        case TOKEN_LPAREN: puts("TOKEN_LPAREN: '('"); break;
        case TOKEN_RPAREN: puts("TOKEN_RPAREN: ')'"); break;
        case TOKEN_LBRACE: puts("TOKEN_LBRACE: '{'"); break;
        case TOKEN_RBRACE: puts("TOKEN_RBRACE: '}'"); break;
        case TOKEN_SEMICOLON: puts("TOKEN_SEMICOLON: ';'"); break;
        case TOKEN_MINUS: puts("TOKEN_MINUS: '-'"); break;
        case TOKEN_TILDE: puts("TOKEN_TILDE: '~'"); break;
        case TOKEN_BANG: puts("TOKEN_BANG: '!'"); break;
        case TOKEN_PLUS: puts("TOKEN_PLUS: '+'"); break;
        case TOKEN_STAR: puts("TOKEN_STAR: '*'"); break;
        case TOKEN_SLASH: puts("TOKEN_SLASH: '/'"); break;
        case TOKEN_PERCENT: puts("TOKEN_PERCENT: '%'"); break;
        case TOKEN_LT: puts("TOKEN_LT '<'"); break;
        case TOKEN_LTE: puts("TOKEN_LTE '<='"); break;
        case TOKEN_GT: puts("TOKEN_GT '>'"); break;
        case TOKEN_GTE: puts("TOKEN_GTE '>='"); break;
        case TOKEN_EQEQ: puts("TOKEN_EQEQ '=='"); break;
        case TOKEN_NEQ: puts("TOKEN_NEQ '!='"); break;
        case TOKEN_AMPAMP: puts("TOKEN_AMPAMP '&&'"); break;
        case TOKEN_PIPEPIPE: puts("TOKEN_PIPEPIPE '||'"); break;
        case TOKEN_AMP: puts("TOKEN_AMP '&'"); break;
        case TOKEN_PIPE: puts("TOKEN_PIPE '|'"); break;
        case TOKEN_CARET: puts("TOKEN_CARET '^'"); break;
        case TOKEN_LSHIFT: puts("TOKEN_LSHIFT '<<'"); break;
        case TOKEN_RSHIFT: puts("TOKEN_RSHIFT '>>'"); break;
        case TOKEN_EQ: puts("TOKEN_EQ '='"); break;
        case TOKEN_RETURN: puts("TOKEN_RETURN 'return'"); break;
        case TOKEN_PLUSPLUS: puts("TOKEN_PLUSPLUS '++'"); break;
        case TOKEN_MINUSMINUS: puts("TOKEN_MINUSMINUS '--'"); break;
        case TOKEN_PLUSEQ: puts("TOKEN_PLUSEQ '+='"); break;
        case TOKEN_MINUSEQ: puts("TOKEN_MINUSEQ '-='"); break;
        case TOKEN_STAREQ: puts("TOKEN_STAREQ '*='"); break;
        case TOKEN_SLASHEQ: puts("TOKEN_SLASHEQ '/='"); break;
        case TOKEN_PERCENTEQ: puts("TOKEN_PERCENTEQ '%='"); break;
        case TOKEN_AMPEQ: puts("TOKEN_AMPEQ '&='"); break;
        case TOKEN_PIPEEQ: puts("TOKEN_PIPEEQ '|='"); break;
        case TOKEN_CARETEQ: puts("TOKEN_CARETEQ '^='"); break;
        case TOKEN_LSHIFTEQ: puts("TOKEN_LSHIFTEQ '<<='"); break;
        case TOKEN_RSHIFTEQ: puts("TOKEN_RSHIFTEQ '>>='"); break;
        case TOKEN_QUESTION: puts("TOKEN_QUESTION '?'"); break;
        case TOKEN_COLON: puts("TOKEN_COLON ':'"); break;
        case TOKEN_IF: puts("TOKEN_IF 'if'"); break;
        case TOKEN_ELSE: puts("TOKEN_ELSE 'else'"); break;
        case TOKEN_FOR: puts("TOKEN_FOR 'for'"); break;
        case TOKEN_DO: puts("TOKEN_DO 'do'"); break;
        case TOKEN_WHILE: puts("TOKEN_WHILE 'while'"); break;
        case TOKEN_BREAK: puts("TOKEN_BREAK 'break'"); break;
        case TOKEN_CONTINUE: puts("TOKEN_CONTINUE 'continue'"); break;
        default: puts("UNKNOWN TOKEN");
    }
}

void debug_print_op(int op_type) {
    switch (op_type) {
        case NEG: puts("NEG '-'"); break;
        case BITW_NOT: puts("BITW_NOT '~'"); break;
        case LOGIC_NOT: puts("LOGIC_NOT '!'"); break;
        case ADD: puts("ADD '+'"); break;
        case SUB: puts("SUB '-'"); break;
        case MUL: puts("MUL '*'"); break;
        case DIV: puts("DIV '/'"); break;
        case MOD: puts("MOD '%'"); break;
        case LT: puts("LT '<'"); break;
        case LTE: puts("LTE '<='"); break;
        case GT: puts("GT '>'"); break;
        case GTE: puts("GTE '>='"); break;
        case EQ: puts("EQ '=='"); break;
        case NEQ: puts("NEQ '!='"); break;
        case LOGIC_AND: puts("LOGIC_AND '&&'"); break;
        case LOGIC_OR: puts("LOGIC_OR '||'"); break;
        case BITW_AND: puts("BITW_AND '&'"); break;
        case BITW_OR: puts("BITW_OR '|'"); break;
        case BITW_XOR: puts("BITW_XOR '^'"); break;
        case LSHIFT: puts("LSHIFT '<<'"); break;
        case RSHIFT: puts("RSHIFT '>>'"); break;
        case PRE_INC: puts("PRE_INC '++'"); break;
        case PRE_DEC: puts("PRE_DEC '--'"); break;
        case AS: puts("AS '='"); break;
        case AS_ADD: puts("AS_ADD '+='"); break;
        case AS_SUB: puts("AS_SUB '-='"); break;
        case AS_MUL: puts("AS_MUL '*='"); break;
        case AS_DIV: puts("AS_DIV '/='"); break;
        case AS_MOD: puts("AS_MOD '%='"); break;
        case AS_BW_AND: puts("AS_BW_AND '&='"); break;
        case AS_BW_OR: puts("AS_BW_OR '|='"); break;
        case AS_BW_XOR: puts("AS_BW_xOR '^='"); break;
        case AS_LSHIFT: puts("AS_LSHIFT '<<='"); break;
        case AS_RSHIFT: puts("AS_RSHIFT '>>='"); break;
    }
}

void debug_print_ast(AST *ast, int n) {
    switch (ast->type) {
        case AST_NOOP:
            print_spaces(n);
            puts("AST_NOOP");
            break;
        case AST_INT:
            print_spaces(n);
            printf("AST_INT: %d\n", ast->node.integer.value);
            break;
        case AST_ID:
            print_spaces(n);
            printf("AST_ID: %s\n", ast->node.id.value);
            break;
        case AST_RET:
            print_spaces(n);
            puts("AST_RET:");

            n += 2;
            print_spaces(n);
            puts("expr:");
            debug_print_ast(ast->node.ret_stmt.expr, n + 2);
            break;
        case AST_FUNC:
            print_spaces(n);
            puts("AST_FUNC:");

            n += 2;
            print_spaces(n); printf("id: %s\n", ast->node.function.id);

            print_spaces(n); puts("type: int"); // hardcoded for now

            print_spaces(n); puts("body:");
            debug_print_ast(ast->node.function.body, n + 2);
            // AST **body = ast->node.function.body;
            // for (; *body != NULL; body++) debug_print_ast(*body, n + 2);
            break;
        case AST_UNOP:
            print_spaces(n);
            puts("AST_UNOP:");

            n += 2;
            print_spaces(n); printf("type: ");
            debug_print_op(ast->node.unop.type);

            print_spaces(n);
            puts("expr:");
            debug_print_ast(ast->node.unop.expr, n + 2);
            break;
        case AST_BINOP:
            print_spaces(n);
            puts("AST_BINOP:");

            n += 2;
            print_spaces(n); printf("type: ");
            debug_print_op(ast->node.binop.type);

            print_spaces(n);
            puts("left:");
            debug_print_ast(ast->node.binop.left, n + 2);

            print_spaces(n);
            puts("right:");
            debug_print_ast(ast->node.binop.right, n + 2);
            break;
        case AST_ASSIGN:
            print_spaces(n);
            puts("AST_ASSIGN:");

            n += 2;
            print_spaces(n); printf("type: ");
            debug_print_op(ast->node.assign.type);

            print_spaces(n);
            printf("id: %s\n", ast->node.assign.id);

            print_spaces(n);
            puts("expr:");
            debug_print_ast(ast->node.assign.expr, n + 2);
            break;
        case AST_DECLARE:
            print_spaces(n);
            puts("AST_DECLARE:");

            n += 2;
            print_spaces(n);
            puts("type: int");
            
            print_spaces(n);
            printf("id: %s\n", ast->node.declare.id);

            print_spaces(n);
            puts("expr:");
            // printf("%d\n", ast->node.declare.expr == NULL);
            debug_print_ast(ast->node.declare.expr, n + 2);
            break;
        case AST_COND:
            print_spaces(n);
            puts("AST_COND");

            n += 2;
            print_spaces(n); puts("cond: ");
            debug_print_ast(ast->node.cond.cond, n + 2);

            print_spaces(n); puts("then: ");
            debug_print_ast(ast->node.cond.then_branch, n + 2);

            print_spaces(n); puts("else: ");
            debug_print_ast(ast->node.cond.else_branch, n + 2);
            break;
        case AST_BLOCK:
            print_spaces(n);
            puts("AST_BLOCK");

            n += 2;
            print_spaces(n); puts("block: ");
            AST **items = ast->node.block.blk_items;
            for (; *items != NULL; items++) debug_print_ast(*items, n + 2);
            break;
        case AST_FOR:
            print_spaces(n);
            puts("AST_FOR");

            n += 2;
            print_spaces(n); puts("start: ");
            debug_print_ast(ast->node.for_loop.start, n + 2);

            print_spaces(n); puts("stop: ");
            debug_print_ast(ast->node.for_loop.stop, n + 2);

            print_spaces(n); puts("step: ");
            debug_print_ast(ast->node.for_loop.step, n + 2);

            print_spaces(n); puts("body: ");
            debug_print_ast(ast->node.for_loop.body, n + 2);
            break;
        case AST_WHILE:
            print_spaces(n);
            puts("AST_WHILE");

            n += 2;
            print_spaces(n); puts("expr: ");
            debug_print_ast(ast->node.dowhile_loop.expr, n + 2);

            print_spaces(n); puts("body: ");
            debug_print_ast(ast->node.dowhile_loop.body, n + 2);
            break;
        case AST_DOWHILE:
            print_spaces(n);
            puts("AST_DOWHILE");

            n += 2;
            print_spaces(n); puts("body: ");
            debug_print_ast(ast->node.dowhile_loop.body, n + 2);

            print_spaces(n); puts("expr: ");
            debug_print_ast(ast->node.dowhile_loop.expr, n + 2);
            break;
        case AST_BREAK:
            print_spaces(n);
            puts("AST_BREAK");
            break;
        case AST_CONTINUE:
            print_spaces(n);
            puts("AST_CONTINUE");
            break;
    }
}

void setup(FILE *fp) {
    fputs("\t.intel_syntax noprefix\n", fp);
    fputs("\t.globl main\n", fp);
}

void write_id(FILE *fp, AST *ast, SymTable *env) {
    char *ident = ast->node.id.value;
    VarMap *var = symtable_get(env, ident);
    if (var == NULL) {
        fprintf(stderr, "err undeclared variable '%s'\n", ident);
        exit(1);
    }
    int offset = var->stack_offset;
    fprintf(fp, "\tmov rax, [rbp-%d]\n", offset);
}

void write_unop(FILE *fp, AST *ast, SymTable *env, State state) {
    write_ast(fp, ast->node.unop.expr, env, state);
    int type = ast->node.unop.type;
    switch (type) {
        case NEG:
            fputs("\tneg rax\n", fp);
            break;
        case BITW_NOT: 
            fputs("\tnot rax\n", fp);
            break;
        case LOGIC_NOT:
            fputs("\tcmp rax, 0\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsete al\n", fp);
            break;
        case PRE_INC:
            fputs("\tinc rax\n", fp);
            break;
        case PRE_DEC:
            fputs("\tdec rax\n", fp);
            break;
    }
    AST *expr = ast->node.unop.expr;
    if (expr->type == AST_ID && (type == PRE_INC || type == PRE_DEC)) {
        char *ident = expr->node.id.value;
        VarMap *var = symtable_get(env, ident);
        if (var == NULL) {
            fprintf(stderr, "err undeclared variable '%s'\n", ident);
            exit(1);
        }
        int offset = var->stack_offset;
        fprintf(fp, "\tmov [rbp-%d], rax\n", offset);
    }
}

void write_binop_operands(FILE *fp, AST *src, AST *dst, SymTable *env, State state) {
    write_ast(fp, src, env, state);
    fputs("\tpush rax\n", fp);
    write_ast(fp, dst, env, state);
    fputs("\tpop rcx\n", fp);
}

void write_binop(FILE *fp, AST *ast, SymTable *env, State state) {
    int tmp = GLOBAL_LABEL;
    AST *left = ast->node.binop.left,
        *right = ast->node.binop.right;
    State new_state;

    switch (ast->node.binop.type) {
        case ADD:
            write_binop_operands(fp, left, right, env, state);
            fputs("\tadd rax, rcx\n", fp);
            break;
        case SUB:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tsub rax, rcx\n", fp);
            break;
        case MUL:
            write_binop_operands(fp, left, right, env, state);
            fputs("\timul rax, rcx\n", fp);
            break;
        case DIV:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcqo\n", fp);
            fputs("\tidiv rcx\n", fp);
            break;
        case MOD:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcqo\n", fp);
            fputs("\tidiv rcx\n", fp);
            fputs("\tmov rax, rdx\n", fp);
            break;
        case EQ:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcmp rax, rcx\n", fp);
            // don't use xor rax, rax here because it changes the eflags
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsete al\n", fp);
            break;
        case NEQ:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetne al\n", fp);
            break;
        case LT:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetl al\n", fp);
            break;
        case LTE:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetle al\n", fp);
            break;
        case GT:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetg al\n", fp);
            break;
        case GTE:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetge al\n", fp);
            break;
        case BITW_OR:
            write_binop_operands(fp, left, right, env, state);
            fputs("\tor rax, rcx\n", fp);
            break;
        case BITW_XOR:
            write_binop_operands(fp, left, right, env, state);
            fputs("\txor rax, rcx\n", fp);
            break;
        case BITW_AND:
            write_binop_operands(fp, left, right, env, state);
            fputs("\tand rax, rcx\n", fp);
            break;
        case LSHIFT:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tsal rcx, rax\n", fp);
            break;
        case RSHIFT:
            write_binop_operands(fp, right, left, env, state);
            fputs("\tshr rcx, rax\n", fp);
            break;
        case LOGIC_OR:
            new_state = state; new_state.flag = false;
            write_ast(fp, left, env, new_state);
            if (left->type != AST_BINOP || left->node.binop.type != LOGIC_OR) {
                fputs("\tcmp rax, 0\n", fp);
                fprintf(fp, "\tjne .L%d\n", tmp + 1);
            }
            write_ast(fp, right, env, new_state);
            fputs("\tcmp rax, 0\n", fp);
            fprintf(fp, "\tjne .L%d\n", tmp + 1);
            break;
        case LOGIC_AND:
            new_state = state; new_state.flag = false;
            write_ast(fp, left, env, new_state);
            if (left->type != AST_BINOP || left->node.binop.type != LOGIC_AND) {
                fputs("\tcmp rax, 0\n", fp);
                fprintf(fp, "\tje .L%d\n", tmp + 1);
            }
            write_ast(fp, right, env, new_state);
            fputs("\tcmp rax, 0\n", fp);
            fprintf(fp, "\tje .L%d\n", tmp + 1);
            break;
    }
    if (state.flag && ast->node.binop.type == LOGIC_AND) {
        fputs("\tmov rax, 1\n", fp);
        fprintf(fp, "\tjmp .L%d\n", tmp + 2);
        fprintf(fp, ".L%d:\n", tmp + 1);
        fputs("\tmov rax, 0\n", fp);
        fprintf(fp, ".L%d:\n", tmp + 2);
        GLOBAL_LABEL += 2;
    }
    if (state.flag && ast->node.binop.type == LOGIC_OR) {
        fputs("\tmov rax, 0\n", fp);
        fprintf(fp, "\tje .L%d\n", tmp + 2);
        fprintf(fp, ".L%d:\n", tmp + 1);
        fputs("\tmov rax, 1\n", fp);
        fprintf(fp, "\tjmp .L%d\n", tmp + 3);
        fprintf(fp, ".L%d:\n", tmp + 2);
        fputs("\tmov rax, 0\n", fp);
        fprintf(fp, ".L%d:\n", tmp + 3);
        GLOBAL_LABEL += 3;
    }
}

void write_assignment(FILE *fp, AST *ast, SymTable *env, State state) {
    char *ident = ast->node.assign.id;
    VarMap *var = symtable_get(env, ident);
    if (var == NULL) {
        fprintf(stderr, "err undeclared variable '%s'\n", ident);
        exit(1);
    }
    int type = ast->node.assign.type;
    if (type == AS) {
        write_ast(fp, ast->node.assign.expr, env, state);
    } else {
        AST *binop = ast_init(AST_BINOP);
        AST *left = ast_init(AST_ID);
        left->node.id.value = ident;
        binop->node.binop.left = left;
        binop->node.binop.right = ast->node.assign.expr;

        switch (ast->node.assign.type) {
            case AS_ADD: binop->node.binop.type = ADD; break;
            case AS_SUB: binop->node.binop.type = SUB; break;
            case AS_MUL: binop->node.binop.type = MUL; break;
            case AS_DIV: binop->node.binop.type = DIV; break;
            case AS_MOD: binop->node.binop.type = MOD; break;
            case AS_BW_AND: binop->node.binop.type = BITW_AND; break;
            case AS_BW_OR: binop->node.binop.type = BITW_OR; break;
            case AS_BW_XOR: binop->node.binop.type = BITW_XOR; break;
            case AS_LSHIFT: binop->node.binop.type = LSHIFT; break;
            case AS_RSHIFT: binop->node.binop.type = RSHIFT; break;
        }
        write_ast(fp, binop, env, state);
    }
    int offset = var->stack_offset;
    fprintf(fp, "\tmov [rbp-%d], rax\n", offset);
}

void write_declare(FILE *fp, AST *ast, SymTable *env, State state) {
    char *ident = ast->node.declare.id;
    if (symtable_cur_contains(env, ident)) {
        fprintf(stderr, "err redeclaration of '%s'\n", ident);
        exit(1);
    }
    if (ast->node.declare.expr == NULL) {
        fputs("\tmov rax, 0\n", fp);
    } else {
        write_ast(fp, ast->node.declare.expr, env, state);
    }
    fputs("\tpush rax\n", fp);
    symtable_insert(env, ident, *(state.stack_index));
    *(state.stack_index) += 8;
}

void write_cond(FILE *fp, AST *ast, SymTable *env, State state) {
    AST *cond = ast->node.cond.cond,
        *then_branch = ast->node.cond.then_branch,
        *else_branch = ast->node.cond.else_branch;

    int tmp = GLOBAL_LABEL;
    GLOBAL_LABEL += 2;

    // check cond, if false jmp to else
    write_ast(fp, cond, env, state);
    fputs("\tcmp rax, 0\n", fp);
    fprintf(fp, "\tje .L%d\n", tmp + 1);

    // then
    write_ast(fp, then_branch, env, state);

    // else, if no else just print label
    if (else_branch->type == AST_NOOP) {
        fprintf(fp, "\t.L%d:\n", tmp + 1);
    } else {
        fprintf(fp, "\tjmp .L%d\n", tmp + 2);
        fprintf(fp, ".L%d:\n", tmp + 1);
        write_ast(fp, else_branch, env, state);
        fprintf(fp, ".L%d:\n", tmp + 2);
    }
}

bool last_is_ret(AST *ast) {
    if (ast->type == AST_RET) return true;
    if (ast->type == AST_BLOCK) {
        AST **items = ast->node.block.blk_items;
        while (*items != NULL) items++;
        return last_is_ret(*(--items));
    }
    if (ast->type == AST_COND) {
        AST *then_branch = ast->node.cond.then_branch,
            *else_branch = ast->node.cond.else_branch;
        if (else_branch->type == AST_NOOP) return last_is_ret(then_branch);
        return last_is_ret(else_branch);
    }
    return false;
}

void write_block(FILE *fp, AST *ast, SymTable *env, State state) {
    SymTable *cur_env = symtable_init(ST_BASE_SIZE, env);
    AST **items = ast->node.block.blk_items;
    for (; *items != NULL; items++) {
        AST *item = *items;
        if (item->type == AST_DECLARE) {
            write_declare(fp, item, cur_env, state);
        } else {
            write_ast(fp, item, cur_env, state);
        }
    }
    int bytes_to_dealloc = 8 * cur_env->used;
    fprintf(fp, "\tadd rsp, %d\n", bytes_to_dealloc);
    *(state.stack_index) -= bytes_to_dealloc;
}

void write_func(FILE *fp, AST *ast, bool flag) {
    fprintf(fp, "%s:\n", ast->node.function.id);
    fputs("\tpush rbp\n", fp);
    fputs("\tmov rbp, rsp\n", fp);
    AST *body = ast->node.function.body;
    AST **items = body->node.block.blk_items;
    if (*items == NULL) {
        fputs("\tmov rax, 0\n", fp);
        fputs("\tmov rsp, rbp\n", fp);
        fputs("\tpop rbp\n", fp);
        fputs("\tret\n", fp);
    } else {
        SymTable *env = symtable_init(ST_BASE_SIZE, NULL);
        int stack_index = 8;
        State state = {&stack_index, GLOBAL_LABEL, GLOBAL_LABEL, true};
        write_block(fp, body, env, state);
        for (; *items != NULL; items++);
        if (!last_is_ret(*(items - 1))) {
            AST *tmp_ret = ast_init(AST_RET), *tmp_int = ast_init(AST_INT);
            tmp_int->node.integer.value = 0;
            tmp_ret->node.ret_stmt.expr = tmp_int;
            write_ast(fp, tmp_ret, env, state);
        }
    }
}

void write_while(FILE *fp, AST *ast, SymTable *env, State state) {
    int tmp = GLOBAL_LABEL;
    GLOBAL_LABEL += 4;
    State new_state = {state.stack_index, tmp + 3, tmp + 4, state.flag};

    // jmp to condition
    fprintf(fp, "\tjmp .L%d\n", tmp + 1);

    // body
    fprintf(fp, ".L%d:\n", tmp + 2);
    fprintf(fp, ".L%d:\n", tmp + 3); // continue
    write_ast(fp, ast->node.while_loop.body, env, new_state);

    // check cond, if true jmp to body
    fprintf(fp, ".L%d:\n", tmp + 1);
    write_ast(fp, ast->node.while_loop.expr, env, new_state);
    fputs("\tcmp rax, 1\n", fp);
    fprintf(fp, "\tje .L%d\n", tmp + 2);

    // post loop
    fprintf(fp, ".L%d:\n", tmp + 4); // break
}

void write_do(FILE *fp, AST *ast, SymTable *env, State state) {
    int tmp = GLOBAL_LABEL;
    GLOBAL_LABEL += 3;
    State new_state = {state.stack_index, tmp + 2, tmp + 3, state.flag};

    // body
    fprintf(fp, ".L%d:\n", tmp + 1);
    fprintf(fp, ".L%d:\n", tmp + 2); // continue
    write_ast(fp, ast->node.dowhile_loop.body, env, new_state);

    // check cond, if true jmp to body
    write_ast(fp, ast->node.dowhile_loop.expr, env, new_state);
    fputs("\tcmp rax, 1\n", fp);
    fprintf(fp, "\tje .L%d\n", tmp + 1);

    // post loop
    fprintf(fp, ".L%d:\n", tmp + 3); // break
}

void write_for(FILE *fp, AST *ast, SymTable *env, State state) {
    SymTable *cur_env = symtable_init(ST_BASE_SIZE, env);
    int tmp = GLOBAL_LABEL;
    GLOBAL_LABEL += 4;
    State new_state = {state.stack_index, tmp + 3, tmp + 4, state.flag};

    // start (initialization), 
    write_ast(fp, ast->node.for_loop.start, cur_env, state);
    fprintf(fp, "\tjmp .L%d\n", tmp + 1);

    // body
    fprintf(fp, ".L%d:\n", tmp + 2);
    write_ast(fp, ast->node.for_loop.body, cur_env, new_state);

    // step (increment)
    fprintf(fp, ".L%d:\n", tmp + 3); // continue
    write_ast(fp, ast->node.for_loop.step, cur_env, state);

    // stop (condition), check cond, if true jmp to body
    fprintf(fp, ".L%d:\n", tmp + 1);
    AST *cond = ast->node.for_loop.stop;
    if (cond->type == AST_NOOP) {
        cond = ast_init(AST_INT);
        cond->node.integer.value = 1;
    }
    write_ast(fp, cond, cur_env, state);
    fputs("\tcmp rax, 1\n", fp);
    fprintf(fp, "\tje .L%d\n", tmp + 2);

    // post loop
    fprintf(fp, ".L%d:\n", tmp + 4); // break
    int bytes_to_dealloc = 8 * cur_env->used;
    fprintf(fp, "\tadd rsp, %d\n", bytes_to_dealloc);
    *(state.stack_index) -= bytes_to_dealloc;
}

void write_ast(FILE *fp, AST *ast, SymTable *env, State state) {
    switch (ast->type) {
        case AST_INT:
            fprintf(fp, "\tmov rax, %d\n", ast->node.integer.value);
            break;
        case AST_ID:
            write_id(fp, ast, env);
            break;
        case AST_RET:
            write_ast(fp, ast->node.ret_stmt.expr, env, state);
            fputs("\tmov rsp, rbp\n", fp);
            fputs("\tpop rbp\n", fp);
            fputs("\tret\n", fp);
            break;
        case AST_FUNC:
            write_func(fp, ast, true);
            break;
        case AST_UNOP:
            write_unop(fp, ast, env, state);
            break;
        case AST_BINOP:
            write_binop(fp, ast, env, state);
            break;
        case AST_ASSIGN:
            write_assignment(fp, ast, env, state);
            break;
        case AST_DECLARE:
            write_declare(fp, ast, env, state);
            break;
        case AST_COND:
            write_cond(fp, ast, env, state);
            break;
        case AST_BLOCK:
            write_block(fp, ast, env, state);
            break;
        case AST_WHILE:
            write_while(fp, ast, env, state);
            break;
        case AST_DOWHILE:
            write_do(fp, ast, env, state);
            break;
        case AST_FOR:
            write_for(fp, ast, env, state);
            break;
        case AST_CONTINUE:
            if (state.cont_label == 0) {
                fputs("err continue not in loop\n", stderr);
                exit(1);
            }
            fprintf(fp, "\tjmp .L%d\n", state.cont_label);
            break;
        case AST_BREAK:
            if (state.break_label == 0) {
                fputs("err break not in loop\n", stderr);
                exit(1);
            }
            fprintf(fp, "\tjmp .L%d\n", state.break_label);
            break;
        default:
    }
}

VarMap *varmap_init(char *id, int offset) {
    VarMap *var = dmalloc(sizeof(VarMap));
    var->id = strdup(id);
    var->stack_offset = offset;
    return var;
}

void varmap_free(VarMap *var) {
    free(var->id);
    free(var);
}

SymTable *symtable_init(int size, SymTable *parent) {
    SymTable *st = dmalloc(size * sizeof(SymTable));
    st->size = size;
    st->used = 0;
    st->arr = dmalloc(sizeof(VarMap *));
    st->parent = parent;
    for (int i = 0; i < size; i++) st->arr[i] = NULL;
    return st;
}

void symtable_free(SymTable *st) {
    st->parent = NULL;
    for (int i = 0; i < st->used; i++) varmap_free(st->arr[i]);
    free(st->arr);
    free(st);
}

void symtable_resize(SymTable *st) {
    VarMap **new_arr = dmalloc((st->size * 2) * sizeof(VarMap *));
    for (int i = 0; i < st->size * 2; i++) new_arr[i] = NULL;
    int id = 0;
    for (int i = 0; i < st->size; i++) {
        if (st->arr[i] != NULL) {
            new_arr[id++] = st->arr[i];
            st->arr[i] = NULL;
        }
    }
    VarMap **tmp = st->arr;
    st->arr = new_arr;
    free(tmp);
}

void symtable_insert(SymTable *st, char *id, int offset) {
    if (st->used == st->size) symtable_resize(st);
    st->arr[st->used++] = varmap_init(id, offset);
}

bool symtable_cur_contains(SymTable *st, char *id) {
    for (int i = 0; i < st->used; i++) {
        VarMap *item = st->arr[i];
        if (strcmp(item->id, id) == 0) return true;
    }
    return false;
}

bool symtable_contains(SymTable *st, char *id) {
    SymTable *stb = st;
    for (; stb != NULL; stb = stb->parent) {
        if (symtable_cur_contains(stb, id)) return true;
    }
    return false;
}

VarMap *symtable_cur_get(SymTable *st, char *id) {
    for (int i = 0; i < st->used; i++) {
        VarMap *item = st->arr[i];
        if (strcmp(item->id, id) == 0) return st->arr[i];
    }
    return NULL;
}

VarMap *symtable_get(SymTable *st, char *id) {
    SymTable *stb = st;
    for (; stb != NULL; stb = stb->parent) {
        VarMap *res = symtable_cur_get(stb, id);
        if (res != NULL) return res;
    }
    return NULL;
}

