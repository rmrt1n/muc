#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

typedef struct Token {
    enum {
        TOKEN_ID, TOKEN_INT, TOKEN_SEMICOLON, TOKEN_EQ,
        TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_LBRACE, TOKEN_RBRACE,
        TOKEN_MINUS, TOKEN_TILDE, TOKEN_BANG,
        TOKEN_PLUS, TOKEN_MUL, TOKEN_DIV, TOKEN_PERCENT,
        TOKEN_AMPAMP, TOKEN_PIPEPIPE, TOKEN_EQEQ, TOKEN_NEQ,
        TOKEN_LT, TOKEN_LTE, TOKEN_GT, TOKEN_GTE,
        TOKEN_AMP, TOKEN_PIPE, TOKEN_CARET, TOKEN_LSHIFT, TOKEN_RSHIFT,
        TOKEN_RETURN
    } type;
    char *value;
} Token;

enum op_type {
    NEG, BITW_NOT, LOGIC_NOT,
    ADD, SUB, MUL, DIV, MOD,
    LT, LTE, GT, GTE, EQ, NEQ,
    LOGIC_AND, LOGIC_OR,
    BITW_AND, BITW_OR, BITW_XOR,
    LSHIFT, RSHIFT
};

typedef struct AST {
    enum {
        AST_INT, AST_ID, AST_FUNC,
        AST_RET, AST_UNOP, AST_BINOP, AST_ASSIGN, AST_DECLARE
    } type;
    union {
        struct {int value;} integer;
        struct {char *value;} id;
        struct {
            // int ret_type;
            char *id;
            struct AST **body;
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
            char *id;
            struct AST *expr;
        } assign;
        struct {
            // int type;
            char *id;
            struct AST *expr;
        } declare;
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
} SymTable;

// grammar
// program -> func
// func -> "int" "main" "(" ")" "{" {stmt} "}"
// stmt -> "return" expr ";"
//       | "int" id ["=" expr] ";"
//       | expr ";"
// expr -> id "=" expr
//       | logic-or
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
// primary -> "(" exp ")" | int | id
// unop -> "-" | "~" | "!"
// int -> [0-9]+
// id -> [a-zA-Z_][a-zA-Z0-9_]*

// helper funcs
void *dmalloc(size_t size);
void *drealloc(void *p, size_t size);
int strtoi(char *str);
void print_spaces(int n);
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
AST *parse_relational(Parser *parser);
AST *parse_equality(Parser *parser);
AST *parse_logic_and(Parser *parser);
AST *parse_logic_or(Parser *parser);
AST *parse_expr(Parser *parser);
AST *parse_stmt(Parser *parser);
AST *parse_func(Parser *parser);
AST *parse(Parser *parser);
// debugging funcs
void debug_print_token(Token *token);
void debug_print_op(int op_type);
void debug_print_ast(AST *ast, int n);
// code gen funcs
void setup(FILE *fp);
void write_unop(FILE *fp, AST *ast);
void write_binop_operands(FILE *fp, AST *src, AST *dst, int *x, bool flag);
void write_binop(FILE *fp, AST *ast, int *x, bool flag);
void write_ast(FILE *fp, AST *ast, int *x, bool flag);
// symtable funcs
VarMap *varmap_init(char *id, int offset);
void varmap_free(VarMap *var);
SymTable *symtable_init(int size);
void symtable_free(SymTable *st);
void symtable_resize(SymTable *st);
void symtable_insert(SymTable *st, char *id, int offset);
bool symtable_contains(SymTable *st, char *id);
VarMap *symtable_get(SymTable *st, char *id);

// global symtable (temporary)
SymTable *global_st;
int global_stack_index = 8;

int main(int argc, char **argv) {
    global_st = symtable_init(1);
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
    int label = 0;
    write_ast(target, ast, &label, true);

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
        // printf("eat ws %d\n", c);
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
    char c;
    while ((c = getc(fp)) != EOF) {
        // printf("%d\n", c);
        eat_whitespace(fp);
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
            case '-': return token_init(TOKEN_MINUS, NULL);
            case '~': return token_init(TOKEN_TILDE, NULL);
            case '!':
                if (peek(fp) == '=') {
                    getc(fp);
                    return token_init(TOKEN_NEQ, NULL);
                }
                return token_init(TOKEN_BANG, NULL);
            case '+': return token_init(TOKEN_PLUS, NULL);
            case '*': return token_init(TOKEN_MUL, NULL);
            case '/': return token_init(TOKEN_DIV, NULL);
            case '%': return token_init(TOKEN_PERCENT, NULL);
            case '&':
                if (peek(fp) == '&') {
                    getc(fp);
                    return token_init(TOKEN_AMPAMP, NULL);
                }
                return token_init(TOKEN_AMP, NULL);
                break;
            case '|':
                if (peek(fp) == '|') {
                    getc(fp);
                    return token_init(TOKEN_PIPEPIPE, NULL);
                }
                return token_init(TOKEN_PIPE, NULL);
                break;
            case '^': return token_init(TOKEN_CARET, NULL);
            case '<':
                if (peek(fp) == '=') {
                    getc(fp);
                    return token_init(TOKEN_LTE, NULL);
                }
                if (peek(fp) == '<') {
                    getc(fp);
                    return token_init(TOKEN_LSHIFT, NULL);
                }
                return token_init(TOKEN_LT, NULL);
                break;
            case '>':
                if (peek(fp) == '=') {
                    getc(fp);
                    return token_init(TOKEN_GTE, NULL);
                }
                if (peek(fp) == '>') {
                    getc(fp);
                    return token_init(TOKEN_RSHIFT, NULL);
                }
                return token_init(TOKEN_GT, NULL);
                break;
            case '=':
                if (peek(fp) == '=') {
                    getc(fp);
                    return token_init(TOKEN_EQEQ, NULL);
                }
                return token_init(TOKEN_EQ, NULL);
                break;
        }
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
            AST **body = ast->node.function.body;
            for (; *body != NULL; body++) ast_free(*body);
            free(ast->node.function.body);
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
            err_unexpected_token(token);
    }
    return node;
}

AST *parse_unary(Parser *parser) {
    AST *node;
    Token *token = parser->cur_token;
    if (token->type == TOKEN_MINUS ||
        token->type == TOKEN_TILDE ||
        token->type == TOKEN_BANG) {
        node = ast_init(AST_UNOP);
        switch (token->type) {
            case TOKEN_MINUS: node->node.unop.type = NEG; break;
            case TOKEN_TILDE: node->node.unop.type = BITW_NOT; break;
            case TOKEN_BANG: node->node.unop.type = LOGIC_NOT; break;
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
    while (token->type == TOKEN_MUL ||
           token->type == TOKEN_DIV ||
           token->type == TOKEN_PERCENT) {
        AST *unary = node;
        node = ast_init(AST_BINOP);
        switch (token->type) {
            case TOKEN_MUL: node->node.binop.type = MUL; break;
            case TOKEN_DIV: node->node.binop.type = DIV; break;
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

AST *parse_assignment(Parser *parser, AST *id) {
    AST *node = id;
    Token *token = parser->cur_token;
    if (token->type == TOKEN_EQ) {
        node = ast_init(AST_ASSIGN);
        parser_eat(parser, token->type);
        node->node.assign.id = strdup(id->node.id.value);
        node->node.assign.expr = parse_expr(parser);
    }
    return node;
}

AST *parse_expr(Parser *parser) {
    AST *node = parse_logic_or(parser);
    if (node->type == AST_ID) return parse_assignment(parser, node);
    return node;
}

AST *parse_return(Parser *parser) {
    AST *node = ast_init(AST_RET);
    parser_eat(parser, TOKEN_RETURN);
    node->node.ret_stmt.expr = parse_expr(parser);
    return node;
}

AST *parse_declaration(Parser *parser) {
    AST *node = ast_init(AST_DECLARE);

    // eat type specifier
    // debug_print_token(parser->cur_token);
    parser_eat(parser, TOKEN_ID);
    // AST *id = parse_primary(parser);
    Token *id = parser->cur_token;
    node->node.declare.id = strdup(id->value);
    parser_eat(parser, TOKEN_ID);
    node->node.declare.expr = NULL;
    Token *token = parser->cur_token;
    if (token->type == TOKEN_EQ) {
        parser_eat(parser, token->type);
        node->node.declare.expr = parse_expr(parser);
    }
    return node;
}

AST *parse_stmt(Parser *parser) {
    AST *node;
    Token *token = parser->cur_token;
    if (token->type == TOKEN_RETURN) {
        node =  parse_return(parser);
    } else if (token->type == TOKEN_ID && strcmp(token->value, "int") == 0) {
        node = parse_declaration(parser);
    } else {
        node = parse_expr(parser);
    }
    parser_eat(parser, TOKEN_SEMICOLON);
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
    parser_eat(parser, TOKEN_LBRACE);
    AST **stmt_ls = dmalloc(sizeof(AST *));
    int size = 1;
    Token *token = parser->cur_token;
    while (token->type != TOKEN_RBRACE) {
        AST *stmt = parse_stmt(parser);
        stmt_ls = drealloc(stmt_ls, (size + 1) * sizeof(AST *));
        stmt_ls[size++ - 1] = stmt;
        token = parser->cur_token;
    }
    stmt_ls[size-1] = NULL;
    parser_eat(parser, TOKEN_RBRACE);
    node->node.function.body = stmt_ls;

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
        case TOKEN_MUL: puts("TOKEN_MUL: '*'"); break;
        case TOKEN_DIV: puts("TOKEN_DIV: '/'"); break;
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
        // default: puts("UNKNOWN TOKEN");
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
    }
}

void debug_print_ast(AST *ast, int n) {
    if (ast == NULL) return;
    switch (ast->type) {
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
            AST **body = ast->node.function.body;
            for (; *body != NULL; body++) debug_print_ast(*body, n + 2);
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
            printf("id: %s\n", ast->node.assign.id);

            print_spaces(n);
            puts("expr:");
            debug_print_ast(ast->node.assign.expr, n + 2);
            break;
    }
}

void setup(FILE *fp) {
    fputs("\t.intel_syntax noprefix\n", fp);
    fputs("\t.globl main\n", fp);
}

void write_unop(FILE *fp, AST *ast) {
    switch (ast->node.unop.type) {
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
    }
}

void write_binop_operands(FILE *fp, AST *src, AST *dst, int *x, bool flag) {
    write_ast(fp, src, x, flag);
    fputs("\tpush rax\n", fp);
    write_ast(fp, dst, x, flag);
    fputs("\tpop rcx\n", fp);
}

void write_binop(FILE *fp, AST *ast, int *x, bool flag) {
    AST *left = ast->node.binop.left, *right = ast->node.binop.right;
    switch (ast->node.binop.type) {
        case ADD:
            write_binop_operands(fp, left, right, x, flag);
            fputs("\tadd rax, rcx\n", fp);
            break;
        case SUB:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tsub rax, rcx\n", fp);
            break;
        case MUL:
            write_binop_operands(fp, left, right, x, flag);
            fputs("\timul rax, rcx\n", fp);
            break;
        case DIV:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcqo\n", fp);
            fputs("\tidiv rcx\n", fp);
            break;
        case MOD:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcqo\n", fp);
            fputs("\tidiv rcx\n", fp);
            fputs("\tmov rax, rdx\n", fp);
            break;
        case EQ:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcmp rax, rcx\n", fp);
            // don't use xor rax, rax here because it changes the eflags
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsete al\n", fp);
            break;
        case NEQ:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetne al\n", fp);
            break;
        case LT:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetl al\n", fp);
            break;
        case LTE:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetle al\n", fp);
            break;
        case GT:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetg al\n", fp);
            break;
        case GTE:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tcmp rax, rcx\n", fp);
            fputs("\tmov rax, 0\n", fp);
            fputs("\tsetge al\n", fp);
            break;
        case BITW_OR:
            write_binop_operands(fp, left, right, x, flag);
            fputs("\tor rax, rcx\n", fp);
            break;
        case BITW_XOR:
            write_binop_operands(fp, left, right, x, flag);
            fputs("\txor rax, rcx\n", fp);
            break;
        case BITW_AND:
            write_binop_operands(fp, left, right, x, flag);
            fputs("\tand rax, rcx\n", fp);
            break;
        case LSHIFT:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tsal rcx, rax\n", fp);
            break;
        case RSHIFT:
            write_binop_operands(fp, right, left, x, flag);
            fputs("\tshr rcx, rax\n", fp);
            break;
        case LOGIC_OR:
            write_ast(fp, left, x, false);
            if (left->type != AST_BINOP || left->node.binop.type != LOGIC_OR) {
                fputs("\tcmp rax, 0\n", fp);
                fprintf(fp, "\tjne .L%d\n", *x + 1);
            }
            write_ast(fp, right, x, false);
            fputs("\tcmp rax, 0\n", fp);
            fprintf(fp, "\tjne .L%d\n", *x + 1);
            break;
        case LOGIC_AND:
            write_ast(fp, left, x, false);
            if (left->type != AST_BINOP || left->node.binop.type != LOGIC_AND) {
                fputs("\tcmp rax, 0\n", fp);
                fprintf(fp, "\tje .L%d\n", *x + 1);
            }
            write_ast(fp, right, x, false);
            fputs("\tcmp rax, 0\n", fp);
            fprintf(fp, "\tje .L%d\n", *x + 1);
            break;
    }
}

void write_ast(FILE *fp, AST *ast, int *x, bool flag) {
    char *ident;
    int offset;
    VarMap *var;
    switch (ast->type) {
        case AST_INT:
            fprintf(fp, "\tmov rax, %d\n", ast->node.integer.value);
            break;
        case AST_ID:
            ident = ast->node.assign.id;
            var = symtable_get(global_st, ident);
            if (var == NULL) {
                fprintf(stderr, "err undeclared variable '%s'\n", ident);
                exit(1);
            }
            offset = var->stack_offset;
            fprintf(fp, "\tmov rax, [rbp-%d]\n", offset);
            break;
        case AST_RET:
            write_ast(fp, ast->node.ret_stmt.expr, x, flag);
            break;
        case AST_FUNC:
            fprintf(fp, "%s:\n", ast->node.function.id);
            fputs("\tpush rbp\n", fp);
            fputs("\tmov rbp, rsp\n", fp);
            AST **body = ast->node.function.body;
            if (*body == NULL) {
                fputs("\tmov rax, 0\n", fp);
            } else {
                for (; *body != NULL; body++) write_ast(fp, *body, x, flag);
                AST *last = *(body - 1);
                if (last->type != AST_RET) fputs("\tmov rax, 0\n", fp);
            }
            fputs("\tmov rsp, rbp\n", fp);
            fputs("\tpop rbp\n", fp);
            fputs("\tret\n", fp);
            break;
        case AST_UNOP:
            write_ast(fp, ast->node.unop.expr, x, flag);
            write_unop(fp, ast);
            break;
        case AST_BINOP:
            write_binop(fp, ast, x, flag);
            if (flag && ast->node.binop.type == LOGIC_AND) {
                fputs("\tmov rax, 1\n", fp);
                fprintf(fp, "\tjmp .L%d\n", *x + 2);
                fprintf(fp, ".L%d:\n", *x + 1);
                fputs("\tmov rax, 0\n", fp);
                fprintf(fp, ".L%d:\n", *x + 2);
                *x += 2;
            }
            if (flag && ast->node.binop.type == LOGIC_OR) {
                fputs("\tmov rax, 0\n", fp);
                fprintf(fp, "\tje .L%d\n", *x + 2);
                fprintf(fp, ".L%d:\n", *x + 1);
                fputs("\tmov rax, 1\n", fp);
                fprintf(fp, "\tjmp .L%d\n", *x + 3);
                fprintf(fp, ".L%d:\n", *x + 2);
                fputs("\tmov rax, 0\n", fp);
                fprintf(fp, ".L%d:\n", *x + 3);
                *x += 3;
            }
            break;
        case AST_ASSIGN:
            ident = ast->node.assign.id;
            var = symtable_get(global_st, ident);
            if (var == NULL) {
                fprintf(stderr, "err undeclared variable '%s'\n", ident);
                exit(1);
            }
            write_ast(fp, ast->node.assign.expr, x, flag);
            offset = var->stack_offset;
            fprintf(fp, "\tmov [rbp-%d], rax\n", offset);
            break;
        case AST_DECLARE:
            ident = ast->node.declare.id;
            if (symtable_contains(global_st, ident)) {
                fprintf(stderr, "err redeclaration of '%s'\n", ident);
                exit(1);
            }
            if (ast->node.declare.expr == NULL) {
                fputs("\tmov rax, 0\n", fp);
            } else {
                write_ast(fp, ast->node.declare.expr, x, flag);
            }
            fputs("\tpush rax\n", fp);
            symtable_insert(global_st, ident, global_stack_index);
            global_stack_index += 8;
            break;
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

SymTable *symtable_init(int size) {
    SymTable *st = dmalloc(size * sizeof(SymTable));
    st->size = size;
    st->used = 0;
    st->arr = dmalloc(sizeof(VarMap *));
    for (int i = 0; i < size; i++) st->arr[i] = NULL;
    return st;
}

void symtable_free(SymTable *st) {
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
    // int i;
    // for (i = st->used - 1; st->arr[i] != NULL; i++);
    // st->arr[i] = varmap_init(id, offset);
    // st->used++;
}

bool symtable_contains(SymTable *st, char *id) {
    for (int i = 0; i < st->used; i++) {
        VarMap *item = st->arr[i];
        if (strcmp(item->id, id) == 0) return true;
    }
    return false;
}

VarMap *symtable_get(SymTable *st, char *id) {
    for (int i = 0; i < st->used; i++) {
        VarMap *item = st->arr[i];
        if (strcmp(item->id, id) == 0) return st->arr[i];
    }
    return NULL;
}

