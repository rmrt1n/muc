#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct Token {
    enum {
        TOKEN_ID, TOKEN_INT, TOKEN_SEMICOLON,
        TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_LBRACE, TOKEN_RBRACE,
        TOKEN_MINUS, TOKEN_TILDE, TOKEN_BANG,
        TOKEN_PLUS, TOKEN_MUL, TOKEN_DIV
    } type;
    char *value;
} Token;

enum {
    NEG, BITW_NOT, LOGIC_NOT,
    ADD, SUB, MUL, DIV
} op_type;

typedef struct AST {
    enum {
        AST_INT, AST_FUNC, AST_RET, AST_UNOP, AST_BINOP
    } type;
    union {
        struct {int value;} integer;
        struct {
            int ret_type;
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
    } node;
} AST;

// parser struct to keep track of current tokens
typedef struct Parser {
    FILE *fp;
    Token *cur_token;
} Parser;

// grammar
// program -> func
// func -> "int" "main" "(" ")" "{" stmt "}"
// stmt -> "return" exp ";"
// exp -> term
// term -> factor {("+" | "-") factor}
// factor -> unary {("*" | "/") unary}
// unary -> unop unary
// primary -> "(" exp ")" | int
// unop -> "-" | "~" | "!"
// int -> (-)[0-9]+

void *dmalloc(size_t size);
void *drealloc(void *p, size_t size);
Token *token_init(int type, char *value);
void token_free(Token *token);
void eat_whitespace(FILE *fp);
char peek(FILE *fp);
Token *get_next_token(FILE *fp);
Token *expect(FILE *fp, int type);
AST *ast_init(int type);
void ast_free(AST *ast);
void expect_id(Token *token, char *id);
int strtoi(char *str);
Parser *parser_init(FILE *fp);
void parser_free(Parser *parser);
AST *parse_expr(Parser *parser);
AST *parse_stmt(Parser *parser);
AST *parse_func(Parser *parser);
AST *parse(Parser *parser);
void print_spaces(int n);
void debug_print_token(Token *token);
void debug_print_op(int op_type);
void debug_print_ast(AST *ast, int n);
void setup(FILE *fp);
void write_expr(FILE *fp, AST *ast);
void write_stmt(FILE *fp, AST *ast);
void write_func(FILE *fp, AST *ast);
void write_ast(FILE *fp, AST *ast);

int main(int argc, char **argv) {
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
    write_ast(target, ast);

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
    char c;
    while ((c = getc(fp)) != EOF) {
        eat_whitespace(fp);
        // get identifier
        if (isalpha(c) || c == '_') {
            int size = 1;
            char *id = dmalloc(size);
            while (isalnum(c) || c == '_') {
                id = drealloc(id, ++size);
                strncat(id, &c, 1);
                c = getc(fp);
            }
            ungetc(c, fp);
            id[size-1] = '\0';
            return token_init(TOKEN_ID, id);
        }
        // get integer literal, no neg int for now
        // if (isdigit(c) || (c == '-' && isdigit(peek(fp)))) {
        if (isdigit(c)) {
            ungetc(c, fp);
            int size = 1;
            char *num = dmalloc(size);
            // c == '-' ? *num = c : ungetc(c, fp);
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
            case '!': return token_init(TOKEN_BANG, NULL);
            case '+': return token_init(TOKEN_PLUS, NULL);
            case '*': return token_init(TOKEN_MUL, NULL);
            case '/': return token_init(TOKEN_DIV, NULL);
        }
    }
    return NULL;
}

void err_unexpected_token(Token *token) {
    fprintf(stderr, "unexpected token: ");
    debug_print_token(token);
    exit(1);
}

Token *expect(FILE *fp, int type) {
    Token *token = get_next_token(fp);
    if (token->type != type) {
        err_unexpected_token(token);
    }
    return token;
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
        case AST_RET:
            ast_free(ast->node.ret_stmt.expr);
            break;
        case AST_FUNC:
            ast_free(ast->node.function.body);
            free(ast->node.function.id);
            break;
        case AST_UNOP:
            ast_free(ast->node.unop.expr);
            break;
        case AST_BINOP:
            ast_free(ast->node.binop.left);
            ast_free(ast->node.binop.right);
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
    switch (token->type) {
        case TOKEN_MINUS:
            parser_eat(parser, TOKEN_MINUS);
            node = ast_init(AST_UNOP);
            node->node.unop.type = NEG;
            node->node.unop.expr = parse_unary(parser);
            break;
        case TOKEN_TILDE:
            parser_eat(parser, TOKEN_TILDE);
            node = ast_init(AST_UNOP);
            node->node.unop.type = BITW_NOT;
            node->node.unop.expr = parse_unary(parser);
            break;
        case TOKEN_BANG:
            parser_eat(parser, TOKEN_BANG);
            node = ast_init(AST_UNOP);
            node->node.unop.type = LOGIC_NOT;
            node->node.unop.expr = parse_unary(parser);
            break;
        default:
            node = parse_primary(parser);
    }
    return node;
}

AST *parse_factor(Parser *parser) {
    AST *node = parse_unary(parser); 
    Token *token = parser->cur_token;
    while (token->type == TOKEN_MUL || token->type == TOKEN_DIV) {
        AST *unary = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = token->type == TOKEN_MUL ? MUL : DIV;
        parser_eat(parser, token->type == TOKEN_MUL ? TOKEN_MUL : TOKEN_DIV);
        node->node.binop.left = unary;
        node->node.binop.right = parse_unary(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_term(Parser *parser) {
    AST *node = parse_factor(parser);
    Token *token = parser->cur_token;
    while (token->type == TOKEN_PLUS || token->type == TOKEN_MINUS) {
        AST *factor = node;
        node = ast_init(AST_BINOP);
        node->node.binop.type = token->type == TOKEN_PLUS ? ADD : SUB;
        parser_eat(parser, token->type == TOKEN_PLUS ? TOKEN_PLUS : TOKEN_MINUS);
        node->node.binop.left = factor;
        node->node.binop.right = parse_factor(parser);
        token = parser->cur_token;
    }
    return node;
}

AST *parse_expr(Parser *parser) {
    return parse_term(parser);
}

AST *parse_stmt(Parser *parser) {
    AST *node = ast_init(AST_RET);
    // parse return keyword
    expect_id(parser->cur_token, "return");
    parser_eat(parser, TOKEN_ID);
    // parse expr
    AST *expr = parse_expr(parser);
    node->node.ret_stmt.expr = expr;
    // parse semicolon
    parser_eat(parser, TOKEN_SEMICOLON);
    return node;
}

AST *parse_func(Parser *parser) {
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
    AST *func_body = parse_stmt(parser);
    node->node.function.body = func_body;
    parser_eat(parser, TOKEN_RBRACE);
    return node;
    
}

AST *parse(Parser *parser) {
    return parse_func(parser);
}

void debug_print_token(Token *token) {
    int type = token->type;
    switch (type) {
        case TOKEN_LPAREN: puts("TOKEN_LPAREN: ("); break;
        case TOKEN_RPAREN: puts("TOKEN_LPAREN: )"); break;
        case TOKEN_LBRACE: puts("TOKEN_LPAREN: {"); break;
        case TOKEN_RBRACE: puts("TOKEN_LPAREN: }"); break;
        case TOKEN_SEMICOLON: puts("TOKEN_LPAREN: ;"); break;
        case TOKEN_MINUS: puts("TOKEN_MINUS: -"); break;
        case TOKEN_TILDE: puts("TOKEN_TILDE: ~"); break;
        case TOKEN_BANG: puts("TOKEN_BANG: !"); break;
        case TOKEN_PLUS: puts("TOKEN_PLUS: +"); break;
        case TOKEN_MUL: puts("TOKEN_MUL: *"); break;
        case TOKEN_DIV: puts("TOKEN_DIV: /"); break;
        // default: puts("UNKNOWN TOKEN");
    }
}

void print_spaces(int n) {
    while (n--) printf(" ");
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
        // default: puts("UNKNOWN OPERATOR");
    }
}

void debug_print_ast(AST *ast, int n) {
    switch (ast->type) {
        case AST_INT:
            print_spaces(n);
            printf("AST_INT: %d\n", ast->node.integer.value);
            break;
        case AST_RET:
            print_spaces(n);
            puts("AST_RET:");

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

void write_binop_operands(FILE *fp, AST *src, AST *dst) {
    write_ast(fp, src);
    fputs("\tpush rax\n", fp);
    write_ast(fp, dst);
    fputs("\tpop rcx\n", fp);
}

void write_binop(FILE *fp, AST *ast) {
    AST *left = ast->node.binop.left, *right = ast->node.binop.right;
    switch (ast->node.binop.type) {
        case ADD:
            write_binop_operands(fp, left, right);
            fputs("\tadd rax, rcx\n", fp);
            break;
        case SUB:
            write_binop_operands(fp, right, left);
            fputs("\tsub rax, rcx\n", fp);
            break;
        case MUL:
            write_binop_operands(fp, left, right);
            fputs("\tmul rcx\n", fp);
            break;
        case DIV:
            fputs("\tmov rdx, 0\n", fp);
            write_binop_operands(fp, right, left);
            fputs("\tcqo\n", fp);
            fputs("\tidiv rcx\n", fp);
            break;
    }
}

void write_ast(FILE *fp, AST *ast) {
    switch (ast->type) {
        case AST_INT:
            fprintf(fp, "\tmov rax, %d\n", ast->node.integer.value);
            break;
        case AST_RET:
            write_ast(fp, ast->node.ret_stmt.expr);
            fputs("\tret\n", fp);
            break;
        case AST_FUNC:
            fprintf(fp, "%s:\n", ast->node.function.id);
            write_ast(fp, ast->node.function.body);
            break;
        case AST_UNOP:
            write_ast(fp, ast->node.unop.expr);
            write_unop(fp, ast);
            break;
        case AST_BINOP:
            write_binop(fp, ast);
            break;
    }
}

