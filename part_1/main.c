#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct Token {
    enum {
        TOKEN_ID, TOKEN_INT, TOKEN_SEMICOLON,
        TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_LBRACE, TOKEN_RBRACE,
    } type;
    char *value;
} Token;

typedef struct AST {
    enum {
        AST_INT, AST_FUNC, AST_RET
    } type;
    union {
        struct {int value;} integer;
        struct {
            int ret_type;
            char *id;
            struct AST *body;
        } function;
        struct {struct AST *expr;} ret_stmt;
    } node;
} AST;

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
AST *parse_expr(FILE *fp);
AST *parse_stmt(FILE *fp);
AST *parse_func(FILE *fp);
AST *parse(FILE *fp);
void print_spaces(int n);
void debug_print_token(Token *token);
void debug_print_ast(AST *ast, int n);
void setup(FILE *fp);
void write_expr(FILE *fp, AST *ast);
void write_stmt(FILE *fp, AST *ast);
void write_func(FILE *fp, AST *ast);
void write_ast(FILE *fp, AST *ast);

// grammar
// program -> func
// func -> "int" "main" "(" ")" "{" stmt "}"
// stmt -> "return" exp ";"
// exp -> int
// int -> (-)[0-9]+

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
    AST *ast = parse(fp);
    // debug_print_ast(ast, 0);

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
        }
    }
    return NULL;
}

Token *expect(FILE *fp, int type) {
    Token *token = get_next_token(fp);
    if (token->type != type) {
        fprintf(stderr, "unexpected token: ");
        debug_print_token(token);
        exit(1);
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
            free(ast);
            break;
        case AST_RET:
            ast_free(ast->node.ret_stmt.expr);
            free(ast);
            break;
        case AST_FUNC:
            ast_free(ast->node.function.body);
            free(ast->node.function.id);
            free(ast);
            break;
    }
}

void expect_id(Token *token, char *id) {
    if (strcmp(token->value, id) != 0) {
        fprintf(stderr, "unexpected token: ");
        debug_print_token(token);
        exit(1);
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

AST *parse_expr(FILE *fp) {
    AST *node = ast_init(AST_INT);
    // expression only integer literal for now
    Token *token = expect(fp, TOKEN_INT);
    node->node.integer.value = strtoi(token->value);
    return node;
}

AST *parse_stmt(FILE *fp) {
    AST *node = ast_init(AST_RET);
    // parse return keyword
    Token *token = expect(fp, TOKEN_ID);
    expect_id(token, "return");
    // parse expr
    AST *expr = parse_expr(fp);
    node->node.ret_stmt.expr = expr;
    // parse semicolon
    expect(fp, TOKEN_SEMICOLON);
    return node;
}

AST *parse_func(FILE *fp) {
    AST *node = ast_init(AST_FUNC);
    // set return type of function
    Token *token = expect(fp, TOKEN_ID);
    expect_id(token, "int");
    // parse func name, for now only main is supported
    token = expect(fp, TOKEN_ID);
    expect_id(token, "main");
    node->node.function.id = strdup("main");
    // parse func params
    expect(fp, TOKEN_LPAREN);
    expect(fp, TOKEN_RPAREN);
    // parse func body
    expect(fp, TOKEN_LBRACE);
    AST *func_body = parse_stmt(fp);
    node->node.function.body = func_body;
    expect(fp, TOKEN_RBRACE);
    return node;
}

AST *parse(FILE *fp) {
    return parse_func(fp);
}

void debug_print_token(Token *token) {
    int type = token->type;
    if (type == TOKEN_ID || type == TOKEN_INT) printf("TOKEN_ID: %s\n", token->value);
    switch (type) {
        case TOKEN_LPAREN: puts("TOKEN_LPAREN: ("); break;
        case TOKEN_RPAREN: puts("TOKEN_LPAREN: )"); break;
        case TOKEN_LBRACE: puts("TOKEN_LPAREN: {"); break;
        case TOKEN_RBRACE: puts("TOKEN_LPAREN: }"); break;
        case TOKEN_SEMICOLON: puts("TOKEN_LPAREN: ;"); break;
    }
}

void print_spaces(int n) {
    while (n--) printf(" ");
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
            debug_print_ast(ast->node.ret_stmt.expr, n += 2);
            break;
        case AST_FUNC:
            print_spaces(n);
            puts("AST_FUNC:");

            n += 2;
            print_spaces(n);
            printf("id: %s\n", ast->node.function.id);

            print_spaces(n);
            puts("type: int"); // hardcoded for now

            print_spaces(n);
            puts("body:");
            debug_print_ast(ast->node.function.body, n += 2);
            break;
    }
}

void setup(FILE *fp) {
    fputs("\t.intel_syntax noprefix\n", fp);
    fputs("\t.globl main\n", fp);
}

void write_expr(FILE *fp, AST *ast) {
    fprintf(fp, "\tmov eax, %d\n", ast->node.integer.value);
}

// now only return statement
void write_stmt(FILE *fp, AST *ast) {
    write_expr(fp, ast->node.ret_stmt.expr);
    fputs("\tret\n", fp);
}

void write_func(FILE *fp, AST *ast) {
    fprintf(fp, "%s:\n", ast->node.function.id);
    write_stmt(fp, ast->node.function.body);
}

void write_ast(FILE *fp, AST *ast) {
    write_func(fp, ast);
}

