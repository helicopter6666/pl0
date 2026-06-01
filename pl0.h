#include <stdio.h>
#include <stdint.h>
typedef uint64_t sym_type;
#define norw        27             /* 保留字总数：原有19个 + 新增8个布尔/数组相关 */
#define txmax       100
#define nmax        14
#define al          10
#define amax        2047
#define levmax      3
#define cxmax       2000
/* 原有符号 */
#define nul         0x1
#define ident       0x2
#define number      0x4
#define plus        0x8
#define minus       0x10
#define times       0x20
#define slash       0x40
#define oddsym      0x80
#define eql         0x100
#define neq         0x200
#define lss         0x400
#define leq         0x800
#define gtr         0x1000
#define geq         0x2000
#define lparen      0x4000
#define rparen      0x8000
#define comma       0x10000
#define semicolon   0x20000
#define period      0x40000
#define becomes     0x80000
#define beginsym    0x100000
#define endsym      0x200000
#define ifsym       0x400000
#define thensym     0x800000
#define whilesym    0x1000000
#define dosym       0x2000000
#define callsym     0x4000000
#define constsym    0x8000000
#define varsym      0x10000000
#define procsym     0x20000000
/* 原有新符号 */
#define elsesym     0x40000000
#define exitsym     0x80000000
#define readsym     0x100000000ULL
#define writesym    0x200000000ULL
#define colon       0x400000000ULL
#define functionsym 0x800000000ULL
#define returnsym   0x1000000000ULL
#define integersym  0x2000000000ULL
#define realsym     0x4000000000ULL
#define realnumber  0x8000000000ULL
/************************ 新增：布尔和数组相关符号 ************************/
#define andsym      0x10000000000ULL
#define orsym       0x20000000000ULL
#define notsym      0x40000000000ULL
#define truesym     0x80000000000ULL
#define falsesym    0x100000000000ULL
#define booleansym  0x200000000000ULL
#define arraysym    0x400000000000ULL
#define ofsym       0x800000000000ULL
#define lsbracket   0x1000000000000ULL   // [
#define rsbracket   0x2000000000000ULL   // ]
#define range       0x4000000000000ULL  // ..
/************************************************************************/
enum object {
    constant,
    variable,
    parameter,
    proc,
    func_obj,
    ARRAY
};
enum type {
    int_type,
    real_type
};
enum fct {
    lit, opr, lod, sto, cal, Int, jmp, jpc,
    litd,          /* 加载实数常量 */
    lodr, stor,    /* 实数变量存取 */
    oprf,          /* 实数运算 */
    adj,           /* 栈调整指令 */
    lda, lodi, stoi/*数组专用指令*/
};
/* 栈元素：既是整数，又是实数 */
typedef union {
    long i;
    double r;
} stack_entry;
typedef struct {
    enum fct f;
    long l;
    long a;
} instruction;
char* err_msg[] = {
    /*  0 */    "",
    /*  1 */    "Found ':=' when expecting '='.",
    /*  2 */    "There must be a number to follow '='.",
    /*  3 */    "There must be an '=' to follow the identifier.",
    /*  4 */    "There must be an identifier to follow 'const', 'var', or 'procedure'.",
    /*  5 */    "Missing ',' or ';'.",
    /*  6 */    "Incorrect procedure name.",
    /*  7 */    "Statement expected.",
    /*  8 */    "Follow the statement is an incorrect symbol.",
    /*  9 */    "'.' expected.",
    /* 10 */    "';' expected.",
    /* 11 */    "Undeclared identifier.",
    /* 12 */    "Illegal assignment.",
    /* 13 */    "':=' expected.",
    /* 14 */    "There must be an identifier to follow the 'call'.",
    /* 15 */    "A constant or variable can not be called.",
    /* 16 */    "'then' expected.",
    /* 17 */    "';' or 'end' expected.",
    /* 18 */    "'do' expected.",
    /* 19 */    "Incorrect symbol.",
    /* 20 */    "Relative operators expected.",
    /* 21 */    "Procedure identifier can not be in an expression.",
    /* 22 */    "Missing ')'.",
    /* 23 */    "The symbol can not be followed by a factor.",
    /* 24 */    "The symbol can not be as the beginning of an expression.",
    /************************ 新增：数组相关错误提示 ************************/
    /* 25 */    "Array index must be integer.",
    /* 26 */    "Array bounds must be constants.",
    /* 27 */    "Missing '['.",
    /* 28 */    "Missing ']'.",
    /* 29 */    "Missing '..'.",
    /* 30 */    "Type mismatch.",
    /************************************************************************/
    /* 31 */    "The number is too great.",
    /* 32 */    "There are too many levels.",
    /* 33 */    "exit not in while",
    /* 34 */    "'(' expected",
    /* 35 */    "':' expected",
    /* 36 */    "argument count mismatch",
    /* 37 */    "type mismatch",
    /* 38 */    "Comment not closed",
    /* 39 */    "while nesting too deep",
};
char ch;
sym_type sym;
char id[al + 1];
long num;
double realnum;          /* 实数常量值 */
long cc;
long ll;
long kk, err;
long cx;
char line[81];
char a[al + 1];
instruction code[cxmax + 1];
char word[norw][al + 1];
sym_type wsym[norw];
sym_type ssym[256];
char mnemonic[17][5];    /* 原有14个 + 新增3个数组指令 */
sym_type declbegsys, statbegsys, facbegsys;
struct {
    char name[al + 1];
    enum object kind;
    enum type typ;       /* 变量、参数或函数的类型 */
    long val;
    long level;
    long addr;
    long paracount;      /* 参数个数，对 proc / func_obj 有效 */
    /************************ 新增：数组专用字段 ************************/
    long low, high;      // 数组下界和上界
    long size;           // 数组元素个数
    /************************************************************************/
} table[txmax + 1];
char infilename[80];
FILE* infile;
long dx;
long lev;
long tx;
long proc_entry_tx;
#define stacksize 50000
stack_entry s[stacksize];   /* 数据栈，改为 union */
double real_pool[500];      /* 实数常量池 */
long real_pool_idx;
#define MAX_WHILE_NEST 32
long while_exit_stack[MAX_WHILE_NEST];
long while_sp;
/* 当前表达式的类型，用于类型检查 */
enum type expr_type;