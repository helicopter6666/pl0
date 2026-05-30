#include <stdio.h> //你的 PL/0 源码确实是由 GCC 编译并运行的；但你写的 PL/0 语言源程序，是由你自己的程序（也就是编译后的 PL/0 编译器）来解析的

#define norw        11             // no. of reserved words 关键字最大数量
#define txmax       100            // length of identifier table 符号表的最大容量。整个 PL/0 程序中，最多只能同时定义 100 个变量、常量和过程名。
#define nmax        14             // max. no. of digits in numbers 允许输入数字的最大位数
#define al          10             // length of identifiers 标识符（变量名、过程名等）的最大有效长度。如果定义的变量名超过 10 个字符，编译器只会截取前 10 个字符。
#define amax        2047           // maximum address 数据段的最大地址空间（代码生成时的最大相对地址偏移量）
#define levmax      3              // maximum depth of block nesting 块嵌套的最大深度。PL/0 支持过程（procedure）的嵌套，这里限制最多只能嵌套 3 层。
#define cxmax       2000           // size of code array 生成的类 P-Code 目标代码数组的最大长度。即整个程序编译后，最多只能包含 2000 条中间代码指令。

#define nul         0x1 //空标记或未识别的非法字符。
#define ident       0x2 //标识符，表示变量名、过程名等。
#define number      0x4 
#define plus        0x8 
#define minus       0x10
#define times       0x20 //乘号
#define slash       0x40 //除号
#define oddsym      0x80 //odd 关键字，表示判断一个数是否为奇数的操作符。
#define eql         0x100
#define neq         0x200 //不等于操作符
#define lss         0x400 //小于操作符
#define leq         0x800 //小于等于操作符
#define gtr         0x1000 //大于操作符
#define geq         0x2000 //大于等于操作符
#define lparen      0x4000 //左括号
#define rparen      0x8000 //右括号
#define comma       0x10000 //逗号
#define semicolon   0x20000 //分号
#define period      0x40000 //句号
#define becomes     0x80000 //赋值符号 ':='，用于变量赋值操作。
#define beginsym    0x100000 //begin 关键字，表示一个块的开始。
#define endsym      0x200000
#define ifsym       0x400000
#define thensym     0x800000
#define whilesym    0x1000000
#define dosym       0x2000000
#define callsym     0x4000000
#define constsym    0x8000000
#define varsym      0x10000000
#define procsym     0x20000000 //procedure 关键字，表示过程定义的开始。

enum object 
{
    constant, variable, proc
};

enum fct //定义的是 PL/0 虚拟机（类 P-Code 虚拟机）的指令集（Opcode / 操作码）
{
    lit, opr, lod, sto, cal, Int, jmp, jpc
};

/*  lit 0, a : load constant a 
    opr 0, a : execute operation a
    lod l, a : load variable l, a
    sto l, a : store variable l, a
    cal l, a : call procedure a at level l
    Int 0, a : increment t-register by a
    jmp 0, a : jump to a
    jpc 0, a : jump conditional to a       */
typedef struct
{
    enum fct f;		// function code
    long l; 		// level
    long a; 		// displacement address
} instruction;

char* err_msg[] =
{
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
/* 25 */    "",
/* 26 */    "",	
/* 27 */    "",
/* 28 */    "",
/* 29 */    "",
/* 30 */    "",
/* 31 */    "The number is too great.",
/* 32 */    "There are too many levels."
};


char ch;               // last character read
unsigned long sym;     // last symbol read
char id[al+1];         // last identifier read多出的一位用来放字符串结束符 \0
//PL/0 不是直接每次都从硬盘文件里读一个字符，而是按行把源码读入一个内存缓冲区（通常叫 line 数组），然后再在内存里处理。
// 下面三个变量用来管理这个行缓冲区：
long num;              // last number read
long cc;               // character count (针对每一行）每次 getch() 读走一个 ch，cc 就会加 1。
long ll;               // line length 当前读入的这一行源码的实际总长度

long kk, err;
long cx;               // code allocation index

char line[81];
char a[al+1];
instruction code[cxmax+1]; //不用0号下标所以+1
char word[norw][al+1]; //关键字表。这个二维字符数组存储了 PL/0 语言中的所有保留字（如 begin、call、const 等）。每个保留字占用一行，长度为 al+1（10 个字符加上一个字符串结束符 \0）。通过这个表，编译器可以快速地识别出输入的标识符是否是一个保留字，从而进行正确的语法分析和代码生成。
unsigned long wsym[norw]; //保留字对应的符号值数组。比如 wsym[0] 就是 beginsym，wsym[1] 就是 callsym，以此类推。这个数组的索引和 word 数组中的保留字是一一对应的关系。通过这个数组，编译器可以快速地将一个保留字转换成它对应的符号值，从而在语法分析阶段进行正确的处理。
unsigned long ssym[256]; //查ASCII码表，ssym['+'] 就是 plus，ssym['-'] 就是 minus，以此类推

char mnemonic[8][3+1];
unsigned long declbegsys, statbegsys, facbegsys; //声明语句、语句、因子开始符号集合。编译器在分析过程中会用到这些集合来判断当前读入的符号是否是一个合法的开始符号，从而进行正确的语法分析。

struct
{
    char name[al+1];
    enum object kind;
    long val;
    long level;
    long addr;
}table[txmax+1];

char infilename[80];
FILE* infile;

// the following variables for block
long dx;		// data allocation index
long lev;		// current depth of block nesting
long tx;		// current table index

// the following array space for interpreter
#define stacksize 50000
long s[stacksize];	// datastore
