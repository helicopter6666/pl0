/* pl/0 compiler with code generation
 * Extensions combined:
 * 1. C-style comments
 * 2. else, exit
 * 3. read / write
 * 4. procedure with parameters
 * 5. boolean type and short-circuit evaluation  // 新增
 * 6. one-dimensional array type                // 新增
 * 7. function type
 * 8. real type
 */
#define _CRT_SECURE_NO_WARNINGS
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "pl0.h"
enum type current_func_type;
/* -------------------------------------------------------------
 *  新增：布尔表达式相关函数声明
 * ------------------------------------------------------------- */
void boolexpr(sym_type fsys, long* true_lab, long* false_lab);
void condition(sym_type fsys);
/* -------------------------------------------------------------
 *  Forward declarations
 * ------------------------------------------------------------- */
void expression(sym_type);
void statement(sym_type);
void block(sym_type, int is_func);
/* -------------------------------------------------------------
 *  Utility functions
 * ------------------------------------------------------------- */
void error(long n)
{
    long i;
    printf("Error=>");
    for (i = 1; i <= cc - 1; i++) printf(" ");
    printf("|%s(%d)\n", err_msg[n], n);
    err++;
}
void getch()
{
    if (cc == ll) {
        if (feof(infile)) {
            printf("************************************\n");
            printf("      program incomplete\n");
            printf("************************************\n");
            exit(1);
        }
        ll = 0; cc = 0;
        printf("%5d ", cx);
        while ((!feof(infile)) && ((ch = getc(infile)) != '\n')) {
            printf("%c", ch);
            ll = ll + 1;
            line[ll] = ch;
        }
        printf("\n");
        ll = ll + 1;
        line[ll] = ' ';
    }
    cc = cc + 1;
    ch = line[cc];
}
void getsym()
{
    long i, k;
    while (ch == ' ' || ch == '\t') getch();
    /* 处理注释 */
    if (ch == '/' && (cc + 1 <= ll) && line[cc + 1] == '*') {
        getch(); getch();
        while (1) {
            if (ch == '*' && (cc + 1 <= ll) && line[cc + 1] == '/') {
                getch(); getch();
                break;
            }
            if (feof(infile) && cc >= ll) { error(38); break; }
            getch();
        }
        getsym();
        return;
    }
    if (isalpha(ch)) {
        k = 0;
        do {
            if (k < al) a[k] = ch;
            k++;
            getch();
        } while (isalpha(ch) || isdigit(ch));
        a[k] = '\0';
        strcpy(id, a);
        sym = ident;
        for (i = 0; i < norw; i++) {
            if (strcmp(id, word[i]) == 0) {
                sym = wsym[i];
                break;
            }
        }
    }
    else if (isdigit(ch)) {
        k = 0; num = 0; sym = number;
        do {
            num = num * 10 + (ch - '0');
            k++; getch();
        } while (isdigit(ch));
        if (ch == '.') {            /* 实数 */
            double frac = 0.0, div = 10.0;
            getch();
            while (isdigit(ch)) {
                frac = frac + (ch - '0') / div;
                div *= 10.0;
                k++;
                getch();
            }
            realnum = num + frac;
            sym = realnumber;
        }
        else {
            if (k > nmax) error(31);
        }
    }
    else if (ch == ':') {
        getch();
        if (ch == '=') { sym = becomes; getch(); }
        else sym = colon;
    }
    else if (ch == '<') {
        getch();
        if (ch == '=') { sym = leq; getch(); }
        else if (ch == '>') { sym = neq; getch(); }
        else sym = lss;
    }
    else if (ch == '>') {
        getch();
        if (ch == '=') { sym = geq; getch(); }
        else sym = gtr;
    }
    /************************ 新增：数组符号识别 ************************/
    else if (ch == '[') {
        sym = lsbracket; getch();
    }
    else if (ch == ']') {
        sym = rsbracket; getch();
    }
    else if (ch == '.') {
        getch();
        if (ch == '.') { sym = range; getch(); }
        else sym = period;
    }
    /************************************************************************/
    else {
        sym = ssym[(unsigned char)ch];
        getch();
    }
}
void gen(enum fct x, long y, long z)
{
    if (cx > cxmax) {
        printf("program too long\n");
        exit(1);
    }
    code[cx].f = x;
    code[cx].l = y;
    code[cx].a = z;
    cx = cx + 1;
}
void test(sym_type s1, sym_type s2, long n)
{
    if (!(sym & s1)) {
        error(n);
        s1 = s1 | s2;
        while (!(sym & s1)) getsym();
    }
}
void enter(enum object k)		// enter object into table
{
    tx = tx + 1;
    strcpy(table[tx].name, id);
    table[tx].kind = k;
    table[tx].typ = int_type;          /* 默认整数 */
    table[tx].paracount = 0;
    switch (k) {
    case constant:
        if (num > amax) { error(31); num = 0; }
        table[tx].val = num;
        break;
    case variable:
    case proc:
        table[tx].level = lev;
        break;
    case func_obj:
        table[tx].level = lev;
        break;
        /************************ 数组类型入表 ************************/
    case ARRAY:
        table[tx].level = lev;
        table[tx].addr = dx;
        table[tx].size = 0;   // 稍后设置
        break;
    }
}
long position(char* id)         // find identifier id in table
{
    long i;
    strcpy(table[0].name, id);
    i = tx;
    while (strcmp(table[i].name, id) != 0)
    {
        i = i - 1;
    }
    return i;
}
void constdeclaration()
{
    if (sym == ident) {
        getsym();
        if (sym == eql || sym == becomes) {
            if (sym == becomes) error(1);
            getsym();
            if (sym == number || sym == realnumber) {
                if (sym == realnumber) error(31);  /* 实数常量暂存入 num 不合适，此处报错 */
                enter(constant);
                getsym();
            }
            else error(2);
        }
        else error(3);
    }
    else error(4);
}
/*修改：变量声明支持数组类型*/
void vardeclaration()
{
    if (sym == ident) {
        enter(variable);   // 先当作变量，后面可能改为数组
        table[tx].typ = int_type;   /* 默认整数 */
        getsym();
        if (sym == colon) {         /* 带类型声明 */
            getsym();
            if (sym == integersym) {
                table[tx].typ = int_type;
                getsym();
            }
            else if (sym == realsym) {
                table[tx].typ = real_type;
                getsym();
            }
            /*数组类型声明处理*/
            else if (sym == arraysym) {
                getsym(); // array
                if (sym != lsbracket) error(27);
                getsym();
                if (sym != number) error(26);
                long low = num;
                getsym();
                if (sym != range) error(29);
                getsym();
                if (sym != number) error(26);
                long high = num;
                getsym();
                if (sym != rsbracket) error(28);
                getsym();
                if (sym != ofsym) error(30);
                getsym();
                if (!(sym == integersym || sym == realsym)) error(30);
                enum type arr_type = (sym == integersym) ? int_type : real_type;
                getsym();
                // 将刚才的 variable 改为 ARRAY
                table[tx].kind = ARRAY;
                table[tx].typ = arr_type;
                table[tx].low = low;
                table[tx].high = high;
                table[tx].size = high - low + 1;
                // 修正地址：因为 enter(variable) 已分配一个空间，现在要分配 size 个
                dx = dx - 1 + table[tx].size;
                table[tx].addr = dx - table[tx].size + 1;
            }
            /************************************************************************/
            else if (sym == booleansym) {
                getsym(); // 布尔类型，仍当作整型
            }
            else error(4);
        }
    }
    else error(4);
}
void listcode(long cx0)         // list code generated for this block
{
    long i;
    for (i = cx0; i <= cx - 1; i++) {
        printf("%10d%5s%3d%5d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
    }
}
/* -------------------------------------------------------------
 *布尔表达式相关函数
 * ------------------------------------------------------------- */
static void primary(sym_type fsys, long* true_lab, long* false_lab)
{
    if (sym == notsym)
    {
        getsym();
        primary(fsys, false_lab, true_lab);   // 交换真假标签
    }
    else if (sym == truesym)
    {
        getsym();
        gen(jmp, 0, *true_lab);
    }
    else if (sym == falsesym)
    {
        getsym();
        gen(jmp, 0, *false_lab);
    }
    else if (sym == lparen)
    {
        getsym();
        boolexpr(fsys | rparen, true_lab, false_lab);
        if (sym != rparen) error(22);
        getsym();
    }
    else   // 关系表达式 或 odd
    {
        condition(fsys);            // 生成比较结果（0/1）在栈顶
        gen(jpc, 0, *false_lab);    // 若为0（假）则跳转到假出口
        gen(jmp, 0, *true_lab);     // 若为1（真）则跳转到真出口
    }
}
static void and_expr(sym_type fsys, long* true_lab, long* false_lab)
{
    primary(fsys | andsym, true_lab, false_lab);
    while (sym == andsym)
    {
        getsym();
        long lab = cx;
        gen(jpc, 0, 0);            // 左假则跳过右操作数
        primary(fsys | andsym, true_lab, false_lab);
        code[lab].a = *false_lab;   // 左假时直接到假出口
    }
}
void boolexpr(sym_type fsys, long* true_lab, long* false_lab)
{
    and_expr(fsys | orsym, true_lab, false_lab);
    while (sym == orsym)
    {
        getsym();
        long lab = cx;
        gen(jmp, 0, 0);             // 左真则跳过右操作数
        and_expr(fsys | orsym, true_lab, false_lab);
        code[lab].a = *true_lab;    // 左真时直接到真出口
    }
}
void condition(sym_type fsys)
{
    unsigned long relop;
    if (sym == oddsym)
    {
        getsym(); expression(fsys | andsym | orsym);
        gen(opr, 0, 6);
    }
    else
    {
        expression(fsys | eql | neq | lss | gtr | leq | geq);
        if (!(sym & (eql | neq | lss | gtr | leq | geq)))
        {
            error(20);
        }
        else
        {
            relop = sym; getsym();
            expression(fsys | andsym | orsym);

            switch (relop)
            {
            case eql:
                gen(opr, 0, 8);
                break;

            case neq:
                gen(opr, 0, 9);
                break;

            case lss:
                gen(opr, 0, 10);
                break;

            case geq:
                gen(opr, 0, 11);
                break;

            case gtr:
                gen(opr, 0, 12);
                break;

            case leq:
                gen(opr, 0, 13);
                break;
            }
        }
    }
}
/* -------------------------------------------------------------
 *  Expression parsing (with real types)
 * ------------------------------------------------------------- */
void factor(sym_type fsys)
{
    long i;
    test(facbegsys, fsys, 24);
    if (sym == ident) {
        i = position(id);
        if (i == 0) error(11);
        else {
            /************************ 数组元素作为因子 ************************/
            if (table[i].kind == ARRAY)
            {
                // 数组变量处理
                getsym(); // 跳过数组名arr
                if (sym != lsbracket) error(27);
                getsym(); // 跳过[
                expression(fsys | rsbracket); // 计算下标
                if (sym != rsbracket) error(28);
                getsym(); // 跳过]
                // 生成数组访问指令
                gen(lda, lev - table[i].level, table[i].addr);
                gen(lit, 0, table[i].low);
                gen(opr, 0, 3);
                gen(opr, 0, 2);
                if (table[i].typ == real_type)
                    gen(lodr, 0, 0); // 数组元素是实数
                else
                    gen(lodi, 0, 0); // 数组元素是整数
                expr_type = table[i].typ;
                // 数组处理完后，绝对不能再调用getsym()！
            }
            /************************************************************************/
            else {
                switch (table[i].kind) {
                case constant:
                    gen(lit, 0, table[i].val);
                    expr_type = int_type;
                    break;
                case variable:
                case parameter:
                    expr_type = table[i].typ;
                    if (table[i].typ == real_type)
                        gen(lodr, lev - table[i].level, table[i].addr);
                    else
                        gen(lod, lev - table[i].level, table[i].addr);
                    break;
                case func_obj:
                {
                    long func_idx = i;
                    getsym();
                    if (sym == lparen) {
                        long nargs = 0;
                        getsym();
                        /* 不再生成 INT 0,1 预留槽，直接计算实参并压栈 */
                        do {
                            expression(fsys | rparen | comma);
                            nargs++;
                            if (sym == comma) getsym();
                        } while (sym != rparen);
                        if (sym == rparen) getsym(); else error(22);
                        if (nargs != table[func_idx].paracount) error(36);
                        gen(cal, lev - table[func_idx].level, table[func_idx].addr);
                        gen(adj, 0, nargs);   /* 调整栈，留下返回值 */
                    }
                    else {
                        /* 无参函数调用（省略括号） */
                        if (table[func_idx].paracount != 0) error(36);
                        gen(cal, lev - table[func_idx].level, table[func_idx].addr);
                        gen(adj, 0, 0);
                    }
                    expr_type = table[func_idx].typ;
                }
                break;
                case proc:
                    error(21);
                    break;
                }
                if (table[i].kind != func_obj) getsym();
            }
        }
    }
    else if (sym == number) {
        if (num > amax) { error(31); num = 0; }
        gen(lit, 0, num);
        expr_type = int_type;
        getsym();
    }
    else if (sym == realnumber) {
        real_pool[real_pool_idx] = realnum;
        gen(litd, 0, real_pool_idx);
        real_pool_idx++;
        expr_type = real_type;
        getsym();
    }
    /************************布尔常量 ************************/
    else if (sym == truesym)
    {
        gen(lit, 0, 1); getsym();
        expr_type = int_type;
    }
    else if (sym == falsesym)
    {
        gen(lit, 0, 0); getsym();
        expr_type = int_type;
    }
    else if (sym == notsym)
    {
        getsym();
        factor(fsys);
        gen(opr, 0, 14);
    }
    /************************************************************************/
    else if (sym == lparen) {
        getsym();
        expression(rparen | fsys);
        if (sym == rparen) getsym(); else error(22);
    }
    test(fsys, lparen, 23);
}
void term(sym_type fsys)
{
    sym_type mulop;
    enum type left_type;
    factor(fsys | times | slash);
    left_type = expr_type;
    while (sym == times || sym == slash) {
        mulop = sym; getsym();
        factor(fsys | times | slash);
        if (expr_type != left_type) error(37);
        if (left_type == real_type) {
            if (mulop == times) gen(oprf, 0, 4); else gen(oprf, 0, 5);
        }
        else {
            if (mulop == times) gen(opr, 0, 4); else gen(opr, 0, 5);
        }
    }
}
void expression(sym_type fsys)
{
    sym_type addop;
    enum type left_type;
    if (sym == plus || sym == minus) {
        addop = sym; getsym();
        term(fsys | plus | minus);
        left_type = expr_type;
        if (addop == minus) {
            if (left_type == real_type) gen(oprf, 0, 1);
            else gen(opr, 0, 1);
        }
    }
    else {
        term(fsys | plus | minus);
        left_type = expr_type;
    }
    while (sym == plus || sym == minus) {
        addop = sym; getsym();
        term(fsys | plus | minus);
        if (expr_type != left_type) error(37);
        if (left_type == real_type) {
            if (addop == plus) gen(oprf, 0, 2);
            else gen(oprf, 0, 3);
        }
        else {
            if (addop == plus) gen(opr, 0, 2);
            else gen(opr, 0, 3);
        }
    }
}
/* -------------------------------------------------------------
 *  Statement parsing
 * ------------------------------------------------------------- */
void statement(sym_type fsys)
{
    long i, cx1, cx2;
    if (sym == ident) {
        i = position(id);
        if (i == 0) error(11);
        getsym();
        /************************ 数组元素作为赋值左值 ************************/
        if (sym == lsbracket)
        {
            // 数组访问
            if (table[i].kind != ARRAY) error(12);
            getsym();
            expression(fsys | rsbracket);   // 下标表达式
            if (sym != rsbracket) error(28);
            getsym();
            // 生成地址计算
            gen(lda, lev - table[i].level, table[i].addr);
            gen(lit, 0, table[i].low);
            gen(opr, 0, 3);   // 减
            gen(opr, 0, 2);   // 加 -> 地址在栈顶
            if (sym == becomes)
            {
                getsym();
                expression(fsys);
                if (expr_type != table[i].typ) error(37);
                if (table[i].typ == real_type)
                    gen(stor, 0, 0);   // 间接存实数
                else
                    gen(stoi, 0, 0);   // 间接存整数
            }
            else error(13);
        }
        /************************************************************************/
        else {
            // 普通变量处理
            if (table[i].kind != variable && table[i].kind != parameter) {
                error(12); i = 0;
            }
            if (sym == becomes) getsym(); else error(13);
            expression(fsys);
            if (i != 0) {
                if (table[i].typ != expr_type) error(37);
                if (table[i].typ == real_type)
                    gen(stor, lev - table[i].level, table[i].addr);
                else
                    gen(sto, lev - table[i].level, table[i].addr);
            }
        }
    }
    else if (sym == callsym) {
        long nargs;
        getsym();
        if (sym != ident) error(14);
        else {
            i = position(id);
            if (i == 0) error(11);
            else if (table[i].kind == proc) {
                getsym();
                nargs = 0;
                if (sym == lparen) {
                    getsym();
                    do {
                        expression(fsys | rparen | comma);
                        nargs++;
                        if (sym == comma) getsym();
                    } while (sym != rparen);
                    if (sym == rparen) getsym(); else error(22);
                }
                if (nargs != table[i].paracount) error(36);
                gen(cal, lev - table[i].level, table[i].addr);
            }
            else error(15);
        }
    }
    else if (sym == ifsym) {
        getsym();
        /************************ 修改：if语句使用布尔表达式 ************************/
        long true_lab, false_lab, end_lab;
        // 生成两个占位跳转指令，用于布尔表达式的真/假出口
        true_lab = cx; gen(jmp, 0, 0);
        false_lab = cx; gen(jmp, 0, 0);
        // 布尔表达式后面可以跟then
        boolexpr(fsys | thensym, &true_lab, &false_lab);
        if (sym == thensym) getsym(); else error(16);
        // 关键修复：then分支的语句后面可以跟else
        code[true_lab].a = cx;
        statement(fsys | elsesym);  // 将elsesym加入fsys集合
        // 生成跳转到if结束的指令
        end_lab = cx; gen(jmp, 0, 0);
        // 回填假分支地址
        code[false_lab].a = cx;
        // 处理else分支
        if (sym == elsesym)
        {
            getsym();
            statement(fsys);  // else分支的语句后面不需要跟else
        }
        // 回填结束地址
        code[end_lab].a = cx;
        /************************************************************************/
    }
    else if (sym == beginsym) {
        getsym();
        statement(fsys | semicolon | endsym);
        while (sym == semicolon || (sym & statbegsys)) {
            if (sym == semicolon) getsym(); else error(10);
            statement(fsys | semicolon | endsym);
        }
        if (sym == endsym) getsym(); else error(17);
    }
    else if (sym == readsym) {
        getsym();
        if (sym != lparen) error(34);
        getsym();
        do {
            if (sym != ident) error(11);
            i = position(id);
            if (i == 0) error(11);
            else if (table[i].kind != variable && table[i].kind != parameter) error(12);
            getsym();
            gen(opr, 0, 16);
            if (table[i].typ == real_type)
                gen(stor, lev - table[i].level, table[i].addr);
            else
                gen(sto, lev - table[i].level, table[i].addr);
            if (sym == comma) getsym(); else break;
        } while (1);
        if (sym != rparen) error(22);
        getsym();
    }
    else if (sym == writesym) {
        getsym();
        if (sym != lparen) error(34);
        getsym();
        do {
            expression(fsys | rparen | comma);
            if (expr_type == real_type) gen(opr, 0, 19);  /* write real */
            else gen(opr, 0, 17);
            if (sym == comma) getsym(); else break;
        } while (1);
        if (sym != rparen) error(22);
        getsym();
    }
    else if (sym == exitsym)
    {
        getsym();
        if (while_sp > 0) {
            gen(jmp, 0, 0);                            /* 生成待回填的跳转指令 */
            /* 插入当前层链表头部 */
            code[cx - 1].a = while_exit_stack[while_sp - 1];
            while_exit_stack[while_sp - 1] = cx - 1;
        }
        else {
            error(33);
        }
    }
    else if (sym == whilesym)
    {
        if (while_sp >= MAX_WHILE_NEST) error(39);
        /* 初始化当前层待回填链表为 -1（空） */
        while_exit_stack[while_sp++] = -1;
        cx1 = cx; getsym();
        /************************ 修改：while语句使用布尔表达式 ************************/
        long true_lab = cx; gen(jmp, 0, 0);
        long false_lab = cx; gen(jmp, 0, 0);
        boolexpr(fsys | dosym, &true_lab, &false_lab);
        if (sym != dosym) error(18);
        getsym();
        code[true_lab].a = cx;   // 条件为真，执行循环体
        statement(fsys);
        gen(jmp, 0, cx1);        // 跳回条件判断
        code[false_lab].a = cx;  // 条件为假，退出循环
        /************************************************************************/
        /* 回填本层所有 exit 指令的目标地址 */
        {
            long patch = while_exit_stack[--while_sp];
            while (patch != -1) {
                long next = code[patch].a;   /* 下一条待回填指令的索引 */
                code[patch].a = cx;          /* 填上真正的出口地址 */
                patch = next;
            }
        }
    }
    else if (sym == returnsym) {
        getsym();
        expression(fsys);
        if (expr_type != current_func_type) {
            error(37);   /* 类型不匹配 */
        }
        gen(opr, 0, 14);
    }
    test(fsys | elsesym, 0, 19);
}
/* -------------------------------------------------------------
 *  Block parsing (modified to support functions + real)
 * ------------------------------------------------------------- */
void block(sym_type fsys, int is_func)
{
    long tx0, cx0, tx1, dx1;
    long saved_proc_tx;
    long npar;          /* 当前子程序参数个数 */
    long func_tx = 0;   /* 函数符号表索引 */
    saved_proc_tx = proc_entry_tx;
    dx = 3; tx0 = proc_entry_tx; table[tx0].addr = cx; gen(jmp, 0, 0);
    if (lev > levmax) error(32);
    do {
        if (sym == constsym) {
            getsym();
            do {
                constdeclaration();
                while (sym == comma) { getsym(); constdeclaration(); }
                if (sym == semicolon) getsym(); else error(5);
            } while (sym == ident);
        }
        if (sym == varsym) {
            getsym();
            do {
                long var_tx_start = tx;   // 记录当前符号表指针，用于批量设置类型
                /* 收集这一行声明的所有变量名 */
                if (sym == ident) {
                    enter(variable);
                    table[tx].typ = int_type;   // 先给一个默认类型
                    getsym();
                    while (sym == comma) {
                        getsym();
                        if (sym == ident) {
                            enter(variable);
                            table[tx].typ = int_type;
                            getsym();
                        }
                        else error(4);
                    }
                    /* 如果有冒号，读取类型并应用到之前所有变量 */
                    if (sym == colon) {
                        getsym();
                        enum type var_type;
                        if (sym == integersym) {
                            var_type = int_type;
                            getsym();
                        }
                        else if (sym == realsym) {
                            var_type = real_type;
                            getsym();
                        }
                        else error(4);
                        /* 回填类型 */
                        for (long i = var_tx_start + 1; i <= tx; i++)
                            table[i].typ = var_type;
                    }
                    if (sym == semicolon) getsym();
                    else error(5);
                }
                else error(4);
            } while (sym == ident);   // 允许同一个 var 下继续声明（用分号分隔）
        }
        while (sym == procsym || sym == functionsym) {
            int isFunc = (sym == functionsym);
            long proc_idx;
            getsym();
            if (sym == ident) {
                if (isFunc) {
                    enter(func_obj);
                    table[tx].typ = int_type;   /* 默认返回整数 */
                }
                else {
                    enter(proc);
                }
                proc_idx = tx;
                getsym();
            }
            else {
                proc_idx = 0;
                error(4);
                /* 跳过错误符号，直到可以安全恢复的位置 */
                while (sym != semicolon && sym != rparen && sym != ident && sym != 0) getsym();
            }
            /* 参数列表 */
            npar = 0;
            if (sym == lparen) {
                long param_base;
                getsym();
                param_base = tx;
                while (sym != rparen) {
                    if (sym == ident) {
                        tx++;
                        strcpy(table[tx].name, id);
                        table[tx].kind = parameter;
                        table[tx].level = lev + 1;
                        table[tx].typ = int_type;
                        npar++; getsym();
                        if (sym == colon) {
                            getsym();
                            if (sym == integersym) {
                                table[tx].typ = int_type;
                                getsym();
                            }
                            else if (sym == realsym) {
                                table[tx].typ = real_type;
                                getsym();
                            }
                            else {
                                error(4);
                                /* 跳过错误，直到 , ) 或 ; */
                                while (sym != comma && sym != rparen && sym != semicolon && sym != 0) getsym();
                            }
                        }
                        if (sym == comma || sym == semicolon) getsym();
                        else if (sym != rparen) {
                            error(5);  /* 缺少分隔符或右括号 */
                            while (sym != comma && sym != semicolon && sym != rparen && sym != 0) getsym();
                            if (sym == comma || sym == semicolon) getsym();
                        }
                    }
                    else {
                        error(4);
                        while (sym != ident && sym != comma && sym != rparen && sym != semicolon && sym != 0) getsym();
                    }
                }
                if (sym == rparen) getsym(); else error(22);
                /* 参数地址分配 */
                long j;
                for (j = 0; j < npar; j++)
                    table[param_base + 1 + j].addr = -(npar - j);
            }
            if (proc_idx) {
                table[proc_idx].paracount = npar;
                if (isFunc) func_tx = proc_idx;  /* 记录函数入口 */
            }
            /* 函数返回类型声明 */
            if (isFunc && sym == colon) {
                getsym();
                if (sym == integersym) {
                    table[proc_idx].typ = int_type;
                    getsym();
                }
                else if (sym == realsym) {
                    table[proc_idx].typ = real_type;
                    getsym();
                }
                else error(4);
            }
            if (sym == semicolon) getsym(); else error(5);

            /* 保存当前函数返回类型，并设置新类型 */
            enum type saved_func_type = current_func_type;
            if (isFunc && proc_idx) {
                current_func_type = table[proc_idx].typ;
            }
            else {
                current_func_type = int_type;   /* 主程序或其他不允许 return 的情况 */
            }
            lev = lev + 1; tx1 = tx; dx1 = dx;
            if (proc_idx) proc_entry_tx = proc_idx;
            block(fsys | semicolon, isFunc);
            lev = lev - 1; tx = tx1; dx = dx1;

            /* 恢复 */
            current_func_type = saved_func_type;
            if (sym == semicolon) {
                getsym();
                test(statbegsys | ident | procsym | functionsym, fsys, 6);
            }
            else error(5);
        }
        test(statbegsys | ident | declbegsys, 0, 7);
    } while (sym & declbegsys);
    code[table[tx0].addr].a = cx;
    table[tx0].addr = cx;
    cx0 = cx; gen(Int, 0, dx + 1);
    statement(fsys | semicolon | endsym);
    /* 函数体尾部：如果最后一条不是 OPR 0,14，则添加默认返回 */
    if (is_func) {
        if (cx == cx0 || code[cx - 1].f != opr || code[cx - 1].a != 14) {
            gen(opr, 0, 14);
        }
    }
    else {
        gen(opr, 0, 0);
    }
    test(fsys, 0, 8);
    listcode(cx0);
    proc_entry_tx = saved_proc_tx;
}
long base(long b, long l)
{
    long b1 = b;
    while (l > 0) {
        b1 = s[b1].i;
        l = l - 1;
    }
    return b1;
}
void interpret()
{
    long p, b, t;
    instruction i;
    printf("start PL/0\n");
    t = 0; b = 1; p = 0;
    s[1].i = 0; s[2].i = 0; s[3].i = 0;

    do {
        i = code[p]; p = p + 1;
        switch (i.f) {
        case lit:
            t = t + 1; s[t].i = i.a;
            break;
        case litd:
            t = t + 1; s[t].r = real_pool[i.a];
            break;
        case opr:
            switch (i.a) {
            case 0: /* return */
                t = b - 1; p = s[t + 3].i; b = s[t + 2].i;
                break;
            case 1:
                s[t].i = -s[t].i;
                break;
            case 2:
                t = t - 1; s[t].i = s[t].i + s[t + 1].i;
                break;
            case 3:
                t = t - 1; s[t].i = s[t].i - s[t + 1].i;
                break;
            case 4:
                t = t - 1; s[t].i = s[t].i * s[t + 1].i;
                break;
            case 5:
                t = t - 1; s[t].i = s[t].i / s[t + 1].i;
                break;
            case 6:
                s[t].i = s[t].i % 2;
                break;
            case 8:
                t = t - 1; s[t].i = (s[t].i == s[t + 1].i);
                break;
            case 9:
                t = t - 1; s[t].i = (s[t].i != s[t + 1].i);
                break;
            case 10:
                t = t - 1; s[t].i = (s[t].i < s[t + 1].i);
                break;
            case 11:
                t = t - 1; s[t].i = (s[t].i >= s[t + 1].i);
                break;
            case 12:
                t = t - 1; s[t].i = (s[t].i > s[t + 1].i);
                break;
            case 13:
                t = t - 1; s[t].i = (s[t].i <= s[t + 1].i);
                break;
            case 14: /* 函数返回：将栈顶值复制到 b-1，恢复帧 */
                s[b - 1] = s[t];
                t = b - 1; p = s[t + 3].i; b = s[t + 2].i;
                break;
            case 16: // read integer
                t = t + 1;
                printf("? ");
                scanf("%ld", &s[t].i);
                break;
            case 17: // write integer
                printf("%10ld\n", s[t].i);
                t = t - 1;
                break;
            case 19: /* write real */
                printf("%10g\n", s[t].r);
                t = t - 1;
                break;
            }
            break;
        case lod:
            t = t + 1; s[t].i = s[base(b, i.l) + i.a].i;
            break;
        case sto:
            s[base(b, i.l) + i.a].i = s[t].i;
            t = t - 1;
            break;
        case lodr:
            t = t + 1; s[t].r = s[base(b, i.l) + i.a].r;
            break;
        case stor:
            s[base(b, i.l) + i.a].r = s[t].r;
            t = t - 1;
            break;
        case oprf:
            switch (i.a) {
            case 1: s[t].r = -s[t].r; break;
            case 2: t = t - 1; s[t].r = s[t].r + s[t + 1].r; break;
            case 3: t = t - 1; s[t].r = s[t].r - s[t + 1].r; break;
            case 4: t = t - 1; s[t].r = s[t].r * s[t + 1].r; break;
            case 5: t = t - 1; s[t].r = s[t].r / s[t + 1].r; break;
            case 8: t = t - 1; s[t].i = (s[t].r == s[t + 1].r); break;
            case 9: t = t - 1; s[t].i = (s[t].r != s[t + 1].r); break;
            case 10:t = t - 1; s[t].i = (s[t].r < s[t + 1].r); break;
            case 11:t = t - 1; s[t].i = (s[t].r >= s[t + 1].r); break;
            case 12:t = t - 1; s[t].i = (s[t].r > s[t + 1].r); break;
            case 13:t = t - 1; s[t].i = (s[t].r <= s[t + 1].r); break;
            }
            break;
        case cal:
            s[t + 1].i = base(b, i.l);
            s[t + 2].i = b;
            s[t + 3].i = p;
            b = t + 1; p = i.a;
            break;
        case Int:
            t = t + i.a;
            break;
        case jmp:
            p = i.a;
            break;
        case jpc:
            if (s[t].i == 0) p = i.a;
            t = t - 1;
            break;
        case adj:
            if (i.a > 1) {
                s[t - (i.a - 1)] = s[t];
                t = t - (i.a - 1);
            }
            break;
            /************************ 数组指令实现 ************************/
        case lda:
            t = t + 1;
            s[t].i = base(b, i.l) + i.a;
            break;
        case lodi:
            s[t].i = s[s[t].i].i;   // 栈顶是地址，将其内容替换为该地址的值
            break;
        case stoi:
            s[s[t - 1].i].i = s[t].i;
            t = t - 2;
            break;
            /************************************************************************/
        }
    } while (p != 0);
    printf("end PL/0\n");
}
/* -------------------------------------------------------------
 *  Main
 * ------------------------------------------------------------- */
int main()
{
    long i;
    for (i = 0; i < 256; i++) ssym[i] = nul;
    /* 保留字（按字典序方便维护） */
    strcpy(word[0], "and");      /************************ 新增 ************************/
    strcpy(word[1], "array");    /************************ 新增 ************************/
    strcpy(word[2], "begin");
    strcpy(word[3], "boolean");  /************************ 新增 ************************/
    strcpy(word[4], "call");
    strcpy(word[5], "const");
    strcpy(word[6], "do");
    strcpy(word[7], "else");
    strcpy(word[8], "end");
    strcpy(word[9], "exit");
    strcpy(word[10], "false");    /************************ 新增 ************************/
    strcpy(word[11], "function");
    strcpy(word[12], "if");
    strcpy(word[13], "integer");
    strcpy(word[14], "not");      /************************ 新增 ************************/
    strcpy(word[15], "odd");
    strcpy(word[16], "of");       /************************ 新增 ************************/
    strcpy(word[17], "or");       /************************ 新增 ************************/
    strcpy(word[18], "procedure");
    strcpy(word[19], "read");
    strcpy(word[20], "real");
    strcpy(word[21], "return");
    strcpy(word[22], "then");
    strcpy(word[23], "true");     /************************ 新增 ************************/
    strcpy(word[24], "var");
    strcpy(word[25], "while");
    strcpy(word[26], "write");
    /* 对应wsym数组 */
    wsym[0] = andsym;      /************************ 新增 ************************/
    wsym[1] = arraysym;    /************************ 新增 ************************/
    wsym[2] = beginsym;
    wsym[3] = booleansym;  /************************ 新增 ************************/
    wsym[4] = callsym;
    wsym[5] = constsym;
    wsym[6] = dosym;
    wsym[7] = elsesym;
    wsym[8] = endsym;
    wsym[9] = exitsym;
    wsym[10] = falsesym;    /************************ 新增 ************************/
    wsym[11] = functionsym;
    wsym[12] = ifsym;
    wsym[13] = integersym;
    wsym[14] = notsym;      /************************ 新增 ************************/
    wsym[15] = oddsym;
    wsym[16] = ofsym;       /************************ 新增 ************************/
    wsym[17] = orsym;       /************************ 新增 ************************/
    wsym[18] = procsym;
    wsym[19] = readsym;
    wsym[20] = realsym;
    wsym[21] = returnsym;
    wsym[22] = thensym;
    wsym[23] = truesym;     /************************ 新增 ************************/
    wsym[24] = varsym;
    wsym[25] = whilesym;
    wsym[26] = writesym;
    ssym['+'] = plus;
    ssym['-'] = minus;
    ssym['*'] = times;
    ssym['/'] = slash;
    ssym['('] = lparen;
    ssym[')'] = rparen;
    ssym['='] = eql;
    ssym[','] = comma;
    ssym['.'] = period;
    ssym[';'] = semicolon;
    ssym['['] = lsbracket;   /************************ 新增 ************************/
    ssym[']'] = rsbracket;   /************************ 新增 ************************/
    /* 指令助记符 */
    strcpy(mnemonic[lit], "LIT");
    strcpy(mnemonic[opr], "OPR");
    strcpy(mnemonic[lod], "LOD");
    strcpy(mnemonic[sto], "STO");
    strcpy(mnemonic[cal], "CAL");
    strcpy(mnemonic[Int], "INT");
    strcpy(mnemonic[jmp], "JMP");
    strcpy(mnemonic[jpc], "JPC");
    strcpy(mnemonic[litd], "LITD");
    strcpy(mnemonic[lodr], "LODR");
    strcpy(mnemonic[stor], "STOR");
    strcpy(mnemonic[oprf], "OPRF");
    strcpy(mnemonic[adj], "ADJ");
    strcpy(mnemonic[lda], "LDA");    /************************ 新增 ************************/
    strcpy(mnemonic[lodi], "LDI");    /************************ 新增 ************************/
    strcpy(mnemonic[stoi], "STI");    /************************ 新增 ************************/
    /* 符号集合 */
    declbegsys = constsym | varsym | procsym | functionsym;
    statbegsys = beginsym | callsym | ifsym | whilesym | readsym | writesym | exitsym;
    facbegsys = ident | number | lparen | realnumber | truesym | falsesym | notsym;
    printf("please input source program file name: ");
    scanf("%s", infilename);
    printf("\n");
    if ((infile = fopen(infilename, "r")) == NULL) {
        printf("File %s can't be opened.\n", infilename);
        exit(1);
    }
    err = 0;
    cc = 0; cx = 0; ll = 0; ch = ' '; getsym();
    lev = 0; tx = 0; proc_entry_tx = 0;
    real_pool_idx = 0;
    block(declbegsys | statbegsys | period, 0);   /* 主程序不是函数 */
    if (sym != period) error(9);
    if (err == 0) {
        interpret();
    }
    else {
        printf("errors in PL/0 program\n");
    }
    fclose(infile);
    return 0;
}