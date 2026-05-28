// pl/0 compiler with code generation

#define _CRT_SECURE_NO_WARNINGS
#define MAX_WHILE_NEST 32
#include <stdlib.h>
#include <string.h>
#include "pl0.h"
#include <ctype.h>

long while_exit_stack[MAX_WHILE_NEST];
long while_sp = 0;


void error(long n)
{
    long i;

    printf("Error=>");
    for (i = 1; i <= cc-1; i++)
    {
        printf(" ");
    }
    
    printf("|%s(%d)\n", err_msg[n], n);
    
    err++;
}

void getch()
{
    if(cc == ll)
    {
        if(feof(infile))
        {
            printf("************************************\n");
            printf("      program incomplete\n");
            printf("************************************\n");
            exit(1);
        }
        
        ll = 0; cc = 0;
        
        printf("%5d ", cx);
        
        while((!feof(infile)) && ((ch=getc(infile))!='\n'))
        {
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

    // 处理 /* 注释
    if (ch == '/' && (cc + 1 <= ll) && line[cc + 1] == '*') {
        getch(); getch();
        while (1) {
            if (ch == '*' && (cc + 1 <= ll) && line[cc + 1] == '/') {
                getch(); getch();
                break;
            }
            if (feof(infile) && cc >= ll) { error(99); break; }
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
        if (k > nmax) error(31);
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
    else {
        sym = ssym[(unsigned char)ch]; getch();
    }
}

void gen(enum fct x, long y, long z)
{
    if(cx > cxmax)
    {
        printf("program too long\n");
        exit(1);
    }
    
    code[cx].f = x; code[cx].l = y; code[cx].a = z;
    
    cx = cx + 1;
}

void test(unsigned long long s1, unsigned long long s2, long n)
{
    if (!(sym & s1))
    {
        error(n);
        s1 = s1 | s2;
        
        while(!(sym & s1))
        {
            getsym();
        }
    }
}

void enter(enum object k)		// enter object into table
{
    tx = tx + 1;

    strcpy(table[tx].name, id);
    
    table[tx].kind = k;
    
    switch(k)
    {
        case constant:
            if(num > amax)
            {
                error(31);
                num = 0;
            }
            table[tx].val = num;
            break;

        case variable:
            table[tx].level = lev; table[tx].addr = dx; dx = dx + 1;
            break;

        case parameter:
            table[tx].level = lev; table[tx].addr = dx; dx = dx + 1;
            break;

        case proc:
            table[tx].level = lev;
            break;
    }
}

long position(char* id)         // find identifier id in table
{
    long i;

    strcpy(table[0].name, id);
 
    i=tx;

    while(strcmp(table[i].name, id) != 0)
    {
        i = i - 1;
    }

    return i;
}

void constdeclaration()
{
    if(sym == ident)
    {
        getsym();
    
        if(sym == eql || sym == becomes)
        {
            if(sym == becomes)
            {
                error(1);
            }
            
            getsym();
            
            if(sym == number)
            {
                enter(constant);
                getsym();
            }
            else
            {
                error(2);
            }
        }
        else
        {
            error(3);
        }
    }
    else
    {
        error(4);
    }
}

void vardeclaration()
{
    if(sym == ident)
    {
        enter(variable); 
        getsym();
    }
    else
    {
        error(4);
    }
}

void listcode(long cx0)         // list code generated for this block
{
    long i;

    for(i=cx0; i<=cx-1; i++)
    {
        printf("%10d%5s%3d%5d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
    }
}

void expression(unsigned long long);

void factor(unsigned long long fsys)
{
    long i;

    test(facbegsys, fsys, 24);

    while(sym & facbegsys)
    {
        if(sym == ident)
        {
            i = position(id);
            
            if(i==0)
            {
                error(11);
            }
            else
            {
                switch(table[i].kind)
                {
                    case constant:
                        gen(lit, 0, table[i].val);
                        break;

                    case variable:
                    case parameter:
                        gen(lod, lev-table[i].level, table[i].addr);
                        break;

                    case proc:
                        error(21);
                        break;
                }
            }

            getsym();
        }
        else if(sym == number)
        {
            if(num>amax)
            {
                error(31); num=0;
            }
            
            gen(lit,0,num);
            getsym();
        }
        else if(sym == lparen)
        {
            getsym();
            expression(rparen|fsys);
        
            if(sym==rparen)
            {
                getsym();
            }
            else
            {
                error(22);
            }
        }

        test(fsys,lparen,23);
    }
}

void term(unsigned long long fsys)
{
    unsigned long long mulop;

    factor(fsys|times|slash);

    while(sym==times || sym==slash)
    {
        mulop = sym; getsym();
    
        factor(fsys|times|slash);
        
        if(mulop == times)
        {
            gen(opr,0,4);
        }
        else{
            gen(opr,0,5);
        }
    }
}

void expression(unsigned long long fsys)
{
    unsigned long long addop;

    if(sym==plus || sym==minus)
    {
        addop=sym; getsym();
       
        term(fsys|plus|minus);
       
        if(addop==minus)
        {
            gen(opr,0,1);
        }
    }
    else
    {
        term(fsys|plus|minus);
    }
    
    while(sym==plus || sym==minus)
    {
        addop=sym; getsym();
    
        term(fsys|plus|minus);
    
        if(addop==plus)
        {
            gen(opr,0,2);
        }
        else
        {
            gen(opr,0,3);
        }
    }
}

void condition(unsigned long long fsys)
{
    unsigned long long relop;

    if(sym==oddsym)
    {
        getsym(); expression(fsys);
        gen(opr, 0, 6);
    }
    else
    {
        expression(fsys|eql|neq|lss|gtr|leq|geq);

        if(!(sym&(eql|neq|lss|gtr|leq|geq)))
        {
            error(20);
        }
        else
        {
            relop=sym; getsym();

            expression(fsys);
            
            switch(relop)
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

void statement(unsigned long long fsys)
{
    long i, cx1, cx2;

    if (sym == ident)
    {
        i = position(id);
        if (i == 0) error(11);
        else if (table[i].kind != variable && table[i].kind != parameter) { error(12); i = 0; }
        getsym();
        if (sym == becomes) getsym(); else error(13);
        expression(fsys);
        if (i != 0) gen(sto, lev - table[i].level, table[i].addr);
    }
    else if (sym == callsym)
    {
        long nargs;
        getsym();
        if (sym != ident) error(14);
        else
        {
            i = position(id);
            if (i == 0) error(11);
            else if (table[i].kind == proc)
            {
                getsym();
                nargs = 0;
                if (sym == lparen)
                {
                    getsym();
                    do
                    {
                        expression(fsys | rparen | comma);
                        nargs++;
                        if (sym == comma) getsym();
                    } while (sym != rparen);
                    if (sym == rparen) getsym(); else error(22);
                }
                if (nargs != table[i].paracount) error(36);
                gen(cal, lev - table[i].level, table[i].addr);
            }
            else { error(15); getsym(); }
        }
    }
    else if (sym == ifsym)
    {
        getsym();
        condition(fsys | thensym | dosym);
        if (sym == thensym) getsym(); else error(16);
        cx1 = cx; gen(jpc, 0, 0);          // 条件为假时跳转（暂时未知地址）
        statement(fsys | elsesym);                  // then 部分        
        if (sym == elsesym)
        {
            getsym();
            cx2 = cx; gen(jmp, 0, 0);      // then 部分结束后跳过 else
            code[cx1].a = cx;              // 条件假时跳转到 else 部分开头
            statement(fsys);                // else 部分
            code[cx2].a = cx;              // then 部分结束后跳转到整个 if 后面
        }
        else
        {
            code[cx1].a = cx;              // 条件假时跳转到整个 if 后面
        }
    }
    else if (sym == beginsym)
    {
        getsym();
        statement(fsys | semicolon | endsym);
        while (sym == semicolon || (sym & statbegsys))
        {
            if (sym == semicolon) getsym(); else error(10);
            statement(fsys | semicolon | endsym);
        }
        if (sym == endsym) getsym(); else error(17);
    }
    else if (sym == readsym)
    {
        getsym();
        if (sym != lparen) error(34);
        getsym();
        do
        {
            if (sym != ident) error(11);
            i = position(id);
            if (i == 0) error(11);
            else if (table[i].kind != variable && table[i].kind != parameter) error(12);
            getsym();
            gen(opr, 0, 16);
            gen(sto, lev - table[i].level, table[i].addr);
            if (sym == comma) getsym(); else break;
        } while (1);
        if (sym != rparen) error(22);
        getsym();
    }
    else if (sym == writesym)
    {
        getsym();
        if (sym != lparen) error(34);
        getsym();
        do
        {
            expression(fsys | rparen | comma);
            gen(opr, 0, 17);
            if (sym == comma) getsym(); else break;
        } while (1);
        if (sym != rparen) error(22);
        getsym();
    }
    else if (sym == exitsym)
    {
        getsym();
        if (while_sp > 0) gen(jmp, 0, while_exit_stack[while_sp - 1]);
        else error(33);
    }
    else if (sym == whilesym)
    {
        if (while_sp >= MAX_WHILE_NEST) error(100);
        else while_exit_stack[while_sp++] = 0;
        cx1 = cx; getsym();
        condition(fsys | dosym);
        cx2 = cx; gen(jpc, 0, 0);
        if (sym == dosym) getsym(); else error(18);
        statement(fsys);
        gen(jmp, 0, cx1);
        code[cx2].a = cx;
        while_exit_stack[--while_sp] = cx;
    }
    test(fsys, 0, 19);
}

void block(unsigned long long fsys)
{
    long tx0;		// initial table index
    long cx0; 		// initial code index
    long tx1;		// save current table index before processing nested procedures
    long dx1;		// save data allocation index
    long saved_proc_tx;	// save proc_entry_tx for nested procedures
    long npar;		// parameter count for current procedure

    saved_proc_tx = proc_entry_tx;

    dx=3; tx0 = proc_entry_tx; table[tx0].addr=cx; gen(jmp,0,0);

    if(lev>levmax)
    {
        error(32);
    }
    
    do
    {
        if(sym==constsym)
        {
            getsym();
        
            do
            {
                constdeclaration();
                while(sym==comma)
                {
                    getsym(); constdeclaration();
                }
                
                if(sym==semicolon)
                {
                    getsym();
                }
                else
                {
                    error(5);
                }
            } while(sym==ident);
        }

        if(sym==varsym)
        {
            getsym();
            do
            {
                vardeclaration();
                while(sym==comma)
                {
                    getsym(); vardeclaration();
                }
                
                if(sym==semicolon)
                {
                    getsym();
                }
                else
                {
                    error(5);
                }
            } while(sym==ident);
        }

        while(sym==procsym)
        {
            long proc_idx;

            getsym();
            if(sym==ident)
            {
                enter(proc); proc_idx = tx; getsym();
            }
            else
            {
                proc_idx = 0; error(4);
            }

            npar = 0;
            if (sym == lparen)
            {
                long param_base;
                getsym();
                param_base = tx;
                do
                {
                    if (sym == ident)
                    {
                        tx++;
                        strcpy(table[tx].name, id);
                        table[tx].kind = parameter;
                        table[tx].level = lev + 1;
                        npar++; getsym();
                    }
                    else error(4);
                    if (sym == comma) getsym();
                } while (sym != rparen);
                if (sym == rparen) getsym(); else error(22);
                /* assign negative addresses: first param at -npar, last at -1 */
                {
                    long j;
                    for (j = 0; j < npar; j++)
                        table[param_base + 1 + j].addr = -(npar - j);
                }
            }
            if (proc_idx) table[proc_idx].paracount = npar;

            if(sym==semicolon)
            {
                getsym();
            }
            else
            {
                error(5);
            }

            lev=lev+1; tx1=tx; dx1=dx;
            if (proc_idx) proc_entry_tx = proc_idx;
            block(fsys|semicolon);
            lev=lev-1; tx=tx1; dx=dx1;

            if(sym==semicolon)
            {
                getsym();
                test(statbegsys|ident|procsym,fsys,6);
            }
            else
            {
                error(5);
            }
        }
        
        test(statbegsys|ident,declbegsys,7);
    } while(sym&declbegsys);
    
    code[table[tx0].addr].a=cx;
    table[tx0].addr=cx;		// start addr of code
    cx0=cx; gen(Int,0,dx);
    statement(fsys|semicolon|endsym);
    gen(opr,0,0); // return
    test(fsys,0,8);
    listcode(cx0);
    proc_entry_tx = saved_proc_tx;
}

long base(long b, long l)
{
    long b1;

    b1=b;

    while (l>0)         // find base l levels down
    {
        b1=s[b1]; l=l-1;
    }

    return b1;
}

void interpret()
{
    long p,b,t;		// program-, base-, topstack-registers
    instruction i;	// instruction register

    printf("start PL/0\n");
    t=0; b=1; p=0;
    s[1]=0; s[2]=0; s[3]=0;
    
    do
    {
        i=code[p]; p=p+1;
    
        switch(i.f)
        {
            case lit:
                t=t+1; s[t]=i.a;
                break;
        
            case opr:
                switch(i.a) 	// operator
                {
                    case 0:	// return
                        t=b-1; p=s[t+3]; b=s[t+2];
                        break;
                 
                    case 1:
                        s[t]=-s[t];
                        break;
                 
                    case 2:
                        t=t-1; s[t]=s[t]+s[t+1];
                        break;
                 
                    case 3:
                        t=t-1; s[t]=s[t]-s[t+1];
                        break;
                 
                    case 4:
                        t=t-1; s[t]=s[t]*s[t+1];
                        break;
                 
                    case 5:
                        t=t-1; s[t]=s[t]/s[t+1];
                        break;
                 
                    case 6:
                        s[t]=s[t]%2;
                        break;
                 
                    case 8:
                        t=t-1; s[t]=(s[t]==s[t+1]);
                        break;
                 
                    case 9:
                        t=t-1; s[t]=(s[t]!=s[t+1]);
                        break;
                 
                    case 10:
                        t=t-1; s[t]=(s[t]<s[t+1]);
                        break;
                 
                    case 11:
                        t=t-1; s[t]=(s[t]>=s[t+1]);
                        break;
                 
                    case 12:
                        t=t-1; s[t]=(s[t]>s[t+1]);
                        break;
                 
                    case 13:
                        t=t-1; s[t]=(s[t]<=s[t+1]);
                        break;
                    
                    //解释器扩展
                    case 16: // read
                        t = t + 1;
                        printf("? ");
                        scanf("%ld", &s[t]);
                        break;

                    case 17: // write
                        printf("%10ld\n", s[t]);
                        t = t - 1;
                        break;
                }
                break;
            
            case lod:
                t=t+1; s[t]=s[base(b,i.l)+i.a];
                break;
            
            case sto:
                s[base(b,i.l)+i.a]=s[t];t=t-1;
                break;
            
            case cal:		// generate new block mark
                s[t+1]=base(b,i.l); s[t+2]=b; s[t+3]=p;
                b=t+1; p=i.a;
                break;
            
            case Int:
                t=t+i.a;
                break;
            
            case jmp:
                p=i.a;
                break;
            
            case jpc:
                if(s[t]==0)
                {
                    p=i.a;
                }
                t=t-1;
        }
    } while(p!=0);
    printf("end PL/0\n");
}

int main()
{
    long i;

    for(i=0; i<256; i++)
    {
        ssym[i]=nul;
    }
   

    //增加的保留字
    strcpy(word[0], "begin");
    strcpy(word[1], "call");
    strcpy(word[2], "const");
    strcpy(word[3], "do");
    strcpy(word[4], "end");
    strcpy(word[5], "if");
    strcpy(word[6], "odd");
    strcpy(word[7], "procedure");
    strcpy(word[8], "then");
    strcpy(word[9], "var");
    strcpy(word[10], "while");
    strcpy(word[11], "else");
    strcpy(word[12], "exit");
    strcpy(word[13], "read");
    strcpy(word[14], "write");

    wsym[0] = beginsym;
    wsym[1] = callsym;
    wsym[2] = constsym;
    wsym[3] = dosym;
    wsym[4] = endsym;
    wsym[5] = ifsym;
    wsym[6] = oddsym;
    wsym[7] = procsym;
    wsym[8] = thensym;
    wsym[9] = varsym;
    wsym[10] = whilesym;
    wsym[11] = elsesym;
    wsym[12] = exitsym;
    wsym[13] = readsym;
    wsym[14] = writesym;
    ssym['+']=plus;
    ssym['-']=minus;
    ssym['*']=times;
    ssym['/']=slash;
    ssym['(']=lparen;
    ssym[')']=rparen;
    ssym['=']=eql;
    ssym[',']=comma;
    ssym['.']=period;
    ssym[';']=semicolon;
  
    strcpy(mnemonic[lit],"LIT");
    strcpy(mnemonic[opr],"OPR");
    strcpy(mnemonic[lod],"LOD");
    strcpy(mnemonic[sto],"STO");
    strcpy(mnemonic[cal],"CAL");
    strcpy(mnemonic[Int],"INT");
    strcpy(mnemonic[jmp],"JMP");
    strcpy(mnemonic[jpc],"JPC");
  
    declbegsys=constsym|varsym|procsym;
    statbegsys = beginsym | callsym | ifsym | whilesym | readsym | writesym | exitsym;
    facbegsys=ident|number|lparen;

    printf("please input source program file name: ");
    scanf("%s",infilename);
    printf("\n");
  
    if((infile=fopen(infilename,"r"))==NULL)
    {
        printf("File %s can't be opened.\n", infilename);
        exit(1);
    }

    err=0;
    cc=0; cx=0; ll=0; ch=' '; getsym();
    lev=0; tx=0; proc_entry_tx = 0;
    block(declbegsys|statbegsys|period);
    
    if(sym!=period)
    {
        error(9);
    }
    
    if(err==0)
    {
        //只生成 P‑code 而不执行，
        //为了验证输入输出和过程调用，需要启用解释器并增加新指令
        interpret();
    }
    else
    {
        printf("errors in PL/0 program\n");
    }
    
    fclose(infile);

    return (0);
}
