/*
	Parser based on https://en.wikipedia.org/wiki/Recursive_descent_parser
	If EMULATOR is defined, this file is contained in the IBM 701 emulator
	(basepath.com/701). Otherwise, it's a standalong console program.

	Written by Marc Rochkind, June 2019. Released into the public domain.

		program = block "." .

		block = [ "const" ident "=" number {"," ident "=" number} ";"]
				[ "var" ident {"," ident} ";"]
				{ "procedure" ident ";" block ";" } statement .

		statement = [ ident ":=" expression | "call" ident
					  | "print" expression
					  | "begin" statement {";" statement } "end"
					  | "if" condition "then" statement
					  | "while" condition "do" statement ].

		condition = "odd" expression |
					expression ("="|"#"|"<"|"<="|">"|">=") expression .

		expression = [ "+"|"-"] term { ("+"|"-") term}.

		term = factor {("*"|"/") factor}.

		factor = ident | number | "(" expression ")".

*/

#ifdef _MSC_VER
#pragma warning (disable : 4996)
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include "debug.h"

#ifdef EMULATOR
#include "pl0compiler.h"
#else
int compile_pl0(const char* path, char* ebuf, int ebuf_size);
#endif

#define SYMLEN 100
#define LABELLEN 20
#define SYMTABLELEN 200

static FILE* in, *out, *out2;
static char* error_buf;
static size_t error_buf_size;
static int linenum;
static char symvalue[SYMLEN], accepted_symvalue[SYMLEN];
static int labelcounter = 0;
static int tempcounter = 0;
static int maxtemp = 0;
static int blocknum = 0;
static int blocklevel = 0;

typedef enum {blocktype = 1, vartype = 2, consttype = 4, proctype = 8} SYMTYPE;
static int numsyms;
struct SYMTABLE {
	int blocknum;
	SYMTYPE symtype;
	char symname[SYMLEN];
	int blocklevel;
	int position;
} symtable[SYMTABLELEN];

static void nextsym(bool init);
static bool error(const char msg[], const char* param1, const char* param2);

static void expression(void);
static void program(void);

#ifndef EMULATOR

int main(int argc, char *argv[])
{
	char ebuf[200];

	if (argc != 2) {
		fprintf(stderr, "Must have one path arg\n");
		exit(1);
	}
	if (!compile_pl0(argv[1], ebuf, sizeof(ebuf))) {
		fprintf(stderr, "ERROR: %s\n", ebuf);
		exit(1);
	}
	exit(0);
}

#endif

static void code(const char* label, const char* s1, const char* s2)
{
	fprintf(out, "%s\t%s%s # @%d\n", label, s1, s2, linenum);
}

static void code2(const char* label, const char* s1, const char* s2)
{
	fprintf(out2, "%s\t%s%s # @%d\n", label, s1, s2, linenum);
}

static void codepush(const char* addr, const char *comment)
{
	code("", "-loadmq ", addr);
	code("", "radd *", "");
	code("", "tr .push", comment);
}

static const char* literal(n)
{
	static char buf[100];

	snprintf(buf, sizeof(buf), "=%d", n);
	return buf;
}

static void codepushn(int n)
{
	codepush(literal(n), "");
}

// putss addr of frame ptr in addr part of ac
// next instr should be storea (e.g., "storea *+1")
static code_getframeptraddr(int blocklevel)
{
	code("", "-radd =@.levels", "");
	code("", "-add ", literal(blocklevel * 2));
	code("", "aleft 18", "");
}

// puts addr of frame (autos) in ac
static code_getframeptr(int blocklevel)
{
	code_getframeptraddr(blocklevel);
	code("", "storea *+1", "");
	code("", "-radd 0", ""); // addr of frame in ac
}

// puts addr of stack word in addr part of ac
// next instr should be storea (e.g., "storea *+2")
static code_getstackaddr(struct SYMTABLE* st)
{
	code_getframeptr(st->blocklevel);
	code("", "-add ", literal(st->position * 2));
	code("", "aleft 18", "");
}

int compile_pl0(const char* path, char* ebuf, int ebuf_size)
{
	char outpath[_MAX_PATH];
	char s[200];

	error_buf = ebuf;
	error_buf[0] = 0;
	error_buf_size = ebuf_size;
	linenum = 1;
	labelcounter = 0;
	tempcounter = 0;
	maxtemp = 0;
	blocknum = 0;
	blocklevel = -1;
	numsyms = 0;
	nextsym(true);
	in = fopen(path, "r");
	if (in == NULL)
		return error("Can't open", path, NULL);
	if (strlen(path) > sizeof(outpath) - 10)
		return error("Path too long", path, NULL);
	strcpy(outpath, path);
	for (char *p = outpath; *p; p++)
		if (*p == '\\')
			*p = '/';
	char *p = strrchr(outpath, '/') + 1;
	strcpy(p, "out.a");
	out = fopen(outpath, "w");
	if (out == NULL)
		return error("Can't open out.a", NULL, NULL);
	strcpy(p, "out2.a");
	out2 = fopen(outpath, "w+");
	if (out2 == NULL)
		return error("Can't open out2.a", NULL, NULL);
	code("# self-load.a, stack.a, print.a, and print-number.a are supplied with the IBM 701 emulator", "", "");
	code("# See basepath.com/701", "", "");
	code("", "incl self-load.a", "");
	program();
	fclose(in);
	rewind(out2);
	while (fgets(s, sizeof(s), out2))
		fputs(s, out);
	fclose(out);
	fclose(out2);
	remove(outpath);
	return ebuf[0] == 0;
}

typedef enum {
	none, ident, number, lparen, rparen, times, slash, plus,
	minus, eql, neq, lss, leq, gtr, geq, callsym, beginsym, semicolon,
	endsym, ifsym, whilesym, becomes, thensym, dosym, constsym, comma,
	varsym, procsym, period, oddsym, printsym, lastsym
} Symbol;
static Symbol sym;

static const char *string_symbol[] = {
	"none", "ident", "number", "lparen", "rparen", "times", "slash", "plus",
	"minus", "eql", "neq", "lss", "leq", "gtr", "geq", "callsym", "beginsym", "semicolon",
	"endsym", "ifsym", "whilesym", "becomes", "thensym", "dosym", "constsym", "comma",
	"varsym", "procsym", "period", "oddsym", "print", "lastsym"
};

const char* make_symname(int n) {
	static char s[SYMLEN + 10];
	if (n >= 0 && n <= numsyms) {
		snprintf(s, sizeof(s), "%s.%d", symtable[n].symname, symtable[n].blocknum);
		return s;
	}
	error("symname() called with bad arg", NULL, NULL);
	return "?";
}

static const char *newsymbol(SYMTYPE symtype, const char* symname, int level, int pos) {
	if (numsyms >= SYMTABLELEN) {
		error("Too many symbols", NULL, NULL);
		return "?";
	}
	else {
		symtable[numsyms].blocknum = blocknum;
		symtable[numsyms].symtype = symtype;
		strcpy(symtable[numsyms].symname, symname);
		symtable[numsyms].blocklevel = level;
		symtable[numsyms].position = pos;
		numsyms++;
		return make_symname(numsyms - 1);
	}
}

static const char* findsym(const char* x, int types, struct SYMTABLE **st) {
	for (int i = numsyms - 1; i >= 0; i--) {
		if (strcmp(x, symtable[i].symname) == 0) {
			if ((types & symtable[i].symtype) == symtable[i].symtype) {
				if (st != NULL)
					*st = &symtable[i];
				return make_symname(i);
			}
		}
	}
	error("Symbol not declared at all or not as correct identifier type", x, NULL);
	return NULL;
}

static int markscope()
{
	return numsyms;
}

static void removescope(int scope) {
	numsyms = scope;
}

static const char *symbol_to_string(Symbol s)
{
	if (s > none && s < lastsym)
		return string_symbol[s];
	return "???";
}

static const char* makelabel(char *s) {
	snprintf(s, LABELLEN, "$%d", ++labelcounter);
	return s;
}

static const char* maketemp(char *s) {
	snprintf(s, LABELLEN, "$temp%d", ++tempcounter);
	if (tempcounter > maxtemp)
		maxtemp = tempcounter;
	return s;
}

static void releasetemp()
{
	tempcounter--;
}

static int accept(Symbol s) {
	if (sym == s) {
		strcpy(accepted_symvalue, symvalue);
		nextsym(false);
		return 1;
	}
	return 0;
}

static int expect(Symbol s) {
	if (accept(s))
		return 1;
	error("unexpected symbol; expected", symbol_to_string(s), symvalue);
	return 0;
}

static void factor(void) {
	if (accept(ident)) {
		struct SYMTABLE *st;

		findsym(accepted_symvalue, vartype | consttype, &st);
		code_getstackaddr(st);
		code("", "storea *+1", "");
		code("", "-radd 0", ""); // value of var/const in ac
	}
	else if (accept(number)) {
		code("", "-radd =", accepted_symvalue);
	}
	else if (accept(lparen)) {
		expression();
		expect(rparen);
	}
	else {
		error("factor: syntax error; got", symbol_to_string(sym), symvalue);
		nextsym(false);
	}
}

static void term(void) {
	Symbol savesym;
	char temp[LABELLEN], temp2[LABELLEN];

	factor();
	while (sym == times || sym == slash) {
		maketemp(temp);
		code("", "-store ", temp);
		savesym = sym;
		nextsym(false);
		factor();
		if (savesym == times) {
			code("", "lright 35", "");
			code("", "-mpy ", temp);
			code("", "lleft 35", ""); // least sig part into ac
		}
		else {
			maketemp(temp2);
			code("", "-store ", temp2);
			code("", "-radd ", temp);
			code("", "lright 35", "");
			code("", "-div ", temp2);
			code("", "lleft 35", ""); // least sig part into ac
		}
		releasetemp();
	}
}

static void expression(void) {
	Symbol sign = none;
	char temp[LABELLEN], temp2[LABELLEN];

	maketemp(temp);
	if (sym == plus || sym == minus) {
		sign = sym;
		nextsym(false);
	}
	term();
	if (sign == minus) {
		code("", "-store ", temp);
		code("", "-rsub ", temp);
	}
	while (sym == plus || sym == minus) {
		sign = sym;
		code("", "-store ", temp);
		nextsym(false);
		term();
		if (sign == plus)
			code("", "-add ", temp);
		else {
			maketemp(temp2);
			code("", "-store ", temp2);
			code("", "-radd ", temp);
			code("", "-sub ", temp2);
			releasetemp();
		}
	}
	releasetemp();
}

static const char *condition(char *false_label) { // generate transfer to returned label if false
	char label[LABELLEN];
	char temp[LABELLEN];

	maketemp(temp);
	makelabel(false_label);
	makelabel(label);
	if (accept(oddsym)) {
		expression();
		code("", "aleft 34", "");
		code("", "trov *+1", ""); // turn overflow indicator off if other bits turned it on
		code("", "aleft 1", "");
		code("", "trov ", label);
		code("", "tr ", false_label);
		code(label, "noop", "");
	}
	else {
		expression();
		Symbol savesym = sym;
		code("", "-store ", temp);
		nextsym(false);
		expression();
		code("", "-sub ", temp);
		switch (savesym) {
		case eql:
			code("", "trzero ", label);
			code("", "tr ", false_label);
			code(label, "noop", "");
			break;
		case neq:
			code("", "trzero ", false_label);
			break;
		case lss:
			code("", "trzero ", false_label);
			code("", "trplus ", label);
			code("", "tr ", false_label);
			code(label, "noop", "");
			break;
		case leq:
			code("", "trzero ", label);
			code("", "trplus ", label);
			code("", "tr ", false_label);
			code(label, "noop", "");
			break;
		case gtr:
			code("", "trzero ", false_label);
			code("", "trplus ", false_label);
			break;
		case geq:
			code("", "trzero ", label);
			code("", "trplus ", false_label);
			code(label, "noop", "");
			break;
		default:
			error("condition: invalid operator", NULL, NULL);
		}
	}
	releasetemp();
	return false_label;
}

static void statement(void) {
	tempcounter = 0;
	if (accept(ident)) {
		char v[SYMLEN];
		char temp[LABELLEN];
		struct SYMTABLE* st;

		maketemp(temp);
		const char* sm = findsym(accepted_symvalue, vartype, &st);
		if (sm == NULL) {
			error("Identifier not declared as var", accepted_symvalue, NULL);
			nextsym(false);
			return;
		}
		strcpy(v, sm);
		expect(becomes);
		expression();
		code("", "-store ", temp);
		code_getstackaddr(st);
		code("", "storea *+2", "");
		code("", "-radd ", temp); // value of rhs in ac
		code("", "-store 0", ""); // store it onto stack
		releasetemp();
	}
	else if (accept(callsym)) {
		expect(ident);
		const char* proc = findsym(accepted_symvalue, proctype, NULL);
		code("", "radd *", "");
		code("", "tr ", proc);
	}
	else if (accept(printsym)) {
		expression();
		code("", "lright 35", "");
		code("", "radd *", "");
		code("", "tr .print-number", "");
	}
	else if (accept(beginsym)) {
		do {
			statement();
		} while (accept(semicolon));
		expect(endsym);
	}
	else if (accept(ifsym)) {
		char false_label[LABELLEN];

		const char *label = condition(false_label);
		expect(thensym);
		statement();
		code(label, "noop", "");
	}
	else if (accept(whilesym)) {
		char looplabel[LABELLEN];
		char false_label[LABELLEN];

		makelabel(looplabel);
		code(looplabel, "noop", "");
		const char *label = condition(false_label);
		expect(dosym);
		statement();
		code("", "tr ", looplabel);
		code(label, "noop", "");
	}
	else {
		error("statement: syntax error", symbol_to_string(sym), NULL);
		nextsym(false);
	}
}

static int block(void) {
	char startlabel[LABELLEN];
	int stack_position = 0;

	blocklevel++;
	makelabel(startlabel);
	blocknum++;
	int scope = markscope();
	if (accept(constsym)) {
		do {
			char s[SYMLEN];
			char buf[100];

			expect(ident);
			strcpy(s, accepted_symvalue);
			expect(eql);
			expect(number);
			const char* sm = newsymbol(consttype, s, blocklevel, stack_position);
			snprintf(buf, sizeof(buf), "=%s", accepted_symvalue);
			codepush(buf, " # push constant");
			stack_position++;
		} while (accept(comma));
		expect(semicolon);
	}
	if (accept(varsym)) {
		do {
			expect(ident);
			const char *s = newsymbol(vartype, accepted_symvalue, blocklevel, stack_position);
			code2(s, "word", "");
			codepush("=0", " # push var");
			stack_position++;
		} while (accept(comma));
		expect(semicolon);
	}
	code("", "tr ", startlabel); // go around procs
	while (accept(procsym)) {

		expect(ident);
		const char *s = newsymbol(proctype, accepted_symvalue, blocklevel, 0);
		code(s, "add =2", "");
		code("", "aleft 6", ""); // get 12 addr bits only
		code("", "aright 6", "");
		code("", "lright 53", ""); // into mq (53 = 35 + 18)
		code("", "radd *", "");
		code("", "tr .push", " # push rtn addr"); // onto stack

		code_getframeptr(blocklevel + 1); // frame ptr for this level in ac
		code("", "lright 35", "");
		code("", "radd *", "");
		code("", "tr .push", " # push prev frame ptr"); // prev frame ptr onto stack

		code_getframeptraddr(blocklevel + 1);
		code("", "storea *+3", "");
		code("", "-radd .stx", ""); // set frame ptr to stack addr of first auto
		code("", "-add =2", "");
		code("", "-store 0", " # set new frame ptr to autos");

		expect(semicolon);
		int numautos = block(); // will push autos
		expect(semicolon);

		// Restore frame ptr for this level
		// Construct bogus SYMTABLE entry for prev frame ptr
		char temp[LABELLEN];
		struct SYMTABLE st;
		st.blocklevel = blocklevel + 1;
		st.position = -1;
		code("", "noop # restore frame ptr", "");
		code_getstackaddr(&st);
		code("", "storea *+1", "");
		code("", "-radd 0", ""); // prev frame ptr in ac
		maketemp(temp);
		code("", "-store ", temp); // save it
		code_getframeptraddr(blocklevel + 1); // addr of frame ptr in levels array
		code("", "storea *+2", "");
		code("", "-radd ", temp); // get prev frame ptr
		code("", "-store 0", ""); // put it in levels array

		code("", "-radd ", literal(numautos * 2 + 4)); // total amount to be popped in half words
		code("", "tr .trpop", "");
		releasetemp();
	}
	code(startlabel, "noop", "");
	statement();
	removescope(scope);
	blocklevel--;
	return stack_position;
}

static void program(void) {
	//codepush("=0", "");					// rtn addr for level 0 (globals)
	//codepush("=0", "");					// prev frame ptr for this level
	// stx + 2 after above push goes into level 0 ptr
	code("", "-radd =@.levels", "");
	code("", "aleft 18", "");
	code("", "storea *+3", "");
	code("", "-radd .stx", "");
	code("", "-add =2", "");
	code("", "-store 0", "");
	nextsym(false);
	block();
	expect(period);
	code("", "stop 1", "");
	for (int i = 1; i <= maxtemp; i++) {
		char s[LABELLEN];

		snprintf(s, LABELLEN, "$temp%d", i);
		code(s, " word", "");
	}
	code("", "incl print.a", "");
	code("", "incl print-number.a", "");
	code("", "incl stack.a", "");
	printf("Compiled OK\n");
}

static void nextsym(bool init)
{
	static char c;

	if (init) {
		c = 0;
		return;
	}
	if (error_buf[0]) {
		sym = none;
		return;
	}
	if (c == 0)
		c = fgetc(in);
	while (isspace(c)) {
		if (c == '\n')
			linenum++;
		c = fgetc(in);
	}
	switch (c) {
	case EOF:
		sym = none;
		break;
	case ';':
		sym = semicolon;
		c = 0;
		break;
	case '(':
		sym = lparen;
		c = 0;
		break;
	case ')':
		sym = rparen;
		c = 0;
		break;
	case '*':
		sym = times;
		c = 0;
		break;
	case '/':
		if ((c = fgetc(in)) == '*') {
			while (true) {
				while ((c = fgetc(in)) != '*' && c != EOF)
					if (c == '\n')
						linenum++;
				if (c == EOF)
					break;
				if (c == '*' && (c = fgetc(in)) == '/') {
					c = 0;
					nextsym(false);
					break;
				}
				if (c == '\n')
					linenum--;
				ungetc(c, in);
			}
		}
		else
			sym = slash;
		break;
	case '+':
		sym = plus;
		c = 0;
		break;
	case '-':
		sym = minus;
		c = 0;
		break;
	case '=':
		sym = eql;
		c = 0;
		break;
	case '#':
		sym = neq;
		c = 0;
		break;
	case '<':
		if ((c = fgetc(in)) == '=') {
			sym = leq;
			c = 0;
		}
		else {
			sym = lss;
		}
		break;
	case '>':
		if ((c = fgetc(in)) == '=') {
			sym = geq;
			c = 0;
		}
		else {
			sym = gtr;
			c = 0;
		}
		break;
	case ',':
		sym = comma;
		c = 0;
		break;
	case ':':
		if ((c = fgetc(in)) == '=') {
			sym = becomes;
			c = 0;
		}
		else {
			error("Unexpected colon", NULL, NULL);
		}
		break;
	case '.':
		sym = period;
		c = 0;
		break;
	default:
		memset(symvalue, 0, sizeof(symvalue));
		symvalue[0] = c;
		while (isalpha(c = fgetc(in)) || isdigit(c)) {
			size_t n = strlen(symvalue);
			if (n >= sizeof(symvalue) - 10) // allow for period and block number
				error("Identifier or symbol too long", symvalue, NULL);
			symvalue[n] = c;
		}
		if (isdigit(symvalue[0]))
			sym = number;
		else if (strcmp(symvalue, "call") == 0)
			sym = callsym;
		else if (strcmp(symvalue, "begin") == 0)
			sym = beginsym;
		else if (strcmp(symvalue, "end") == 0)
			sym = endsym;
		else if (strcmp(symvalue, "if") == 0)
			sym = ifsym;
		else if (strcmp(symvalue, "while") == 0)
			sym = whilesym;
		else if (strcmp(symvalue, "then") == 0)
			sym = thensym;
		else if (strcmp(symvalue, "do") == 0)
			sym = dosym;
		else if (strcmp(symvalue, "const") == 0)
			sym = constsym;
		else if (strcmp(symvalue, "var") == 0)
			sym = varsym;
		else if (strcmp(symvalue, "procedure") == 0)
			sym = procsym;
		else if (strcmp(symvalue, "odd") == 0)
			sym = oddsym;
		else if (strcmp(symvalue, "print") == 0)
			sym = printsym;
		else if (isalpha(symvalue[0]))
			sym = ident;
		else
			error("Unknown character or symbol", symvalue, NULL);
	}
}

static bool error(const char msg[], const char* param1, const char* param2)
{
	if (!error_buf[0]) { // return first error only
		snprintf(error_buf, error_buf_size, "ERROR @%d: %s", linenum, msg);
		if (param1 != NULL) {
			size_t n = strlen(error_buf);
			snprintf(error_buf + n, error_buf_size - n, ": %s", param1);
		}
		if (param2 != NULL) {
			size_t n = strlen(error_buf);
			snprintf(error_buf + n, error_buf_size - n, " [got: %s]", param2);
		}
	}
	return false;
}
