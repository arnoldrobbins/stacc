/* stacc --- still another compiler-compiler */
/*           (a recursive descent parser generator) */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define INDENT()	tab_over()
#define STEP_IN()	indentation++
#define STEP_OUT()	indentation--

#define INBUFSIZE	200	/* must be > MAXLINE */
#define PBLIMIT		95	/* max no. chars pushed back before full line */
#define MEMSIZE		4000	/* for symbol tables */
#define TABSETTING	3	/* tab width for indentation of output */
#define UNKNOWN		4	/* fourth possible value of state variable */
				/*  in addition to NOMATCH FAILURE, ACCEPT */
#define MAXACTC		5000	/* max characters in actions */
#define MAXACT		200	/* max lines of actions */
#define MAXERRC		5000	/* max characters in error actions */
#define MAXERR		200	/* max lines of error actions */

#define RATFOR	0
#define PASCAL	1
#define PL1	2
#define PLP	3
#define C	4

#define TERMINAL_DECL	1
#define COMMON_DECL	2
#define STATE_DECL	3
#define SCANNER_DECL	4
#define SYMBOL_DECL	5
#define EXTTERM_DECL	6
#define EPSILON_DECL	7

/* typedefs for hsearch routine */
typedef struct entry {
	char *key;
	char *data;
	} ENTRY;

#define TRUE	1
#define FALSE	0

#define YES	TRUE
#define NO	FALSE

#define MAXLINE	102

typedef enum {
	FIND,
	ENTER
	} ACTION;

extern ENTRY *hsearch ();

char inbuf [INBUFSIZE] = "\0\0\0";	/* a few extra NULs, just in case */
int ibp = 0;
FILE *pfd = NULL;
FILE *stdout2 = NULL;	/* a Ratfor anachronism ... */
char *pascaltemp = NULL;

char act_text [MAXACTC+1];
char erract_text [MAXERRC+1];	/* for 1-basing */
int num_actions = 0;
int num_erractions = 0;
int act_inx [MAXACT];
int erract_inx [MAXERR];
int next_act;
int next_erract;

char symboltext [MAXLINE];
int symbol;

int next_term_val = 0;
int last_term_val = 0;

int language = C;
int linenumber = 1;
int indentation = 0;
int advance;
int svarval = UNKNOWN;

ENTRY terminal;

char common_name [MAXLINE] = "rdp.com";
char statevar [MAXLINE] = "state";
char scanner [MAXLINE] = "getsym";
char symbolvar [MAXLINE] = "symbol";
char epsilon_name [MAXLINE] = "epsilon";

int Argc = 0;
char **Argv = NULL;

char *lang_names[] = {
	"RATFOR",
	"PASCAL",
	"PL1",
	"PLP",
	"C",
	NULL
};

int Debug = FALSE;

#include "st.parse.h"

main(argc, argv)
int argc;
char **argv;
{
	int pstate;

	Argc = argc;
	Argv = argv;

	initialize();
	rdp (& pstate);
	cleanup();

	exit (0);
}

#include "st.parse.c"

/* actions --- gather up accept and error actions */

actions (gpst)
int *gpst;
{
	num_actions = 0;
	num_erractions = 0;
	next_act = 0;
	next_erract = 0;

	for (;;)
		if (symbol == '!')
		{
			/* note use of secret knowledge of 'ngetch's buffer structure: */
			addtext (& inbuf [ibp], act_text, & next_act, MAXACTC,
				& num_actions, MAXACT, act_inx);
			inbuf [ibp] = '\0';
			linenumber++;
			getsym();
		}
		else if (symbol == '?')
		{
			/* note use of secret knowledge of 'ngetch's buffer structure: */
			addtext (& inbuf [ibp], erract_text, & next_erract,
				MAXERRC, & num_erractions, MAXERR, erract_inx);
			inbuf [ibp] = '\0';
			linenumber++;
			getsym();
		}
		else
			break;

	*gpst = ACCEPT;
}



/* addtext --- add line of text to store, update index, check for errs */

addtext (text, store, avail, maxavail, entries, maxent, inx)
char text[], store [];
int *avail, maxavail, *entries, maxent, inx [];
{
	int l, junk;

	if (++(*entries) >= maxent)
	{
		errmsg ("too many action/erroraction lines", & junk);
		error ("stacc processing terminated");
	}
	inx [*entries] = *avail;

	l = strlen (text);
	if (*avail + l + 1 >= maxavail)
	{
		errmsg ("too much action/erraction text", & junk);
		error ("stacc processing terminated");
	}

	strcpy (&store[*avail], text);
	*avail += l + 1;
}



/* cleanup --- finish up stacc's processing */

cleanup()
{
	if (language == PASCAL)
	{
		(void) rewind (pfd);
		(void) fcopy (pfd, stdout);
		(void) fclose (pfd);
		(void) unlink (pascaltemp);
	}
}


/* decl_common --- fetch name of include file holding Ratfor common blocks */

decl_common()
{
	get_string();
	strcpy (common_name, symboltext);
	getsym();
}



/* decl_epsilon --- fetch name of symbol used to represent "empty" */

decl_epsilon()
{
	get_string();
	strcpy (epsilon_name, symboltext);
	getsym();
}



/* decl_scanner --- fetch name of lexical analyzer */

decl_scanner()
{
	get_string();
	strcpy (scanner, symboltext);
	getsym();
}



/* decl_statevar --- get name of parser state variable */

decl_statevar()
{
	get_string();
	strcpy (statevar, symboltext);
	getsym();
}



/* decl_symvar --- fetch name of "current symbol" variable */

decl_symvar()
{
	get_string();
	strcpy (symbolvar, symboltext);
	getsym();
}



/* decl_tail --- handle tail end (after the dot) of a declaration */

decl_tail (gpst)
int *gpst;
{
	int i;
	int strlsr();

	static struct _table {
		char *text;
		int sym;
	} table[] = {
		"terminal",	TERMINAL_DECL,
		"common",	COMMON_DECL,
		"state",	STATE_DECL,
		"scanner",	SCANNER_DECL,
		"symbol",	SYMBOL_DECL,
		"ext_term",	EXTTERM_DECL,
		"epsilon",	EPSILON_DECL
	};

	i = strlsr ((char *) table, sizeof (table), sizeof (table[0]), symboltext);
	if (i == EOF)
		errmsg ("illegal declarator", gpst);
	else
		switch (table[i].sym) {
		case TERMINAL_DECL:
			getsym();
			termlist (gpst);	/* declare terminal symbols */
			break;

		case COMMON_DECL:
			decl_common();	/* declare Ratfor common blocks */
			break;

		case STATE_DECL:
			decl_statevar();   /* declare parse state variable */
			break;

		case SCANNER_DECL:
			decl_scanner();		/* declare scanner routine */
			break;

		case SYMBOL_DECL:
			decl_symvar();	/* declare "current symbol" var */
			break;

		case EXTTERM_DECL:
			getsym();
			extlist (gpst);	/* acknowledge external terminals */
			break;

		case EPSILON_DECL:		/* declare "empty" symbol */
			decl_epsilon();
			break;
		}

	*gpst = ACCEPT;
}



/* emit_statedefs --- emit defines for parser states */

emit_statedefs()
{
	switch (language) {

	case RATFOR:
	case PL1:
	case PASCAL:
	case C:
	case PLP:
		o_defn ("NOMATCH", 1);
		o_defn ("FAILURE", 2);
		o_defn ("ACCEPT", 3);
		break;
	}
}



/* errmsg --- print error message, attempt to recover parse */

errmsg (msg, svar)
char msg [];
int *svar;
{
	fprintf (stderr, "%d:  %s\n", linenumber, msg);

	for (;;)
	{
		/* skip symbols up to a convenient stopping point */
		if (symbol == ';'
		|| symbol == ')'
		|| symbol == ']'
		|| symbol == '}'
		|| symbol == EOF)
			break;
		if (symbol == '!'
		|| symbol == '?')
		{
			/* secret knowledge of 'ngetch's input buffer: */
			inbuf [ibp] = '\0';
		}
		getsym();
	}

	*svar = ACCEPT;
}

/* error --- write an error message and die */

error (msg)
char *msg;
{
	fprintf (stderr, "%s\n", msg);
	exit (1);
}

/* fcopy --- fast move of stuff from one file to another */

/*ARGSUESED*/
fcopy (in, out)
FILE *in, *out;
{
	char buf[BUFSIZ];

	while (fread (buf, sizeof (char), sizeof buf, in) != 0)	/* 0 on EOF */
		(void) fwrite (buf, sizeof (char), sizeof buf, out);
}


/* get_string --- get (possibly quoted) character string from input */

get_string()
{
	char c, quote;
	char ngetch();

	int i, junk;

	do {
		c = ngetch (&c);
		if (c == '\n')
			linenumber++;
	} while (c == ' ' || c == '\t' || c == '\n');

	if (isalpha (c) || c == '_')
	{
		putback (c);
		scan_id();
	}
	else if (c == '\'' || c == '"')
	{
		quote = c;
		i = -1;		/* will immediately be incremented */
		do {
			i++;
			symboltext [i] = ngetch (& c);
		} while (c != quote && (int) c !=  EOF && i < MAXLINE - 1);
		if ((int) c == EOF)
			errmsg ("missing quote or string too long", & junk);
		symboltext [i] = '\0';
	}
	else
		errmsg ("identifier or string expected", & junk);
}



/* getsym --- get next symbol from input stream */

getsym()
{
	char c;
	char ngetch();

	for(;;)
	{
		/* until a symbol is found */
		do {
			c = ngetch (&c);
			if (c == '\n')
				linenumber++;
		} while (c == ' ' || c == '\t' || c == '\n');

		switch (c) {
		case '.': case '=': case ';': case '|': case ':': case '$':
		case '(': case ')': case '[': case ']': case '{': case '}':
		case '!': case '?': case EOF:
			symbol = c;
			break;
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
		case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
		case 's': case 't': case 'u': case 'v': case 'w': case 'x':
		case 'y': case 'z': case 'A': case 'B': case 'C': case 'D':
		case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
		case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
		case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V':
		case 'W': case 'X': case 'Y': case 'Z':
			putback (c);
			scan_id();
			if (strcmp (symboltext, epsilon_name) == 0)
				symbol = EPSILONSYM;
			else
			{
				terminal.key = symboltext;
				terminal.data = NULL;
				if (hsearch (terminal, FIND) != NULL)
					symbol = TERMIDSYM;
				else
					symbol = NONTERMIDSYM;
			}
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			putback (c);
			scan_int();
			break;
		case '"':
		case '\'':
			putback (c);
			scan_char();
			break;
		case '-':
			scan_is();
			break;
		case '#':       /* (comment) */
			/* secret knowledge, used to throw away the input buffer: */
			inbuf [ibp] = '\0';
			continue;
			break;
		default:
			fprintf (stderr, "%d:  bad symbol '%c'\n", linenumber, c);
			continue;
		}
		return;
	}
}





/* get_language --- determine language to be used for actions */

get_language()
{
	int i;
	int strlsr();

	static struct _table {
		char *text;
		int lang;
	} table[] = {
		"ratfor",	RATFOR,
		"pl1",		PL1,
		"pl/1",		PL1,
		"pl/i",		PL1,
		"pascal",	PASCAL,
		"c",		C,
		"plp",		PLP
	};

	if (Argv[1] != NULL)
	{
		for (i = 0; Argv[1][i] != '\0'; i++)
			if (isupper (Argv[1][i]))
				Argv[1][i] = tolower (Argv[1][i]);
		i = strlsr ((char *) table, sizeof (table),
			sizeof (table[0]), Argv[1]);
		if (i == EOF)
		{
			fprintf (stderr, "%s:  unsupported language\n", Argv[1]);
			exit (1);
		}
		else
			language = table[i].lang;
	}
	/* else
		no language given, default to C */
}



/* initialize --- initialize everything */

initialize()
{
	char *cp, *mktemp();
	static char filename[15] = "st.parse.";

	hcreate (131);	/* hash table of terminal symbols */

	if (strcmp (Argv[1], "-d") == 0)
	{
		Debug = TRUE;
		Argv++;
		Argc--;
	}

	get_language();

	if (language == PASCAL)
	{
		cp = mktemp ("/tmp/stacc.XXXXXX");
		if ((pfd = fopen (cp, "w+")) == NULL)
			error ("can't open Pascal temporary file");
		pascaltemp = cp;	/* save file name for unlinking later */
	}

	switch (language) {
	case C:
		strcat (filename, "c");
		break;

	case RATFOR:
		strcat (filename, "r");
		break;

	case PASCAL:
		strcat (filename, "p");
		break;

	case PL1:
		strcat (filename, "pl1");
		break;

	case PLP:
		strcat (filename, "plp");
		break;
	}

	if (freopen (filename, "w", stdout) == NULL)
	{
		fprintf (stderr, "couldn't create %s\n", filename);
		exit (1);
	}
	else if ((stdout2 = fopen ("st.parse.h", "w")) == NULL)
		error ("couldn't create st.parse.h");

	emit_statedefs();

	getsym();
}

/* max --- return the maximum of two numbers */

max (a, b)
int a, b;
{
	return (a > b ? a : b);
}


/* ngetch --- get a (possibly pushed back) input character */

char ngetch (c)
char *c;
{
	char *fgets ();

	if (inbuf [ibp] == '\0')
	{
		ibp = 0;
		if (fgets (inbuf, sizeof inbuf, stdin) == NULL)
		{
			*c = EOF;
			return EOF;
		}
	}

	*c = inbuf [ibp++];

	return *c;
}



/* o_accept_actions --- output accept actions, properly indented */

o_accept_actions ()
{
	int l, i;
	FILE *fd;

	fd = stdout;
	if (language == PASCAL)
		fd = pfd;

	for (l = 1; l <= num_actions; l++)
	{
		INDENT();
		i = act_inx [l];
		if (act_text [i] == ' ')
			fputs (& act_text [i + 1], fd);
		else
			fputs (& act_text [i], fd);
	}
}



/* o_alt --- output code to check one of several alternatives */

o_alt ()
{
	switch (language) {

	case RATFOR:
	case C:
		INDENT();
		fprintf (stdout, "if (%s == NOMATCH) {\n", statevar);
		svarval = NOMATCH;
		STEP_IN();
		break;

	case PL1:
	case PLP:
		INDENT();
		fprintf (stdout, "if (%s = NOMATCH) then do;\n", statevar);
		svarval = NOMATCH;
		STEP_IN();
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "if (%s = NOMATCH) then begin\n", statevar);
		svarval = NOMATCH;
		STEP_IN();
		break;
	}
}



/* o_begin_rept --- begin repeated rhs */

o_begin_rept()
{
	switch (language) {

	case RATFOR:
		INDENT();
		fprintf (stdout, "repeat {\n");
		STEP_IN();
		break;

	case PL1:
	case PLP:
		INDENT();
		fprintf (stdout, "do while (%s = ACCEPT);\n", statevar);
		STEP_IN();
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "repeat\n");
		STEP_IN();
		break;

	case C:
		INDENT();
		fprintf (stdout, "do {\n");
		STEP_IN();
		break;
	}
}



/* o_begin_routine --- output subroutine header information */

o_begin_routine (name)
char name [];
{
	switch (language) {

	case RATFOR:
		fprintf (stdout, "\n\n\nsubroutine %s (gpst)\n", name);
		fprintf (stdout, "integer gpst\n");
		fprintf (stdout, "include '%s'\n", common_name);
		fprintf (stdout, "integer %s\n", statevar);
		o_error_actions();
		o_accept_actions();
		svarval = UNKNOWN;
		break;

	case PL1:
		fprintf (stdout, "\n\n\n%s : procedure (gpst) recursive;\n",
				name);
		STEP_IN();
		INDENT();
		fprintf (stdout, "declare gpst fixed binary;\n\n");
		INDENT(); 
		fprintf (stdout, "declare %s fixed binary;\n", statevar);
		o_error_actions();
		o_accept_actions();
		svarval = UNKNOWN;
		break;

	case PASCAL:
		fprintf (stdout, "procedure %s (var gpst : integer); forward;\n",
		    name);
		fprintf (pfd, "\n\n\nprocedure %s;\n", name);
		fprintf (pfd, "\nlabel 99;\n");
		o_error_actions();
		fprintf (pfd, "\nvar %s : integer;\n", statevar);
		STEP_IN();
		o_accept_actions();
		STEP_OUT();
		fprintf (pfd, "\nbegin\n");
		STEP_IN();
		svarval = UNKNOWN;
		break;

	case C:
		fprintf (stdout, "\n\n\n%s (gpst)\n", name);
		fprintf (stdout, "int *gpst;\n");
		fprintf (stdout, "{\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "extern int %s;\n", symbolvar);
		INDENT(); 
		fprintf (stdout, "int %s();\n", scanner);
		INDENT(); 
		fprintf (stdout, "int %s;\n", statevar);
		o_error_actions();
		o_accept_actions();
		svarval = UNKNOWN;
		break;

	case PLP:
		fprintf (stdout, "\n\n\n%s : procedure (gpst);\n", name);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "declare gpst fixed binary;\n\n");
		INDENT(); 
		fprintf (stdout, "declare %s fixed binary;\n", statevar);
		o_error_actions();
		o_accept_actions();
		svarval = UNKNOWN;
		break;
	}
}



/* o_begin_seq --- output first test in a sequence of elements */

o_begin_seq()
{
	switch (language) {

	case RATFOR:
	case C:
		INDENT();
		fprintf (stdout, "if (%s == ACCEPT) {\n", statevar);
		svarval = ACCEPT;
		STEP_IN();
		break;

	case PL1:
	case PLP:
		INDENT();
		fprintf (stdout, "if (%s = ACCEPT) then do;\n", statevar);
		svarval = ACCEPT;
		STEP_IN();
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "if (%s = ACCEPT) then begin\n", statevar);
		svarval = ACCEPT;
		STEP_IN();
		break;
	}
}



/* o_call_nonterm ---- call nonterminal parsing routine */

o_call_nonterm (name)
char name [];
{
	switch (language) {

	case RATFOR:
		INDENT();
		fprintf (stdout, "call %s (%s)\n", name, statevar);
		svarval = UNKNOWN;
		break;

	case PL1:
	case PLP:
		INDENT();
		fprintf (stdout, "call %s (%s)\n", name, statevar);
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "%s (%s);\n", name, statevar);
		svarval = UNKNOWN;
		break;

	case C:
		INDENT();
		fprintf (stdout, "%s (& %s);\n", name, statevar);
		svarval = UNKNOWN;
		break;
	}
}


/* o_choice_actions --- cleanup and action code after a "quick select" choice */

o_choice_actions()
{
	o_accept_actions();

	if (advance == YES)
	{
		switch (language) {

		case RATFOR:
			INDENT();
			fprintf (stdout, "call %s\n", scanner);
			break;

		case PL1:
		case PLP:
			INDENT();
			fprintf (stdout, "call %s;\n", scanner);
			break;

		case PASCAL:
			INDENT();
			fprintf (pfd, "%s\n", scanner);
			break;

		case C:
			INDENT();
			fprintf (stdout, "%s ();\n", scanner);
			break;
		}

	}

	if (num_erractions > 0)
		fprintf (stderr, "%d:  error actions illegal here\n", linenumber);
}



/* o_choice_end --- output cleanup code after a "quick select" choice */

o_choice_end()
{
	switch (language) {

	case RATFOR:
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
	case PLP:
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT(); 
		fprintf (pfd, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case C:
		INDENT(); 
		fprintf (stdout, "break;\n");
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}
}



/* o_choice_start --- output header for beginning of "quick select" choice */

o_choice_start (val)
char val [];
{
	switch (language) {

	case RATFOR:
		INDENT(); 
		fprintf (stdout, "when (%s) {\n", val);
		STEP_IN();
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (stdout, "%s = ACCEPT\n", statevar);
		}
		svarval = ACCEPT;
		break;

	case PL1:
		INDENT();
		fprintf (stdout, "else if (%s = %s) then do;\n", symbolvar, val);
		STEP_IN();
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (stdout, "%s = ACCEPT;\n", statevar);
		}
		svarval = ACCEPT;
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "%s: begin\n", val);
		STEP_IN();
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (pfd, "%s := ACCEPT;\n", statevar);
		}
		svarval = ACCEPT;
		break;

	case C:
		INDENT();
		fprintf (stdout, "case %s: {\n", val);
		STEP_IN();
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (stdout, "%s = ACCEPT;\n", statevar);
		}
		svarval = ACCEPT;
		break;

	case PLP:
		INDENT();
		fprintf (stdout, "when (%s) do;\n", val);
		STEP_IN();
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (stdout, "%s = ACCEPT;\n", statevar);
		}
		svarval = ACCEPT;
		break;
	}
}



/* o_defn --- output definition for a terminal symbol */

o_defn (sym, val)
char sym [];
int val;
{
	switch (language) {

	case RATFOR:
		fprintf (stdout2, "define(%s,%d)\n", sym, val);
		break;

	case PL1:
	case PLP:
		fprintf (stdout2, "%%replace %s by %d;\n", sym, val);
		break;

	case PASCAL:
		fprintf (stdout2, "%s = %d;\n", sym, val);
		break;

	case C:
		fprintf (stdout2, "#define %s %d\n", sym, val);
		break;

	}
}



/* o_endalt --- close the test for one of many alternatives */

o_endalt()
{
	switch (language) {

	case RATFOR:
	case C:
		INDENT();
		fprintf (stdout, "}\n");
		svarval = UNKNOWN;
		STEP_OUT();
		break;

	case PL1:
	case PLP:
		INDENT();
		fprintf (stdout, "end;\n");
		svarval = UNKNOWN;
		STEP_OUT();
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "end;\n");
		svarval = UNKNOWN;
		STEP_OUT();
		break;
	}
}



/* o_end_nonterm --- perform actions after nonterminal symbol */

o_end_nonterm()
{
	switch (language) {

	case RATFOR:
		INDENT(); 
		fprintf (stdout, "select (%s)\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "when (FAILURE) {\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "gpst = FAILURE\n");
		INDENT(); 
		fprintf (stdout, "return\n");
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		if (num_erractions > 0)
		{
			INDENT(); 
			fprintf (stdout, "when (NOMATCH) {\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "when (ACCEPT) {\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
		INDENT(); 
		fprintf (stdout, "if (%s = FAILURE) then do;\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "gpst = FAILURE;\n");
		INDENT(); 
		fprintf (stdout, "return;\n");
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		if (num_erractions > 0)
		{
			INDENT(); 
			fprintf (stdout, "else if (%s = NOMATCH) then do;\n",statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "else if (%s = ACCEPT) then do;\n",statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT(); 
		fprintf (pfd, "case %s of\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "FAILURE: begin\n");
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "gpst := FAILURE;\n");
		INDENT(); 
		fprintf (pfd, "goto 99\n");
		INDENT(); 
		fprintf (pfd, "end;\n");
		STEP_OUT();
		if (num_erractions > 0)
		{
			INDENT(); 
			fprintf (pfd, "NOMATCH: begin\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
		}
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (pfd, "ACCEPT: begin\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
		}
		INDENT(); 
		fprintf (pfd, "otherwise end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case C:
		INDENT(); 
		fprintf (stdout, "switch (%s) {\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "case FAILURE: {\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "*gpst = FAILURE;\n");
		INDENT(); 
		fprintf (stdout, "return;\n");
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		if (num_erractions > 0)
		{
			INDENT(); 
			fprintf (stdout, "case NOMATCH: {\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "break;\n");
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "case ACCEPT: {\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "break;\n");
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PLP:
		INDENT(); 
		fprintf (stdout, "select (%s);\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "when (FAILURE) do;\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "gpst = FAILURE;\n");
		INDENT(); 
		fprintf (stdout, "return;\n");
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		if (num_erractions > 0)
		{
			INDENT(); 
			fprintf (stdout, "when (NOMATCH) do;\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "when (ACCEPT) do;\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}
}



/* o_end_opt --- actions at end of optional rhs */

o_end_opt()
{
	switch (language) {

	case RATFOR:
		INDENT(); 
		fprintf (stdout, "select (%s)\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "when (NOMATCH)");
		if (num_erractions > 0)
			fprintf (stdout, " {\n");
		else
			fprintf (stdout, "\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT\n", statevar);
		if (num_erractions > 0)
		{
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
		}
		STEP_OUT();
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "when (ACCEPT) {\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
		INDENT(); 
		fprintf (stdout, "if (%s = NOMATCH) then", statevar);
		if (num_erractions > 0)
			fprintf (stdout, " do;\n");
		else
			fprintf (stdout, "\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		if (num_erractions > 0)
		{
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
		}
		STEP_OUT();
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "else if (%s = ACCEPT) then do;\n",
			    statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT(); 
		fprintf (pfd, "case %s of\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "NOMATCH:");
		if (num_erractions > 0)
			fprintf (pfd, " begin\n");
		else
			fprintf (pfd, "\n");
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "%s := ACCEPT;\n", statevar);
		if (num_erractions > 0)
		{
			o_error_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
		}
		STEP_OUT();
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (pfd, "ACCEPT: begin\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
		}
		INDENT(); 
		fprintf (pfd, "otherwise end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case C:
		INDENT(); 
		fprintf (stdout, "switch (%s) {\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "case NOMATCH: {\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		if (num_erractions > 0)
			o_error_actions();
		INDENT(); 
		fprintf (stdout, "break;\n");
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "case ACCEPT: {\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "break;\n");
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PLP:
		INDENT(); 
		fprintf (stdout, "select (%s);\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "when (NOMATCH)");
		if (num_erractions > 0)
			fprintf (stdout, " do;\n");
		else
			fprintf (stdout, "\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		if (num_erractions > 0)
		{
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
		}
		STEP_OUT();
		if (num_actions > 0)
		{
			INDENT(); 
			fprintf (stdout, "when (ACCEPT) do;\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}
}



/* o_end_par --- terminate parenthesized rhs */

o_end_par ()
{
	switch (language) {

	case RATFOR:
		if (num_actions > 0 && num_erractions <= 0)
		{
			INDENT(); 
			fprintf (stdout, "if (%s == ACCEPT) {\n", statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions > 0)
		{
			INDENT(); 
			fprintf (stdout, "if (%s == NOMATCH) {\n", statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions <= 0)
			;  /* do nothing */
		else
		{
			INDENT(); 
			fprintf (stdout, "select (%s)\n", statevar);
			STEP_IN();
			INDENT(); 
			fprintf (stdout, "when (NOMATCH) {\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "when (ACCEPT) {\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;

	case PL1:
		if (num_actions > 0 && num_erractions <= 0)
		{
			INDENT();
			fprintf (stdout, "if (%s = ACCEPT) then do;\n", statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions > 0)
		{
			INDENT();
			fprintf (stdout, "if (%s = NOMATCH) then do;\n", statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions <= 0)
			;  /* do nothing */
		else
		{
			INDENT(); 
			fprintf (stdout, "if (%s = NOMATCH) then do;\n", statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "else if (%s = ACCEPT) then do;\n",
			    statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;

	case PASCAL:
		if (num_actions > 0 && num_erractions <= 0)
		{
			INDENT();
			fprintf (pfd, "if (%s = ACCEPT) then begin\n", statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions > 0)
		{
			INDENT();
			fprintf (pfd,"if (%s = NOMATCH) then begin\n",statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions <= 0)
			;  /* do nothing */
		else
		{
			INDENT(); 
			fprintf (pfd, "case %s of\n", statevar);
			STEP_IN();
			INDENT(); 
			fprintf (pfd, "NOMATCH: begin\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
			INDENT(); 
			fprintf (pfd, "ACCEPT: begin\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
			STEP_OUT();
			INDENT(); 
			fprintf (pfd, "otherwise end;\n");
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;

	case C:
		if (num_actions > 0 && num_erractions <= 0)
		{
			INDENT(); 
			fprintf (stdout, "if (%s == ACCEPT) {\n", statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions > 0)
		{
			INDENT(); 
			fprintf (stdout, "if (%s == NOMATCH) {\n", statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions <= 0)
			;  /* do nothing */
		else
		{
			INDENT(); 
			fprintf (stdout, "switch (%s) {\n", statevar);
			STEP_IN();
			INDENT(); 
			fprintf (stdout, "case NOMATCH: {\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "break;\n");
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "case ACCEPT: {\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "break;\n");
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "}\n");
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;

	case PLP:
		if (num_actions > 0 && num_erractions <= 0)
		{
			INDENT();
			fprintf (stdout, "if (%s = ACCEPT) then do;\n", statevar);
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions > 0)
		{
			INDENT();
			fprintf (stdout, "if (%s = NOMATCH) then do;\n", statevar);
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		else if (num_actions <= 0 && num_erractions <= 0)
			;  /* do nothing */
		else
		{
			INDENT(); 
			fprintf (stdout, "select (%s);\n", statevar);
			STEP_IN();
			INDENT(); 
			fprintf (stdout, "when (NOMATCH) do;\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "when (ACCEPT) do;\n");
			STEP_IN();
			o_accept_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "end;\n");
			STEP_OUT();
		}
		svarval = UNKNOWN;
		break;
	}
}



/* o_end_rept --- terminate repeated rhs */

o_end_rept ()
{
	switch (language) {

	case RATFOR:
		INDENT();
		fprintf (stdout, "} until (%s ~= ACCEPT)\n", statevar);
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
	case PLP:
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "until (%s <> ACCEPT);\n", statevar);
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case C:
		INDENT();
		fprintf (stdout, "} while (%s == ACCEPT);\n", statevar);
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}

	o_end_opt();
}


/* o_end_routine ---  output cleanup code for a parsing routine */

o_end_routine ()
{
	switch (language) {

	case RATFOR:
		fprintf (stdout, "gpst = %s\n", statevar);
		fprintf (stdout, "return\n");
		fprintf (stdout, "end\n");
		break;

	case PL1:
	case PLP:
		INDENT(); 
		fprintf (stdout, "gpst = %s;\n", statevar);
		INDENT(); 
		fprintf (stdout, "return;\n");
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		break;

	case PASCAL:
		INDENT(); 
		fprintf (pfd, "gpst := %s;\n", statevar);
		INDENT(); 
		fprintf (pfd, "99:\n");
		STEP_OUT();
		fprintf (pfd, "end;\n");
		break;

	case C:
		INDENT(); 
		fprintf (stdout, "*gpst = %s;\n", statevar);
		STEP_OUT();
		fprintf (stdout, "}\n");
		break;
	}
}



/* o_end_seq --- output code to terminate the test for a sequence */

o_end_seq ()
{
	switch (language) {

	case RATFOR:
	case C:
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
	case PLP:
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT(); 
		fprintf (pfd, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}
}



/* o_end_term --- output cleanup and action code after a terminal */

o_end_term ()
{
	switch (language) {

	case RATFOR:
		o_accept_actions();
		if (advance == YES)
		{
			INDENT();
			fprintf (stdout, "call %s\n", scanner);
		}
		INDENT(); 
		fprintf (stdout, "}\n");
		if (num_erractions > 0)
		{
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "else {\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
		}
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
	case PLP:
		o_accept_actions();
		if (advance == YES)
		{
			INDENT();
			fprintf (stdout, "call %s;\n", scanner);
		}
		INDENT(); 
		fprintf (stdout, "end;\n");
		if (num_erractions > 0)
		{
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "else do;\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "end;\n");
		}
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PASCAL:
		o_accept_actions();
		if (advance == YES)
		{
			INDENT();
			fprintf (pfd, "%s\n", scanner);
		}
		INDENT(); 
		fprintf (pfd, "end");
		if (num_erractions > 0)
		{
			STEP_OUT();
			fprintf (pfd, "\n");
			INDENT(); 
			fprintf (pfd, "else begin\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (pfd, "end;\n");
		}
		else
			fprintf (pfd, ";\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case C:
		o_accept_actions();
		if (advance == YES)
		{
			INDENT();
			fprintf (stdout, "%s ();\n", scanner);
		}
		INDENT(); 
		fprintf (stdout, "}\n");
		if (num_erractions > 0)
		{
			STEP_OUT();
			INDENT(); 
			fprintf (stdout, "else {\n");
			STEP_IN();
			o_error_actions();
			INDENT(); 
			fprintf (stdout, "}\n");
		}
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}
}



/* o_epsilon --- output "empty" match */

o_epsilon ()
{
	switch (language) {

	case RATFOR:
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (stdout, "%s = ACCEPT\n", statevar);
			svarval = ACCEPT;
		}
		break;

	case PL1:
	case C:
	case PLP:
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (stdout, "%s = ACCEPT;\n", statevar);
			svarval = ACCEPT;
		}
		break;

	case PASCAL:
		if (svarval != ACCEPT)
		{
			INDENT();
			fprintf (pfd, "%s := ACCEPT;\n", statevar);
			svarval = ACCEPT;
		}
		break;
	}
}



/* o_error_actions --- output error actions, properly indented */

o_error_actions ()
{
	int i, l;
	FILE *fd;

	fd = stdout;
	if (language == PASCAL)
		fd = pfd;

	for (l = 1; l <= num_erractions; l++)
	{
		INDENT();
		i = erract_inx [l];
		if (erract_text [i] == ' ')
			fputs (& erract_text [i + 1], fd);
		else
			fputs (& erract_text [i], fd);
	}
}



/* o_match --- see if current symbol matches a terminal symbol */

o_match (sym)
char sym [];
{
	switch (language) {

	case RATFOR:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH\n", statevar);
		}
		INDENT(); 
		fprintf (stdout, "if (%s == %s) {\n", symbolvar, sym);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT\n", statevar);
		svarval = ACCEPT;
		break;

	case PL1:
	case PLP:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
		}
		INDENT();
		fprintf (stdout, "if (%s = %s) then do;\n", symbolvar, sym);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		svarval = ACCEPT;
		break;

	case PASCAL:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (pfd, "%s := NOMATCH;\n", statevar);
		}
		INDENT();
		fprintf (pfd, "if (%s = %s) then begin\n", symbolvar, sym);
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "%s := ACCEPT;\n", statevar);
		svarval = ACCEPT;
		break;

	case C:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
		}
		INDENT(); 
		fprintf (stdout, "if (%s == %s) {\n", symbolvar, sym);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		svarval = ACCEPT;
		break;
	}
}



/* o_match_range --- see if current symbol is within a range of terminals */

o_match_range (from, to)
char from [], to [];
{
	switch (language) {

	case RATFOR:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH\n", statevar);
		}
		INDENT(); 
		fprintf (stdout, "if (%s <= %s && %s <= %s) {\n",
		    from, symbolvar, symbolvar, to);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT\n", statevar);
		svarval = ACCEPT;
		break;

	case PL1:
	case PLP:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
		}
		INDENT(); 
		fprintf (stdout, "if ((%s <= %s) & (%s <= %s)) then do;\n",
		    from, symbolvar, symbolvar, to);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		svarval = ACCEPT;
		break;

	case PASCAL:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (pfd, "%s := NOMATCH;\n", statevar);
		}
		INDENT(); 
		fprintf (pfd, "if ((%s <= %s) AND (%s <= %s)) then begin\n",
		    from, symbolvar, symbolvar, to);
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "%s := ACCEPT;\n", statevar);
		svarval = ACCEPT;
		break;

	case C:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
		}
		INDENT(); 
		fprintf (stdout, "if (%s <= %s && %s <= %s) {\n",
		    from, symbolvar, symbolvar, to);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "%s = ACCEPT;\n", statevar);
		svarval = ACCEPT;
		break;
	}
}



/* o_selection_start --- output start of a "quick select" sequence */

o_selection_start ()
{
	switch (language) {

	case RATFOR:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH\n", statevar);
			svarval = NOMATCH;
		}
		INDENT(); 
		fprintf (stdout, "select (%s)\n", symbolvar);
		STEP_IN();
		break;

	case PL1:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
			svarval = NOMATCH;
		}
		INDENT(); 
		fprintf (stdout, "if ('0'b) then\n");
		STEP_IN();
		INDENT(); 
		fprintf (stdout, ";\n");
		STEP_OUT();
		break;

	case PASCAL:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (pfd, "%s := NOMATCH;\n", statevar);
			svarval = NOMATCH;
		}
		INDENT(); 
		fprintf (pfd, "case %s of\n", symbolvar);
		STEP_IN();
		break;

	case C:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
			svarval = NOMATCH;
		}
		INDENT(); 
		fprintf (stdout, "switch (%s) {\n", symbolvar);
		STEP_IN();
		break;

	case PLP:
		if (svarval != NOMATCH)
		{
			INDENT();
			fprintf (stdout, "%s = NOMATCH;\n", statevar);
			svarval = NOMATCH;
		}
		INDENT(); 
		fprintf (stdout, "select (%s);\n", symbolvar);
		STEP_IN();
		break;
	}
}



/* o_selection_end --- output end of a "quick select" sequence */

o_selection_end ()
{
	switch (language) {

	case RATFOR:
		STEP_OUT();
		break;

	case C:
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		break;

	case PASCAL:
		INDENT(); 
		fprintf (pfd, "otherwise end;\n");
		STEP_OUT();
		break;

	case PLP:
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		break;
	}
}



/* o_test_seq_failure --- output code to check for incomplete sequence */

o_test_seq_failure ()
{
	switch (language) {

	case RATFOR:
		INDENT();
		fprintf (stdout, "if (%s ~= ACCEPT) {\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "gpst = FAILURE\n");
		INDENT(); 
		fprintf (stdout, "return\n");
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PL1:
	case PLP:
		INDENT();
		fprintf (stdout, "if (%s ^= ACCEPT) then do;\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "gpst = FAILURE;\n");
		INDENT(); 
		fprintf (stdout, "return;\n");
		INDENT(); 
		fprintf (stdout, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case PASCAL:
		INDENT();
		fprintf (pfd, "if (%s <> ACCEPT) then begin\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (pfd, "gpst := FAILURE;\n");
		INDENT(); 
		fprintf (pfd, "goto 99\n");
		INDENT(); 
		fprintf (pfd, "end;\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;

	case C:
		INDENT();
		fprintf (stdout, "if (%s != ACCEPT) {\n", statevar);
		STEP_IN();
		INDENT(); 
		fprintf (stdout, "*gpst = FAILURE;\n");
		INDENT(); 
		fprintf (stdout, "return;\n");
		INDENT(); 
		fprintf (stdout, "}\n");
		STEP_OUT();
		svarval = UNKNOWN;
		break;
	}
}


/* putback --- push character back onto input */

putback (c)
char c;
{
	if (--ibp < 0)
		fprintf (stderr, "%d: too many characters pushed back\n",
			linenumber);
	else
		inbuf [ibp] = c;
}



/* scan_char --- read a quoted character, convert to integer */

scan_char ()
{
	char c, quote;
	char ngetch();
	int escaped_char = FALSE;
	char buf[MAXLINE];
	int i;

	quote = ngetch (& quote);
	for (i = 0; (c = ngetch (& c)) != quote; i++)
	{
		switch (c) {
		case '\\':
			escaped_char = TRUE;
			buf[i++] = c;
			c = ngetch (& c);
			break;

		case '\n':
			fprintf (stderr, "%d:  missing quote\n", linenumber);
			goto out;
			break;
		
		case EOF:
			error ("unexpected end of file in quoted character");
			break;
		}

		buf[i] = c;
	}
out:
	buf [i] = '\0';

	if (language != C)
		if (escaped_char)
		{
			char buf2[MAXLINE];

			sprintf (buf2, "C escapes not allowed in %s",
				lang_names[language]);
			error (buf2);
		}
		else if (i > 1)
		{
			char buf2[MAXLINE];

			sprintf (buf2,
				"multi character constants not allowed in %s",
				lang_names[language]);
			error (buf2);
		}


	if (Debug)
		fprintf (stderr, "quote = <%c>\tstring = <%s>\n", quote, buf);

	switch (language) {

	case RATFOR:
		(void) sprintf (symboltext, "'%c'c", c);
		break;

	case C:
		(void) sprintf (symboltext, "'%s'", buf);
		break;

	case PASCAL:
	case PL1:
	case PLP:
		(void) sprintf (symboltext, "%d", (int) c);
		break;

	}

	symbol = TERMIDSYM;
}


/* scan_id --- get next identifier from input stream */

scan_id ()
{
	char c;
	char ngetch();

	int i;

	i = 0;
	for (c = ngetch (& c); isalnum (c) || c == '_'; c = ngetch (& c))
	{
		symboltext [i] = c;
		i++;
	}
	putback (c);

	symboltext [i] = '\0';
}


/* scan_int --- scan integer present in input stream */

scan_int ()
{
	char c;
	char ngetch ();

	int i;

	i = 0;
	for (c = ngetch (& c); isdigit (c); c = ngetch (& c))
	{
		symboltext [i] = c;
		i++;
	}
	symboltext [i] = '\0';
	putback (c);

	symbol = INTSYM;
}


/* scan_is --- get "->" symbol from input stream */

scan_is ()
{
	char c;
	char ngetch ();

	if (ngetch (& c) != '>')
		fprintf (stderr, "%d:  -> symbol is ill-formed\n", linenumber);
	symbol = ISSYM;
}


/* strlsr --- linear search stab for an entry equal to str */

int strlsr (stab, tsize, esize, str)
char *stab, str[];
int tsize, esize;
{
	/* stab should have been declared like this:

	static struct {
		char *s;
		....
		} stab[] = {
			"string1",	...
			"string2",	...
			"string3",	...
			...
			};
	
	The call to strlsr should look like this:

	i = strlsr (stab, sizeof (stab), sizeof (stab[0]), str);
	*/

	register int j;
	int strcmp ();

	for (j = 0; & stab [j * esize] < stab + tsize; j++)
		if (strcmp (str, *(char **)(stab + esize * j)) == 0)
			return j;

	return EOF;
}

/* tab_over --- output spaces 'til we reach an appropriate column */

tab_over ()
{
	int i;
	FILE *fd;

	static char blanks [MAXLINE];
	static int first = TRUE;
	if (first)
	{
		for (i = 0; i <= MAXLINE - 2; i++)
			blanks[i] = ' ';
		blanks[i] = '\0';
		first = FALSE;
	}

	fd = stdout;
	if (language == PASCAL)
		fd = pfd;

	i = max (70, MAXLINE - indentation * TABSETTING);
	fputs (& blanks [i], fd);
}
