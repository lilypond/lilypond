%{ // -*-Fundamental-*-
/*
  lexer.ll -- implement the Flex lexer

  source file of the LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/


/*
  backup rules

  after making a change to the lexer rules, run 
      flex -b <this lexer file>
  and make sure that 
      lex.backup
  contains no backup states, but only the reminder
      Compressed tables always back up.
 (don-t forget to rm lex.yy.cc :-)
 */



#include <cstdio>
#include <cctype>
#include <cerrno>

/* Flex >= 2.5.29 fix; FlexLexer.h's multiple include bracing breaks
   when building the actual lexer.  */

#define LEXER_CC

#include <iostream>
using namespace std;

#include "context-def.hh"
#include "identifier-smob.hh"
#include "international.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "lily-lexer.hh"
#include "lilypond-input-version.hh"
#include "main.hh"
#include "music-function.hh"
#include "parse-scm.hh"
#include "parser.hh"
#include "source-file.hh"
#include "std-string.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"

/*
RH 7 fix (?)
*/
#define isatty HORRIBLEKLUDGE

void strip_trailing_white (string&);
void strip_leading_white (string&);
string lyric_fudge (string s);
int music_function_type (SCM);
SCM lookup_markup_command (string s);
bool is_valid_version (string s);


#define start_quote()	\
	yy_push_state (quote);\
	yylval.string = new string

#define start_lyric_quote()	\
	yy_push_state (lyric_quote);\
	yylval.string = new string

#define yylval \
	(*(YYSTYPE*)lexval)

#define yylloc \
	(*(YYLTYPE*)lexloc)

#define YY_USER_ACTION	add_lexed_char (YYLeng ());
/*

LYRICS		({AA}|{TEX})[^0-9 \t\n\f]*

*/


SCM scan_fraction (string);
SCM (* scm_parse_error_handler) (void *);



%}

%option c++
%option noyywrap
%option nodefault
%option debug
%option yyclass="Lily_lexer"
%option stack
%option never-interactive 
%option warn

%x chords
%x figures
%x incl
%x lyrics
%x lyric_quote
%x longcomment
%x markup
%x notes
%x quote
%x sourcefileline
%x sourcefilename
%x version

A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}
PUNCT		[?!:'`]
ACCENT		\\[`'"^]
NATIONAL	[\001-\006\021-\027\031\036\200-\377]
TEX		{AA}|-|{PUNCT}|{ACCENT}|{NATIONAL}
WORD		{A}{AN}*
DASHED_WORD		{A}({AN}|-)*
DASHED_KEY_WORD		\\{DASHED_WORD}
ALPHAWORD	{A}+
DIGIT		{N}
UNSIGNED	{N}+
E_UNSIGNED	\\{N}+
FRACTION	{N}+\/{N}+
INT		-?{UNSIGNED}
REAL		({INT}\.{N}*)|(-?\.{N}+)
KEYWORD		\\{WORD}
WHITE		[ \n\t\f\r]
HORIZONTALWHITE		[ \t]
BLACK		[^ \n\t\f\r]
RESTNAME	[rs]
NOTECOMMAND	\\{A}+
MARKUPCOMMAND	\\({A}|[-_])+
LYRICS		({AA}|{TEX})[^0-9 \t\n\r\f]*
ESCAPED		[nt\\'"]
EXTENDER	__
HYPHEN		--
BOM_UTF8	\357\273\277
%%


<*>\r		{
	// windows-suck-suck-suck
}

<INITIAL,chords,lyrics,figures,notes>{BOM_UTF8} {
  if (this->lexloc->line_number () != 1 || this->lexloc->column_number () != 0)
    {
      LexerError (_ ("stray UTF-8 BOM encountered").c_str ());
      exit (1);
    }
  if (be_verbose_global)
     message (_ ("Skipping UTF-8 BOM"));
}

<INITIAL,chords,figures,incl,lyrics,markup,notes>{
  "%{"	{
	yy_push_state (longcomment);
  }
  %[^{\n\r].*[\n\r]	{
  }
  %[^{\n\r]	{ // backup rule
  }
  %[\n\r]	{
  }
  %[^{\n\r].*	{
  }
  {WHITE}+ 	{

  }
}

<INITIAL,notes,figures,chords,markup>{
	\"		{
		start_quote ();
	}
}

<INITIAL,chords,lyrics,notes,figures>\\version{WHITE}*	{
	yy_push_state (version);
}
<INITIAL,chords,lyrics,notes,figures>\\sourcefilename{WHITE}*	{
	yy_push_state (sourcefilename);
}
<INITIAL,chords,lyrics,notes,figures>\\sourcefileline{WHITE}*	{
	yy_push_state (sourcefileline);
}
<version>\"[^"]*\"     { /* got the version number */
	string s (YYText () + 1);
	s = s.substr (0, s.rfind ('\"'));

	yy_pop_state ();

	SCM top_scope = scm_car (scm_last_pair (scopes_));
	scm_module_define (top_scope, ly_symbol2scm ("version-seen"), SCM_BOOL_T);

	if (!is_valid_version (s))
		return INVALID;


}
<sourcefilename>\"[^"]*\"     {
	string s (YYText () + 1);
	s = s.substr (0, s.rfind ('\"'));

	yy_pop_state ();
	this->here_input().get_source_file ()->name_ = s;
	message (_f ("Renaming input to: `%s'", s.c_str ()));
	progress_indication ("\n");
	scm_module_define (scm_car (scopes_),
		     ly_symbol2scm ("input-file-name"),
		     scm_makfrom0str (s.c_str ()));

}

<sourcefileline>{INT}	{
	int i;
	sscanf (YYText (), "%d", &i);

//	this->set_debug (1); 
	yy_pop_state ();
	this->here_input ().get_source_file ()->set_line (here_input ().start (), i);
}

<version>. 	{
	LexerError (_ ("quoted string expected after \\version").c_str ());
	yy_pop_state ();
}
<sourcefilename>. 	{
	LexerError (_ ("quoted string expected after \\sourcefilename").c_str ());
	yy_pop_state ();
}
<sourcefileline>. 	{
	LexerError (_ ("integer expected after \\sourcefileline").c_str ());
	yy_pop_state ();
}
<longcomment>{
	[^\%]* 		{
	}
	\%*[^}%]*		{

	}
	"%"+"}"		{
		yy_pop_state ();
	}
	<<EOF>> 	{
		LexerError (_ ("EOF found inside a comment").c_str ());
		is_main_input_ = false;
		if (! close_input ()) 
		  yyterminate (); // can't move this, since it actually rets a YY_NULL
	}
}


<INITIAL,chords,lyrics,notes,figures>\\maininput           {
	if (!is_main_input_)
	{
		start_main_input ();
		is_main_input_ = true;
	}
	else
		error (_ ("\\maininput not allowed outside init files"));
}

<INITIAL,chords,lyrics,figures,notes>\\include           {
	yy_push_state (incl);
}
<incl>\"[^"]*\"   { /* got the include file name */
	string s (YYText ()+1);
	s = s.substr (0, s.rfind ('"'));

	new_input (s, sources_);
	yy_pop_state ();
}
<incl>\\{BLACK}*{WHITE} { /* got the include identifier */
	string s = YYText () + 1;
	strip_trailing_white (s);
	if (s.length () && (s[s.length () - 1] == ';'))
	  s = s.substr (0, s.length () - 1);

	SCM sid = lookup_identifier (s);
	if (scm_is_string (sid)) {
		new_input (ly_scm2string (sid), sources_);
		yy_pop_state ();
	} else { 
	    string msg (_f ("wrong or undefined identifier: `%s'", s ));

	    LexerError (msg.c_str ());
	    SCM err = scm_current_error_port ();
	    scm_puts ("This value was found in the table: ", err);
	    scm_display (sid, err);
	  }
}
<incl>\"[^"]*   { // backup rule
	error (_ ("end quote missing"));
	exit (1);
}
<chords,notes,figures>{RESTNAME} 	{
	char const *s = YYText ();
	yylval.scm = scm_makfrom0str (s);
	return RESTNAME;
}
<chords,notes,figures>R		{
	return MULTI_MEASURE_REST;
}
<INITIAL,chords,figures,lyrics,markup,notes>#	{ //embedded scm
	int n = 0;
	Input hi = here_input();
	hi.step_forward ();
	SCM sval = ly_parse_scm (hi.start (), &n, hi,
		be_safe_global && is_main_input_);

	if (sval == SCM_UNDEFINED)
	{
		sval = SCM_UNSPECIFIED;
		error_level_ = 1;
 	}

	for (int i = 0; i < n; i++)
	{
		yyinput ();
	}
	char_count_stack_.back () += n;

	if (unpack_identifier (sval) != SCM_UNDEFINED)
	{
		yylval.scm = unpack_identifier(sval);
		return identifier_type (yylval.scm);
	}
		
	yylval.scm = sval;
	return SCM_TOKEN;
}
<INITIAL,notes,lyrics>{ 
	\<\<	{
		return DOUBLE_ANGLE_OPEN;
	}
	\>\>	{
		return DOUBLE_ANGLE_CLOSE;
	}
}

<INITIAL,notes>{
	\<	{
		return ANGLE_OPEN;
	}
	\>	{
		return ANGLE_CLOSE;
	}
}

<figures>{
	_	{
		return FIGURE_SPACE;
	}
	\>		{
		return FIGURE_CLOSE;
	}
	\< 	{
		return FIGURE_OPEN;
	}
}

<notes,figures>{
	{ALPHAWORD}	{
		return scan_bare_word (YYText ());
	}

	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1); 
	}
	{FRACTION}	{
		yylval.scm =  scan_fraction (YYText ());
		return FRACTION;
	}

	{DIGIT}		{
		yylval.i = String_convert::dec2int (string (YYText ()));
		return DIGIT;
	}
	{UNSIGNED}		{
		yylval.i = String_convert::dec2int (string (YYText ()));
		return UNSIGNED;
	}
	{E_UNSIGNED}	{
		yylval.i = String_convert::dec2int (string (YYText () +1));
		return E_UNSIGNED;
	}
}

<quote,lyric_quote>{
	\\{ESCAPED}	{
		*yylval.string += to_string (escaped_char (YYText ()[1]));
	}
	[^\\"]+	{
		*yylval.string += YYText ();
	}
	\"	{

		yy_pop_state ();

		/* yylval is union. Must remember STRING before setting SCM*/
		string *sp = yylval.string;
		yylval.scm = scm_makfrom0str (sp->c_str ());
		delete sp;
		return is_lyric_state () ? LYRICS_STRING : STRING;
	}
	.	{
		*yylval.string += YYText ();
	}
}

<lyrics>{
	\" {
		start_lyric_quote ();
	}
	{FRACTION}	{
		yylval.scm =  scan_fraction (YYText ());
		return FRACTION;
	}
	{UNSIGNED}		{
		yylval.i = String_convert::dec2int (string (YYText ()));
		return UNSIGNED;
	}
	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1);
	}
	{LYRICS} {
		/* ugr. This sux. */
		string s (YYText ()); 
		if (s == "__")
			return yylval.i = EXTENDER;
		if (s == "--")
			return yylval.i = HYPHEN;
		s = lyric_fudge (s);

		char c = s[s.length () - 1];
		if (c == '{' ||  c == '}') // brace open is for not confusing dumb tools.
			here_input ().warning (
				_ ("Brace found at end of lyric.  Did you forget a space?"));
		yylval.scm = scm_makfrom0str (s.c_str ());


		return LYRICS_STRING;
	}
	. {
		return YYText ()[0];
	}
}
<chords>{
	{ALPHAWORD}	{
		return scan_bare_word (YYText ());
	}
	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1);
	}
	{FRACTION}	{
		yylval.scm =  scan_fraction (YYText ());
		return FRACTION;
	}
	{UNSIGNED}		{
		yylval.i = String_convert::dec2int (string (YYText ()));
		return UNSIGNED;
	}
	-  {
		return CHORD_MINUS;
	}
	:  {
		return CHORD_COLON;
	}
	\/\+ {
		return CHORD_BASS;
	}
	\/  {
		return CHORD_SLASH;
	}
	\^  {
		return CHORD_CARET;
	}
	. {
		return YYText ()[0];
	}
}


<markup>{
	\\score {
		return SCORE;
	}
	{MARKUPCOMMAND} {
		string str (YYText () + 1);
		SCM s = lookup_markup_command (str);

		if (scm_is_pair (s) && scm_is_symbol (scm_cdr (s)) ) {
			yylval.scm = scm_car(s);
			SCM tag = scm_cdr(s);
			if (tag == ly_symbol2scm("markup0"))
				return MARKUP_HEAD_MARKUP0;
			if (tag == ly_symbol2scm("empty"))
				return MARKUP_HEAD_EMPTY;
			else if (tag == ly_symbol2scm ("markup0-markup1"))
				return MARKUP_HEAD_MARKUP0_MARKUP1;
			else if (tag == ly_symbol2scm ("markup-list0"))
				return MARKUP_HEAD_LIST0;
			else if (tag == ly_symbol2scm ("scheme0"))
				return MARKUP_HEAD_SCM0;
			else if (tag == ly_symbol2scm ("scheme0-scheme1"))
				return MARKUP_HEAD_SCM0_SCM1;
			else if (tag == ly_symbol2scm ("scheme0-markup1"))
				return MARKUP_HEAD_SCM0_MARKUP1;
			else if (tag == ly_symbol2scm ("scheme0-scheme1-markup2"))
				return MARKUP_HEAD_SCM0_SCM1_MARKUP2;
			else if (tag == ly_symbol2scm ("scheme0-scheme1-scheme2"))
				return MARKUP_HEAD_SCM0_SCM1_SCM2;
			else {
				programming_error ("no parser tag defined for this markup signature"); 
				ly_display_scm (s);
				assert(false);
			}
		} else
			return scan_escaped_word (str);
	}
	[{}]	{
		return YYText ()[0];
	}
	[^#{}"\\ \t\n\r\f]+ {
		string s (YYText ()); 

		char c = s[s.length () - 1];
		/* brace open is for not confusing dumb tools.  */
		if (c == '{' ||  c == '}')
			here_input ().warning (
				_ ("Brace found at end of markup.  Did you forget a space?"));
		yylval.scm = scm_makfrom0str (s.c_str ());


		return STRING;
	}
	.  {
		return YYText()[0];
	}
}

<*><<EOF>> {
	if (is_main_input_)
	{
		is_main_input_ = false;
		if (!close_input ())
 	        /* Returns YY_NULL */
			yyterminate ();
	}
	else if (!close_input ())
 	        /* Returns YY_NULL */
 	  	yyterminate ();
}

<INITIAL>{
	{DASHED_WORD}	{
		return scan_bare_word (YYText ());
	}
	{DASHED_KEY_WORD}	{
		return scan_escaped_word (YYText () + 1);
	}
}

{WORD}	{
	return scan_bare_word (YYText ());
}
{KEYWORD}	{
	return scan_escaped_word (YYText () + 1);
}
{REAL}		{
	Real r;
	int cnv = sscanf (YYText (), "%lf", &r);
	assert (cnv == 1);
	(void) cnv;

	yylval.scm = scm_from_double (r);
	return REAL;
}

{UNSIGNED}	{
	yylval.i = String_convert::dec2int (string (YYText ()));
	return UNSIGNED;
}


[{}]	{

	return YYText ()[0];
}
[*:=]		{
	char c = YYText ()[0];

	return c;
}

<INITIAL,notes,figures>.	{
	return YYText ()[0];
}

<INITIAL,lyrics,notes,figures>\\. {
    char c = YYText ()[1];

    switch (c) {
    case '>':
	return E_ANGLE_CLOSE;
    case '<':
	return E_ANGLE_OPEN;
    case '!':
	return E_EXCLAMATION;
    case '(':
	return E_OPEN;
    case ')':
	return E_CLOSE;
    case '[':
	return E_BRACKET_OPEN;
    case '+':
	return E_PLUS;
    case ']':
	return E_BRACKET_CLOSE;
    case '~':
	return E_TILDE;
    case '\\':
	return E_BACKSLASH;

    default:
	return E_CHAR;
    }
}

<*>.		{
	string msg = _f ("invalid character: `%c'", YYText ()[0]);
	LexerError (msg.c_str ());
	return YYText ()[0];
}

%%

void
Lily_lexer::push_chord_state (SCM tab)
{
	pitchname_tab_stack_ = scm_cons (tab, pitchname_tab_stack_);
	yy_push_state (chords);
}

void
Lily_lexer::push_figuredbass_state ()
{
	yy_push_state (figures);
}

void
Lily_lexer::push_initial_state ()
{
	yy_push_state (INITIAL);
}

void
Lily_lexer::push_lyric_state ()
{
	yy_push_state (lyrics);
}

void
Lily_lexer::push_markup_state ()
{
	yy_push_state (markup);
}

void
Lily_lexer::push_note_state (SCM tab)
{
	pitchname_tab_stack_ = scm_cons (tab, pitchname_tab_stack_);
	yy_push_state (notes);
}

void
Lily_lexer::pop_state ()
{
	if (YYSTATE == notes || YYSTATE == chords)
		pitchname_tab_stack_ = scm_cdr (pitchname_tab_stack_);
	yy_pop_state ();
}

int
Lily_lexer::identifier_type (SCM sid)
{
	int k = try_special_identifiers (&yylval.scm , sid);
	return k >= 0  ? k : SCM_IDENTIFIER;
}


int
Lily_lexer::scan_escaped_word (string str)
{
	// use more SCM for this.

//	SCM sym = ly_symbol2scm (str.c_str ());

	int i = lookup_keyword (str);
 	if (i == MARKUP && is_lyric_state ())
 		return LYRIC_MARKUP;
	if (i != -1)
		return i;

	SCM sid = lookup_identifier (str);
	if (is_music_function (sid))
	{
		yylval.scm = get_music_function_transform (sid);
		return music_function_type (yylval.scm);
	}

	if (sid != SCM_UNDEFINED)
	{
		yylval.scm = sid;
		return identifier_type (sid);
	}

	string msg (_f ("unknown escaped string: `\\%s'", str));	
	LexerError (msg.c_str ());

	yylval.scm = scm_makfrom0str (str.c_str ());

	return STRING;
}

int
Lily_lexer::scan_bare_word (string str)
{
	SCM sym = ly_symbol2scm (str.c_str ());
	if ((YYSTATE == notes) || (YYSTATE == chords)) {
		SCM handle = SCM_BOOL_F;
		if (scm_is_pair (pitchname_tab_stack_))
			handle = scm_hashq_get_handle (scm_car (pitchname_tab_stack_), sym);
		
		if (scm_is_pair (handle)) {
			yylval.scm = scm_cdr (handle);
			if (unsmob_pitch (yylval.scm)) 
	                    return (YYSTATE == notes) ? NOTENAME_PITCH : TONICNAME_PITCH;
			else if (scm_is_symbol (yylval.scm))
			    return DRUM_PITCH;
		}
		else if ((handle = scm_hashq_get_handle (chordmodifier_tab_, sym))!= SCM_BOOL_F)
		{
		    yylval.scm = scm_cdr (handle);
		    return CHORD_MODIFIER;
		}
	}

	yylval.scm = scm_makfrom0str (str.c_str ());
	return STRING;
}

bool
Lily_lexer::is_note_state () const
{
	return YY_START == notes;
}

bool
Lily_lexer::is_chord_state () const
{
	return YY_START == chords;
}

bool
Lily_lexer::is_lyric_state () const
{
	return YY_START == lyrics;
}

bool
Lily_lexer::is_figure_state () const
{
	return YY_START == figures;
}

/*
 urg, belong to string (_convert)
 and should be generalised 
 */
void
strip_leading_white (string&s)
{
	ssize i = 0;
	for (;  i < s.length (); i++)
		if (!isspace (s[i]))
			break;

	s = s.substr (i);
}

void
strip_trailing_white (string&s)
{
	ssize i = s.length ();	
	while (i--) 
		if (!isspace (s[i]))
			break;

	s = s.substr (0, i + 1);
}



Lilypond_version oldest_version ("2.7.38");


bool
is_valid_version (string s)
{
  Lilypond_version current ( MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL );
  Lilypond_version ver (s);
  if (! ((ver >= oldest_version) && (ver <= current)))
	{	
		non_fatal_error (_f ("Incorrect lilypond version: %s (%s, %s)", ver.to_string (), oldest_version.to_string (), current.to_string ()));
		non_fatal_error (_ ("Consider updating the input with the convert-ly script")); 
		return false;
    }
  return true;
}
	

/*
  substitute _ and \,
*/
string
lyric_fudge (string s)
{
  char *chars = string_copy (s);

  for (char *p = chars; *p ; p++)
    {
      if (*p == '_' && (p == chars || *(p-1) != '\\'))
	*p = ' ';
    }
  
  s = string (chars);
  delete[] chars;

  ssize i = 0;	
  if ((i = s.find ("\\,")) != NPOS)   // change "\," to TeX's "\c "
    {
      * (((char*)s.c_str ()) + i + 1) = 'c';
      s = s.substr (0, i + 2) + " " + s.substr (i - 2);
    }

  return s;
}

/*
Convert "NUM/DEN" into a '(NUM . DEN) cons.
*/
SCM
scan_fraction (string frac)
{
	ssize i = frac.find ('/');
	string left = frac.substr (0, i);
	string right = frac.substr (i + 1, (frac.length () - i + 1));

	int n = String_convert::dec2int (left);
	int d = String_convert::dec2int (right);
	return scm_cons (scm_from_int (n), scm_from_int (d));
}

SCM
lookup_markup_command (string s)
{
	SCM proc = ly_lily_module_constant ("lookup-markup-command");
	return scm_call_1 (proc, scm_makfrom0str (s.c_str ()));
}

struct Parser_signature
{
	char *symbol;
	int token_type;
};
static SCM signature_hash_table;

static void init_signature_hash_table ()
{
	signature_hash_table = scm_gc_protect_object (scm_c_make_hash_table (31));
	Parser_signature sigs[]  = {
		{"scm", MUSIC_FUNCTION_SCM},
		{"music", MUSIC_FUNCTION_MUSIC},
		{"scm-music", MUSIC_FUNCTION_SCM_MUSIC},
		{"scm-scm", MUSIC_FUNCTION_SCM_SCM},
		{"music-music", MUSIC_FUNCTION_MUSIC_MUSIC},
		{"scm-music-music", MUSIC_FUNCTION_SCM_MUSIC_MUSIC},
		{"scm-scm-music-music", MUSIC_FUNCTION_SCM_SCM_MUSIC_MUSIC},
		{"scm-scm-music", MUSIC_FUNCTION_SCM_SCM_MUSIC},
		{"scm-scm-scm-music", MUSIC_FUNCTION_SCM_SCM_SCM_SCM_MUSIC},
		{"scm-scm-scm-scm-music", MUSIC_FUNCTION_SCM_SCM_SCM_MUSIC},
		{"scm-scm-scm", MUSIC_FUNCTION_SCM_SCM_SCM},
		{"markup", MUSIC_FUNCTION_MARKUP},
		{"markup-music", MUSIC_FUNCTION_MARKUP_MUSIC},
		{"markup-markup", MUSIC_FUNCTION_MARKUP_MARKUP},
		{"markup-music-music", MUSIC_FUNCTION_MARKUP_MUSIC_MUSIC},
		{"markup-markup-music", MUSIC_FUNCTION_MARKUP_MARKUP_MUSIC},
		{"noarg", MUSIC_FUNCTION},
		{0,0}
	};

	for (int i = 0; sigs[i].symbol; i++)
		scm_hashq_set_x (signature_hash_table, ly_symbol2scm (sigs[i].symbol),
				 scm_from_int (sigs[i].token_type));
}

int
music_function_type (SCM func)
{
	if (!signature_hash_table)
		init_signature_hash_table ();

	SCM type = scm_object_property (func, ly_symbol2scm ("music-function-signature-keyword"));
	SCM token_type = scm_hashq_ref (signature_hash_table, type, SCM_BOOL_F);
	if (!scm_is_number (token_type))
		{
		programming_error (_ ("can't find signature for music function"));
		return MUSIC_FUNCTION_SCM;
		}
	
	return scm_to_int (token_type);
}

/* Shut up lexer warnings.  */
#if YY_STACK_USED

static void
yy_push_state (int)
{
}

static void
yy_pop_state ()
{
}

static int
yy_top_state ()
{
  return 0;
}

static void
silence_lexer_warnings ()
{
   (void) yy_start_stack_ptr;
   (void) yy_start_stack_depth;
   (void) yy_start_stack;
   (void) yy_push_state;
   (void) yy_pop_state;
   (void) yy_top_state;
   (void) silence_lexer_warnings;
}
#endif
