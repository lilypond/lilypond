%{ // -*-Fundamental-*-
/*
  lexer.ll -- implement the Flex lexer

  source file of the LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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


#include <stdio.h>
#include <ctype.h>
#include <errno.h>

/* Flex >= 2.5.29 fix; FlexLexer.h's multiple include bracing breaks
   when building the actual lexer.  */

#define LEXER_CC

#include <iostream>

using namespace std;

#include "music-function.hh"
#include "source-file.hh"
#include "parse-scm.hh"
#include "lily-guile.hh"
#include "string.hh"
#include "string-convert.hh"
#include "lily-lexer.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "parser.hh"
#include "warn.hh"
#include "main.hh"
#include "version.hh"
#include "lilypond-input-version.hh"
#include "context-def.hh"
#include "identifier-smob.hh"

/*
RH 7 fix (?)
*/
#define isatty HORRIBLEKLUDGE

void strip_trailing_white (String&);
void strip_leading_white (String&);
String lyric_fudge (String s);
int music_function_type (SCM);
SCM lookup_markup_command (String s);
bool is_valid_version (String s);


#define start_quote()	\
	yy_push_state (quote);\
	yylval.string = new String

#define start_lyric_quote()	\
	yy_push_state (lyric_quote);\
	yylval.string = new String

#define yylval \
	(*(YYSTYPE*)lexval)

#define YY_USER_ACTION	add_lexed_char (YYLeng ());
/*

LYRICS		({AA}|{TEX})[^0-9 \t\n\f]*

*/


SCM scan_fraction (String);
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
%x encoding
%x figures
%x incl
%x lyrics
%x lyric_quote
%x longcomment
%x markup 
%x notes
%x quote
%x renameinput
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
%%


<*>\r		{
	// windows-suck-suck-suck
}

<INITIAL,chords,incl,markup,lyrics,notes,figures>{
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

<INITIAL,notes>\\encoding{WHITE}* {
	yy_push_state (encoding);
}
<INITIAL,chords,lyrics,notes,figures>\\version{WHITE}*	{
	yy_push_state (version);
}
<INITIAL,chords,lyrics,notes,figures>\\renameinput{WHITE}*	{
	yy_push_state (renameinput);
}
<encoding>\"[^"]*\"     {
	String s (YYText () + 1);
	s = s.left_string (s.index_last ('\"'));
	set_encoding (s);
	yy_pop_state ();
}
<version>\"[^"]*\"     { /* got the version number */
	String s (YYText () + 1);
	s = s.left_string (s.index_last ('\"'));

	yy_pop_state ();
	if (!is_valid_version (s))
		return INVALID;
}
<renameinput>\"[^"]*\"     {
	String s (YYText ()+1);
	s = s.left_string (s.index_last ('\"'));

	yy_pop_state();
	this->here_input().source_file_->name_ = s;
	progress_indication ("\n");
	progress_indication (_f ("input renamed to: `%s'", s.to_str0 ()));
	progress_indication ("\n");
	scm_module_define (scm_car (scopes_),
		     ly_symbol2scm ("input-file-name"),
		     scm_makfrom0str (s.to_str0()));

}
<encoding>. 	{
	LexerError (_ ("No quoted string found after \\encoding").to_str0 ());
	yy_pop_state ();
}
<version>. 	{
	LexerError (_ ("No quoted string found after \\version").to_str0 ());
	yy_pop_state ();
}
<renameinput>. 	{
	LexerError (_ ("No quoted string found after \\renameinput").to_str0 ());
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
		LexerError (_ ("EOF found inside a comment").to_str0 ());
		main_input_b_ = false;
		if (! close_input ()) 
		  yyterminate (); // can't move this, since it actually rets a YY_NULL
	}
}


<INITIAL,chords,lyrics,notes,figures>\\maininput           {
	if (!main_input_b_)
	{
		start_main_input ();
		main_input_b_ = true;
	}
	else
		error (_ ("\\maininput not allowed outside init files"));
}

<INITIAL,chords,lyrics,figures,notes>\\include           {
	yy_push_state (incl);
}
<incl>\"[^"]*\";?   { /* got the include file name */
/* FIXME: semicolon? */
	String s (YYText ()+1);
	s = s.left_string (s.index_last ('"'));

	new_input (s, sources_);
	yy_pop_state ();
}
<incl>\\{BLACK}*;?{WHITE} { /* got the include identifier */
/* FIXME: semicolon? */
	String s = YYText () + 1;
	strip_trailing_white (s);
	if (s.length () && (s[s.length () - 1] == ';'))
	  s = s.left_string (s.length () - 1);

	SCM sid = lookup_identifier (s);
	if (scm_is_string (sid)) {
		new_input (ly_scm2string (sid), sources_);
		yy_pop_state ();
	} else { 
	    String msg (_f ("wrong or undefined identifier: `%s'", s ));

	    LexerError (msg.to_str0 ());
	    SCM err = scm_current_error_port ();
	    scm_puts ("This value was found in the table: ", err);
	    scm_display (sid, err);
	  }
}
<incl>\"[^"]*   { // backup rule
	error (_ ("Missing end quote"));
	exit (1);
}
<chords,notes,figures>{RESTNAME} 	{
	const char *s = YYText ();
	yylval.scm = scm_makfrom0str (s);
	return RESTNAME;
}
<chords,notes,figures>R		{
	return MULTI_MEASURE_REST;
}
<INITIAL,markup,chords,lyrics,notes,figures>#	{ //embedded scm
	//char const* s = YYText () + 1;
	char const* s = here_str0 ();
	int n = 0;
	SCM sval = ly_parse_scm (s, &n, here_input (),
		safe_global_b && main_input_b_);

	if (sval == SCM_UNDEFINED)
	{
		sval = SCM_UNSPECIFIED;
		error_level_ = 1;
 	}

	for (int i=0; i < n; i++)
	{
		yyinput ();
	}
	char_count_stack_.top () += n;

	if (unpack_identifier (sval) != SCM_UNDEFINED)
	{
		yylval.scm = unpack_identifier(sval);
		return identifier_type (yylval.scm);
	}
		
	yylval.scm = sval;
	return SCM_T;
}
<INITIAL,notes,lyrics>{ 
	\<\<   {
		return DOUBLE_ANGLE_OPEN;
	}
	\>\>   {
		return DOUBLE_ANGLE_CLOSE;
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
		yylval.i = String_convert::dec2int (String (YYText ()));
		return DIGIT;
	}
	{UNSIGNED}		{
		yylval.i = String_convert::dec2int (String (YYText ()));
		return UNSIGNED;
	}
	{E_UNSIGNED}	{
		yylval.i = String_convert::dec2int (String (YYText () +1));
		return E_UNSIGNED;
	}

	\" {
		start_quote ();
	}
}

\"		{
	start_quote ();
}
<quote>{
	\\{ESCAPED}	{
		*yylval.string += to_string (escaped_char (YYText ()[1]));
	}
	[^\\"]+	{
		*yylval.string += YYText ();
	}
	\"	{

		yy_pop_state ();

		/* yylval is union. Must remember STRING before setting SCM*/
		String *sp = yylval.string;
		yylval.scm = scm_makfrom0str (sp->to_str0 ());
		delete sp;
		return STRING;
	}
	.	{
		*yylval.string += YYText ();
	}
}
<lyric_quote>{
	\\{ESCAPED}	{
		*yylval.string += to_string (escaped_char (YYText ()[1]));
	}
	[^\\"]+	{
		*yylval.string += YYText ();
	}
	\"	{

		yy_pop_state ();

		/* yylval is union. Must remember STRING before setting SCM*/
		String *sp = yylval.string;
		yylval.scm = scm_makfrom0str (sp->to_str0 ());
		delete sp;
		return LYRICS_STRING;
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
		yylval.i = String_convert::dec2int (String (YYText ()));
		return UNSIGNED;
	}
	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1);
	}
	{LYRICS} {
		/* ugr. This sux. */
		String s (YYText ()); 
		if (s == "__")
			return yylval.i = EXTENDER;
		if (s == "--")
			return yylval.i = HYPHEN;
		s = lyric_fudge (s);

		char c = s[s.length () - 1];
		if (c == '{' ||  c == '}') // brace open is for not confusing dumb tools.
			here_input ().warning (
				_ ("Brace found at end of lyric.  Did you forget a space?"));
		yylval.scm = scm_makfrom0str (s.to_str0 ());


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
		yylval.i = String_convert::dec2int (String (YYText ()));
		return UNSIGNED;
	}
	\" {
		start_quote ();
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
	\" {
		start_quote ();
	}
	\< {
		return '<';
	}
	\> {
		return '>';
	}
	\\score {
		return SCORE;
	}
	{MARKUPCOMMAND} {
		String str (YYText () + 1);
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
				programming_error ("No parser tag defined for this signature. Abort"); 
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
		String s (YYText ()); 

		char c = s[s.length () - 1];
		/* brace open is for not confusing dumb tools.  */
		if (c == '{' ||  c == '}')
			here_input ().warning (
				_ ("Brace found at end of markup.  Did you forget a space?"));
		yylval.scm = scm_makfrom0str (s.to_str0 ());


		return STRING;
	}
	.  {
		return YYText()[0];
	}
}

<<EOF>> {
	if (main_input_b_)
	{
		main_input_b_ = false;
		if (!close_input ())
 	        /* Returns YY_NULL */
			yyterminate ();
	}
	else if (!close_input ())
 	        /* Returns YY_NULL */
 	  	yyterminate ();
}


{WORD}	{
	return scan_bare_word (YYText ());
}
{KEYWORD}	{
	return scan_escaped_word (YYText () + 1);
}
{REAL}		{
	Real r;
	int cnv=sscanf (YYText (), "%lf", &r);
	assert (cnv == 1);

	yylval.scm = scm_make_real (r);
	return REAL;
}

{UNSIGNED}	{
	yylval.i = String_convert::dec2int (String (YYText ()));
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
    char c= YYText ()[1];

    switch (c) {
    case '>':
	return E_BIGGER;
    case '<':
	return E_SMALLER;
    case '!':
	return E_EXCLAMATION;
    case '(':
	return E_OPEN;
    case ')':
	return E_CLOSE;
    case '[':
	return E_LEFTSQUARE;
    case ']':
	return E_RIGHTSQUARE;
    case '~':
	return E_TILDE;
    case '\\':
	return E_BACKSLASH;

    default:
	return E_CHAR;
    }
}

<*>.		{
	String msg = _f ("invalid character: `%c'", YYText ()[0]);
	LexerError (msg.to_str0 ());
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
Lily_lexer::scan_escaped_word (String str)
{
	// use more SCM for this.

//	SCM sym = ly_symbol2scm (str.to_str0 ());

	int l = lookup_keyword (str);
	if (l != -1) {
		return l;
	}
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

	String msg (_f ("unknown escaped string: `\\%s'", str));	
	LexerError (msg.to_str0 ());

	yylval.scm = scm_makfrom0str (str.to_str0 ());

	return STRING;
}

int
Lily_lexer::scan_bare_word (String str)
{
	SCM sym = ly_symbol2scm (str.to_str0 ());
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

	yylval.scm = scm_makfrom0str (str.to_str0 ());
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
 urg, belong to String (_convert)
 and should be generalised 
 */
void
strip_leading_white (String&s)
{
	int i=0;
	for (;  i < s.length (); i++) 
		if (!isspace (s[i]))
			break;

	s = s.nomid_string (0, i);
}

void
strip_trailing_white (String&s)
{
	int i=s.length ();	
	while (i--) 
		if (!isspace (s[i]))
			break;

	s = s.left_string (i+1);
}



/* 2.1.2x something -> \property -> \set. */ 
Lilypond_version oldest_version ("2.3.22");


bool
is_valid_version (String s)
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
String
lyric_fudge (String s)
{
  char  * chars  =s.get_copy_str0 ();

  for (char * p = chars; *p ; p++)
    {
      if (*p == '_' && (p == chars || *(p-1) != '\\'))
	*p = ' ';
    }
  
  s = String (chars);
  delete[] chars;

  int i =0;	
  if ((i=s.index ("\\,")) != -1)   // change "\," to TeX's "\c "
    {
      * (s.get_str0 () + i + 1) = 'c';
      s = s.left_string (i+2) + " " + s.right_string (s.length ()-i-2);
    }

  return s;
}

/*
Convert "NUM/DEN" into a '(NUM . DEN) cons.
*/
SCM
scan_fraction (String frac)
{
	int i = frac.index ('/');
	int l = frac.length ();
	String left = frac.left_string (i);
	String right = frac.right_string (l - i - 1);

	int n = String_convert::dec2int (left);
	int d = String_convert::dec2int (right);
	return scm_cons (scm_int2num (n), scm_int2num (d));
}

// Breaks for flex 2.5.31
#if 0
/* avoid silly flex induced gcc warnings */
static void yy_push_state (int) {;}
static void yy_pop_state () {;}
static int yy_top_state () { return 0; }

static void
avoid_silly_flex_induced_gcc_warnings ()
{
	(void)yy_start_stack_ptr;
	(void)yy_start_stack_depth;
	(void)yy_start_stack;
	yy_push_state (0);
	yy_pop_state ();
	yy_top_state ();
	avoid_silly_flex_induced_gcc_warnings ();
}
#endif

SCM
lookup_markup_command (String s)
{
	SCM proc = ly_scheme_function ("lookup-markup-command");
	return scm_call_1 (proc, scm_makfrom0str (s.to_str0 ()));
}


int
music_function_type (SCM func)
{
	SCM type= scm_object_property (func, ly_symbol2scm ("music-function-signature-keyword"));
	if (type == ly_symbol2scm ("scm"))
	{
		return MUSIC_FUNCTION_SCM;
	}
	else if (type == ly_symbol2scm ("music"))
	{
		return MUSIC_FUNCTION_MUSIC;
	}
	else if (type == ly_symbol2scm ("scm-music"))
	{
		return MUSIC_FUNCTION_SCM_MUSIC;
	}
	else if (type == ly_symbol2scm ("scm-scm"))
	{
		return MUSIC_FUNCTION_SCM_SCM;
	}
	else if (type == ly_symbol2scm ("music-music"))
	{
		return MUSIC_FUNCTION_MUSIC_MUSIC;
	}
	else if (type == ly_symbol2scm ("scm-music-music"))
	{
		return MUSIC_FUNCTION_SCM_MUSIC_MUSIC;
	}
	else if (type == ly_symbol2scm ("scm-scm-music"))
	{
		return MUSIC_FUNCTION_SCM_SCM_MUSIC;
	}
	else if (type == ly_symbol2scm ("markup"))
	{
		return MUSIC_FUNCTION_MARKUP;
	}
	else if (type == ly_symbol2scm ("markup-music"))
	{
		return MUSIC_FUNCTION_MARKUP_MUSIC;
	}
	else if (type == ly_symbol2scm ("markup-markup"))
	{
		return MUSIC_FUNCTION_MARKUP_MARKUP;
	}
	else if (type == ly_symbol2scm ("markup-music-music"))
	{
		return MUSIC_FUNCTION_MARKUP_MUSIC_MUSIC;
	}
	else if (type == ly_symbol2scm ("markup-markup-music"))
	{
		return MUSIC_FUNCTION_MARKUP_MARKUP_MUSIC;
	}
	else if (type == ly_symbol2scm ("noarg"))
	{
		return MUSIC_FUNCTION;
	}
	else
		{
		/* TODO: print location */
		error ("Can not find signature for music function.");
		}

	return MUSIC_FUNCTION_SCM;
}
