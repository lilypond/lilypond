%{ // -*-Fundamental-*-
/*
  lexer.l -- implement the Flex lexer

  source file of the LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

#include <iostream>

#include "parse-scm.hh"
#include "score.hh"
#include "lily-guile.hh"
#include "string.hh"
#include "string-convert.hh"
#include "my-lily-lexer.hh"
#include "input-file-results.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "parser.hh"
#include "warn.hh"
#include "main.hh"
#include "request.hh"
#include "version.hh"
#include "lilypond-input-version.hh"
#include "translator-def.hh"
#include "music-output-def.hh"
#include "identifier-smob.hh"

/*
RH 7 fix (?)
*/
#define isatty HORRIBLEKLUDGE

void strip_trailing_white (String&);
void strip_leading_white (String&);
String lyric_fudge (String s);


bool
valid_version_b (String s);



#define start_quote()	\
	yy_push_state (quote);\
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
%option yyclass="My_lily_lexer"
%option stack
%option never-interactive 
%option warn

%x version
%x chords
%x incl
%x lyrics
%x notes
%x figures
%x quote
%x longcomment


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
LYRICS		({AA}|{TEX})[^0-9 \t\n\f]*
ESCAPED		[nt\\'"]
EXTENDER	__
HYPHEN		--
%%


<*>\r		{
	// windows-suck-suck-suck
}

<INITIAL,chords,incl,lyrics,notes,figures>{
  "%{"	{
	yy_push_state (longcomment);
  }
  %[^{\n].*\n	{
  }
  %[^{\n]	{ // backup rule
  }
  %\n	{
  }
  %[^{\n].*	{
  }
  {WHITE}+ 	{

  }
}

<INITIAL,chords,lyrics,notes,figures>\\version{WHITE}*	{
	yy_push_state (version);
}
<version>\"[^"]*\"     { /* got the version number */
	String s (YYText ()+1);
	s = s.left_string (s.index_last ('"'));

	yy_pop_state ();
	if (!valid_version_b (s))
		return INVALID;
}
<version>. 	{
	LexerError ("No quoted string found after \\version");
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
		error (_ ("\\maininput disallowed outside init files"));
}

<INITIAL,chords,lyrics,figures,notes>\\include           {
	yy_push_state (incl);
}
<incl>\"[^"]*\";?   { /* got the include file name */
	String s (YYText ()+1);
	s = s.left_string (s.index_last ('"'));

	new_input (s, &global_input_file->sources_ );
	yy_pop_state ();
}
<incl>\\{BLACK}*;?{WHITE} { /* got the include identifier */
	String s = YYText () + 1;
	strip_trailing_white (s);
	if (s.length () && (s[s.length () - 1] == ';'))
	  s = s.left_string (s.length () - 1);

	SCM sid = lookup_identifier (s);
	if (gh_string_p (sid)) {
		new_input (ly_scm2string (sid), &global_input_file->sources_);
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
<INITIAL,chords,lyrics,notes,figures>\\\${BLACK}*{WHITE}	{
	String s=YYText () + 2;
	s=s.left_string (s.length () - 1);
	return scan_escaped_word (s); 
}
<INITIAL,chords,lyrics,notes,figures>\${BLACK}*{WHITE}		{
	String s=YYText () + 1;
	s=s.left_string (s.length () - 1);
	return scan_bare_word (s);
}
<INITIAL,chords,lyrics,notes,figures>\\\${BLACK}*		{ // backup rule
	error (_("white expected"));
	exit (1);
}
<INITIAL,chords,lyrics,notes,figures>\${BLACK}*		{ // backup rule
	error (_("white expected"));
	exit (1);
}

<INITIAL,chords,lyrics,notes,figures>#	{ //embedded scm
	//char const* s = YYText () + 1;
	char const* s = here_str0 ();
	int n = 0;
	if (main_input_b_ && safe_global_b) {
		error (_ ("Can't evaluate Scheme in safe mode"));
		yylval.scm =  SCM_EOL;
		return SCM_T;
	}
	SCM sval = ly_parse_scm (s, &n, here_input());

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

<lyrics>{
	\" {
		start_quote ();
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


		return STRING;
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

<<EOF>> {


	if (! close_input ()) { 
 	  yyterminate (); // can't move this, since it actually rets a YY_NULL
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
	int cnv=sscanf (YYText (), "%lf", &r);
	assert (cnv == 1);

	yylval.scm = gh_double2scm (r);
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
My_lily_lexer::push_note_state ()
{
	yy_push_state (notes);
}

void
My_lily_lexer::push_figuredbass_state()
{
	yy_push_state (figures);
}
void
My_lily_lexer::push_chord_state ()
{
	yy_push_state (chords);
}

void
My_lily_lexer::push_lyric_state ()
{
	yy_push_state (lyrics);
}

void
My_lily_lexer::pop_state ()
{
	yy_pop_state ();
}

int
My_lily_lexer::identifier_type(SCM sid)
{
	if (gh_string_p (sid)) {
		yylval.scm = sid; 
		return STRING_IDENTIFIER;
	} else if (gh_number_p (sid)) {
		yylval.scm = sid;
		return NUMBER_IDENTIFIER;
	} else if (unsmob_translator_def (sid)) {
		yylval.scm = sid;
		return TRANSLATOR_IDENTIFIER;
	} else if (unsmob_score (sid)) {
		yylval.scm =sid;
		return SCORE_IDENTIFIER;
	} else if (Music * mus =unsmob_music (sid)) {
		yylval.scm = sid;
		
		return dynamic_cast<Request*> (mus) ? REQUEST_IDENTIFIER : MUSIC_IDENTIFIER;
	} else if (unsmob_duration (sid)) {
		yylval.scm = sid;
		return DURATION_IDENTIFIER;
	} else if (unsmob_music_output_def (sid)) {
		yylval.scm = sid;
		return MUSIC_OUTPUT_DEF_IDENTIFIER;
	}
	return SCM_IDENTIFIER;
}


int
My_lily_lexer::scan_escaped_word (String str)
{
	// use more SCM for this.

	SCM sym = ly_symbol2scm (str.to_str0 ());

	int l = lookup_keyword (str);
	if (l != -1) {
		return l;
	}
	SCM sid = lookup_identifier (str);
	if (sid != SCM_UNDEFINED)
	{
		yylval.scm = sid;
		return identifier_type (sid);
	}

	if ((YYSTATE != notes) && (YYSTATE != chords)) {
		SCM pitch = scm_hashq_get_handle (pitchname_tab_, sym);
		
		if (gh_pair_p (pitch))
		{
			yylval.scm = ly_cdr (pitch);
			return NOTENAME_PITCH;
		}
	}
	String msg (_f ("unknown escaped string: `\\%s'", str));	
	LexerError (msg.to_str0 ());

	yylval.scm = scm_makfrom0str (str.to_str0 ());

	return STRING;
}

int
My_lily_lexer::scan_bare_word (String str)
{
	SCM sym = ly_symbol2scm (str.to_str0 ());
	if ((YYSTATE == notes) || (YYSTATE == chords)) {
		SCM pitch = scm_hashq_get_handle (pitchname_tab_, sym);
		if (gh_pair_p (pitch)) {
		    yylval.scm = ly_cdr (pitch);
                    return (YYSTATE == notes) ? NOTENAME_PITCH : TONICNAME_PITCH;
		} else if ((pitch = scm_hashq_get_handle (chordmodifier_tab_, sym))!= SCM_BOOL_F)
		{
		    yylval.scm = ly_cdr (pitch);
		    return CHORDMODIFIER_PITCH;
		}
	}

	yylval.scm = scm_makfrom0str (str.to_str0 ());
	return STRING;
}

bool
My_lily_lexer::note_state_b () const
{
	return YY_START == notes;
}

bool
My_lily_lexer::chord_state_b () const
{
	return YY_START == chords;
}

bool
My_lily_lexer::lyric_state_b () const
{
	return YY_START == lyrics;
}

bool
My_lily_lexer::figure_state_b () const
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



/* 1.3.146 == removal of ; */ 
Lilypond_version oldest_version ("1.3.146");


bool
valid_version_b (String s)
{
  Lilypond_version current ( MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL );
  Lilypond_version ver (s);
  if (! ((ver >= oldest_version) && (ver <= current)))
	{	
		non_fatal_error (_f ("Incorrect lilypond version: %s (%s, %s)", ver.string (), oldest_version.string (), current.string ()));
		non_fatal_error (_ ("Consider updating the input with the convert-ly script")); 
		return false;
    }
  return true;
}
	

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
	return gh_cons (gh_int2scm (n), gh_int2scm (d));
}
		
