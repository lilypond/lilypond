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

#include "lily-guile.hh"
#include "string.hh"
#include "string-convert.hh"
#include "my-lily-lexer.hh"
#include "array.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "parser.hh"
#include "debug.hh"
#include "main.hh"
#include "musical-request.hh"
#include "identifier.hh"
#include "version.hh"
#include "mudela-version.hh"
#include "translator-def.hh"

void strip_trailing_white (String&);
void strip_leading_white (String&);


bool
valid_version_b (String s);



#define start_quote()	\
	yy_push_state (quote);\
	yylval.string = new String

#define yylval (*(YYSTYPE*)lexval_l)

#define YY_USER_ACTION	add_lexed_char (YYLeng ());
/*

LYRICS		({AA}|{TEX})[^0-9 \t\n\f]*

*/

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
%x quote
%x longcomment


A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}
PUNCT		[?!:']
ACCENT		\\[`'"^]
NATIONAL  [\001-\006\021-\027\031\036\200-\377]
TEX		{AA}|-|{PUNCT}|{ACCENT}|{NATIONAL}
WORD		{A}{AN}*
ALPHAWORD	{A}+
DIGIT		{N}
UNSIGNED	{N}+
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

<INITIAL,chords,incl,lyrics,notes>{
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

<INITIAL,chords,lyrics,notes>\\version{WHITE}*	{
	yy_push_state (version);
}
<version>\"[^"]*\";?   { /* got the include file name */
	String s (YYText ()+1);
	s = s.left_str (s.index_last_i ('"'));
	DEBUG_OUT << "#version `" << s << "'\n";
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
		LexerError (_ ("EOF found inside a comment").ch_C ());
		if (! close_input ()) 
		  yyterminate (); // can't move this, since it actually rets a YY_NULL
	}
}


<INITIAL,chords,lyrics,notes>\\maininput           {
	if (!main_input_b_)
	{
		start_main_input ();
		main_input_b_ = true;
	}
	else
		error (_ ("\\maininput disallowed outside init files"));
}

<INITIAL,chords,lyrics,notes>\\include           {
	yy_push_state (incl);
}
<incl>\"[^"]*\";?   { /* got the include file name */
	String s (YYText ()+1);
	s = s.left_str (s.index_last_i ('"'));
	DEBUG_OUT << "#include `" << s << "'\n";
	new_input (s,source_global_l);
	yy_pop_state ();
}
<incl>\\{BLACK}*;?{WHITE} { /* got the include identifier */
	String s = YYText () + 1;
	strip_trailing_white (s);
	if (s.length_i () && (s[s.length_i () - 1] == ';'))
	  s = s.left_str (s.length_i () - 1);
	DEBUG_OUT << "#include `\\" << s << "'\n";
	SCM sid = lookup_identifier (s);
	if (gh_string_p (sid)) {
		new_input (ly_scm2string (sid), source_global_l);
		yy_pop_state ();
	} else { 
	    String msg (_f ("wrong or undefined identifier: `%s'", s ));	
	    LexerError (msg.ch_C ());
	  }
}
<incl>\"[^"]*   { // backup rule
	cerr << _ ("Missing end quote") << endl;
	exit (1);
}
<chords,notes>{RESTNAME} 	{
	const char *s = YYText ();
	yylval.scm = ly_str02scm (s);
	return RESTNAME;
}
<chords,notes>R		{
	return MEASURES;
}
<INITIAL,chords,lyrics,notes>\\\${BLACK}*{WHITE}	{
	String s=YYText () + 2;
	s=s.left_str (s.length_i () - 1);
	return scan_escaped_word (s); 
}
<INITIAL,chords,lyrics,notes>\${BLACK}*{WHITE}		{
	String s=YYText () + 1;
	s=s.left_str (s.length_i () - 1);
	return scan_bare_word (s);
}
<INITIAL,chords,lyrics,notes>\\\${BLACK}*		{ // backup rule
	cerr << _ ("white expected") << endl;
	exit (1);
}
<INITIAL,chords,lyrics,notes>\${BLACK}*		{ // backup rule
	cerr << _ ("white expected") << endl;
	exit (1);
}
<INITIAL,chords,lyrics,notes>#	{ //embedded scm
	//char const* s = YYText () + 1;
	char const* s = here_ch_C ();
	int n = 0;
	if (main_input_b_ && safe_global_b) {
		error (_ ("Can't evaluate Scheme in safe mode"));
		yylval.scm =  SCM_EOL;
		return SCM_T;
	}
	yylval.scm = ly_parse_scm (s, &n);
	DEBUG_OUT << "Scheme: ";
	if (flower_dstream)
		ly_display_scm (yylval.scm);
	
	for (int i=0; i < n; i++)
	{
		yyinput ();
	}
	char_count_stack_.top () += n;

	return SCM_T;
}
<notes>{
	{ALPHAWORD}	{
		return scan_bare_word (YYText ());
	}

	{NOTECOMMAND}	{
		return scan_escaped_word (YYText () + 1); 
	}

	{DIGIT}		{
		yylval.i = String_convert::dec2_i (String (YYText ()));
		return DIGIT;
	}
	{UNSIGNED}		{
		yylval.i = String_convert::dec2_i (String (YYText ()));
		return UNSIGNED;
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
		*yylval.string += to_str (escaped_char(YYText()[1]));
	}
	[^\\"]+	{
		*yylval.string += YYText ();
	}
	\"	{
		DEBUG_OUT << "quoted string: `" << *yylval.string << "'\n";
		yy_pop_state ();

		/* yylval is union. Must remember STRING before setting SCM*/
		String *sp = yylval.string;
		yylval.scm = ly_str02scm  (sp->ch_C ());
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
	{UNSIGNED}		{
		yylval.i = String_convert::dec2_i (String (YYText ()));
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
		int i = 0;
               	while ((i=s.index_i ("_")) != -1) // change word binding "_" to " "
			*(s.ch_l () + i) = ' ';
		if ((i=s.index_i ("\\,")) != -1)   // change "\," to TeX's "\c "
			{
			*(s.ch_l () + i + 1) = 'c';
			s = s.left_str (i+2) + " " + s.right_str (s.length_i ()-i-2);
			}

		char c = s[s.length_i () - 1];
		if (c == '{' ||  c == '}') // brace open is for not confusing dumb tools.
			here_input ().warning (
				"Brace found at end of lyric. Did you forget a space?");
		yylval.scm = ly_str02scm (s.ch_C ());

		DEBUG_OUT << "lyric : `" << s << "'\n";
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
	{UNSIGNED}		{
		yylval.i = String_convert::dec2_i (String (YYText ()));
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
	\^  {
		return CHORD_CARET;
	}
	. {
		return YYText ()[0];
	}
}

<<EOF>> {
	DEBUG_OUT << "<<eof>>";

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
	DEBUG_OUT  << "REAL" << r<<'\n';
	yylval.real = r;
	return REAL;
}

{UNSIGNED}	{
	yylval.i = String_convert::dec2_i (String (YYText ()));
	return UNSIGNED;
}

[{}]	{

	DEBUG_OUT << "parens\n";
	return YYText ()[0];
}
[*:=]		{
	char c = YYText ()[0];
	DEBUG_OUT << "misc char" <<c<<"\n";
	return c;
}

<INITIAL,notes>.	{
	return YYText ()[0];
}

<INITIAL,lyrics,notes>\\. {
    char c= YYText ()[1];

    switch (c) {
    case '>':
	return E_BIGGER;
    case '<':
	return E_SMALLER;
    case '!':
	return E_EXCLAMATION;
    default:
	return E_CHAR;
    }
}

<*>.		{
	String msg = _f ("invalid character: `%c'", YYText ()[0]);
	LexerError (msg.ch_C ());
	return YYText ()[0];
}

%%

void
My_lily_lexer::push_note_state ()
{
	yy_push_state (notes);
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
My_lily_lexer::scan_escaped_word (String str)
{
	// use more SCM for this.

	SCM sym = ly_symbol2scm (str.ch_C());

	int l = lookup_keyword (str);
	if (l != -1) {
		return l;
	}
	SCM sid = lookup_identifier (str);
	if (gh_string_p (sid)) {
		yylval.scm = sid; 
		return STRING_IDENTIFIER;
	} else if (gh_number_p (sid)) {
		yylval.scm = sid;
		return NUMBER_IDENTIFIER;
	} else if (Translator_def* tr = unsmob_translator_def (sid)) {
		yylval.scm = sid;
		return TRANSLATOR_IDENTIFIER;
	} else if (Music * mus =unsmob_music (sid)) {
		yylval.scm = sid;
		
		return dynamic_cast<Request*> (mus) ? REQUEST_IDENTIFIER : MUSIC_IDENTIFIER;
	}



	Identifier * id = unsmob_identifier (sid);
	if (id) {
		yylval.id = id;
		return id->token_code_i_;
	} else if (sid != SCM_UNDEFINED) {
		yylval.scm = sid;
		return SCM_IDENTIFIER;
	}

	if ((YYSTATE != notes) && (YYSTATE != chords)) {
		SCM pitch = scm_hashq_ref (pitchname_tab_, sym, SCM_BOOL_F);
		
		if (pitch != SCM_BOOL_F)
		{
			yylval.pitch = new Musical_pitch (pitch);
			yylval.pitch->set_spot (Input (source_file_l (), 
			  here_ch_C ()));
			return NOTENAME_PITCH;
		}
	}
	String msg (_f ("unknown escaped string: `\\%s'", str));	
	LexerError (msg.ch_C ());

	yylval.scm = ly_str02scm(str.ch_C());

	return STRING;
}

int
My_lily_lexer::scan_bare_word (String str)
{
	SCM sym = ly_symbol2scm (str.ch_C ());
	if ((YYSTATE == notes) || (YYSTATE == chords)) {
		SCM pitch = scm_hashq_ref (pitchname_tab_, sym, SCM_BOOL_F);
		if (pitch != SCM_BOOL_F) {
		    yylval.pitch = new Musical_pitch (pitch);
		    yylval.pitch->set_spot (Input (source_file_l (), 
		      here_ch_C ()));
                    return (YYSTATE == notes) ? NOTENAME_PITCH : TONICNAME_PITCH;
		} else if ((pitch = scm_hashq_ref (chordmodifier_tab_, sym, SCM_BOOL_F))!= SCM_BOOL_F)
		{
		    yylval.pitch = new Musical_pitch (pitch);
		    yylval.pitch->set_spot (Input (source_file_l (), 
		      here_ch_C ()));
		    return CHORDMODIFIER_PITCH;
		}
	}

	yylval.scm = ly_str02scm (str.ch_C());
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

/*
 urg, belong to String(_convert)
 and should be generalised 
 */
void
strip_leading_white (String&s)
{
	int i=0;
	for (;  i < s.length_i (); i++) 
		if (!isspace (s[i]))
			break;

	s = s.nomid_str (0, i);
}

void
strip_trailing_white (String&s)
{
	int i=s.length_i ();	
	while (i--) 
		if (!isspace (s[i]))
			break;

	s = s.left_str (i+1);
}




bool
valid_version_b (String s)
{
  Mudela_version current ( MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL );
  Mudela_version ver (s);
  if (!((ver >= oldest_version) && (ver <= current)))
	{	
		non_fatal_error (_f ("incorrect mudela version: %s (%s, %s)", ver.str (), oldest_version.str (), current.str ()));
		non_fatal_error (_("Consider converting the input with the convert-mudela script")); 
		return false;
    }
  return true;
}
	
