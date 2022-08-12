%{ // -*- mode: c++; c-file-style: "linux"; indent-tabs-mode: t -*-
/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Mode and indentation are at best a rough approximation based on TAB
 * formatting (reasonable for compatibility with unspecific editor
 * modes as Flex modes are hard to find) and need manual correction
 * frequently.  Without a reasonably dependable way of formatting a
 * Flex file sensibly, there is little point in trying to fix the
 * inconsistent state of indentation.
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

#include "context-def.hh"
#include "duration.hh"
#include "international.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "main.hh"
#include "music.hh"
#include "music-function.hh"
#include "parse-scm.hh"
#include "parser.hh"
#include "pitch.hh"
#include "source-file.hh"
#include "std-string.hh"
#include "version.hh"
#include "warn.hh"
#include "lily-imports.hh"

using std::string;

/*
RH 7 fix (?)
*/
#define isatty HORRIBLEKLUDGE

// There's a lot of fallthrough in the generated code.
#if __GNUC__ >= 7
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#endif

// generated code for yyFlexLexer::LexerInput 
// contains a std::streamsize to int conversion
// ignore warning
#pragma GCC diagnostic ignored "-Wconversion"
// generated code contains some old-style casts
#pragma GCC diagnostic ignored "-Wold-style-cast"
// generated code also contains some useless casts
#pragma GCC diagnostic ignored "-Wuseless-cast"

void strip_trailing_white (string&);
void strip_leading_white (string&);
string lyric_fudge (string s);
SCM lookup_markup_command (string s);
SCM lookup_markup_list_command (string s);


#define start_quote() do {                      \
                yy_push_state (quote);          \
                yylval = SCM_EOL;               \
        } while (0)

/*
  The inside of \"violin1" is marked by commandquote mode
*/

#define start_command_quote() do {		\
                yy_push_state (commandquote);	\
                yylval = SCM_EOL;               \
        } while (0)

#define yylval (*lexval_)

#define yylloc (*lexloc_)

#define YY_USER_ACTION	add_lexed_char (YYLeng ());


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
%x longcomment
%x maininput
%x markup
%x notes
%x quote
%x commandquote
%x sourcefileline
%x sourcefilename
%x version

/* The strategy concerning multibyte characters is to accept them but
 * call YYText_utf8 for patterns that might contain them, in order to
 * get a single code path responsible for flagging non-UTF-8 input:
 * Patterns for accepting only valid UTF-8 without backing up are
 * really hard to do and complex, and if nice error messages are
 * wanted, one would need patterns catching the invalid input as well.
 *
 * Since editors and operating environments don't necessarily behave
 * reasonably in the presence of mixed encodings, we flag encoding
 * errors also in identifiers, comments, and strings where it would be
 * conceivable to just transparently work with the byte string.  But
 * the whole point of caring about UTF-8 in here at all is too avoid
 * stranger errors later when input passes into backends or log files
 * or console output or error messages.
 */

A		[a-zA-Z\200-\377]
AA		{A}|_
N		[0-9]
ANY_CHAR	(.|\n)
SYMBOL		{A}([-_]{A}|{A})*
COMMAND		\\{SYMBOL}
/* SPECIAL category is for every letter that needs to get passed to
 * the parser rather than being redefinable by the user */
SPECIAL		[-+*/=<>{}!?_^'',.:]
SHORTHAND	(.|\\.)
UNSIGNED	{N}+
E_UNSIGNED	\\{N}+
FRACTION	{N}+\/{N}+
INT		-?{UNSIGNED}
REAL		({INT}\.{N}*)|(-?\.{N}+)
STRICTREAL      {UNSIGNED}\.{UNSIGNED}
WHITE		[ \n\t\f\r]
HORIZONTALWHITE		[ \t]
BLACK		[^ \n\t\f\r]
RESTNAME	[rs]
ESCAPED		[nt\\''""]
EXTENDER	__
HYPHEN		--
BOM_UTF8	\357\273\277
FIG_ALT_SYMB	[+\-\!]
FIG_ALT_EXPR	{WHITE}*{FIG_ALT_SYMB}({FIG_ALT_SYMB}|{WHITE})*

%%


<*>\r		{
	// swallow and ignore carriage returns
}

   /* Use the trailing context feature. Otherwise, the BOM will not be
      found if the file starts with an identifier definition. */
<INITIAL,chords,lyrics,figures,notes>{BOM_UTF8}/.* {
  if (lexloc_->line_number () != 1 || lexloc_->column_number () != 0)
    {
      LexerWarning (_ ("stray UTF-8 BOM encountered").c_str ());
      // exit (1);
    }
  debug_output (_ ("Skipping UTF-8 BOM"));
}

<INITIAL,chords,figures,incl,lyrics,markup,notes>{
  "%{"	{
	yy_push_state (longcomment);
  }
  %[^{\n\r][^\n\r]*[\n\r]?	{
	  (void) YYText_utf8 ();
  }
  %[\n\r]?	{
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
<version>\"[^""]*\"     { /* got the version number */
	string s (YYText_utf8 () + 1);
	s = s.substr (0, s.rfind ('\"'));

	yy_pop_state ();

	SCM parsed_version = Lily::parse_and_check_version (ly_string2scm (s));
	// from_scm<bool> would return false for a non-boolean value, which
	// is not what we want here.
	bool parsed_version_valid = scm_is_true (parsed_version);

	if (is_main_input_ && include_stack_.size () == main_input_level_) {
		SCM top_scope = scm_car (scm_last_pair (scopes_));
		// For version-seen, #f means no \version found, #t means
		// that a version was found but it could not be parsed,
		// any other value is the version found (as list).
		SCM version_seen_value = parsed_version_valid ? parsed_version : SCM_BOOL_T;
		scm_module_define (top_scope,
				   ly_symbol2scm ("version-seen"),
				   version_seen_value);
	}

	if (!parsed_version_valid) {
                yylval = SCM_UNSPECIFIED;
		return INVALID;
        }
}
<sourcefilename>\"[^""]*\"     {
	string s (YYText_utf8 () + 1);
	s = s.substr (0, s.rfind ('\"'));

	yy_pop_state ();
	here_input().get_source_file ()->name_ = s;
	message (_f ("Renaming input to: `%s'", s.c_str ()));
	scm_module_define (scm_car (scopes_),
		     ly_symbol2scm ("input-file-name"),
		     ly_string2scm (s));

}

<sourcefileline>{INT}	{
	int i;
	sscanf (YYText (), "%d", &i);

	yy_pop_state ();
	here_input ().get_source_file ()->set_line (here_input ().start (), i);
}

<version>{ANY_CHAR} 	{
	LexerError (_ ("quoted string expected after \\version").c_str ());
	yy_pop_state ();
}
<sourcefilename>{ANY_CHAR} 	{
	LexerError (_ ("quoted string expected after \\sourcefilename").c_str ());
	yy_pop_state ();
}
<sourcefileline>{ANY_CHAR} 	{
	LexerError (_ ("integer expected after \\sourcefileline").c_str ());
	yy_pop_state ();
}
<longcomment>{
	[^\%]* 		{
		(void) YYText_utf8 ();
	}
	\%*[^}%]*		{
		(void) YYText_utf8 ();
	}
	"%"+"}"		{
		yy_pop_state ();
	}
}


<INITIAL,chords,lyrics,notes,figures>\\maininput           {
	if (!is_main_input_)
	{
		start_main_input ();
		main_input_level_ = include_stack_.size ();
		is_main_input_ = true;
		int state = YYSTATE;
		yy_push_state (maininput);
		yy_push_state (state);
	}
	else
		LexerError (_ ("\\maininput not allowed outside init files").c_str ());
}

<INITIAL,chords,lyrics,figures,notes>\\include           {
	yy_push_state (incl);
}
<incl>\"[^""]*\"   { /* got the include file name */
	string s (YYText_utf8 ()+1);
	s = s.substr (0, s.rfind ('"'));

	new_input (s, sources_);
	yy_pop_state ();
}
<incl>\\{BLACK}*{WHITE}? { /* got the include identifier */
	string s = YYText_utf8 () + 1;
	strip_trailing_white (s);
	if (s.length () && (s[s.length () - 1] == ';'))
	  s = s.substr (0, s.length () - 1);

	SCM sid = lookup_identifier_symbol (ly_symbol2scm (s));
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
<incl>(\$|#) { // scm for the filename
	Input start = here_input();
	start.step_forward ();

	Input parsed;
	SCM sval = parse_embedded_scheme (start, parser_, &parsed);
	sval = eval_scm (sval, start);
	skip_chars (parsed.size ());

	if (scm_is_string (sval)) {
		new_input (ly_scm2string (sval), sources_);
		yy_pop_state ();
	} else {
		LexerError (_ ("string expected after \\include").c_str ());
		if (!SCM_UNBNDP (sval)) {
			SCM err = scm_current_error_port ();
			scm_puts ("This value was found instead: ", err);
			scm_display (sval, err);
		}
	}
}

<incl,version,sourcefilename>\"[^""]*   { // backup rule
	LexerError (_ ("end quote missing").c_str ());
	yy_pop_state ();
}

    /* Flex picks the longest matching pattern including trailing
     * contexts.  Without the backup pattern, r-. does not trigger the
     * {RESTNAME} rule but rather the {SYMBOL}/[-_] rule coming later,
     * needed for avoiding backup states.
     */

<chords,notes,figures>{RESTNAME}/[-_]	|  // pseudo backup rule
<chords,notes,figures>{RESTNAME} 	{
	char const *s = YYText ();
	yylval = scm_from_latin1_string (s);
	return RESTNAME;
}
<chords,notes,figures>q/[-_]	| // pseudo backup rule
<chords,notes,figures>q	{
        yylval = SCM_UNSPECIFIED;
	return CHORD_REPETITION;
}

<chords,notes,figures>R/[-_]	| // pseudo backup rule
<chords,notes,figures>R		{
        yylval = SCM_UNSPECIFIED;
	return MULTI_MEASURE_REST;
}
<INITIAL,chords,figures,lyrics,markup,notes>#	{ //embedded scm
	Input hi = here_input();
	hi.step_forward ();
	Input parsed;
	SCM sval = parse_embedded_scheme (hi, parser_, &parsed);

	if (SCM_UNBNDP (sval))
		error_level_ = 1;

	skip_chars (parsed.size ());

	yylval = sval;
	return SCM_TOKEN;
}

<INITIAL,chords,figures,lyrics,markup,notes>\$	{ //immediate scm
	Input hi = here_input();
	hi.step_forward ();

	Input parsed;
	SCM sval = parse_embedded_scheme (hi, parser_, &parsed);
	skip_chars (parsed.size ());
	sval = eval_scm (sval, hi, '$');

	if (YYSTATE == markup && ly_is_procedure (sval))
	{
		SCM sig = Lily::markup_command_signature (sval);
		if (scm_is_true (sig))
		{
			yylval = sval;
			int token = MARKUP_FUNCTION;
			if (scm_is_true (Lily::markup_list_function_p (sval)))
				token = MARKUP_LIST_FUNCTION;
			push_markup_predicates (sig);
			return token;
		}
	}
	int token = scan_scm_id (sval);
	if (!scm_is_eq (yylval, SCM_UNSPECIFIED))
		return token;
}

<INITIAL,notes,lyrics,chords>{
	\<\<	{
                yylval = SCM_UNSPECIFIED;
		return DOUBLE_ANGLE_OPEN;
	}
	\>\>	{
                yylval = SCM_UNSPECIFIED;
		return DOUBLE_ANGLE_CLOSE;
	}
}

<INITIAL,notes,chords>{
	\<	{
                yylval = SCM_UNSPECIFIED;
		return ANGLE_OPEN;
	}
	\>	{
                yylval = SCM_UNSPECIFIED;
		return ANGLE_CLOSE;
	}
}

<figures>{
	_	{
                yylval = SCM_UNSPECIFIED;
		return FIGURE_SPACE;
	}
	\>		{
                yylval = SCM_UNSPECIFIED;
		return FIGURE_CLOSE;
	}
	\< 	{
                yylval = SCM_UNSPECIFIED;
		return FIGURE_OPEN;
	}
	\\\+	{
		yylval = SCM_UNSPECIFIED;
		return E_PLUS;
	}
	\\!	{
		yylval = SCM_UNSPECIFIED;
		return E_EXCLAMATION;
	}
	\\\\	{
		yylval = SCM_UNSPECIFIED;
		return E_BACKSLASH;
	}
	{FIG_ALT_EXPR}|(\[{FIG_ALT_EXPR}\])		{
		yylval = scm_from_latin1_string (YYText ());
		return FIGURE_ALTERATION_EXPR;
	}
	[][]	{
		yylval = SCM_UNSPECIFIED;
		return	YYText ()[0];
	}
}

<notes,figures>{
	{SYMBOL}/[-_]	| // backup rule
	{SYMBOL}	{
		return scan_bare_word (YYText_utf8 ());
	}
	\\\"	{
		start_command_quote ();
	}
	{COMMAND}/[-_]	| // backup rule
	{COMMAND}	{
		return scan_escaped_word (YYText_utf8 () + 1); 
	}
	{FRACTION}	{
		yylval =  scan_fraction (YYText ());
		return FRACTION;
	}
	{STRICTREAL}	{
		yylval = scm_c_read_string (YYText ());
		return REAL;
	}
	{UNSIGNED}/[/.]	| // backup rule
	{UNSIGNED}	{
		yylval = scm_c_read_string (YYText ());
		return UNSIGNED;
	}
	{E_UNSIGNED}	{
		yylval = scm_c_read_string (YYText () + 1);
		return E_UNSIGNED;
	}
}

<quote,commandquote>{
	\\{ESCAPED}	{
                char c = escaped_char (YYText ()[1]);
		yylval = scm_cons (scm_from_latin1_stringn (&c, 1),
                                   yylval);
	}
	[^\\""]+	{
                yylval = scm_cons (scm_from_utf8_string (YYText_utf8 ()),
                                   yylval);
	}
	\"	{

		/* yylval is union. Must remember STRING before setting SCM*/

                yylval = scm_string_concatenate_reverse (yylval,
                                                         SCM_UNDEFINED,
                                                         SCM_UNDEFINED);

		if (get_state () == commandquote) {
			yy_pop_state ();
			return scan_escaped_word (ly_scm2string (yylval));
		}

		yy_pop_state ();

		return STRING;
	}
	\\	{
                yylval = scm_cons (scm_from_latin1_string (YYText ()),
                                   yylval);
	}
}

<lyrics>{
	\" {
		start_quote ();
	}
	{FRACTION}	{
		yylval =  scan_fraction (YYText ());
		return FRACTION;
	}
	{STRICTREAL}	{
		yylval = scm_c_read_string (YYText ());
		return REAL;
	}
	{UNSIGNED}/[/.]	| // backup rule
	{UNSIGNED}		{
		yylval = scm_c_read_string (YYText ());
		return UNSIGNED;
	}
	\\\"	{
		start_command_quote ();
	}
	{COMMAND}/[-_]	| // backup rule
	{COMMAND}	{
		return scan_escaped_word (YYText_utf8 () + 1);
	}
	\\.|\|	{
		// UTF-8 already covered by COMMAND
		return scan_shorthand (YYText ());
	}
	/* Characters needed to express durations, assignments */
	[*.=]	{
                yylval = SCM_UNSPECIFIED;
		return YYText ()[0];
	}
	[^|*.=$#{}\"\\ \t\n\r\f0-9][^$#{}\"\\ \t\n\r\f0-9]* {
		/* ugr. This sux. */
		string s (YYText_utf8 ());
                yylval = SCM_UNSPECIFIED;
		if (s == "__")
			return EXTENDER;
		if (s == "--")
			return HYPHEN;
		s = lyric_fudge (s);
		yylval = ly_string2scm (s);

		return SYMBOL;
	}
	/* This should really just cover {} */
	[{}] {
                yylval = SCM_UNSPECIFIED;
		return YYText ()[0];
	}
}
<chords>{
	{SYMBOL}/[-_]	| // backup rule
	{SYMBOL}	{
		return scan_bare_word (YYText_utf8 ());
	}
	\\\"	{
		start_command_quote ();
	}
	{COMMAND}/[-_]	| // backup rule
	{COMMAND}	{
		return scan_escaped_word (YYText_utf8 () + 1);
	}
	{FRACTION}	{
		yylval =  scan_fraction (YYText ());
		return FRACTION;
	}
	{UNSIGNED}/\/	| // backup rule
	{UNSIGNED}		{
		yylval = scm_c_read_string (YYText ());
		return UNSIGNED;
	}
	-  {
                yylval = SCM_UNSPECIFIED;
		return CHORD_MINUS;
	}
	:  {
                yylval = SCM_UNSPECIFIED;
		return CHORD_COLON;
	}
	\/\+ {
                yylval = SCM_UNSPECIFIED;
		return CHORD_BASS;
	}
	\/  {
                yylval = SCM_UNSPECIFIED;
		return CHORD_SLASH;
	}
	\^  {
                yylval = SCM_UNSPECIFIED;
		return CHORD_CARET;
	}
}


<markup>{
	\\score {
                yylval = SCM_UNSPECIFIED;
		return SCORE;
	}
	\\score-lines {
		yylval = SCM_UNSPECIFIED;
		return SCORELINES;
	}
	\\\"	{
		start_command_quote ();
	}
	{COMMAND}/[-_]	| // backup rule
	{COMMAND} {
		string str (YYText_utf8 () + 1);

                int token_type = MARKUP_FUNCTION;
		SCM s = lookup_markup_command (str);

		// lookup-markup-command returns a pair with the car
		// being the function to call, and the cdr being the
		// call signature specified to define-markup-command,
		// a list of predicates.

                if (!scm_is_pair (s)) {
		  // If lookup-markup-command was not successful, we
		  // try lookup-markup-list-command instead.
		  // If this fails as well, we just scan and return
		  // the escaped word.
		  s = lookup_markup_list_command (str);
		  if (scm_is_pair (s))
		    token_type = MARKUP_LIST_FUNCTION;
		  else
		    return scan_escaped_word (str);
                }

		// If the list of predicates is, say,
		// (number? number? markup?), then tokens
		// EXPECT_MARKUP EXPECT_SCM EXPECT_SCM EXPECT_NO_MORE_ARGS
		// will be generated.  Note that we have to push them
		// in reverse order, so the first token pushed in the
		// loop will be EXPECT_NO_MORE_ARGS.

		yylval = scm_car(s);

		// yylval now contains the function to call as token
		// value (for token type MARKUP_FUNCTION or
		// MARKUP_LIST_FUNCTION).

		push_markup_predicates (scm_cdr (s));

		return token_type;
	}
	[^$#{}\"\\ \t\n\r\f]+ {
		string s (YYText_utf8 ()); 

		yylval = ly_string2scm (s);
		return SYMBOL;
	}
	[{}]  {
                yylval = SCM_UNSPECIFIED;
		return YYText ()[0];
	}
}

<longcomment><<EOF>> {
		LexerError (_ ("EOF found inside a comment").c_str ());
		yy_pop_state ();
	}

<quote,commandquote><<EOF>> {
	LexerError (_ ("EOF found inside string").c_str ());
	yy_pop_state ();
}

<<EOF>> {
        yylval = SCM_UNSPECIFIED;
        if (is_main_input_)
	{
		is_main_input_ = include_stack_.size () > main_input_level_;
		if (!is_main_input_)
		{
			main_input_level_ = 0;
			pop_state ();
			if (YYSTATE != maininput)
			{
				LexerError (_ ("Unfinished main input").c_str ());
				do {
					yy_pop_state ();
				} while (YYSTATE != maininput);
			}
			extra_tokens_ = SCM_EOL;
			yy_pop_state ();
		}
		close_input ();
		if (!YY_CURRENT_BUFFER || !is_main_input_)
 	        /* Returns YY_NULL */
			yyterminate ();
	}
	else
	{
		close_input ();
		if (!YY_CURRENT_BUFFER)
			/* Returns YY_NULL */
			yyterminate ();
	}
}

<maininput>{ANY_CHAR} {
	while (include_stack_.size () > main_input_level_)
	{
		close_input ();
		if (!YY_CURRENT_BUFFER)
			break;
	}
	yyterminate ();
}

<INITIAL>{
	{SYMBOL}/[-_]	| // backup rule
	{SYMBOL}	{
		return scan_bare_word (YYText_utf8 ());
	}
	\\\"	{
		start_command_quote ();
	}
	{COMMAND}/[-_]	| // backup rule
	{COMMAND}	{
		return scan_escaped_word (YYText_utf8 () + 1);
	}
}

{FRACTION}	{
	yylval =  scan_fraction (YYText ());
	return FRACTION;
}

-{UNSIGNED}	| // backup rule
{REAL}		{
	yylval = scm_c_read_string (YYText ());
	return REAL;
}

{UNSIGNED}/\/	| // backup rule
{UNSIGNED}	{
	yylval = scm_c_read_string (YYText ());
	return UNSIGNED;
}


-/\.	{ // backup rule
        yylval = SCM_UNSPECIFIED;
	return YYText ()[0];
}

<INITIAL,chords,lyrics,figures,notes>{SPECIAL}	{
        yylval = SCM_UNSPECIFIED;
	return YYText ()[0];
}

<INITIAL,chords,lyrics,figures,notes>{SHORTHAND}	{
	return scan_shorthand (YYText_utf8 ()); // should not be utf-8
}

<*>.[\200-\277]*	{
	string msg = _f ("invalid character: `%s'", YYText_utf8 ());
	LexerError (msg.c_str ());
        yylval = SCM_UNSPECIFIED;
	return '%';  // Better not return half a utf8 character.
}

%%

/* Make the lexer generate a token of the given type as the next token.
 TODO: make it possible to define a value for the token as well */
void
Lily_lexer::push_extra_token (Input const &where, int token_type, SCM scm)
{
	extra_tokens_ = scm_cons (scm_cons2 (where.smobbed_copy (),
					     to_scm (token_type),
					     scm), extra_tokens_);
}

int
Lily_lexer::pop_extra_token ()
{
	if (scm_is_null (extra_tokens_))
		return -1;

  /* produce requested token */
	yylloc = *unsmob<Input> (scm_caar (extra_tokens_));
	int type = from_scm<int> (scm_cadar (extra_tokens_));
	yylval = scm_cddar (extra_tokens_);
	extra_tokens_ = scm_cdr (extra_tokens_);
	return type;
}

void
Lily_lexer::push_chord_state ()
{
	SCM alist = Lily::pitchnames;
	push_pitch_names (alist);
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
Lily_lexer::push_note_state ()
{
 	SCM alist = Lily::pitchnames;
	push_pitch_names (alist);
	yy_push_state (notes);
}

void
Lily_lexer::push_drum_state ()
{
	SCM alist = lookup_identifier_symbol (ly_symbol2scm ("drumPitchNames"));
	push_pitch_names (alist);
	yy_push_state (notes);
}

void
Lily_lexer::pop_state ()
{
	if (YYSTATE == notes || YYSTATE == chords)
		pitchname_tab_stack_ = scm_cdr (pitchname_tab_stack_);

	// don't cross the maininput threshold
	if (YYSTATE != maininput)
		yy_pop_state ();

}

void
Lily_lexer::push_markup_predicates (SCM sig)
{
	push_extra_token (here_input (), EXPECT_NO_MORE_ARGS);
	for (SCM s = sig; scm_is_pair(s); s = scm_cdr(s)) {
		SCM predicate = scm_car(s);

		if (scm_is_eq (predicate, SCM (Lily::markup_list_p)))
			push_extra_token (here_input (), EXPECT_MARKUP_LIST);
		else if (scm_is_eq (predicate, SCM (Lily::markup_p)))
			push_extra_token (here_input (), EXPECT_MARKUP);
		else
			push_extra_token (here_input (), EXPECT_SCM, predicate);
	}
}


int
Lily_lexer::identifier_type (SCM sid)
{
	int k = try_special_identifiers (&yylval , sid);
	return k >= 0  ? k : SCM_IDENTIFIER;
}


int
Lily_lexer::scan_escaped_word (const string &str)
{
	SCM sym = ly_symbol2scm (str);

        yylval = SCM_UNSPECIFIED;
	int i = lookup_keyword (sym);

	if (i != -1)
		return i;

	SCM sid = lookup_identifier_symbol (sym);
	if (Music *m = unsmob<Music> (sid))
	{
		m->set_spot (override_input (here_input ()));
	}

	if (!SCM_UNBNDP (sid))
		return scan_scm_id (sid);

	string msg (_f ("unknown escaped string: `\\%s'", str));
	LexerError (msg.c_str ());

	yylval = ly_string2scm (str);

	return STRING; // SYMBOL would cause additional processing
}

int
Lily_lexer::scan_shorthand (const string &str)
{
	SCM sid = lookup_identifier_symbol (ly_symbol2scm (str));
	if (Music *m = unsmob<Music> (sid))
	{
		m->set_spot (override_input (here_input ()));
	}

	if (!SCM_UNBNDP (sid))
		return scan_scm_id (sid);

	string msg (_f ("undefined character or shorthand: %s", str));
	LexerError (msg.c_str ());

	yylval = ly_string2scm (str);

	return STRING;
}

int
Lily_lexer::scan_scm_id (SCM sid)
{
	if (Music_function *fun = unsmob<Music_function> (sid))
	{
		int funtype = SCM_FUNCTION;

		yylval = sid;

		SCM s = fun->get_signature ();
		SCM cs = scm_car (s);

		if (scm_is_pair (cs))
		{
			cs = SCM_CAR (cs);
		}

		if (scm_is_eq (cs, SCM (Lily::ly_music_p)))
			funtype = MUSIC_FUNCTION;
		else if (scm_is_eq (cs, SCM (Lily::ly_event_p)))
			funtype = EVENT_FUNCTION;
		else if (ly_is_procedure (cs))
			funtype = SCM_FUNCTION;
		else programming_error ("Bad syntax function predicate");

		push_extra_token (here_input (), EXPECT_NO_MORE_ARGS);
		for (s = scm_cdr (s); scm_is_pair (s); s = scm_cdr (s))
		{
			SCM optional = SCM_UNDEFINED;
			cs = scm_car (s);

			if (scm_is_pair (cs))
			{
				optional = SCM_CDR (cs);
				cs = SCM_CAR (cs);
			}

			if (ly_is_procedure (cs))
				push_extra_token (here_input (), EXPECT_SCM, cs);
			else
			{
				programming_error ("Function parameter without type-checking predicate");
				continue;
			}
			if (!scm_is_eq (optional, SCM_UNDEFINED))
				push_extra_token (here_input (), EXPECT_OPTIONAL, optional);
		}
		return funtype;
	}
	yylval = sid;
	return identifier_type (sid);
}

int
Lily_lexer::scan_word (SCM & output, SCM sym)
{
	if ((YYSTATE == notes) || (YYSTATE == chords)) {
		SCM handle = SCM_BOOL_F;
		if (scm_is_pair (pitchname_tab_stack_))
			handle = scm_hashq_get_handle (scm_cdar (pitchname_tab_stack_), sym);

		if (scm_is_pair (handle)) {
			output = scm_cdr (handle);
			if (unsmob<Pitch> (yylval))
	                    return (YYSTATE == notes) ? NOTENAME_PITCH : TONICNAME_PITCH;
			else if (scm_is_symbol (yylval))
			    return DRUM_PITCH;
		}
		else if ((YYSTATE == chords)
			&& scm_is_true (handle = scm_hashq_get_handle (chordmodifier_tab_, sym)))
		{
		    output = scm_cdr (handle);
		    return CHORD_MODIFIER;
		}
	}
	output = SCM_UNDEFINED;
	return -1;
}

int
Lily_lexer::scan_bare_word (const string &str)
{
	int state = scan_word (yylval, ly_symbol2scm (str));
	if (state >= 0)
	{
		return state;
	}
	yylval = ly_string2scm (str);
	return SYMBOL;
}

int
Lily_lexer::get_state () const
{
	return YY_START;
}

bool
Lily_lexer::is_note_state () const
{
	return get_state () == notes;
}

bool
Lily_lexer::is_chord_state () const
{
	return get_state () == chords;
}

bool
Lily_lexer::is_lyric_state () const
{
	return get_state () == lyrics;
}

// The extra_token parameter specifies how to convert multiple values
// into additional tokens.  For '#', additional values get pushed as
// SCM_IDENTIFIER.  For '$', they get checked for their type and get
// pushed as a corresponding *_IDENTIFIER token.  Since the latter
// tampers with yylval, it can only be done from the lexer itself, so
// this function is private.

SCM
Lily_lexer::eval_scm (SCM readerdata, Input location, char extra_token)
{
	SCM sval = SCM_UNDEFINED;

	if (!SCM_UNBNDP (readerdata))
	{
		sval = evaluate_embedded_scheme (readerdata,
				    location,
				    parser_);
	}

	if (SCM_UNBNDP (sval))
	{
		error_level_ = 1;
		return SCM_UNSPECIFIED;
	}

	if (extra_token && SCM_VALUESP (sval))
	{
		size_t nvals = scm_c_nvalues (sval);

		if (nvals > 0) {
			while (--nvals) {
				SCM v = scm_c_value_ref (sval, nvals);
				if (Music *m = unsmob<Music> (v))
				{
					if (!unsmob<Input> (get_property (m, "origin")))
						m->set_spot (override_input (here_input ()));
				}

				int token;
				switch (extra_token) {
				case '$':
					token = scan_scm_id (v);
					if (!scm_is_eq (yylval, SCM_UNSPECIFIED))
						push_extra_token (here_input (),
								  token, yylval);
					break;
				case '#':
					push_extra_token (here_input (),
							  SCM_IDENTIFIER, v);
					break;
				}
			}
			sval = scm_c_value_ref (sval, 0);
		} else
			sval = SCM_UNSPECIFIED;
	}

	if (Music *m = unsmob<Music> (sval))
	{
		if (!unsmob<Input> (get_property (m, "origin")))
			m->set_spot (override_input (here_input ()));
	}

	return sval;
}

/* Check for valid UTF-8 that has no overlong or surrogate codes and
   is in the range 0-0x10ffff */

const char *
Lily_lexer::YYText_utf8 ()
{
	const char * const p =  YYText ();
	for (int i=0; p[i];) {
		if ((p[i] & 0xff) < 0x80) {
			++i;
			continue;
		}
		int oldi = i; // start of character
		int more = 0; // # of followup bytes, 0 if bad
		switch (p[i++] & 0xff) {
			// 0xc0 and 0xc1 are overlong prefixes for
			// 0x00-0x3f and 0x40-0x7f respectively, bad.
		case 0xc2:	// 0x80-0xbf
		case 0xc3:	// 0xc0-0xff
		case 0xc4:	// 0x100-0x13f
		case 0xc5:	// 0x140-0x17f
		case 0xc6:	// 0x180-0x1bf
		case 0xc7:	// 0x1c0-0x1ff
		case 0xc8:	// 0x200-0x23f
		case 0xc9:	// 0x240-0x27f
		case 0xca:	// 0x280-0x2bf
		case 0xcb:	// 0x2c0-0x2ff
		case 0xcc:	// 0x300-0x33f
		case 0xcd:	// 0x340-0x37f
		case 0xce:	// 0x380-0x3bf
		case 0xcf:	// 0x3c0-0x3ff
		case 0xd0:	// 0x400-0x43f
		case 0xd1:	// 0x440-0x47f
		case 0xd2:	// 0x480-0x4bf
		case 0xd3:	// 0x4c0-0x4ff
		case 0xd4:	// 0x500-0x53f
		case 0xd5:	// 0x540-0x57f
		case 0xd6:	// 0x580-0x5bf
		case 0xd7:	// 0x5c0-0x5ff
		case 0xd8:	// 0x600-0x63f
		case 0xd9:	// 0x640-0x67f
		case 0xda:	// 0x680-0x6bf
		case 0xdb:	// 0x6c0-0x6ff
		case 0xdc:	// 0x700-0x73f
		case 0xdd:	// 0x740-0x77f
		case 0xde:	// 0x780-0x7bf
		case 0xdf:	// 0x7c0-0x7ff
			more = 1; // 2-byte sequences, 0x80-0x7ff
			break;
		case 0xe0:
			// don't allow overlong sequences for 0-0x7ff
			if ((p[i] & 0xff) < 0xa0)
				break;
		case 0xe1:	// 0x1000-0x1fff
		case 0xe2:	// 0x2000-0x2fff
		case 0xe3:	// 0x3000-0x3fff
		case 0xe4:	// 0x4000-0x4fff
		case 0xe5:	// 0x5000-0x5fff
		case 0xe6:	// 0x6000-0x6fff
		case 0xe7:	// 0x7000-0x7fff
		case 0xe8:	// 0x8000-0x8fff
		case 0xe9:	// 0x9000-0x9fff
		case 0xea:	// 0xa000-0xafff
		case 0xeb:	// 0xb000-0xbfff
		case 0xec:	// 0xc000-0xcfff
			more = 2; // 3-byte sequences, 0x7ff-0xcfff
			break;
		case 0xed:	// 0xd000-0xdfff
			// Don't allow surrogate codes 0xd800-0xdfff
			if ((p[i] & 0xff) >= 0xa0)
				break;
		case 0xee:	// 0xe000-0xefff
		case 0xef:	// 0xf000-0xffff
			more = 2; // 3-byte sequences,
				  // 0xd000-0xd7ff, 0xe000-0xffff
			break;
		case 0xf0:
			// don't allow overlong sequences for 0-0xffff
			if ((p[i] & 0xff) < 0x90)
				break;
		case 0xf1:	// 0x40000-0x7ffff
		case 0xf2:	// 0x80000-0xbffff
		case 0xf3:	// 0xc0000-0xfffff
			more = 3; // 4-byte sequences, 0x10000-0xfffff
			break;
		case 0xf4:
			// don't allow more than 0x10ffff
			if ((p[i] & 0xff) >= 0x90)
				break;
			more = 3; // 4-byte sequence, 0x100000-0x10ffff
			break;
		}
		if (more) {
			// check that all continuation bytes are valid
			do {
				if ((p[i++] & 0xc0) != 0x80)
					break;
			} while (--more);
			if (!more)
				continue;
		}
		Input h = here_input ();
		h.set (h.get_source_file (), h.start () + oldi, h.start () + i);
		h.warning (_ ("non-UTF-8 input").c_str ());
	}
	return p;
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



/*
  substitute _
*/
string
lyric_fudge (string s)
{
	size_t i=0;

	while ((i = s.find ('_', i)) != string::npos)
	{
		s[i++] = ' ';
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

	return scm_cons (scm_c_read_string (left.c_str ()),
			 scm_c_read_string (right.c_str ()));
}

SCM
lookup_markup_command (string s)
{
	return Lily::lookup_markup_command (ly_string2scm (s));
}

SCM
lookup_markup_list_command (string s)
{
	return Lily::lookup_markup_list_command (ly_string2scm (s));
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
