/* -*- mode: c++; c-file-style: "linux"; indent-tabs-mode: t -*- */
/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
 * modes as Bison modes are hard to find) and need manual correction
 * frequently.  Without a reasonably dependable way of formatting a
 * Bison file sensibly, there is little point in trying to fix the
 * inconsistent state of indentation.
 */

%{

#define yyerror Lily_parser::parser_error

/* We use custom location type: Input objects */
#define YYLTYPE Input
#define YYSTYPE SCM
#define YYLLOC_DEFAULT(Current,Rhs,N) \
	((Current).set_location ((Rhs)[1], (Rhs)[N]))

#define YYPRINT(file, type, value)                                      \
        do {                                                            \
                if (scm_is_eq (value, SCM_UNSPECIFIED))                 \
                        break;                                          \
                char *p = scm_to_locale_string                          \
                        (scm_simple_format (SCM_BOOL_F,                 \
                                            scm_from_locale_string ("~S"), \
                                            scm_list_1 (value)));       \
                fputs (p, file);                                        \
                free (p);                                               \
        } while (0)

%}

%parse-param {Lily_parser *parser}
%parse-param {SCM *retval}
%lex-param {Lily_parser *parser}
%error-verbose
%debug

/* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesn't trigger a move
of the parse stack onto the heap. */

%left PREC_BOT
%nonassoc REPEAT REPEAT_IDENTIFIER
%nonassoc ALTERNATIVE

/* The above precedences tackle the shift/reduce problem

1.  \repeat
	\repeat .. \alternative

    \repeat { \repeat .. \alternative }

or

    \repeat { \repeat } \alternative
*/

%nonassoc COMPOSITE
%left ADDLYRICS

%right ':' UNSIGNED REAL E_UNSIGNED EVENT_IDENTIFIER EVENT_FUNCTION '^' '_'
       HYPHEN EXTENDER DURATION_IDENTIFIER

 /* The above are needed for collecting tremoli and other items (that
    could otherwise be interpreted as belonging to the next function
    argument) greedily, and together with the next rule will serve to
    join numbers and units greedily instead of allowing them into
    separate function arguments
 */

%nonassoc NUMBER_IDENTIFIER

%left PREC_TOP




%pure-parser
%locations



%{ // -*-Fundamental-*-

/*
FIXME:

   * The rules for who is protecting what are very shady.  Uniformise
     this.

   * There are too many lexical modes?
*/

#include "config.hh"

#include <cctype>
#include <cstdlib>
#include <cstdio>
using namespace std;

#include "book.hh"
#include "context-def.hh"
#include "context-mod.hh"
#include "dimensions.hh"
#include "file-path.hh"
#include "input.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "main.hh"
#include "misc.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "text-interface.hh"
#include "warn.hh"

void
Lily_parser::parser_error (Input const *i, Lily_parser *parser, SCM *, const string &s)
{
	parser->parser_error (*i, s);
}

#define MYBACKUP(Token, Value, Location)				\
do									\
	if (yychar == YYEMPTY)						\
	{								\
		if (Token)						\
			parser->lexer_->push_extra_token (Token, Value); \
		parser->lexer_->push_extra_token (BACKUP);		\
	} else {							\
		parser->parser_error					\
			(Location, _("Too much lookahead"));		\
	}								\
while (0)


#define MYREPARSE(Location, Pred, Token, Value)				\
do									\
	if (yychar == YYEMPTY)						\
	{								\
		parser->lexer_->push_extra_token (Token, Value);	\
		parser->lexer_->push_extra_token (REPARSE,		\
						  Pred);		\
	} else {							\
		parser->parser_error					\
			(Location, _("Too much lookahead"));		\
	}								\
while (0)

%}


%{

#define MY_MAKE_MUSIC(x, spot) \
	make_music_with_input (ly_symbol2scm (x), \
			       parser->lexer_->override_input (spot))

/* ES TODO:
- Don't use lily module, create a new module instead.
- delay application of the function
*/
#define LOWLEVEL_MAKE_SYNTAX(proc, args)	\
  scm_apply_0 (proc, args)
/* Syntactic Sugar. */
#define MAKE_SYNTAX(name, location, ...)				\
	LOWLEVEL_MAKE_SYNTAX (ly_lily_module_constant (name), scm_list_n (parser->self_scm (), make_input (parser->lexer_->override_input (location)), ##__VA_ARGS__, SCM_UNDEFINED))
#define START_MAKE_SYNTAX(name, ...)					\
	scm_list_n (ly_lily_module_constant (name) , ##__VA_ARGS__, SCM_UNDEFINED)
#define FINISH_MAKE_SYNTAX(start, location, ...)			\
	LOWLEVEL_MAKE_SYNTAX (scm_car (start), scm_cons2 (parser->self_scm (), make_input (parser->lexer_->override_input (location)), scm_append_x (scm_list_2 (scm_cdr (start), scm_list_n (__VA_ARGS__, SCM_UNDEFINED)))))

SCM get_next_unique_context_id ();
SCM get_next_unique_lyrics_context_id ();

#undef _
#if !HAVE_GETTEXT
#define _(x) x
#else
#include <libintl.h>
#define _(x) gettext (x)
#endif


static Music *make_music_with_input (SCM name, Input where);
SCM check_scheme_arg (Lily_parser *parser, Input loc,
		      SCM arg, SCM args, SCM pred, SCM disp = SCM_UNDEFINED);
SCM make_music_from_simple (Lily_parser *parser, Input loc, SCM pitch);
SCM loc_on_music (Input loc, SCM arg);
SCM make_chord_elements (Input loc, SCM pitch, SCM dur, SCM modification_list);
SCM make_chord_step (SCM step, Rational alter);
SCM make_simple_markup (SCM a);
SCM make_duration (SCM t, int dots = 0);
bool is_regular_identifier (SCM id, bool multiple=false);
SCM try_string_variants (SCM pred, SCM str);
int yylex (YYSTYPE *s, YYLTYPE *loc, Lily_parser *parser);

%}

/* The third option is an alias that will be used to display the
   syntax error.  Bison CVS now correctly handles backslash escapes.

   FIXME: Bison needs to translate some of these, eg, STRING.

*/

/* Keyword tokens with plain escaped name.  */
%token END_OF_FILE 0 "end of input"
%token ACCEPTS "\\accepts"
%token ADDLYRICS "\\addlyrics"
%token ALIAS "\\alias"
%token ALTERNATIVE "\\alternative"
%token BOOK "\\book"
%token BOOKPART "\\bookpart"
%token CHANGE "\\change"
%token CHORDMODE "\\chordmode"
%token CHORDS "\\chords"
%token CONSISTS "\\consists"
%token CONTEXT "\\context"
%token DEFAULT "\\default"
%token DEFAULTCHILD "\\defaultchild"
%token DENIES "\\denies"
%token DESCRIPTION "\\description"
%token DRUMMODE "\\drummode"
%token DRUMS "\\drums"
%token FIGUREMODE "\\figuremode"
%token FIGURES "\\figures"
%token HEADER "\\header"
%token INVALID "\\version-error"
%token LAYOUT "\\layout"
%token LYRICMODE "\\lyricmode"
%token LYRICS "\\lyrics"
%token LYRICSTO "\\lyricsto"
%token MARKUP "\\markup"
%token MARKUPLIST "\\markuplist"
%token MIDI "\\midi"
%token NAME "\\name"
%token NOTEMODE "\\notemode"
%token OVERRIDE "\\override"
%token PAPER "\\paper"
%token REMOVE "\\remove"
%token REPEAT "\\repeat"
%token REST "\\rest"
%token REVERT "\\revert"
%token SCORE "\\score"
%token SCORELINES "\\score-lines"
%token SEQUENTIAL "\\sequential"
%token SET "\\set"
%token SIMULTANEOUS "\\simultaneous"
%token TEMPO "\\tempo"
%token TYPE "\\type"
%token UNSET "\\unset"
%token WITH "\\with"

/* Keyword token exceptions.  */
%token NEWCONTEXT "\\new"


/* Other string tokens.  */

%token CHORD_BASS "/+"
%token CHORD_CARET "^"
%token CHORD_COLON ":"
%token CHORD_MINUS "-"
%token CHORD_SLASH "/"
%token ANGLE_OPEN "<"
%token ANGLE_CLOSE ">"
%token DOUBLE_ANGLE_OPEN "<<"
%token DOUBLE_ANGLE_CLOSE ">>"
%token E_BACKSLASH "\\"
%token E_EXCLAMATION "\\!"
%token E_PLUS "\\+"
%token EXTENDER "__"

/*
If we give names, Bison complains.
*/
%token FIGURE_CLOSE /* "\\>" */
%token FIGURE_OPEN /* "\\<" */
%token FIGURE_SPACE "_"
%token HYPHEN "--"

%token MULTI_MEASURE_REST


%token E_UNSIGNED
%token UNSIGNED

/* Artificial tokens, for more generic function syntax */
%token EXPECT_MARKUP "markup?"
%token EXPECT_SCM "scheme?"
%token BACKUP "(backed-up?)"
%token REPARSE "(reparsed?)"
%token EXPECT_MARKUP_LIST "markup-list?"
%token EXPECT_OPTIONAL "optional?"
/* After the last argument. */
%token EXPECT_NO_MORE_ARGS;

/* An artificial token for parsing embedded Lilypond */
%token EMBEDDED_LILY "#{"

%token BOOK_IDENTIFIER
%token CHORD_BODY_IDENTIFIER
%token CHORD_MODIFIER
%token CHORD_REPETITION
%token CONTEXT_DEF_IDENTIFIER
%token CONTEXT_MOD_IDENTIFIER
%token DRUM_PITCH
%token PITCH_IDENTIFIER
%token PITCH_ARG
%token DURATION_IDENTIFIER
%token EVENT_IDENTIFIER
%token EVENT_FUNCTION
%token FRACTION
%token LYRIC_ELEMENT
%token MARKUP_FUNCTION
%token MARKUP_LIST_FUNCTION
%token MARKUP_IDENTIFIER
%token MARKUPLIST_IDENTIFIER
%token MUSIC_FUNCTION
%token MUSIC_IDENTIFIER
%token NOTENAME_PITCH
%token NUMBER_IDENTIFIER
%token OUTPUT_DEF_IDENTIFIER
%token REAL
%token REPEAT_IDENTIFIER
%token RESTNAME
%token SCM_ARG
%token SCM_FUNCTION
%token SCM_IDENTIFIER
%token SCM_TOKEN
%token STRING
%token SYMBOL_LIST
%token TONICNAME_PITCH

%left '-' '+'

/* We don't assign precedence to / and *, because we might need varied
prec levels in different prods */

%left UNARY_MINUS

%%

start_symbol:
	lilypond
	| EMBEDDED_LILY {
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);
	} embedded_lilypond {
		parser->lexer_->pop_state ();
                *retval = $3;
 	}
	;

lilypond:	/* empty */ { $$ = SCM_UNSPECIFIED; }
	| lilypond toplevel_expression {
	}
	| lilypond assignment {
	}
	| lilypond error {
		parser->error_level_ = 1;
	}
	| lilypond INVALID	{
		parser->error_level_ = 1;
	}
	;


toplevel_expression:
	{
		parser->lexer_->add_scope (get_header (parser));
	} lilypond_header {
		parser->lexer_->set_identifier (ly_symbol2scm ("$defaultheader"), $2);
	}
	| book_block {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-book-handler");
		scm_call_2 (proc, parser->self_scm (), $1);
	}
	| bookpart_block {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-bookpart-handler");
		scm_call_2 (proc, parser->self_scm (), $1);
	}
	| score_block {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-score-handler");
		scm_call_2 (proc, parser->self_scm (), $1);
	}
	| composite_music {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-music-handler");
		scm_call_2 (proc, parser->self_scm (), $1);
	}
	| full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
		scm_call_2 (proc, parser->self_scm (), scm_list_1 ($1));
	}
	| full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
		scm_call_2 (proc, parser->self_scm (), $1);
	}
	| SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($1);
	}
	| embedded_scm_active
	{
		SCM out = SCM_UNDEFINED;
		if (Text_interface::is_markup ($1))
			out = scm_list_1 ($1);
		else if (Text_interface::is_markup_list ($1))
			out = $1;
		if (scm_is_pair (out))
		{
			SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
			scm_call_2 (proc, parser->self_scm (), out);
		} else if (!scm_is_eq ($1, SCM_UNSPECIFIED))
			parser->parser_error (@1, _("bad expression type"));
	}
	| output_def {
		SCM id = SCM_EOL;
		Output_def * od = unsmob_output_def ($1);

		if (od->c_variable ("is-paper") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultpaper");
		else if (od->c_variable ("is-midi") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultmidi");
		else if (od->c_variable ("is-layout") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultlayout");

		parser->lexer_->set_identifier (id, $1);
	}
	;

embedded_scm_bare:
	SCM_TOKEN
	{
		$$ = parser->lexer_->eval_scm_token ($1);
	}
	| SCM_IDENTIFIER
	;

embedded_scm_active:
	SCM_IDENTIFIER
	| scm_function_call
	;

embedded_scm_bare_arg:
	SCM_ARG
	| SCM_TOKEN
	{
		$$ = parser->lexer_->eval_scm_token ($1);
	}
	| FRACTION
	| full_markup_list
	| context_modification
	| score_block
	| context_def_spec_block
	| book_block
	| bookpart_block
	| output_def
	;

/* The generic version may end in music, or not */

embedded_scm:
	embedded_scm_bare
	| scm_function_call
	;

/* embedded_scm_arg is _not_ casting pitches to music by default, this
 * has to be done by the function itself.  Note that this may cause
 * the results of scm_function_call or embedded_scm_bare_arg to be
 * turned into music from pitches as well.  Note that this creates a
 * distinctly awkward situation for calculated drum pitches.  Those
 * are at the current point of time rejected as music constituents as
 * they can't be distinguished from "proper" symbols.
 */

embedded_scm_arg:
	embedded_scm_bare_arg
	| scm_function_call
	| music_assign
	;

scm_function_call:
	SCM_FUNCTION function_arglist {
		$$ = MAKE_SYNTAX ("music-function", @$,
					 $1, $2);
	}
	;

embedded_lilypond_number:
	'-' embedded_lilypond_number
	{
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| bare_number_common
	| UNSIGNED NUMBER_IDENTIFIER
	{
		$$ = scm_product ($1, $2);
	}
	;

embedded_lilypond:
	/* empty */
	{
		// FIXME: @$ does not contain a useful source location
		// for empty rules, and the only token in the whole
		// production, EMBEDDED_LILY, is synthetic and also
		// contains no source location.
		$$ = MAKE_SYNTAX ("void-music", @$);
	}
	| identifier_init_nonumber
	| embedded_lilypond_number
	| post_event post_events
	{
		$$ = scm_reverse_x ($2, SCM_EOL);
		if (Music *m = unsmob_music ($1))
		{
			if (m->is_mus_type ("post-event-wrapper"))
				$$ = scm_append
					(scm_list_2 (m->get_property ("elements"),
						     $$));
			else
				$$ = scm_cons ($1, $$);
		}
		if (scm_is_pair ($$)
		    && scm_is_null (scm_cdr ($$)))
			$$ = scm_car ($$);
		else
		{
			Music * m = MY_MAKE_MUSIC ("PostEvents", @$);
			m->set_property ("elements", $$);
			$$ = m->unprotect ();
		}
	}
	| multiplied_duration
	| music_embedded music_embedded music_list {
		$3 = scm_reverse_x ($3, SCM_EOL);
		if (unsmob_music ($2))
			$3 = scm_cons ($2, $3);
		if (unsmob_music ($1))
			$3 = scm_cons ($1, $3);
		$$ = MAKE_SYNTAX ("sequential-music", @$, $3);
	}
	| error {
		parser->error_level_ = 1;
                $$ = SCM_UNSPECIFIED;
	}
	| INVALID embedded_lilypond {
		parser->error_level_ = 1;
                $$ = $2;
	}
	;


lilypond_header_body:
	/* empty */ { $$ = SCM_UNSPECIFIED; }
	| lilypond_header_body assignment  {

	}
	| lilypond_header_body embedded_scm  {

	}
	;

lilypond_header:
	HEADER '{' lilypond_header_body '}'	{
		$$ = parser->lexer_->remove_scope ();
	}
	;

/*
	DECLARATIONS
*/
assignment_id:
	STRING		{ $$ = $1; }
	;

assignment:
	assignment_id '=' identifier_init  {
	        parser->lexer_->set_identifier ($1, $3);
                $$ = SCM_UNSPECIFIED;
	}
	| assignment_id property_path '=' identifier_init {
		SCM path = scm_cons (scm_string_to_symbol ($1), $2);
		parser->lexer_->set_identifier (path, $4);
                $$ = SCM_UNSPECIFIED;
	}
	;


identifier_init:
	identifier_init_nonumber
	| number_expression
	| post_event_nofinger post_events
	{
		$$ = scm_reverse_x ($2, SCM_EOL);
		if (Music *m = unsmob_music ($1))
		{
			if (m->is_mus_type ("post-event-wrapper"))
				$$ = scm_append
					(scm_list_2 (m->get_property ("elements"),
						     $$));
			else
				$$ = scm_cons ($1, $$);
		}
		if (scm_is_pair ($$)
		    && scm_is_null (scm_cdr ($$)))
			$$ = scm_car ($$);
		else
		{
			Music * m = MY_MAKE_MUSIC ("PostEvents", @$);
			m->set_property ("elements", $$);
			$$ = m->unprotect ();
		}
	}
	;

identifier_init_nonumber:
	score_block
	| book_block
	| bookpart_block
	| output_def
	| context_def_spec_block
	| music_assign
	| FRACTION
	| string
        | embedded_scm
	| full_markup_list
        | context_modification
	;

context_def_spec_block:
	CONTEXT '{' context_def_spec_body '}'
		{
		$$ = $3;
		unsmob_context_def ($$)->origin ()->set_spot (@$);
	}
	;

context_mod_arg:
	embedded_scm
	|
	{
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);
	}
	composite_music
	{
		parser->lexer_->pop_state ();
		$$ = $2;
	}
	;

context_mod_embedded:
	context_mod_arg
	{
		if (unsmob_music ($1)) {
			SCM proc = parser->lexer_->lookup_identifier ("context-mod-music-handler");
			$1 = scm_call_2 (proc, parser->self_scm (), $1);
		}
		if (unsmob_context_mod ($1))
			$$ = $1;
		else {
			parser->parser_error (@1, _ ("not a context mod"));
			$$ = Context_mod ().smobbed_copy ();
		}
	}
	;


context_def_spec_body:
	/**/ {
		$$ = Context_def::make_scm ();
	}
	| CONTEXT_DEF_IDENTIFIER {
		$$ = $1;
	}
	| context_def_spec_body context_mod {
		if (!SCM_UNBNDP ($2))
			unsmob_context_def ($$)->add_context_mod ($2);
	}
	| context_def_spec_body context_modification {
                Context_def *td = unsmob_context_def ($$);
                SCM new_mods = unsmob_context_mod ($2)->get_mods ();
                for (SCM m = new_mods; scm_is_pair (m); m = scm_cdr (m)) {
                    td->add_context_mod (scm_car (m));
                }
	}
	| context_def_spec_body context_mod_embedded {
                Context_def *td = unsmob_context_def ($$);
                SCM new_mods = unsmob_context_mod ($2)->get_mods ();
                for (SCM m = new_mods; scm_is_pair (m); m = scm_cdr (m)) {
                    td->add_context_mod (scm_car (m));
                }
	}
	;



book_block:
	BOOK '{' book_body '}' 	{
		$$ = $3;
		unsmob_book ($$)->origin ()->set_spot (@$);
		pop_paper (parser);
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-book"), SCM_BOOL_F);
	}
	;

/* FIXME:
   * Use 'handlers' like for toplevel-* stuff?
   * grok \layout and \midi?  */
book_body:
	{
		Book *book = new Book;
		init_papers (parser);
		book->paper_ = dynamic_cast<Output_def*> (unsmob_output_def (parser->lexer_->lookup_identifier ("$defaultpaper"))->clone ());
		book->paper_->unprotect ();
		push_paper (parser, book->paper_);
		book->header_ = get_header (parser);
                $$ = book->unprotect ();
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-book"), $$);
	}
	| BOOK_IDENTIFIER {
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-book"), $1);
	}
	| book_body paper_block {
		unsmob_book ($1)->paper_ = unsmob_output_def ($2);
		set_paper (parser, unsmob_output_def ($2));
	}
	| book_body bookpart_block {
		SCM proc = parser->lexer_->lookup_identifier ("book-bookpart-handler");
		scm_call_2 (proc, $1, $2);
	}
	| book_body score_block {
		SCM proc = parser->lexer_->lookup_identifier ("book-score-handler");
		scm_call_2 (proc, $1, $2);
	}
	| book_body composite_music {
		SCM proc = parser->lexer_->lookup_identifier ("book-music-handler");
		scm_call_3 (proc, parser->self_scm (), $1, $2);
	}
	| book_body full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
		scm_call_2 (proc, $1, scm_list_1 ($2));
	}
	| book_body full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
		scm_call_2 (proc, $1, $2);
	}
	| book_body SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($2);
	}
	| book_body embedded_scm_active
	{
		SCM out = SCM_UNDEFINED;
		if (Text_interface::is_markup ($2))
			out = scm_list_1 ($2);
		else if (Text_interface::is_markup_list ($2))
			out = $2;
		if (scm_is_pair (out))
		{
			SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
			scm_call_2 (proc, $1, out);
		} else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("bad expression type"));
	}
	| book_body
	{
		parser->lexer_->add_scope (unsmob_book ($1)->header_);
	} lilypond_header
	| book_body error {
                Book *book = unsmob_book ($1);
		book->paper_ = 0;
		book->scores_ = SCM_EOL;
		book->bookparts_ = SCM_EOL;
	}
	;

bookpart_block:
	BOOKPART '{' bookpart_body '}' {
		$$ = $3;
		unsmob_book ($$)->origin ()->set_spot (@$);
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), SCM_BOOL_F);
	}
	;

bookpart_body:
	{
		Book *book = new Book;
                $$ = book->unprotect ();
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), $$);
	}
	| BOOK_IDENTIFIER {
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), $1);
	}
	| bookpart_body paper_block {
		unsmob_book ($$)->paper_ = unsmob_output_def ($2);
	}
	| bookpart_body score_block {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-score-handler");
		scm_call_2 (proc, $1, $2);
	}
	| bookpart_body composite_music {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-music-handler");
		scm_call_3 (proc, parser->self_scm (), $1, $2);
	}
	| bookpart_body full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
		scm_call_2 (proc, $1, scm_list_1 ($2));
	}
	| bookpart_body full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
		scm_call_2 (proc, $1, $2);
	}
	| bookpart_body SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($2);
	}
	| bookpart_body embedded_scm_active
	{
		SCM out = SCM_UNDEFINED;
		if (Text_interface::is_markup ($2))
			out = scm_list_1 ($2);
		else if (Text_interface::is_markup_list ($2))
			out = $2;
		if (scm_is_pair (out))
		{
			SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
			scm_call_2 (proc, $1, out);
		} else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("bad expression type"));
	}
	| bookpart_body
	{
                Book *book = unsmob_book ($1);
		if (!ly_is_module (book->header_))
			book->header_ = ly_make_module (false);
		parser->lexer_->add_scope (book->header_);
	} lilypond_header
	| bookpart_body error {
                Book *book = unsmob_book ($1);
		book->paper_ = 0;
		book->scores_ = SCM_EOL;
	}
	;

score_block:
	SCORE '{' score_body '}' 	{
		$$ = $3;
	}
	;

score_body:
	music {
		SCM scorify = ly_lily_module_constant ("scorify-music");
		$$ = scm_call_2 (scorify, $1, parser->self_scm ());

		unsmob_score ($$)->origin ()->set_spot (@$);
	}
	| embedded_scm_active {
		Score *score;
		if (unsmob_score ($1))
			score = new Score (*unsmob_score ($1));
		else {
			score = new Score;
			parser->parser_error (@1, _("score expected"));
		}
		unsmob_score ($$)->origin ()->set_spot (@$);
		$$ = score->unprotect ();
	}
	| score_body
	{
                Score *score = unsmob_score ($1);
		if (!ly_is_module (score->get_header ()))
			score->set_header (ly_make_module (false));
		parser->lexer_->add_scope (score->get_header ());
	} lilypond_header
	| score_body output_def {
                Output_def *od = unsmob_output_def ($2);
		if (od->lookup_variable (ly_symbol2scm ("is-paper")) == SCM_BOOL_T)
		{
			parser->parser_error (@2, _("\\paper cannot be used in \\score, use \\layout instead"));

		}
		else
		{
			unsmob_score ($1)->add_output_def (od);
		}
	}
	| score_body error {
		unsmob_score ($$)->error_found_ = true;
	}
	;


/*
	OUTPUT DEF
*/

paper_block:
	output_def {
                Output_def *od = unsmob_output_def ($1);

		if (od->lookup_variable (ly_symbol2scm ("is-paper")) != SCM_BOOL_T)
		{
			parser->parser_error (@1, _ ("need \\paper for paper block"));
			$$ = get_paper (parser)->unprotect ();
		}
	}
	;


output_def:
	output_def_body '}' {
		$$ = $1;

		parser->lexer_->remove_scope ();
		parser->lexer_->pop_state ();
	}
	;

output_def_head:
	PAPER {
                Output_def *p = get_paper (parser);
		p->input_origin_ = @$;
		parser->lexer_->add_scope (p->scope_);
                $$ = p->unprotect ();
	}
	| MIDI    {
		Output_def *p = get_midi (parser);
		$$ = p->unprotect ();
		parser->lexer_->add_scope (p->scope_);
	}
	| LAYOUT 	{
		Output_def *p = get_layout (parser);

		parser->lexer_->add_scope (p->scope_);
		$$ = p->unprotect ();
	}
	;

output_def_head_with_mode_switch:
	output_def_head {
		parser->lexer_->push_initial_state ();
		$$ = $1;
	}
	;

// We need this weird nonterminal because both music as well as a
// context definition can start with \context and the difference is
// only apparent after looking at the next token.  If it is '{', there
// is still time to escape from notes mode.

music_or_context_def:
	music_arg
	| context_def_spec_block
	;

output_def_body:
	output_def_head_with_mode_switch '{' {
		$$ = $1;
		unsmob_output_def ($$)->input_origin_.set_spot (@$);
	}
	| output_def_head_with_mode_switch '{' OUTPUT_DEF_IDENTIFIER 	{
		Output_def *o = unsmob_output_def ($3);
		o->input_origin_.set_spot (@$);
		$$ = o->self_scm ();
		parser->lexer_->remove_scope ();
		parser->lexer_->add_scope (o->scope_);
	}
	| output_def_body assignment  {

	}
	| output_def_body embedded_scm  {

	}
	| output_def_body
	{
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);
	} music_or_context_def
	{
		parser->lexer_->pop_state ();
		if (unsmob_context_def ($3))
			assign_context_def (unsmob_output_def ($1), $3);
		else {

			SCM proc = parser->lexer_->lookup_identifier
				     ("output-def-music-handler");
			scm_call_3 (proc, parser->self_scm (),
				    $1, $3);
		}
	}
	| output_def_body error {

	}
	;

tempo_event:
	TEMPO steno_duration '=' tempo_range	{
		$$ = MAKE_SYNTAX ("tempo", @$, SCM_EOL, $2, $4);
	}
	| TEMPO scalar steno_duration '=' tempo_range	{
		$$ = MAKE_SYNTAX ("tempo", @$, $2, $3, $5);
	}
	| TEMPO scalar {
		$$ = MAKE_SYNTAX ("tempo", @$, $2);
	} %prec ':'
	;

/*
The representation of a  list is reversed to have efficient append.  */

music_list:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| music_list music_embedded {
		if (unsmob_music ($2))
			$$ = scm_cons ($2, $1);
	}
	| music_list error {
		Music *m = MY_MAKE_MUSIC("Music", @$);
		// ugh. code dup
		m->set_property ("error-found", SCM_BOOL_T);
		$$ = scm_cons (m->self_scm (), $1);
		m->unprotect (); /* UGH */
	}
	;

braced_music_list:
	'{' music_list '}'
	{
		$$ = scm_reverse_x ($2, SCM_EOL);
	}
	;

music:	music_arg
	| lyric_element_music
	;

music_embedded:
	music
	{
		if (unsmob_music ($1)->is_mus_type ("post-event")) {
			parser->parser_error (@1, _ ("unexpected post-event"));
			$$ = SCM_UNSPECIFIED;
		}
	}
	| music_embedded_backup
	{
		$$ = $1;
	}
	| music_embedded_backup BACKUP lyric_element_music
	{
		$$ = $3;
	}
	| multiplied_duration post_events
	{
		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);

		parser->default_duration_ = *unsmob_duration ($1);
		n->set_property ("duration", $1);

		if (scm_is_pair ($2))
			n->set_property ("articulations",
					 scm_reverse_x ($2, SCM_EOL));
		$$ = n->unprotect ();
	}
	;

music_embedded_backup:
	embedded_scm
	{
		if (scm_is_eq ($1, SCM_UNSPECIFIED))
			$$ = $1;
		else if (Music *m = unsmob_music ($1)) {
			if (m->is_mus_type ("post-event")) {
				parser->parser_error
					(@1, _ ("unexpected post-event"));
				$$ = SCM_UNSPECIFIED;
			} else
				$$ = $1;
		} else if (parser->lexer_->is_lyric_state ()
			   && Text_interface::is_markup ($1))
			MYBACKUP (LYRIC_ELEMENT, $1, @1);
		else {
			@$.warning (_ ("Ignoring non-music expression"));
			$$ = $1;
		}
	}
	;

music_arg:
	simple_music
	{
	        $$ = make_music_from_simple (parser, @1, $1);
                if (!unsmob_music ($$))
		{
                        parser->parser_error (@1, _ ("music expected"));
			$$ = MAKE_SYNTAX ("void-music", @$);
		}
	}
	| composite_music %prec COMPOSITE
	;

music_assign:
	simple_music
	| composite_music %prec COMPOSITE
	;

repeated_music:
	REPEAT simple_string unsigned_number music
	{
		$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, SCM_EOL);
	}
	| REPEAT_IDENTIFIER music
	{
		$$ = MAKE_SYNTAX ("repeat", @$, scm_car ($1), scm_cdr ($1),
				  $2, SCM_EOL);
	}
	| REPEAT simple_string unsigned_number music ALTERNATIVE braced_music_list
	{
		$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, $6);
	}
	| REPEAT_IDENTIFIER music ALTERNATIVE braced_music_list
	{
		$$ = MAKE_SYNTAX ("repeat", @$, scm_car ($1), scm_cdr ($1),
				  $2, $4);
	}
	;

sequential_music:
	SEQUENTIAL braced_music_list {
		$$ = MAKE_SYNTAX ("sequential-music", @$, $2);
	}
	| braced_music_list {
		$$ = MAKE_SYNTAX ("sequential-music", @$, $1);
	}
	;

simultaneous_music:
	SIMULTANEOUS braced_music_list {
		$$ = MAKE_SYNTAX ("simultaneous-music", @$, $2);
	}
	| DOUBLE_ANGLE_OPEN music_list DOUBLE_ANGLE_CLOSE	{
		$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_reverse_x ($2, SCM_EOL));
	}
	;

simple_music:
	event_chord
	| music_property_def
	| context_change
	;

context_modification:
        WITH
	{
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);
	} '{' context_mod_list '}'
        {
                parser->lexer_->pop_state ();
                $$ = $4;
        }
        | WITH CONTEXT_MOD_IDENTIFIER
        {
                $$ = $2;
        }
        | CONTEXT_MOD_IDENTIFIER
        {
                $$ = $1;
        }
	| WITH context_modification_arg
	{
		if (unsmob_music ($2)) {
			SCM proc = parser->lexer_->lookup_identifier ("context-mod-music-handler");
			$2 = scm_call_2 (proc, parser->self_scm (), $2);
		}
		if (unsmob_context_mod ($2))
			$$ = $2;
		else {
			parser->parser_error (@2, _ ("not a context mod"));
			$$ = Context_mod ().smobbed_copy ();
		}
	}
        ;

context_modification_arg:
	embedded_scm_closed
	| MUSIC_IDENTIFIER
	;

optional_context_mod:
        /**/ {
            $$ = SCM_EOL;
        }
        | context_modification
        {
              $$ = $1;
        }
        ;

context_mod_list:
        /**/ {
            $$ = Context_mod ().smobbed_copy ();
        }
        | context_mod_list context_mod  {
		if (!SCM_UNBNDP ($2))
			unsmob_context_mod ($1)->add_context_mod ($2);
        }
        | context_mod_list CONTEXT_MOD_IDENTIFIER {
                 Context_mod *md = unsmob_context_mod ($2);
                 if (md)
                     unsmob_context_mod ($1)->add_context_mods (md->get_mods ());
        }
	| context_mod_list context_mod_embedded {
		unsmob_context_mod ($1)->add_context_mods
			(unsmob_context_mod ($2)->get_mods ());
        }
        ;

composite_music:
	complex_music
	| music_bare
	;

/* Music that can be parsed without lookahead */
closed_music:
	music_bare
	| complex_music_prefix closed_music
	{
		$$ = FINISH_MAKE_SYNTAX ($1, @$, $2);
	}
	| music_function_call_closed
	;

music_bare:
	mode_changed_music
	| MUSIC_IDENTIFIER
	| grouped_music_list
	;

grouped_music_list:
	simultaneous_music		{ $$ = $1; }
	| sequential_music		{ $$ = $1; }
	;

/* Function argument lists are arguably the most complex part in the
 * parser.  They are pretty tricky to understand because of the way
 * they are processed, and because of optional arguments that can be
 * omitted.  When there are several optional arguments in a row,
 * omitting one automatically omits all following arguments.  Optional
 * arguments can only be omitted when either
 *
 * a) the omission is explicitly started with \default
 * b) the omission is implicitly started by an argument not matching
 *    its predicate, and there is a mandatory argument later that can
 *    "catch" the argument that does not fit.
 *
 * When argument parsing starts, the lexer pushes EXPECT_SCM tokens
 * (corresponding to mandatory arguments and having a predicate
 * function as semantic value) or EXPECT_OPTIONAL EXPECT_SCM (where
 * the semantic value of the EXPECT_OPTIONAL token is the default to
 * use when the optional argument is omitted, and EXPECT_SCM again has
 * the argument predicate as semantic value) in reverse order to the
 * parser, followed by EXPECT_NO_MORE_ARGS.  The argument list is then
 * processed inside-out while actual tokens are consumed.
 *
 * This means that the argument list tokens determine the actions
 * taken as they arrive.  The structure of the argument list is known
 * to the parser and stored in its parse stack when the first argument
 * is being parsed.  What the parser does not know is which predicates
 * will match and whether or not \default will be appearing in the
 * argument list, and where.
 *
 * Many of the basic nonterminals used for argument list scanning come
 * in a "normal" and a "closed" flavor.  A closed expression is one
 * that can be parsed without a lookahead token.  That makes it
 * feasible for an optional argument that may need to be skipped:
 * skipping can only be accomplished by pushing back the token into
 * the lexer, and that only works when there is no lookahead token.
 *
 * Sequences of 0 or more optional arguments are scanned using either
 * function_arglist_backup or function_arglist_nonbackup.  The first
 * is used when optional arguments are followed by at least one
 * mandatory argument: in that case optional arguments may be skipped
 * by either a false predicate (in which case the expression will be
 * pushed back as one or more tokens, preceded by a BACKUP token) or
 * by using \default.
 *
 * If optional arguments are at the end of the argument list, they are
 * instead scanned using function_arglist_nonbackup: here the only
 * manner to enter into skipping of optional arguments is the use of
 * \default.
 *
 * The argument list of a normal function call is parsed using
 * function_arglist.  The part of an argument list before a mandatory
 * argument is parsed using function_arglist_optional.
 *
 * The difference is that leading optional arguments are scanned using
 * function_arglist_nonbackup and function_arglist_backup,
 * respectively.
 *
 * Most other details are obvious in the rules themselves.
 *
 */

function_arglist_nonbackup_common:
	EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup '-' UNSIGNED
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n)))
			$$ = scm_cons (n, $3);
		else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @5);
			t->set_property ("digit", $5);
			$$ = check_scheme_arg (parser, @4, t->unprotect (),
					       $3, $2, n);
		}
		
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup '-' REAL
	{
		$$ = check_scheme_arg (parser, @4,
				       scm_difference ($5, SCM_UNDEFINED),
				       $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup '-' NUMBER_IDENTIFIER
	{
		$$ = check_scheme_arg (parser, @4,
				       scm_difference ($5, SCM_UNDEFINED),
				       $3, $2);
	}
	;

function_arglist_closed_nonbackup:
	function_arglist_nonbackup_common
	| function_arglist_closed_common
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup embedded_scm_arg_closed
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup bare_number_closed
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup SCM_IDENTIFIER
	{
		$$ = check_scheme_arg (parser, @4,
				       try_string_variants ($2, $4),
				       $3, $2, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup STRING
	{
		$$ = check_scheme_arg (parser, @4,
				       try_string_variants ($2, $4),
				       $3, $2, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup full_markup
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	;

symbol_list_arg:
	SYMBOL_LIST
	| SYMBOL_LIST '.' symbol_list_rev
	{
		$$ = scm_append (scm_list_2 ($1, scm_reverse_x ($3, SCM_EOL)));
	}
	;

symbol_list_rev:
	symbol_list_part
	| symbol_list_rev '.' symbol_list_part
	{
		$$ = scm_append_x (scm_list_2 ($3, $1));
	}
	;

// symbol_list_part delivers elements in reverse copy.

symbol_list_part:
	symbol_list_element
	{
		SCM sym_l_p = ly_lily_module_constant ("symbol-list?");
		$$ = try_string_variants (sym_l_p, $1);
		if (SCM_UNBNDP ($$)) {
			parser->parser_error (@1, _("not a symbol"));
			$$ = SCM_EOL;
		} else
			$$ = scm_reverse ($$);
	}
	;


symbol_list_element:
	STRING
	| embedded_scm_bare
	;


function_arglist_nonbackup:
	function_arglist_nonbackup_common
	| function_arglist_common
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup embedded_scm_arg
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
			$$ = scm_cons ($4, $3);
		else
			$$ = check_scheme_arg (parser, @4,
					       make_music_from_simple
					       (parser, @4, $4),
					       $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup bare_number_common
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE duration_length
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE bare_number_common
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE SCM_ARG
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE lyric_element_music
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE symbol_list_arg
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	;

function_arglist_nonbackup_reparse:
	EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup SCM_IDENTIFIER
	{
		$$ = $3;
		SCM res = try_string_variants ($2, $4);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res))
				MYREPARSE (@4, $2, SYMBOL_LIST, res);
			else
				MYREPARSE (@4, $2, SCM_ARG, res);
		else if (scm_is_true
			 (scm_call_1
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup STRING
	{
		$$ = $3;
		SCM res = try_string_variants ($2, $4);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res))
				MYREPARSE (@4, $2, SYMBOL_LIST, res);
			else
				MYREPARSE (@4, $2, SCM_ARG, res);
		else if (scm_is_true
			 (scm_call_1
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup full_markup
	{
		$$ = $3;
		if (scm_is_true (scm_call_1 ($2, $4)))
			MYREPARSE (@4, $2, SCM_ARG, $4);
		else if (scm_is_true
			 (scm_call_1
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup UNSIGNED
	{
		$$ = $3;
		if (scm_is_true (scm_call_1 ($2, $4)))
			MYREPARSE (@4, $2, REAL, $4);
		else {
			SCM d = make_duration ($4);
			if (SCM_UNBNDP (d) || scm_is_false (scm_call_1 ($2, d)))
				MYREPARSE (@4, $2, REAL, $4); // trigger error
			else
				MYREPARSE (@4, $2, DURATION_IDENTIFIER, d);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup DURATION_IDENTIFIER {
		$$ = $3;
		MYREPARSE (@4, $2, DURATION_IDENTIFIER, $4);
	}
	;


// function_arglist_backup can't occur at the end of an argument
// list.  It needs to be careful about avoiding lookahead only until
// it has made a decision whether or not to accept the parsed entity.
// At that point of time, music requiring lookahead to parse becomes
// fine.
function_arglist_backup:
	function_arglist_common
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup embedded_scm_arg_closed
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (SCM_ARG, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup REPEAT simple_string unsigned_number
	{
		$4 = MAKE_SYNTAX ("repeat", @4, $5, $6,
				  MY_MAKE_MUSIC ("Music", @4)->unprotect (),
				  SCM_EOL);
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = $3;
			MYREPARSE (@4, $2, REPEAT_IDENTIFIER, scm_cons ($5, $6));
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (REPEAT_IDENTIFIER, scm_cons ($5, $6), @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup chord_body
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = $3;
			MYREPARSE (@4, $2, CHORD_BODY_IDENTIFIER, $4);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (CHORD_BODY_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup post_event_nofinger
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (EVENT_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup full_markup
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
			$$ = scm_cons ($4, $3);
		else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (LYRIC_ELEMENT, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup UNSIGNED
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			MYREPARSE (@4, $2, REAL, $4);
			$$ = $3;
		} else {
			SCM d = make_duration ($4);
			if (SCM_UNBNDP (d) || scm_is_false (scm_call_1 ($2, d)))
			{
				$$ = scm_cons (loc_on_music (@3, $1), $3);
				MYBACKUP (UNSIGNED, $4, @4);
			} else {
				MYREPARSE (@4, $2, DURATION_IDENTIFIER, d);
				$$ = $3;
			}
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup REAL
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = $3;
			MYREPARSE (@4, $2, REAL, $4);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (REAL, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup NUMBER_IDENTIFIER
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (NUMBER_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup '-' UNSIGNED
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n))) {
			$$ = $3;
			MYREPARSE (@5, $2, REAL, n);
		} else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @5);
			t->set_property ("digit", $5);
			$$ = t->unprotect ();
			if (scm_is_true (scm_call_1 ($2, $$)))
				$$ = scm_cons ($$, $3);
			else {
				$$ = scm_cons (loc_on_music (@3, $1), $3);
				MYBACKUP (UNSIGNED, $5, @5);
				parser->lexer_->push_extra_token ('-');
			}
		}
		
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup '-' REAL
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n))) {
			MYREPARSE (@5, $2, REAL, n);
			$$ = $3;
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (REAL, n, @5);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup '-' NUMBER_IDENTIFIER
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n))) {
			$$ = scm_cons (n, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (NUMBER_IDENTIFIER, n, @5);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup PITCH_IDENTIFIER
	{
		SCM m = make_music_from_simple (parser, @4, $4);
		if (unsmob_music (m) && scm_is_true (scm_call_1 ($2, m)))
		{
			MYREPARSE (@4, $2, PITCH_IDENTIFIER, $4);
			$$ = $3;
		} else if (scm_is_true (scm_call_1 ($2, $4)))
		{
			MYREPARSE (@4, $2, PITCH_ARG, $4);
			$$ = $3;
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (PITCH_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup NOTENAME_PITCH
	{
		SCM m = make_music_from_simple (parser, @4, $4);
		if (unsmob_music (m) && scm_is_true (scm_call_1 ($2, m)))
		{
			MYREPARSE (@4, $2, NOTENAME_PITCH, $4);
			$$ = $3;
		} else if (scm_is_true (scm_call_1 ($2, $4)))
		{
			MYREPARSE (@4, $2, PITCH_ARG, $4);
			$$ = $3;
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (NOTENAME_PITCH, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup TONICNAME_PITCH
	{
		SCM m = make_music_from_simple (parser, @4, $4);
		if (unsmob_music (m) && scm_is_true (scm_call_1 ($2, m)))
		{
			MYREPARSE (@4, $2, TONICNAME_PITCH, $4);
			$$ = $3;
		} else if (scm_is_true (scm_call_1 ($2, $4)))
		{
			MYREPARSE (@4, $2, PITCH_ARG, $4);
			$$ = $3;
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (TONICNAME_PITCH, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup DURATION_IDENTIFIER
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			MYREPARSE (@4, $2, DURATION_IDENTIFIER, $4);
			$$ = $3;
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (DURATION_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup SCM_IDENTIFIER
	{
		SCM res = try_string_variants ($2, $4);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res)) {
				$$ = $3;
				MYREPARSE (@4, $2, SYMBOL_LIST, res);
			}
			else
				$$ = scm_cons (res, $3);
		else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (SCM_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup STRING
	{
		SCM res = try_string_variants ($2, $4);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res)) {
				$$ = $3;
				MYREPARSE (@4, $2, SYMBOL_LIST, res);
			}
			else
				$$ = scm_cons (res, $3);
		else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (STRING, $4, @4);
		}
	}
	| function_arglist_backup REPARSE music_assign
	{
		if (scm_is_true (scm_call_1 ($2, $3)))
			$$ = scm_cons ($3, $1);
		else
			$$ = check_scheme_arg (parser, @3,
					       make_music_from_simple
					       (parser, @3, $3),
					       $1, $2);
	}
	| function_arglist_backup REPARSE pitch_arg
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}		
	| function_arglist_backup REPARSE bare_number_common
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_backup REPARSE duration_length
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_backup REPARSE symbol_list_arg
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	;

function_arglist:
	function_arglist_nonbackup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_nonbackup DEFAULT
	{
		$$ = scm_cons (loc_on_music (@4, $1), $3);
	}
	;

function_arglist_skip_nonbackup:
	function_arglist_nonbackup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_nonbackup
	{
		$$ = scm_cons (loc_on_music (@3, $1), $3);
	}
	;

function_arglist_common:
	EXPECT_NO_MORE_ARGS {
		$$ = SCM_EOL;
	}
	| EXPECT_SCM function_arglist_optional embedded_scm_arg
	{
		if (scm_is_true (scm_call_1 ($1, $3)))
			$$ = scm_cons ($3, $2);
		else
			$$ = check_scheme_arg (parser, @3,
					       make_music_from_simple
					       (parser, @3, $3),
					       $2, $1);
	}
	| EXPECT_SCM function_arglist_optional bare_number_common
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_optional post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_optional '-' NUMBER_IDENTIFIER
	{
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		$$ = check_scheme_arg (parser, @4, n, $2, $1);
	}
	| function_arglist_common_reparse REPARSE SCM_ARG
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE lyric_element_music
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE bare_number_common
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE duration_length
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE symbol_list_arg
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	;

function_arglist_common_reparse:
	EXPECT_SCM function_arglist_optional SCM_IDENTIFIER
	{
		$$ = $2;
		SCM res = try_string_variants ($1, $3);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res))
				MYREPARSE (@3, $1, SYMBOL_LIST, res);
			else
				MYREPARSE (@3, $1, SCM_ARG, res);
		else if (scm_is_true
			 (scm_call_1
			  ($1, make_music_from_simple (parser, @3, $3))))
			MYREPARSE (@3, $1, LYRIC_ELEMENT, $3);
		else
			// This is going to flag a syntax error, we
			// know the predicate to be false.
			MYREPARSE (@3, $1, SCM_ARG, $3);
	}
	| EXPECT_SCM function_arglist_optional STRING
	{
		$$ = $2;
		SCM res = try_string_variants ($1, $3);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res))
				MYREPARSE (@3, $1, SYMBOL_LIST, res);
			else
				MYREPARSE (@3, $1, SCM_ARG, res);
		else if (scm_is_true
			 (scm_call_1
			  ($1, make_music_from_simple (parser, @3, $3))))
			MYREPARSE (@3, $1, LYRIC_ELEMENT, $3);
		else
			// This is going to flag a syntax error, we
			// know the predicate to be false.
			MYREPARSE (@3, $1, SCM_ARG, $3);
	}
	| EXPECT_SCM function_arglist_optional full_markup
	{
		$$ = $2;
		if (scm_is_true (scm_call_1 ($1, $3)))
			MYREPARSE (@3, $1, SCM_ARG, $3);
		else if (scm_is_true
			 (scm_call_1
			  ($1, make_music_from_simple (parser, @3, $3))))
			MYREPARSE (@3, $1, LYRIC_ELEMENT, $3);
		else
			// This is going to flag a syntax error, we
			// know the predicate to be false.
			MYREPARSE (@3, $1, SCM_ARG, $3);
	}
	| EXPECT_SCM function_arglist_optional UNSIGNED
	{
		$$ = $2;
		if (scm_is_true (scm_call_1 ($1, $3)))
			MYREPARSE (@3, $1, REAL, $3);
		else {
			SCM d = make_duration ($3);
			if (SCM_UNBNDP (d) || scm_is_false (scm_call_1 ($1, d)))
				MYREPARSE (@3, $1, REAL, $3);
			else
				MYREPARSE (@3, $1, DURATION_IDENTIFIER, d);
		}
	}
	| EXPECT_SCM function_arglist_optional DURATION_IDENTIFIER
	{
		$$ = $2;
		MYREPARSE (@3, $1, DURATION_IDENTIFIER, $3);
	}
	| EXPECT_SCM function_arglist_optional '-' UNSIGNED
	{
		$$ = $2;
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($1, n)))
			MYREPARSE (@4, $1, REAL, n);
		else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @4);
			t->set_property ("digit", $4);
			SCM m = t->unprotect ();
			if (scm_is_true (scm_call_1 ($1, m)))
				MYREPARSE (@4, $1, SCM_ARG, m);
			else
				MYREPARSE (@4, $1, SCM_ARG, $4);
		}
		
	}
	| EXPECT_SCM function_arglist_optional '-' REAL
	{
		$$ = $2;
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		MYREPARSE (@4, $1, REAL, n);
	}
	;

function_arglist_closed:
	function_arglist_closed_nonbackup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_nonbackup DEFAULT
	{
		$$ = scm_cons (loc_on_music (@4, $1), $3);
	}
	;

function_arglist_closed_common:
	EXPECT_NO_MORE_ARGS {
		$$ = SCM_EOL;
	}
	| EXPECT_SCM function_arglist_optional embedded_scm_arg_closed
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_optional bare_number_common_closed
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_optional '-' NUMBER_IDENTIFIER
	{
		$$ = check_scheme_arg (parser, @3,
				       scm_difference ($4, SCM_UNDEFINED),
				       $2, $1);
	}
	| EXPECT_SCM function_arglist_optional post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| function_arglist_common_reparse REPARSE SCM_ARG
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE bare_number_common_closed
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE symbol_list_arg
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	;

function_arglist_optional:
	function_arglist_backup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_backup DEFAULT
	{
		$$ = scm_cons (loc_on_music (@4, $1), $3);
	}
	| function_arglist_skip_backup BACKUP
	;

function_arglist_skip_backup:
	function_arglist_backup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_backup
	{
		$$ = scm_cons (loc_on_music (@3, $1), $3);
	}
	;

embedded_scm_closed:
	embedded_scm_bare
	| scm_function_call_closed
	;

embedded_scm_arg_closed:
	embedded_scm_bare_arg
	| scm_function_call_closed
	| closed_music
	;

scm_function_call_closed:
	SCM_FUNCTION function_arglist_closed {
		$$ = MAKE_SYNTAX ("music-function", @$,
					 $1, $2);
	}
	;

music_function_call:
	MUSIC_FUNCTION function_arglist {
		$$ = MAKE_SYNTAX ("music-function", @$,
					 $1, $2);
	}
	;


optional_id:
	/**/ { $$ = SCM_EOL; }
	| '=' simple_string {
		$$ = $2;
	}
	;

complex_music:
	music_function_call
	| repeated_music		{ $$ = $1; }
	| re_rhythmed_music	{ $$ = $1; }
	| complex_music_prefix music
	{
		$$ = FINISH_MAKE_SYNTAX ($1, @$, $2);
	}
	;

complex_music_prefix:
	CONTEXT symbol optional_id optional_context_mod {
                Context_mod *ctxmod = unsmob_context_mod ($4);
                SCM mods = SCM_EOL;
                if (ctxmod)
                        mods = ctxmod->get_mods ();
		$$ = START_MAKE_SYNTAX ("context-specification", $2, $3, mods, SCM_BOOL_F);
	}
	| NEWCONTEXT symbol optional_id optional_context_mod {
                Context_mod *ctxmod = unsmob_context_mod ($4);
                SCM mods = SCM_EOL;
                if (ctxmod)
                        mods = ctxmod->get_mods ();
		$$ = START_MAKE_SYNTAX ("context-specification", $2, $3, mods, SCM_BOOL_T);
	}
	;

mode_changed_music:
	mode_changing_head grouped_music_list {
		if ($1 == ly_symbol2scm ("chords"))
		{
		  $$ = MAKE_SYNTAX ("unrelativable-music", @$, $2);
		}
		else
		{
		  $$ = $2;
		}
		parser->lexer_->pop_state ();
	}
	| mode_changing_head_with_context optional_context_mod grouped_music_list {
                Context_mod *ctxmod = unsmob_context_mod ($2);
                SCM mods = SCM_EOL;
                if (ctxmod)
                        mods = ctxmod->get_mods ();
		$$ = MAKE_SYNTAX ("context-specification", @$, $1, SCM_EOL, mods, SCM_BOOL_T, $3);
		if ($1 == ly_symbol2scm ("ChordNames"))
		{
		  $$ = MAKE_SYNTAX ("unrelativable-music", @$, $$);
		}
		parser->lexer_->pop_state ();
	}
	;

mode_changing_head:
	NOTEMODE {
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);

		$$ = ly_symbol2scm ("notes");
	}
	| DRUMMODE
		{
		SCM nn = parser->lexer_->lookup_identifier ("drumPitchNames");
		parser->lexer_->push_note_state (nn);

		$$ = ly_symbol2scm ("drums");
	}
	| FIGUREMODE {
		parser->lexer_->push_figuredbass_state ();

		$$ = ly_symbol2scm ("figures");
	}
	| CHORDMODE {
		SCM nn = parser->lexer_->lookup_identifier ("chordmodifiers");
		parser->lexer_->chordmodifier_tab_ = alist_to_hashq (nn);
		nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_chord_state (nn);
		$$ = ly_symbol2scm ("chords");

	}
	| LYRICMODE
		{ parser->lexer_->push_lyric_state ();
		$$ = ly_symbol2scm ("lyrics");
	}
	;

mode_changing_head_with_context:
	DRUMS {
		SCM nn = parser->lexer_->lookup_identifier ("drumPitchNames");
		parser->lexer_->push_note_state (nn);

		$$ = ly_symbol2scm ("DrumStaff");
	}
	| FIGURES {
		parser->lexer_->push_figuredbass_state ();

		$$ = ly_symbol2scm ("FiguredBass");
	}
	| CHORDS {
		SCM nn = parser->lexer_->lookup_identifier ("chordmodifiers");
		parser->lexer_->chordmodifier_tab_ = alist_to_hashq (nn);
		nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_chord_state (nn);
		$$ = ly_symbol2scm ("ChordNames");
	}
	| LYRICS
		{ parser->lexer_->push_lyric_state ();
		$$ = ly_symbol2scm ("Lyrics");
	}
	;

new_lyrics:
	ADDLYRICS { parser->lexer_->push_lyric_state (); }
	/*cont */
	composite_music {
	/* Can also use music at the expensive of two S/Rs similar to
           \repeat \alternative */
		parser->lexer_->pop_state ();

		$$ = scm_cons ($3, SCM_EOL);
	}
	| new_lyrics ADDLYRICS {
		parser->lexer_->push_lyric_state ();
	} composite_music {
		parser->lexer_->pop_state ();
		$$ = scm_cons ($4, $1);
	}
	;

re_rhythmed_music:
	composite_music new_lyrics {
		$$ = MAKE_SYNTAX ("add-lyrics", @$, $1, scm_reverse_x ($2, SCM_EOL));
	} %prec COMPOSITE
	| LYRICSTO simple_string {
		parser->lexer_->push_lyric_state ();
	} music {
		parser->lexer_->pop_state ();
		$$ = MAKE_SYNTAX ("lyric-combine", @$, $2, $4);
	}
	;

context_change:
	CHANGE STRING '=' STRING  {
		$$ = MAKE_SYNTAX ("context-change", @$, scm_string_to_symbol ($2), $4);
	}
	;


property_path:
	symbol_list_rev  {
		$$ = scm_reverse_x ($1, SCM_EOL);
	}
	| symbol_list_rev property_path {
		$$ = scm_reverse_x ($1, $2);
	}
	;

property_operation:
	symbol '=' scalar {
		$$ = scm_list_3 (ly_symbol2scm ("assign"), $1, $3);
	}
	| UNSET symbol {
		$$ = scm_list_2 (ly_symbol2scm ("unset"), $2);
	}
	| OVERRIDE property_path '=' scalar {
		if (scm_ilength ($2) < 2) {
			parser->parser_error (@2, _("bad grob property path"));
			$$ = SCM_UNDEFINED;
		} else {
			$$ = scm_cons (ly_symbol2scm ("push"),
				       scm_cons2 (scm_car ($2),
						  $4,
						  scm_cdr ($2)));
		}
	}
	| REVERT revert_arg {
		$$ = scm_cons (ly_symbol2scm ("pop"), $2);
	}
	;

// This is all quite awkward for the sake of substantial backward
// compatibility while at the same time allowing a more "natural" form
// of specification not separating grob specification from grob
// property path.  The purpose of this definition of revert_arg is to
// allow the symbol list which specifies grob and property to revert
// to be optionally be split into two parts after the grob (which in
// this case is just the first element of the list).  symbol_list_part
// is only one path component, but it can be parsed without lookahead,
// so we can follow it with a synthetic BACKUP token when needed.  If
// the first symbol_list_part already contains multiple elements (only
// possible if a Scheme expression provides them), we just parse for
// additional elements introduced by '.', which is what the
// SYMBOL_LIST backup in connection with the immediately following
// rule using symbol_list_arg does.
//
// As long as we don't have our coffers filled with both grob and at
// least one grob property specification, the rest of the required
// symbol list chain may be provided either with or without a leading
// dot.  This is for both allowing the traditional
// \revert Accidental #'color
// as well as well as the "naive" form
// \revert Accidental.color

revert_arg:
	revert_arg_backup BACKUP symbol_list_arg
	{
		$$ = $3;
	}
	;

revert_arg_backup:
	revert_arg_part
	{
		if (scm_is_null ($1)
		    || scm_is_null (scm_cdr ($1)))
			MYBACKUP (SCM_ARG, $1, @1);
		else
			MYBACKUP (SYMBOL_LIST, scm_reverse_x ($1, SCM_EOL), @1);
	}
	;

// revert_arg_part delivers results in reverse
revert_arg_part:
	symbol_list_part
	| revert_arg_backup BACKUP SCM_ARG '.' symbol_list_part
	{
		$$ = scm_append_x (scm_list_2 ($5, $3));
	}
	| revert_arg_backup BACKUP SCM_ARG symbol_list_part
	{
		$$ = scm_append_x (scm_list_2 ($4, $3));
	}		
	;

context_def_mod:
	CONSISTS { $$ = ly_symbol2scm ("consists"); }
	| REMOVE { $$ = ly_symbol2scm ("remove"); }

	| ACCEPTS { $$ = ly_symbol2scm ("accepts"); }
	| DEFAULTCHILD { $$ = ly_symbol2scm ("default-child"); }
	| DENIES { $$ = ly_symbol2scm ("denies"); }

	| ALIAS { $$ = ly_symbol2scm ("alias"); }
	| TYPE { $$ = ly_symbol2scm ("translator-type"); }
	| DESCRIPTION { $$ = ly_symbol2scm ("description"); }
	| NAME { $$ = ly_symbol2scm ("context-name"); }
	;

context_mod:
	property_operation { $$ = $1; }
	| context_def_mod STRING {
		$$ = scm_list_2 ($1, $2);
	}
	| context_def_mod embedded_scm
	{
		if (!scm_is_string ($2)
		    && ly_symbol2scm ("consists") != $1
		    && ly_symbol2scm ("remove") != $1)
		{
			$$ = SCM_EOL;
			parser->parser_error (@1, _ ("only \\consists and \\remove take non-string argument."));
		}
		else
		{
			$$ = scm_list_2 ($1, $2);
		}
	}
	;

// If defined, at least two members.
grob_prop_spec:
	symbol_list_rev
	{
		SCM l = scm_reverse_x ($1, SCM_EOL);
		if (scm_is_pair (l)
		    && to_boolean
		    (scm_object_property (scm_car (l),
					  ly_symbol2scm ("is-grob?"))))
			l = scm_cons (ly_symbol2scm ("Bottom"), l);
		if (scm_is_null (l) || scm_is_null (scm_cdr (l))) {
			parser->parser_error (@1, _ ("bad grob property path"));
			l = SCM_UNDEFINED;
		}
		$$ = l;
	}
	;

// If defined, at least three members
grob_prop_path:
	grob_prop_spec
	{
		if (!SCM_UNBNDP ($1) && scm_is_null (scm_cddr ($1)))
		{
			parser->parser_error (@1, _ ("bad grob property path"));
			$$ = SCM_UNDEFINED;
		}
	}
	| grob_prop_spec property_path
	{
		if (!SCM_UNBNDP ($1)) {
			$$ = scm_append_x (scm_list_2 ($1, $2));
			if (scm_is_null (scm_cddr ($$))) {
				parser->parser_error (@$, _ ("bad grob property path"));
				$$ = SCM_UNDEFINED;
			}
		}

	}
	;

// Exactly two elements or undefined
context_prop_spec:
	symbol_list_rev
	{
		SCM l = scm_reverse_x ($1, SCM_EOL);
		switch (scm_ilength (l)) {
		case 1:
			l = scm_cons (ly_symbol2scm ("Bottom"), l);
		case 2:
			break;
		default:
			parser->parser_error (@1, _ ("bad context property path"));
			l = SCM_UNDEFINED;
		}
		$$ = l;
	}
	;

simple_music_property_def:
	OVERRIDE grob_prop_path '=' scalar {
		if (SCM_UNBNDP ($2))
			$$ = SCM_UNDEFINED;
		else {
			$$ = scm_list_5 (scm_car ($2),
					 ly_symbol2scm ("OverrideProperty"),
					 scm_cadr ($2),
					 $4,
					 scm_cddr ($2));
		}
	}
	| REVERT simple_revert_context revert_arg {
		$$ = scm_list_4 ($2,
				 ly_symbol2scm ("RevertProperty"),
				 scm_car ($3),
				 scm_cdr ($3));
	}
	| SET context_prop_spec '=' scalar {
		if (SCM_UNBNDP ($2))
			$$ = SCM_UNDEFINED;
		else
			$$ = scm_list_4 (scm_car ($2),
					 ly_symbol2scm ("PropertySet"),
					 scm_cadr ($2),
					 $4);
	}
	| UNSET context_prop_spec {
		if (SCM_UNBNDP ($2))
			$$ = SCM_UNDEFINED;
		else
			$$ = scm_list_3 (scm_car ($2),
					 ly_symbol2scm ("PropertyUnset"),
					 scm_cadr ($2));
	}
	;


// This is all quite awkward for the sake of substantial backward
// compatibility while at the same time allowing a more "natural" form
// of specification not separating grob specification from grob
// property path.  The purpose of this definition of
// simple_revert_context is to allow the symbol list which specifies
// grob and property to revert to be optionally be split into two
// parts after the grob (which may be preceded by a context
// specification, a case which we distinguish by checking whether the
// first symbol is a valid grob symbol instead).
//
// See revert_arg above for the main work horse of this arrangement.
// simple_revert_context just caters for the context and delegates the
// rest of the job to revert_arg.

simple_revert_context:
	symbol_list_part
	{
		$1 = scm_reverse_x ($1, SCM_EOL);
		if (scm_is_null ($1)
		    || to_boolean
		    (scm_object_property (scm_car ($1),
					  ly_symbol2scm ("is-grob?")))) {
			$$ = ly_symbol2scm ("Bottom");
			parser->lexer_->push_extra_token (SCM_IDENTIFIER, $1);
		} else {
			$$ = scm_car ($1);
			parser->lexer_->push_extra_token (SCM_IDENTIFIER,
							  scm_cdr ($1));
		}
	}
	;

music_property_def:
	simple_music_property_def {
		if (SCM_UNBNDP ($1))
			$$ = MAKE_SYNTAX ("void-music", @1);
		else
			$$ = LOWLEVEL_MAKE_SYNTAX (ly_lily_module_constant ("property-operation"), scm_cons2 (parser->self_scm (), make_input (@$), $1));
	}
	;

string:
	STRING {
		$$ = $1;
	}
	| full_markup
	;

simple_string: STRING {
		$$ = $1;
	}
	| embedded_scm_bare
	{
		if (scm_is_string ($1)) {
			$$ = $1;
		} else {
			parser->parser_error (@1, (_ ("simple string expected")));
			$$ = scm_string (SCM_EOL);
		}
	}
	;

symbol:
	STRING {
		$$ = scm_string_to_symbol ($1);
	}
	| embedded_scm_bare
	{
		// This is a bit of overkill but makes the same
		// routine responsible for all symbol interpretations.
		$$ = try_string_variants (ly_lily_module_constant ("symbol?"),
					  $1);
		if (SCM_UNBNDP ($$))
		{
			parser->parser_error (@1, (_ ("symbol expected")));
			// Generate a unique symbol in case it is used
			// for an assignment or similar
			$$ = scm_make_symbol (ly_string2scm ("undefined"));
		}
	}
	;

scalar:
	embedded_scm_arg
	| SCM_IDENTIFIER
	| bare_number
	// The following is a rather defensive variant of admitting
	// negative numbers: the grammar would permit number_factor or
	// even number_expression.  However, function arguments allow
	// only this simple kind of negative number, so to have things
	// like \tweak and \override behave reasonably similar, it
	// makes sense to rule out things like -- which are rather an
	// accent in function argument contexts.
	| '-' bare_number
	{
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| STRING
	| full_markup
	;

event_chord:
	simple_element post_events {
		// Let the rhythmic music iterator sort this mess out.
		if (scm_is_pair ($2)) {
		        $$ = make_music_from_simple (parser, @1, $1);
			if (unsmob_music ($$))
                                unsmob_music ($$)->set_property ("articulations",
                                                                 scm_reverse_x ($2, SCM_EOL));
                        else
			{
                                parser->parser_error (@1, _("music expected"));
				$$ = MAKE_SYNTAX ("void-music", @1);
			}
		}
	} %prec ':'
	| simple_chord_elements post_events	{
		SCM elts = ly_append2 ($1, scm_reverse_x ($2, SCM_EOL));

		Input i;
		/* why is this giving wrong start location? -ns
		 * i = @$; */
		i.set_location (@1, @2);
		$$ = MAKE_SYNTAX ("event-chord", i, elts);
	} %prec ':'
	| CHORD_REPETITION optional_notemode_duration post_events {
		Input i;
		i.set_location (@1, @3);
		$$ = MAKE_SYNTAX ("repetition-chord", i,
				  $2, scm_reverse_x ($3, SCM_EOL));
	} %prec ':'
	| MULTI_MEASURE_REST optional_notemode_duration post_events {
		Input i;
		i.set_location (@1, @3);
		$$ = MAKE_SYNTAX ("multi-measure-rest", i, $2,
				  scm_reverse_x ($3, SCM_EOL));
	} %prec ':'
	| command_element
	| note_chord_element
	;


note_chord_element:
	chord_body optional_notemode_duration post_events
	{
		Music *m = unsmob_music ($1);
		SCM dur = unsmob_duration ($2)->smobbed_copy ();
		SCM es = m->get_property ("elements");
		SCM postevs = scm_reverse_x ($3, SCM_EOL);

		for (SCM s = es; scm_is_pair (s); s = scm_cdr (s))
		  unsmob_music (scm_car (s))->set_property ("duration", dur);
		es = ly_append2 (es, postevs);

		m-> set_property ("elements", es);
		m->set_spot (@$);
		$$ = m->self_scm ();
	} %prec ':'
	;

chord_body:
	ANGLE_OPEN chord_body_elements ANGLE_CLOSE
	{
		$$ = MAKE_SYNTAX ("event-chord", @$, scm_reverse_x ($2, SCM_EOL));
	}
	| CHORD_BODY_IDENTIFIER
	;

chord_body_elements:
	/* empty */ 		{ $$ = SCM_EOL; }
	| chord_body_elements chord_body_element {
		if (!SCM_UNBNDP ($2))
			$$ = scm_cons ($2, $1);
	}
	;

chord_body_element:
	pitch exclamations questions octave_check post_events
	{
		bool q = to_boolean ($3);
		bool ex = to_boolean ($2);
		SCM check = $4;
		SCM post = $5;

		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		n->set_property ("pitch", $1);
		if (q)
			n->set_property ("cautionary", SCM_BOOL_T);
                if (ex || q)
			n->set_property ("force-accidental", SCM_BOOL_T);

		if (scm_is_pair (post)) {
			SCM arts = scm_reverse_x (post, SCM_EOL);
			n->set_property ("articulations", arts);
		}
		if (scm_is_number (check))
		{
			int q = scm_to_int (check);
			n->set_property ("absolute-octave", scm_from_int (q-1));
		}

		$$ = n->unprotect ();
	}
	| DRUM_PITCH post_events {
		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		n->set_property ("drum-type", $1);

		if (scm_is_pair ($2)) {
			SCM arts = scm_reverse_x ($2, SCM_EOL);
			n->set_property ("articulations", arts);
		}
		$$ = n->unprotect ();
	}
	| music_function_chord_body
	{
		Music *m = unsmob_music ($1);

		while (m && m->is_mus_type ("music-wrapper-music")) {
			$$ = m->get_property ("element");
			m = unsmob_music ($$);
		}

		if (!(m && m->is_mus_type ("rhythmic-event"))) {
			parser->parser_error (@$, _ ("not a rhythmic event"));
			$$ = SCM_UNDEFINED;
		}
	}
	;

music_function_chord_body:
	music_function_call
	| MUSIC_IDENTIFIER
	;

// Event functions may only take closed arglists, otherwise it would
// not be clear whether a following postevent should be associated
// with the last argument of the event function or with the expression
// for which the function call acts itself as event.

music_function_call_closed:
	MUSIC_FUNCTION function_arglist_closed {
		$$ = MAKE_SYNTAX ("music-function", @$,
					 $1, $2);
	}
	;

event_function_event:
	EVENT_FUNCTION function_arglist_closed {
		$$ = MAKE_SYNTAX ("music-function", @$,
					 $1, $2);
	}
	;

command_element:
	command_event {
		$$ = $1;
	}
	;

command_event:
	tempo_event {
		$$ = $1;
	}
	;


post_events:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| post_events post_event {
		$$ = $1;
		if (Music *m = unsmob_music ($2))
		{
			if (m->is_mus_type ("post-event-wrapper"))
			{
				for (SCM p = m->get_property ("elements");
				     scm_is_pair (p);
				     p = scm_cdr (p))
				{
					$$ = scm_cons (scm_car (p), $$);
				}
			} else {
				m->set_spot (@2);
				$$ = scm_cons ($2, $$);
			}
		}
	}
	;

post_event_nofinger:
	direction_less_event {
		$$ = $1;
	}
	| script_dir music_function_call_closed {
		$$ = $2;
		if (!unsmob_music ($2)->is_mus_type ("post-event")) {
			parser->parser_error (@2, _ ("post-event expected"));
			$$ = SCM_UNSPECIFIED;
		} else if (!SCM_UNBNDP ($1))
		{
			unsmob_music ($$)->set_property ("direction", $1);
		}
	}
	| HYPHEN {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _ ("have to be in Lyric mode for lyrics"));
		$$ = MY_MAKE_MUSIC ("HyphenEvent", @$)->unprotect ();
	}
	| EXTENDER {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _ ("have to be in Lyric mode for lyrics"));
		$$ = MY_MAKE_MUSIC ("ExtenderEvent", @$)->unprotect ();
	}
	| script_dir direction_reqd_event {
		if (!SCM_UNBNDP ($1))
		{
			Music *m = unsmob_music ($2);
			m->set_property ("direction", $1);
		}
		$$ = $2;
	}
	| script_dir direction_less_event {
		if (!SCM_UNBNDP ($1))
		{
			Music *m = unsmob_music ($2);
			m->set_property ("direction", $1);
		}
		$$ = $2;
	}
	| '^' fingering
	{
		$$ = $2;
		unsmob_music ($$)->set_property ("direction", scm_from_int (UP));
	}
	| '_' fingering
	{
		$$ = $2;
		unsmob_music ($$)->set_property ("direction", scm_from_int (DOWN));
	}			
	;

post_event:
	post_event_nofinger
	| '-' fingering {
		$$ = $2;
	}
	;

string_number_event:
	E_UNSIGNED {
		Music *s = MY_MAKE_MUSIC ("StringNumberEvent", @$);
		s->set_property ("string-number", $1);
		$$ = s->unprotect ();
	}
	;

direction_less_event:
	string_number_event
	| EVENT_IDENTIFIER	{
		$$ = $1;
	}
	| tremolo_type  {
               Music *a = MY_MAKE_MUSIC ("TremoloEvent", @$);
               a->set_property ("tremolo-type", $1);
               $$ = a->unprotect ();
        }
	| event_function_event	
	;

direction_reqd_event:
	gen_text_def {
		$$ = $1;
	}
	| script_abbreviation {
		SCM s = parser->lexer_->lookup_identifier ("dash" + ly_scm2string ($1));
		Music *a = MY_MAKE_MUSIC ("ArticulationEvent", @$);
		if (scm_is_string (s))
			a->set_property ("articulation-type", s);
		else parser->parser_error (@1, _ ("expecting string as script definition"));
		$$ = a->unprotect ();
	}
	;

octave_check:
	/**/ { $$ = SCM_EOL; }
	| '=' quotes { $$ = $2; }
	;

quotes:
	/* empty */
	{
                $$ = SCM_INUM0;
        }
	| sub_quotes
        | sup_quotes
        ;

sup_quotes:
	'\'' {
		$$ = scm_from_int (1);
	}
	| sup_quotes '\'' {
		$$ = scm_oneplus ($1);
	}
	;

sub_quotes:
	',' {
		$$ = scm_from_int (-1);
	}
	| sub_quotes ',' {
		$$ = scm_oneminus ($1);
	}
	;

steno_pitch:
	NOTENAME_PITCH quotes {
                if (!scm_is_eq (SCM_INUM0, $2))
                {
                        Pitch p = *unsmob_pitch ($1);
                        p = p.transposed (Pitch (scm_to_int ($2),0,0));
                        $$ = p.smobbed_copy ();
                }
	}
	;

/*
ugh. duplication
*/

steno_tonic_pitch:
	TONICNAME_PITCH	quotes {
                if (!scm_is_eq (SCM_INUM0, $2))
                {
                        Pitch p = *unsmob_pitch ($1);
                        p = p.transposed (Pitch (scm_to_int ($2),0,0));
                        $$ = p.smobbed_copy ();
                }
	}
	;

pitch:
	steno_pitch
	| PITCH_IDENTIFIER quotes {
                if (!scm_is_eq (SCM_INUM0, $2))
                {
                        Pitch p = *unsmob_pitch ($1);
                        p = p.transposed (Pitch (scm_to_int ($2),0,0));
                        $$ = p.smobbed_copy ();
                }
	}
	;

pitch_arg:
	PITCH_ARG quotes {
                if (!scm_is_eq (SCM_INUM0, $2))
                {
                        Pitch p = *unsmob_pitch ($1);
                        p = p.transposed (Pitch (scm_to_int ($2),0,0));
                        $$ = p.smobbed_copy ();
                }
	}
	;

gen_text_def:
	full_markup {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		t->set_property ("text", $1);
		$$ = t->unprotect ();
	}
	| STRING {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		t->set_property ("text",
			make_simple_markup ($1));
		$$ = t->unprotect ();
	}
	| embedded_scm_closed
	{
		Music *m = unsmob_music ($1);
		if (m && m->is_mus_type ("post-event"))
			$$ = $1;
		else if (Text_interface::is_markup ($1)) {
			Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
			t->set_property ("text", $1);
			$$ = t->unprotect ();
		} else
			parser->parser_error (@1, _ ("not an articulation"));
	}
	;

fingering:
	UNSIGNED {
		Music *t = MY_MAKE_MUSIC ("FingeringEvent", @$);
		t->set_property ("digit", $1);
		$$ = t->unprotect ();
	}
	;

script_abbreviation:
	'^'		{
		$$ = scm_from_locale_string ("Hat");
	}
	| '+'		{
		$$ = scm_from_locale_string ("Plus");
	}
	| '-' 		{
		$$ = scm_from_locale_string ("Dash");
	}
 	| '!'		{
		$$ = scm_from_locale_string ("Bang");
	}
	| ANGLE_CLOSE	{
		$$ = scm_from_locale_string ("Larger");
	}
	| '.' 		{
		$$ = scm_from_locale_string ("Dot");
	}
	| '_' {
		$$ = scm_from_locale_string ("Underscore");
	}
	;

script_dir:
	'_'	{ $$ = scm_from_int (DOWN); }
	| '^'	{ $$ = scm_from_int (UP); }
	| '-'	{ $$ = SCM_UNDEFINED; }
	;

duration_length:
	multiplied_duration {
		$$ = $1;
	}
	;

maybe_notemode_duration:
	{
		$$ = SCM_UNDEFINED;
	} %prec ':'
	| multiplied_duration	{
		$$ = $1;
		parser->default_duration_ = *unsmob_duration ($$);
	}
;


optional_notemode_duration:
	maybe_notemode_duration
	{
		if (SCM_UNBNDP ($$))
			$$ = parser->default_duration_.smobbed_copy ();
	}
	;

steno_duration:
	UNSIGNED dots		{
		$$ = make_duration ($1, scm_to_int ($2));
		if (SCM_UNBNDP ($$))
		{
			parser->parser_error (@1, _ ("not a duration"));
			$$ = Duration ().smobbed_copy ();
		}
	}
	| DURATION_IDENTIFIER dots	{
		Duration *d = unsmob_duration ($1);
		Duration k (d->duration_log (),
                            d->dot_count () + scm_to_int ($2));
		k = k.compressed (d->factor ());
                scm_remember_upto_here_1 ($1);
		$$ = k.smobbed_copy ();
	}
	;

multiplied_duration:
	steno_duration {
		$$ = $1;
	}
	| multiplied_duration '*' UNSIGNED {
		$$ = unsmob_duration ($$)->compressed (scm_to_int ($3)).smobbed_copy ();
	}
	| multiplied_duration '*' FRACTION {
		Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3)));

		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
	}
	;

dots:
	/* empty */ 	{
		$$ = SCM_INUM0;
	}
	| dots '.' {
		$$ = scm_oneplus ($1);
	}
	;

tremolo_type:
	':'	{
		$$ = SCM_INUM0;
	}
	| ':' UNSIGNED {
		if (SCM_UNBNDP (make_duration ($2)))
			parser->parser_error (@2, _ ("not a duration"));
		$$ = $2;
	}
	;

bass_number:
	UNSIGNED { $$ = $1; }
	| STRING { $$ = $1; }
	| full_markup { $$ = $1; }
	| embedded_scm_bare
	{
		// as an integer, it needs to be non-negative, and otherwise
		// it needs to be suitable as a markup.
		if (scm_is_integer ($1)
		    ? scm_is_true (scm_negative_p ($1))
		    : !Text_interface::is_markup ($1))
		{
			parser->parser_error (@1, _ ("bass number expected"));
			$$ = SCM_INUM0;
		}
	}
	;

figured_bass_alteration:
	'-' 	{ $$ = ly_rational2scm (FLAT_ALTERATION); }
	| '+'	{ $$ = ly_rational2scm (SHARP_ALTERATION); }
	| '!'	{ $$ = scm_from_int (0); }
	;

bass_figure:
	FIGURE_SPACE {
		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent", @$);
		$$ = bfr->unprotect ();
	}
	| bass_number  {
		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent", @$);
		$$ = bfr->self_scm ();

		if (scm_is_number ($1))
			bfr->set_property ("figure", $1);
		else if (Text_interface::is_markup ($1))
			bfr->set_property ("text", $1);

		bfr->unprotect ();
	}
	| bass_figure ']' {
		$$ = $1;
		unsmob_music ($1)->set_property ("bracket-stop", SCM_BOOL_T);
	}
	| bass_figure figured_bass_alteration {
		Music *m = unsmob_music ($1);
		if (scm_to_double ($2)) {
			SCM salter = m->get_property ("alteration");
			SCM alter = scm_is_number (salter) ? salter : scm_from_int (0);
			m->set_property ("alteration",
					 scm_sum (alter, $2));
		} else {
			m->set_property ("alteration", scm_from_int (0));
		}
	}
	| bass_figure figured_bass_modification  {
		Music *m = unsmob_music ($1);
		m->set_property ($2, SCM_BOOL_T);
	}
	;


figured_bass_modification:
	E_PLUS		{
		$$ = ly_symbol2scm ("augmented");
	}
	| E_EXCLAMATION {
		$$ = ly_symbol2scm ("no-continuation");
	}
	| '/'		{
		$$ = ly_symbol2scm ("diminished");
	}
	| E_BACKSLASH {
		$$ = ly_symbol2scm ("augmented-slash");
	}
	;

br_bass_figure:
	bass_figure {
		$$ = $1;
	}
	| '[' bass_figure {
		$$ = $2;
		unsmob_music ($$)->set_property ("bracket-start", SCM_BOOL_T);
	}
	;

figure_list:
	/**/		{
		$$ = SCM_EOL;
	}
	| figure_list br_bass_figure {
		$$ = scm_cons ($2, $1);
	}
	;

figure_spec:
	FIGURE_OPEN figure_list FIGURE_CLOSE {
		$$ = scm_reverse_x ($2, SCM_EOL);
	}
	;


optional_rest:
	/**/   { $$ = SCM_BOOL_F; }
	| REST { $$ = SCM_BOOL_T; }
	;

simple_element:
	pitch exclamations questions octave_check maybe_notemode_duration optional_rest {
		if (!parser->lexer_->is_note_state ())
			parser->parser_error (@1, _ ("have to be in Note mode for notes"));
		if (!SCM_UNBNDP ($2)
                    || !SCM_UNBNDP ($3)
                    || scm_is_number ($4)
                    || !SCM_UNBNDP ($5)
                    || scm_is_true ($6))
		{
			Music *n = 0;
			if (scm_is_true ($6))
				n = MY_MAKE_MUSIC ("RestEvent", @$);
			else
				n = MY_MAKE_MUSIC ("NoteEvent", @$);
			
			n->set_property ("pitch", $1);
			if (SCM_UNBNDP ($5))
				n->set_property ("duration",
						 parser->default_duration_.smobbed_copy ());
			else
				n->set_property ("duration", $5);
			
			if (scm_is_number ($4))
			{
				int q = scm_to_int ($4);
				n->set_property ("absolute-octave", scm_from_int (q-1));
			}
			
			if (to_boolean ($3))
				n->set_property ("cautionary", SCM_BOOL_T);
			if (to_boolean ($2) || to_boolean ($3))
				n->set_property ("force-accidental", SCM_BOOL_T);
			
			$$ = n->unprotect ();
		}
	}
	| DRUM_PITCH optional_notemode_duration {
		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		n->set_property ("duration", $2);
		n->set_property ("drum-type", $1);

		$$ = n->unprotect ();
	}
 	| RESTNAME optional_notemode_duration		{
		Music *ev = 0;
 		if (ly_scm2string ($1) == "s") {
			/* Space */
			ev = MY_MAKE_MUSIC ("SkipEvent", @$);
		  }
		else {
			ev = MY_MAKE_MUSIC ("RestEvent", @$);

		    }
		ev->set_property ("duration", $2);
 		$$ = ev->unprotect ();
	}
	;

simple_chord_elements:
	new_chord {
                if (!parser->lexer_->is_chord_state ())
                        parser->parser_error (@1, _ ("have to be in Chord mode for chords"));
                $$ = $1;
	}
	| figure_spec optional_notemode_duration {
		for (SCM s = $1; scm_is_pair (s); s = scm_cdr (s))
		{
			unsmob_music (scm_car (s))->set_property ("duration", $2);
		}
		$$ = $1;
	}
	;

lyric_element:
	full_markup {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _ ("markup outside of text script or \\lyricmode"));
		$$ = $1;
	}
	| STRING {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _ ("unrecognized string, not in text script or \\lyricmode"));
		$$ = $1;
	}
	| LYRIC_ELEMENT
	;

lyric_element_music:
	lyric_element optional_notemode_duration post_events {
		$$ = MAKE_SYNTAX ("lyric-event", @$, $1, $2);
		if (scm_is_pair ($3))
			unsmob_music ($$)->set_property
				("articulations", scm_reverse_x ($3, SCM_EOL));
	} %prec ':'
	;

new_chord:
	steno_tonic_pitch optional_notemode_duration   {
		$$ = make_chord_elements (@$, $1, $2, SCM_EOL);
	}
	| steno_tonic_pitch optional_notemode_duration chord_separator chord_items {
		SCM its = scm_reverse_x ($4, SCM_EOL);
		$$ = make_chord_elements (@$, $1, $2, scm_cons ($3, its));
	} %prec ':'
	;

chord_items:
	/**/ {
		$$ = SCM_EOL;
	}
	| chord_items chord_item {
		$$ = scm_cons ($2, $$);
	}
	;

chord_separator:
	CHORD_COLON {
		$$ = ly_symbol2scm ("chord-colon");
	}
	| CHORD_CARET {
		$$ = ly_symbol2scm ("chord-caret");
	}
	| CHORD_SLASH steno_tonic_pitch {
 		$$ = scm_list_2 (ly_symbol2scm ("chord-slash"), $2);
	}
	| CHORD_BASS steno_tonic_pitch {
		$$ = scm_list_2 (ly_symbol2scm ("chord-bass"), $2);
	}
	;

chord_item:
	chord_separator {
		$$ = $1;
	}
	| step_numbers {
		$$ = scm_reverse_x ($1, SCM_EOL);
	}
	| CHORD_MODIFIER  {
		$$ = $1;
	}
	;

step_numbers:
	step_number { $$ = scm_cons ($1, SCM_EOL); }
	| step_numbers '.' step_number {
		$$ = scm_cons ($3, $$);
	}
	;

step_number:
	UNSIGNED {
		$$ = make_chord_step ($1, 0);
        }
	| UNSIGNED '+' {
		$$ = make_chord_step ($1, SHARP_ALTERATION);
	}
	| UNSIGNED CHORD_MINUS {
		$$ = make_chord_step ($1, FLAT_ALTERATION);
	}
	;

tempo_range:
	UNSIGNED {
		$$ = $1;
	}
	| UNSIGNED '-' UNSIGNED {
		$$ = scm_cons ($1, $3);
	}
	;

/*
	UTILITIES

TODO: should deprecate in favor of Scheme?

 */
number_expression:
	number_expression '+' number_term {
		$$ = scm_sum ($1, $3);
	}
	| number_expression '-' number_term {
		$$ = scm_difference ($1, $3);
	}
	| number_term
	;

number_term:
	number_factor {
		$$ = $1;
	}
	| number_factor '*' number_factor {
		$$ = scm_product ($1, $3);
	}
	| number_factor '/' number_factor {
		$$ = scm_divide ($1, $3);
	}
	;

number_factor:
	'-'  number_factor { /* %prec UNARY_MINUS */
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| bare_number
	;

bare_number_common:
	bare_number_common_closed
	| REAL NUMBER_IDENTIFIER
	{
		$$ = scm_product ($1, $2);
	}
	;

bare_number_common_closed:
	REAL
	| NUMBER_IDENTIFIER
	;

bare_number:
	bare_number_common
	| UNSIGNED
	| UNSIGNED NUMBER_IDENTIFIER	{
		$$ = scm_product ($1, $2);
	}
	;

bare_number_closed:
	UNSIGNED
	| bare_number_common_closed
	;

unsigned_number:
	UNSIGNED
	| NUMBER_IDENTIFIER
	;

exclamations:
		{ $$ = SCM_UNDEFINED; }
	| exclamations '!'
        {
                if (SCM_UNBNDP ($1))
                        $$ = SCM_BOOL_T;
                else
                        $$ = scm_not ($1);
        }
	;

questions:
	{ $$ = SCM_UNDEFINED; }
	| questions '?'
        {
                if (SCM_UNBNDP ($1))
                        $$ = SCM_BOOL_T;
                else
                        $$ = scm_not ($1);
        }
	;

full_markup_list:
	MARKUPLIST
		{ parser->lexer_->push_markup_state (); }
	markup_list {
		$$ = $3;
		parser->lexer_->pop_state ();
	}
	;

full_markup:
	MARKUP
		{ parser->lexer_->push_markup_state (); }
	markup_top {
		$$ = $3;
		parser->lexer_->pop_state ();
	}
	;

markup_top:
	markup_list {
		$$ = scm_list_2 (ly_lily_module_constant ("line-markup"),  $1);
	}
	| markup_head_1_list simple_markup
	{
		$$ = scm_car (MAKE_SYNTAX ("composed-markup-list",
					   @2, $1, scm_list_1 ($2)));
	}
	| simple_markup	{
		$$ = $1;
	}
	;

markup_scm:
	embedded_scm_bare
	{
		if (Text_interface::is_markup ($1))
			MYBACKUP (MARKUP_IDENTIFIER, $1, @1);
		else if (Text_interface::is_markup_list ($1))
			MYBACKUP (MARKUPLIST_IDENTIFIER, $1, @1);
		else {
			parser->parser_error (@1, _ ("not a markup"));
			MYBACKUP (MARKUP_IDENTIFIER, scm_string (SCM_EOL), @1);
		}
	} BACKUP
	;
			

markup_list:
	markup_composed_list {
		$$ = $1;
	}
	| markup_uncomposed_list
	;

markup_uncomposed_list:
	markup_braced_list {
		$$ = $1;
	}
	| markup_command_list {
		$$ = scm_list_1 ($1);
	}
	| markup_scm MARKUPLIST_IDENTIFIER
	{
		$$ = $2;
	}
	| SCORELINES {
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);
	} '{' score_body '}' {
		$$ = scm_list_1 (scm_list_2 (ly_lily_module_constant ("score-lines-markup-list"), $4));
		parser->lexer_->pop_state ();
	}
	;

markup_composed_list:
	markup_head_1_list markup_uncomposed_list {
		$$ = MAKE_SYNTAX ("composed-markup-list",
				  @2, $1, $2);
	}
	;

markup_braced_list:
	'{' markup_braced_list_body '}'	{
		$$ = scm_reverse_x ($2, SCM_EOL);
	}
	;

markup_braced_list_body:
	/* empty */	{  $$ = SCM_EOL; }
	| markup_braced_list_body markup {
		$$ = scm_cons ($2, $1);
	}
	| markup_braced_list_body markup_list {
		$$ = scm_reverse_x ($2, $1);
	}
	;

markup_command_list:
	MARKUP_LIST_FUNCTION markup_command_list_arguments {
	  $$ = scm_cons ($1, scm_reverse_x($2, SCM_EOL));
	}
	;

markup_command_basic_arguments:
	EXPECT_MARKUP_LIST markup_command_list_arguments markup_list {
	  $$ = scm_cons ($3, $2);
	}
	| EXPECT_SCM markup_command_list_arguments embedded_scm_closed {
	  $$ = check_scheme_arg (parser, @3, $3, $2, $1);
	}
	| EXPECT_NO_MORE_ARGS {
	  $$ = SCM_EOL;
	}
	;

markup_command_list_arguments:
	markup_command_basic_arguments { $$ = $1; }
	| EXPECT_MARKUP markup_command_list_arguments markup {
	  $$ = scm_cons ($3, $2);
	}
	;

markup_head_1_item:
	MARKUP_FUNCTION EXPECT_MARKUP markup_command_list_arguments {
	  $$ = scm_cons ($1, scm_reverse_x ($3, SCM_EOL));
	}
	;

markup_head_1_list:
	markup_head_1_item	{
		$$ = scm_list_1 ($1);
	}
	| markup_head_1_list markup_head_1_item	{
		$$ = scm_cons ($2, $1);
	}
	;

simple_markup:
	STRING {
		$$ = make_simple_markup ($1);
	}
	| SCORE {
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (nn);
	} '{' score_body '}' {
		$$ = scm_list_2 (ly_lily_module_constant ("score-markup"), $4);
		parser->lexer_->pop_state ();
	}
	| MARKUP_FUNCTION markup_command_basic_arguments {
		$$ = scm_cons ($1, scm_reverse_x ($2, SCM_EOL));
	}
	| markup_scm MARKUP_IDENTIFIER
	{
		$$ = $2;
	}
	;

markup:
	markup_head_1_list simple_markup
	{
		$$ = scm_car (MAKE_SYNTAX ("composed-markup-list",
					   @2, $1, scm_list_1 ($2)));
	}
	| simple_markup	{
		$$ = $1;
	}
	;

%%

void
Lily_parser::set_yydebug (bool x)
{
	yydebug = x;
}

SCM
Lily_parser::do_yyparse ()
{
        SCM retval = SCM_UNDEFINED;
	yyparse (this, &retval);
        return retval;
}





/*

It is a little strange to have this function in this file, but
otherwise, we have to import music classes into the lexer.

*/
int
Lily_lexer::try_special_identifiers (SCM *destination, SCM sid)
{
	if (unsmob_book (sid)) {
		Book *book =  unsmob_book (sid)->clone ();
		*destination = book->self_scm ();
		book->unprotect ();

		return BOOK_IDENTIFIER;
	} else if (scm_is_number (sid)) {
		*destination = sid;
		return NUMBER_IDENTIFIER;
        } else if (unsmob_context_def (sid)) {
                Context_def *def= unsmob_context_def (sid)->clone ();

                *destination = def->self_scm ();
                def->unprotect ();

                return CONTEXT_DEF_IDENTIFIER;
        } else if (unsmob_context_mod (sid)) {
                *destination = unsmob_context_mod (sid)->smobbed_copy ();

                return CONTEXT_MOD_IDENTIFIER;
	} else if (Music *mus = unsmob_music (sid)) {
		mus = mus->clone ();
		*destination = mus->self_scm ();
		bool is_event = mus->is_mus_type ("post-event");
		mus->unprotect ();
		return is_event ? EVENT_IDENTIFIER : MUSIC_IDENTIFIER;
	} else if (unsmob_pitch (sid)) {
		*destination = unsmob_pitch (sid)->smobbed_copy ();
		return PITCH_IDENTIFIER;
	} else if (unsmob_duration (sid)) {
		*destination = unsmob_duration (sid)->smobbed_copy ();
		return DURATION_IDENTIFIER;
	} else if (unsmob_output_def (sid)) {
		Output_def *p = unsmob_output_def (sid);
		p = p->clone ();

		*destination = p->self_scm ();
		p->unprotect ();
		return OUTPUT_DEF_IDENTIFIER;
	}

	return -1;
}

SCM
get_next_unique_context_id ()
{
	return scm_from_locale_string ("$uniqueContextId");
}


SCM
get_next_unique_lyrics_context_id ()
{
	static int new_context_count;
	char s[128];
	snprintf (s, sizeof (s)-1, "uniqueContext%d", new_context_count++);
	return scm_from_locale_string (s);
}

// check_scheme_arg checks one argument with a given predicate for use
// in an argument list and throws a syntax error if it is unusable.
// The argument is prepended to the argument list in any case.  After
// throwing a syntax error, the argument list is terminated with #f as
// its last cdr in order to mark it as uncallable while not losing
// track of its total length.
//
// There are a few special considerations: if optional argument disp
// is given (otherwise it defaults to SCM_UNDEFINED), it will be used
// instead of arg in a prospective error message.  This is useful if
// arg is not the actual argument but rather a transformation of it.
//
// If arg itself is SCM_UNDEFINED, the predicate is considered false
// and an error message using disp is produced unconditionally.

SCM check_scheme_arg (Lily_parser *parser, Input loc,
		      SCM arg, SCM args, SCM pred, SCM disp)
{
	if (SCM_UNBNDP (arg))
		args = scm_cons (disp, args);
	else {
		args = scm_cons (arg, args);
		if (scm_is_true (scm_call_1 (pred, arg)))
			return args;
	}
	scm_set_cdr_x (scm_last_pair (args), SCM_EOL);
	MAKE_SYNTAX ("argument-error", loc, scm_length (args), pred,
		     SCM_UNBNDP (disp) ? arg : disp);
	scm_set_cdr_x (scm_last_pair (args), SCM_BOOL_F);
	return args;
}

SCM loc_on_music (Input loc, SCM arg)
{
	if (Music *m = unsmob_music (arg))
	{
		m = m->clone ();
		m->set_spot (loc);
		return m->unprotect ();
	}
	return arg;
}

SCM
try_string_variants (SCM pred, SCM str)
{
	// a matching predicate is always ok
	if (scm_is_true (scm_call_1 (pred, str)))
		return str;
	// a symbol may be interpreted as a list of symbols if it helps
	if (scm_is_symbol (str)) {
		str = scm_list_1 (str);
		if (scm_is_true (scm_call_1 (pred, str)))
			return str;
		return SCM_UNDEFINED;
	}

	// If this cannot be a string representation of a symbol list,
	// we are through.

	if (!is_regular_identifier (str, true))
		return SCM_UNDEFINED;

	str = scm_string_split (str, SCM_MAKE_CHAR ('.'));
	for (SCM p = str; scm_is_pair (p); p = scm_cdr (p))
		scm_set_car_x (p, scm_string_to_symbol (scm_car (p)));

	// Let's attempt the symbol list interpretation first.

	if (scm_is_true (scm_call_1 (pred, str)))
		return str;

	// If there is just one symbol in the list, we might interpret
	// it as a single symbol

	if (scm_is_null (scm_cdr (str)))
	{
		str = scm_car (str);
		if (scm_is_true (scm_call_1 (pred, str)))
			return str;
	}

	return SCM_UNDEFINED;
}

bool
is_regular_identifier (SCM id, bool multiple)
{
  if (!scm_is_string (id))
	  return false;

  string str = ly_scm2string (id);

  bool middle = false;

  for (string::iterator it=str.begin(); it != str.end (); it++)
  {
	  int c = *it & 0xff;
	  if ((c >= 'a' && c <= 'z')
	      || (c >= 'A' && c <= 'Z')
	      || c > 0x7f)
		  middle = true;
	  else if (middle && (c == '-' || c == '_' || (multiple && c == '.')))
		  middle = false;
	  else
		  return false;
  }
  return middle;
}

SCM
make_music_from_simple (Lily_parser *parser, Input loc, SCM simple)
{
	if (unsmob_music (simple))
		return simple;
	if (parser->lexer_->is_note_state ()) {
		if (scm_is_symbol (simple)) {
			Music *n = MY_MAKE_MUSIC ("NoteEvent", loc);
			n->set_property ("duration", parser->default_duration_.smobbed_copy ());
			n->set_property ("drum-type", simple);
			return n->unprotect ();
		}
		if (unsmob_pitch (simple)) {
			Music *n = MY_MAKE_MUSIC ("NoteEvent", loc);
			n->set_property ("duration", parser->default_duration_.smobbed_copy ());
			n->set_property ("pitch", simple);
			return n->unprotect ();
		}
		return simple;
	} else if (parser->lexer_->is_lyric_state ()) {
		if (Text_interface::is_markup (simple))
			return MAKE_SYNTAX ("lyric-event", loc, simple,
					    parser->default_duration_.smobbed_copy ());
	} else if (parser->lexer_->is_chord_state ()) {
		if (unsmob_pitch (simple))
			return MAKE_SYNTAX
				("event-chord",
				 loc,
				 make_chord_elements (loc, simple,
						      parser->default_duration_.smobbed_copy (),
						      SCM_EOL));
	}
	return simple;
}

Music *
make_music_with_input (SCM name, Input where)
{
       Music *m = make_music_by_name (name);
       m->set_spot (where);
       return m;
}

SCM
make_simple_markup (SCM a)
{
	return a;
}

SCM
make_duration (SCM d, int dots)
{
	int t = scm_to_int (d);
	if (t > 0 && (t & (t-1)) == 0)
		return Duration (intlog2 (t), dots).smobbed_copy ();
	else
		return SCM_UNDEFINED;
}

SCM
make_chord_step (SCM step_scm, Rational alter)
{
        int step = scm_to_int (step_scm);

	if (step == 7)
		alter += FLAT_ALTERATION;

	while (step < 0)
		step += 7;
	Pitch m ((step -1) / 7, (step - 1) % 7, alter);
	return m.smobbed_copy ();
}


SCM
make_chord_elements (Input loc, SCM pitch, SCM dur, SCM modification_list)
{
	SCM chord_ctor = ly_lily_module_constant ("construct-chord-elements");
	SCM res = scm_call_3 (chord_ctor, pitch, dur, modification_list);
	for (SCM s = res; scm_is_pair (s); s = scm_cdr (s))
	{
		unsmob_music (scm_car (s))->set_spot (loc);
	}
	return res;
}

int
yylex (YYSTYPE *s, YYLTYPE *loc, Lily_parser *parser)
{
	Lily_lexer *lex = parser->lexer_;

	lex->lexval_ = s;
	lex->lexloc_ = loc;
	lex->prepare_for_next_token ();
	return lex->yylex ();
}
