/* -*- mode: c++; c-file-style: "linux"; indent-tabs-mode: t -*- */
/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#define YYPRINT(file, type, value)					\
	do {								\
		if (scm_is_eq (value, SCM_UNSPECIFIED))			\
			break;						\
		SCM s = Display::value_to_lily_string (value);		\
		char *p = scm_to_locale_string (s);			\
		fputs (p, file);					\
		free (p);						\
	} while (0)


#define lookup_identifier(x) lookup_identifier_symbol (ly_symbol2scm (x))

%}

%parse-param {Lily_parser *parser}
%parse-param {SCM *retval}
%lex-param {Lily_parser *parser}
%define parse.error verbose
%debug

/* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesn't trigger a move
of the parse stack onto the heap. */

%left PREC_BOT
%nonassoc REPEAT
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
	HYPHEN EXTENDER DURATION_IDENTIFIER '!' '\'' ','

 /* The above are needed for collecting tremoli and other items (that
    could otherwise be interpreted as belonging to the next function
    argument) greedily, and together with the next rule will serve to
    join numbers and units greedily instead of allowing them into
    separate function arguments
 */

%nonassoc NUMBER_IDENTIFIER

%left PREC_TOP

%define api.pure full
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

#include "book.hh"
#include "context.hh"
#include "context-def.hh"
#include "context-mod.hh"
#include "dimensions.hh"
#include "file-path.hh"
#include "input.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "ly-module.hh"
#include "ly-scm-list.hh"
#include "misc.hh"
#include "music.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "std-vector.hh"
#include "text-interface.hh"
#include "warn.hh"
#include "lily-imports.hh"

void
Lily_parser::parser_error (Input const *i, Lily_parser *parser, SCM *, const std::string &s)
{
	parser->parser_error (*i, s);
}

// The following are somewhat precarious constructs as they may change
// the value of the lookahead token.  That implies that the lookahead
// token must not yet have made an impact on the state stack other
// than causing the reduction of the current rule, or switching the
// lookahead token while Bison is mulling it over will cause trouble.

#define MYBACKUP(Token, Value, Location)				\
	do {								\
		if (yychar != YYEMPTY)					\
			parser->lexer_->push_extra_token		\
				(yylloc, yychar, yylval);		\
		parser->lexer_->push_extra_token			\
			(Location, Token, Value);			\
		parser->lexer_->push_extra_token (Location, BACKUP);	\
		yychar = YYEMPTY;					\
	} while (0)


#define MYREPARSE(Location, Pred, Token, Value)				\
	do {								\
		if (yychar != YYEMPTY)					\
			parser->lexer_->push_extra_token		\
				(yylloc, yychar, yylval);		\
		parser->lexer_->push_extra_token			\
			(Location, Token, Value);			\
		parser->lexer_->push_extra_token			\
			(Location, REPARSE, Pred);			\
		yychar = YYEMPTY;					\
	} while (0)

%}


%{

#define MY_MAKE_MUSIC(x, spot) \
	make_music_with_input (ly_symbol2scm (x), \
			       parser->lexer_->override_input (spot))

/* ES TODO:
- delay application of the function
*/

#define LOWLEVEL_MAKE_SYNTAX(location, proc, ...)			\
	with_location							\
		(parser->lexer_->override_input (location).smobbed_copy (), \
		 proc,							\
		 ##__VA_ARGS__)

/* Syntactic Sugar. */
#define MAKE_SYNTAX(name, location, ...)				\
	LOWLEVEL_MAKE_SYNTAX (location, Syntax::name, ##__VA_ARGS__)

#define START_MAKE_SYNTAX(name, ...)					\
	ly_list (Syntax::name, ##__VA_ARGS__)

#define FINISH_MAKE_SYNTAX(start, location, ...)			\
	LOWLEVEL_MAKE_SYNTAX						\
		(location,						\
		 Guile_user::apply,					\
		 scm_car (start),					\
		 scm_append_x						\
		 (ly_list (scm_cdr (start),				\
		           ly_list (__VA_ARGS__))))

#undef _
#if !HAVE_GETTEXT
#define _(x) x
#else
#include <libintl.h>
#define _(x) gettext (x)
#endif

using std::string;

static Music *make_music_with_input (SCM name, Input where);
bool add_post_events (Music *music, SCM events);
SCM reverse_music_list (Lily_parser *parser, Input loc, SCM lst, bool preserve, bool compress);
SCM check_scheme_arg (Lily_parser *parser, Input loc,
		      SCM arg, SCM args, SCM pred, SCM disp = SCM_UNDEFINED);
SCM make_music_from_simple (Lily_parser *parser, Input loc, SCM pitch);
SCM loc_on_copy (Lily_parser *parser, Input loc, SCM arg);
SCM make_chord_elements (Input loc, SCM pitch, SCM dur, SCM modification_list);
SCM make_chord_step (SCM step, Rational alter);
SCM make_simple_markup (SCM a);
SCM make_duration (SCM t, int dots = 0, SCM factor = SCM_UNDEFINED);
bool is_regular_identifier (SCM id, bool multiple=false);
SCM make_reverse_key_list (SCM keys);
SCM try_word_variants (SCM pred, SCM str);
SCM try_string_variants (SCM pred, SCM str);
SCM post_event_cons (SCM ev, SCM tail);
void property_path_dot_warning (Input loc, SCM lst);
int yylex (YYSTYPE *s, YYLTYPE *loc, Lily_parser *parser);


// generated code may trigger conversion warnings
#pragma GCC diagnostic ignored "-Wconversion"

// generated code triggers a false positive in GCC 11
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98753
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"

// generated code contains some old-style casts
#pragma GCC diagnostic ignored "-Wold-style-cast"

// generated code contains some useless casts
#pragma GCC diagnostic ignored "-Wuseless-cast"

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
%token ETC "\\etc"
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
%token E_BACKSLASH "\\\\"
%token E_EXCLAMATION "\\!"
%token E_PLUS "\\+"
%token EXTENDER "__"

/*
If we give names, Bison complains.
*/
%token FIGURE_CLOSE /* "\\>" */
%token FIGURE_OPEN /* "\\<" */
%token FIGURE_SPACE "_"
%token FIGURE_ALTERATION_EXPR
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
%token CHORD_MODIFIER
%token CHORD_REPETITION
%token DRUM_PITCH
 /* Artificial token for durations in argument lists */
%token DURATION_ARG
%token DURATION_IDENTIFIER
%token EVENT_IDENTIFIER
%token EVENT_FUNCTION
%token FRACTION
%token LOOKUP_IDENTIFIER
%token LYRIC_ELEMENT
%token MARKUP_FUNCTION
%token MARKUP_LIST_FUNCTION
%token MARKUP_IDENTIFIER
%token MARKUPLIST_IDENTIFIER
%token MUSIC_FUNCTION
%token MUSIC_IDENTIFIER
%token NOTENAME_PITCH
%token NUMBER_IDENTIFIER
%token PITCH_IDENTIFIER
%token REAL
%token RESTNAME
%token SCM_ARG
%token SCM_FUNCTION
%token SCM_IDENTIFIER
%token SCM_TOKEN
%token STRING
%token SYMBOL_LIST
%token TONICNAME_PITCH
%token SYMBOL

%left '-' '+'

/* We don't assign precedence to / and *, because we might need varied
prec levels in different prods */

%left UNARY_MINUS

%%

start_symbol:
	lilypond
	| EMBEDDED_LILY {
		parser->lexer_->push_note_state ();
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
	header_block {
		parser->lexer_->set_identifier (ly_symbol2scm ("$defaultheader"), $1);
	}
	| book_block {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-book-handler");
		ly_call (proc, $1);
	}
	| bookpart_block {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-bookpart-handler");
		ly_call (proc, $1);
	}
	| BOOK_IDENTIFIER {
		SCM sym = unsmob<Book>($1)->paper_
			? ly_symbol2scm ("toplevel-book-handler")
			: ly_symbol2scm ("toplevel-bookpart-handler");

		SCM proc = parser->lexer_->lookup_identifier_symbol (sym);
		ly_call (proc, $1);
	}
	| score_block {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-score-handler");
		ly_call (proc, $1);
	}
	| composite_music {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-music-handler");
		ly_call (proc, $1);
	}
	| full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
		ly_call (proc, ly_list ($1));
	}
	| full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
		ly_call (proc, $1);
	}
	| SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($1, @1);
	}
	| embedded_scm_active
	{
		SCM out = SCM_UNDEFINED;
		if (Text_interface::is_markup ($1))
			out = ly_list ($1);
		else if (Text_interface::is_markup_list ($1))
			out = $1;
		if (scm_is_pair (out))
		{
			SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
			ly_call (proc, out);
		} else if (unsmob<Score> ($1))
		{
			SCM proc = parser->lexer_->lookup_identifier ("toplevel-score-handler");
			ly_call (proc, $1);
		} else if (Output_def * od = unsmob<Output_def> ($1)) {
			SCM id = SCM_EOL;
			SCM kind = od->c_variable ("output-def-kind");
			if (scm_is_eq (kind, ly_symbol2scm ("paper")))
				id = ly_symbol2scm ("$defaultpaper");
			else if (scm_is_eq (kind, ly_symbol2scm ("midi")))
				id = ly_symbol2scm ("$defaultmidi");
			else if (scm_is_eq (kind, ly_symbol2scm ("layout")))
				id = ly_symbol2scm ("$defaultlayout");

			parser->lexer_->set_identifier (id, $1);
		} else if (ly_is_module ($1))
		{
			SCM module = get_header (parser);
			ly_module_copy (module, $1);
			parser->lexer_->set_identifier
				(ly_symbol2scm ("$defaultheader"), module);
		} else if (!scm_is_eq ($1, SCM_UNSPECIFIED))
			parser->parser_error (@1, _("bad expression type"));
	}
	| output_def {
		SCM id = SCM_EOL;
		Output_def * od = unsmob<Output_def> ($1);
		SCM kind = od->c_variable ("output-def-kind");
		if (scm_is_eq (kind, ly_symbol2scm ("paper")))
			id = ly_symbol2scm ("$defaultpaper");
		else if (scm_is_eq (kind, ly_symbol2scm ("midi")))
			id = ly_symbol2scm ("$defaultmidi");
		else if (scm_is_eq (kind, ly_symbol2scm ("layout")))
			id = ly_symbol2scm ("$defaultlayout");

		parser->lexer_->set_identifier (id, $1);
	}
	;

lookup:
	LOOKUP_IDENTIFIER
	| LOOKUP_IDENTIFIER '.' symbol_list_rev
	{
		$$ = loc_on_copy (parser, @$,
				  nested_property ($1, scm_reverse_x ($3, SCM_EOL)));
	}
	;

embedded_scm_bare:
	SCM_TOKEN
	{
		$$ = parser->lexer_->eval_scm_token ($1, @1);
	}
	| SCM_IDENTIFIER
	;

embedded_scm_active:
	SCM_IDENTIFIER
	| scm_function_call
	| lookup
	;

embedded_scm_bare_arg:
	SCM_ARG
	| SCM_TOKEN
	{
		$$ = parser->lexer_->eval_scm_token ($1, @1);
	}
	| FRACTION
	| partial_markup
	| full_markup_list
	| context_modification
	| header_block
	| score_block
	| context_def_spec_block
	| book_block
	| bookpart_block
	| output_def
	| lookup
	;

/* The generic version may end in music, or not */

embedded_scm:
	embedded_scm_bare
	| scm_function_call
	| lookup
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
		$$ = MAKE_SYNTAX (music_function, @$,
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
		$$ = MAKE_SYNTAX (void_music, @$);
	}
	| identifier_init_nonumber
	| embedded_lilypond_number
	| post_event
	{
		if (!unsmob<Music> ($1))
			$$ = MY_MAKE_MUSIC ("PostEvents", @$)->unprotect ();
	}
	| duration post_events %prec ':'
	{
		if (scm_is_pair ($2)) {
			Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);

			parser->default_duration_ = *unsmob<Duration> ($1);
			set_property (n, "duration", $1);
			set_property (n, "articulations",
					 scm_reverse_x ($2, SCM_EOL));
			$$ = n->unprotect ();
		}
	}
	| music_embedded music_embedded music_list {
		SCM tail = SCM_EOL;
		if (unsmob<Music> ($1))
			tail = scm_cons ($1, tail);
		if (unsmob<Music> ($2))
			tail = scm_cons ($2, tail);
		$$ = reverse_music_list (parser, @$,
					 scm_append_x (ly_list ($3, tail)),
					 true, true);
		if (scm_is_pair ($$)) // unpackaged list
			if (scm_is_null (scm_cdr ($$)))
				$$ = scm_car ($$); // single expression
			else
				$$ = MAKE_SYNTAX (sequential_music, @$, $$);
		else if (scm_is_null ($$))
			$$ = MAKE_SYNTAX (void_music, @$);
		// else already packaged post-event
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
	| lilypond_header_body SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($2, @2);
	}
	| lilypond_header_body embedded_scm_active {
		if (ly_is_module ($2))
			ly_module_copy (scm_current_module (), $2);
		else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("bad expression type"));
	}
	;

lilypond_header:
	HEADER '{' lilypond_header_body '}'	{
		$$ = parser->lexer_->remove_scope ();
	}
	;

header_block:
	{
		parser->lexer_->add_scope (get_header (parser));
	} lilypond_header {
		$$ = $2;
	}
	;

/*
	DECLARATIONS
*/
assignment_id:
	STRING
	{
		$$ = scm_string_to_symbol ($1);
	}
	| SYMBOL
	{
		$$ = scm_string_to_symbol ($1);
	}
	;

assignment:
	assignment_id '=' identifier_init  {
	        parser->lexer_->set_identifier ($1, $3);
                $$ = SCM_UNSPECIFIED;
	}
	| assignment_id '.' property_path '=' identifier_init {
		SCM path = scm_cons ($1, $3);
		parser->lexer_->set_identifier (path, $5);
                $$ = SCM_UNSPECIFIED;
	}
	| markup_mode_word '=' identifier_init
	{
		if (scm_is_false (Lily::markup_function_p ($3)))
		{
			parser->parser_error (@3, _ ("Not a markup function"));
		} else {
			Lily::define_markup_command_internal
 				(scm_string_to_symbol ($1), $3, SCM_BOOL_F);
		}
		$$ = SCM_UNSPECIFIED;
	}
	;


identifier_init:
	identifier_init_nonumber
	| number_expression
	| symbol_list_part_bare '.' property_path
	{
		$$ = scm_reverse_x ($1, $3);
	}
	| symbol_list_part_bare ',' property_path
	{
		$$ = scm_reverse_x ($1, $3);
	}
	| post_event_nofinger post_events
	{
		$$ = post_event_cons ($1, scm_reverse_x ($2, SCM_EOL));
		if (scm_is_pair ($$)
		    && scm_is_null (scm_cdr ($$)))
			$$ = scm_car ($$);
		else
		{
			Music * m = MY_MAKE_MUSIC ("PostEvents", @$);
			set_property (m, "elements", $$);
			$$ = m->unprotect ();
		}
	}
	;

identifier_init_nonumber:
	header_block
	| score_block
	| book_block
	| bookpart_block
	| output_def
	| context_def_spec_block
	| music_assign
	| pitch_or_music
	| FRACTION
	| string
	| embedded_scm
	| partial_markup
	| full_markup_list
        | context_modification
	| partial_function ETC
	{
		$$ = MAKE_SYNTAX (partial_music_function, @$,
				  scm_reverse_x ($1, SCM_EOL));
	}
	;

// Partial functions

partial_function_scriptable:
	MUSIC_FUNCTION function_arglist_partial
	{
		$$ = scm_acons ($1, $2, SCM_EOL);
	}
	| EVENT_FUNCTION function_arglist_partial
	{
		$$ = scm_acons ($1, $2, SCM_EOL);
	}
	| SCM_FUNCTION function_arglist_partial
	{
		$$ = scm_acons ($1, $2, SCM_EOL);
	}
	| MUSIC_FUNCTION EXPECT_SCM function_arglist_optional partial_function
	{
		$$ = scm_acons ($1, $3, $4);
	}
	| EVENT_FUNCTION EXPECT_SCM function_arglist_optional partial_function
	{
		$$ = scm_acons ($1, $3, $4);
	}
	| SCM_FUNCTION EXPECT_SCM function_arglist_optional partial_function
	{
		$$ = scm_acons ($1, $3, $4);
	}
	| MUSIC_FUNCTION EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup partial_function
	{
		$$ = scm_acons ($1, $4, $5);
	}
	| EVENT_FUNCTION EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup partial_function
	{
		$$ = scm_acons ($1, $4, $5);
	}
	| SCM_FUNCTION EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup partial_function
	{
		$$ = scm_acons ($1, $4, $5);
	}
	;

partial_function:
	partial_function_scriptable
	| OVERRIDE grob_prop_path '='
	{
		if (SCM_UNBNDP ($2))
			$$ = ly_list (SCM_BOOL_F);
		else
			$$ = scm_cons
				(ly_list (Syntax::property_override,
			                  scm_cdr ($2), scm_car ($2)),
				 SCM_EOL);
	}
	| SET context_prop_spec '='
	{
		if (SCM_UNBNDP ($2))
			$$ = ly_list (SCM_BOOL_F);
		else
			$$ = scm_cons
				(ly_list (Syntax::property_set,
				          scm_cadr ($2), scm_car ($2)),
				 SCM_EOL);
	}
	| OVERRIDE grob_prop_path '=' partial_function
	{
		if (SCM_UNBNDP ($2))
			$$ = ly_list (SCM_BOOL_F);
		else
			$$ = scm_cons
				(ly_list (Syntax::property_override,
			       	          scm_cdr ($2), scm_car ($2)),
				 $4);
	}
	| SET context_prop_spec '=' partial_function
	{
		if (SCM_UNBNDP ($2))
			$$ = ly_list (SCM_BOOL_F);
		else
			$$ = scm_cons
				(ly_list (Syntax::property_set,
					  scm_cadr ($2), scm_car ($2)),
				 $4);
	}
	| REPEAT simple_string unsigned_number
	{
		$$ = scm_cons (ly_list (Syntax::repeat, $3, $2), SCM_EOL);
	}
	| REPEAT simple_string unsigned_number partial_function
	{
		$$ = scm_cons (ly_list (Syntax::repeat, $3, $2), $4);
	}
	| REPEAT simple_string
	{
		$$ = scm_cons (ly_list (Syntax::repeat, $2), SCM_EOL);
	}
	| REPEAT simple_string partial_function
	{
		$$ = scm_cons (ly_list (Syntax::repeat, $2), $3);
	}
// Stupid duplication because we already expect ETC here.  It will follow anyway.
	| script_dir markup_mode markup_partial_function
	{
		if (SCM_UNBNDP ($1))
			$1 = SCM_INUM0;
		$3 = MAKE_SYNTAX (partial_markup, @3, $3);
		parser->lexer_->pop_state ();
// This relies on partial_function always being followed by ETC
		$$ = ly_list (ly_list (MAKE_SYNTAX (partial_text_script, @$, $3),
				       $3, $1));
	}
	| script_dir partial_function_scriptable
	{
		if (SCM_UNBNDP ($1))
			$1 = SCM_INUM0;
		$$ = scm_acons (Syntax::create_script_function, ly_list ($1), $2);
	}
	| script_dir
	{
		if (SCM_UNBNDP ($1))
			$1 = SCM_INUM0;
		$$ = scm_acons (Syntax::create_script_function, ly_list ($1), SCM_EOL);
	}
	;

context_def_spec_block:
	CONTEXT '{' context_def_spec_body '}'
	{
		$$ = $3;
		Context_def *td = unsmob<Context_def> ($$);
		if (!td) {
			$$ = Context_def::make_scm ();
			td = unsmob<Context_def> ($$);
		}
		td->origin ()->set_spot (@$);
	}
	;

context_mod_arg:
	embedded_scm
	|
	{
		parser->lexer_->push_note_state ();
	}
	composite_music
	{
		parser->lexer_->pop_state ();
		$$ = $2;
	}
	;


context_def_spec_body:
	/**/ {
		$$ = SCM_UNSPECIFIED;
	}
	| context_def_spec_body context_mod {
		if (!SCM_UNBNDP ($2)) {
			Context_def *td = unsmob<Context_def> ($$);
			if (!td) {
				$$ = Context_def::make_scm ();
				td = unsmob<Context_def> ($$);
			}
			unsmob<Context_def> ($$)->add_context_mod ($2);
		}
	}
	| context_def_spec_body context_modification {
                Context_def *td = unsmob<Context_def> ($$);
		if (!td) {
			$$ = Context_def::make_scm ();
			td = unsmob<Context_def> ($$);
		}
                SCM new_mods = unsmob<Context_mod> ($2)->get_mods ();
                for (SCM m = new_mods; scm_is_pair (m); m = scm_cdr (m)) {
                    td->add_context_mod (scm_car (m));
                }
	}
	| context_def_spec_body context_mod_arg {
		Context_def *td = unsmob<Context_def> ($1);
		if (scm_is_eq ($2, SCM_UNSPECIFIED))
			;
		else if (!td && unsmob<Context_def> ($2))
			$$ = $2;
		else {
			if (!td) {
				$$ = Context_def::make_scm ();
				td = unsmob<Context_def> ($$);
			}
			if (unsmob<Music> ($2)) {
				SCM proc = parser->lexer_->lookup_identifier ("context-mod-music-handler");
				$2 = ly_call (proc, $2);
			}
			if (Context_mod *cm = unsmob<Context_mod> ($2)) {
				for (SCM m = cm->get_mods (); scm_is_pair (m); m = scm_cdr (m)) {
					td->add_context_mod (scm_car (m));
				}
			} else
				parser->parser_error (@2, _ ("not a context mod"));
		}
	}
	;



book_block:
	BOOK '{' book_body '}' 	{
		$$ = $3;
		unsmob<Book> ($$)->origin ()->set_spot (@$);
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
		book->paper_ = unsmob<Output_def> (parser->lexer_->lookup_identifier ("$defaultpaper"))->clone ();
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
		unsmob<Book> ($1)->paper_ = unsmob<Output_def> ($2);
		set_paper (parser, unsmob<Output_def> ($2));
	}
	| book_body bookpart_block {
		SCM proc = parser->lexer_->lookup_identifier ("book-bookpart-handler");
		ly_call (proc, $1, $2);
	}
	| book_body score_block {
		SCM proc = parser->lexer_->lookup_identifier ("book-score-handler");
		ly_call (proc, $1, $2);
	}
	| book_body composite_music {
		SCM proc = parser->lexer_->lookup_identifier ("book-music-handler");
		ly_call (proc, $1, $2);
	}
	| book_body full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
		ly_call (proc, $1, ly_list ($2));
	}
	| book_body full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
		ly_call (proc, $1, $2);
	}
	| book_body SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($2, @2);
	}
	| book_body embedded_scm_active
	{
		SCM out = SCM_UNDEFINED;
		if (Text_interface::is_markup ($2))
			out = ly_list ($2);
		else if (Text_interface::is_markup_list ($2))
			out = $2;
		if (scm_is_pair (out))
		{
			SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
			ly_call (proc, $1, out);
		} else if (unsmob<Score> ($2))
		{
			SCM proc = parser->lexer_->lookup_identifier ("book-score-handler");
			ly_call (proc, $1, $2);
		} else if (Output_def *od = unsmob<Output_def> ($2)) {
			if (scm_is_eq (od->lookup_variable (ly_symbol2scm ("output-def-kind")),
				       ly_symbol2scm ("paper"))) {
				unsmob<Book> ($1)->paper_ = od;
				set_paper (parser, od);
			} else {
				parser->parser_error (@2, _ ("need \\paper for paper block"));
			}
		} else if (ly_is_module ($2))
		{
			ly_module_copy (unsmob<Book> ($1)->header_, $2);
		} else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("bad expression type"));
	}
	| book_body
	{
		parser->lexer_->add_scope (unsmob<Book> ($1)->header_);
	} lilypond_header
	| book_body error {
                Book *book = unsmob<Book> ($1);
		book->paper_ = 0;
		book->scores_ = SCM_EOL;
		book->bookparts_ = SCM_EOL;
	}
	;

bookpart_block:
	BOOKPART '{' bookpart_body '}' {
		$$ = $3;
		unsmob<Book> ($$)->origin ()->set_spot (@$);
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
		unsmob<Book> ($$)->paper_ = unsmob<Output_def> ($2);
	}
	| bookpart_body score_block {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-score-handler");
		ly_call (proc, $1, $2);
	}
	| bookpart_body composite_music {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-music-handler");
		ly_call (proc, $1, $2);
	}
	| bookpart_body full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
		ly_call (proc, $1, ly_list ($2));
	}
	| bookpart_body full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
		ly_call (proc, $1, $2);
	}
	| bookpart_body SCM_TOKEN {
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($2, @2);
	}
	| bookpart_body embedded_scm_active
	{
		SCM out = SCM_UNDEFINED;
		if (Text_interface::is_markup ($2))
			out = ly_list ($2);
		else if (Text_interface::is_markup_list ($2))
			out = $2;
		if (scm_is_pair (out))
		{
			SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
			ly_call (proc, $1, out);
		} else if (unsmob<Score> ($2))
		{
			SCM proc = parser->lexer_->lookup_identifier ("bookpart-score-handler");
			ly_call (proc, $1, $2);
		} else if (Output_def *od = unsmob<Output_def> ($2)) {
			if (scm_is_eq (od->lookup_variable (ly_symbol2scm ("output-def-kind")),
				       ly_symbol2scm ("paper"))) {
				unsmob<Book> ($1)->paper_ = od;
			} else {
				parser->parser_error (@2, _ ("need \\paper for paper block"));
			}
		} else if (ly_is_module ($2)) {
			Book *book = unsmob<Book> ($1);
			if (!ly_is_module (book->header_))
				book->header_ = ly_make_module ();
			ly_module_copy (book->header_, $2);
		} else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("bad expression type"));
	}
	| bookpart_body
	{
                Book *book = unsmob<Book> ($1);
		if (!ly_is_module (book->header_))
			book->header_ = ly_make_module ();
		parser->lexer_->add_scope (book->header_);
	} lilypond_header
	| bookpart_body error {
                Book *book = unsmob<Book> ($1);
		book->paper_ = 0;
		book->scores_ = SCM_EOL;
	}
	;

score_block:
	SCORE '{' score_body '}' 	{
		unsmob<Score> ($3)->origin ()->set_spot (@$);
		$$ = $3;
	}
	;

score_body:
	score_items {
		if (!unsmob<Score> ($1)) {
			parser->parser_error (@1, _("Missing music in \\score"));
			$$ = (new Score)->unprotect ();
			if (scm_is_pair ($1) && ly_is_module (scm_car ($1)))
			{
				unsmob<Score> ($$)->set_header (scm_car ($1));
				$1 = scm_cdr ($1);
			}
			for (SCM p = scm_reverse_x ($1, SCM_EOL);
			     scm_is_pair (p); p = scm_cdr (p))
			{
				unsmob<Score> ($$)->
					add_output_def (unsmob<Output_def> (scm_car (p)));
			}
		}
	}
	| score_body error {
		unsmob<Score> ($$)->error_found_ = true;
	}
	;

score_item:
	embedded_scm
	| music
	| output_def
	;

score_items:
	/* empty */
	{
		$$ = SCM_EOL;
	}
	| score_items score_item
	{
		Output_def *od = unsmob<Output_def> ($2);
		if (od) {
			if (scm_is_eq (od->lookup_variable (ly_symbol2scm ("output-def-kind")),
				       ly_symbol2scm ("paper")))
			{
				parser->parser_error (@2, _("\\paper cannot be used in \\score, use \\layout instead"));
				od = 0;
				$2 = SCM_UNSPECIFIED;
			}
		} else if (!unsmob<Score> ($$)) {
			if (unsmob<Music> ($2)) {
				$2 = Lily::scorify_music ($2);
			}
			if (unsmob<Score> ($2))
			{
				$$ = $2;
				$2 = SCM_UNSPECIFIED;
			}
		}
		Score *score = unsmob<Score> ($$);
		if (score && scm_is_pair ($1)) {
			if (ly_is_module (scm_car ($1)))
			{
				score->set_header (scm_car ($1));
				$1 = scm_cdr ($1);
			}
			for (SCM p = scm_reverse_x ($1, SCM_EOL);
			     scm_is_pair (p); p = scm_cdr (p))
			{
				score->add_output_def (unsmob<Output_def> (scm_car (p)));
			}
		}
		if (od) {
			if (score)
				score->add_output_def (od);
			else if (scm_is_pair ($$) && ly_is_module (scm_car ($$)))
				scm_set_cdr_x ($$, scm_cons ($2, scm_cdr ($$)));
			else
				$$ = scm_cons ($2, $$);
		} else if (ly_is_module ($2)) {
			SCM module = SCM_UNSPECIFIED;
			if (score) {
				module = score->get_header ();
				if (!ly_is_module (module))
				{
					module = ly_make_module ();
					score->set_header (module);
				}
			} else if (scm_is_pair ($$) && ly_is_module (scm_car ($$)))
				module = scm_car ($$);
			else {
				module = ly_make_module ();
				$$ = scm_cons (module, $$);
			}
			ly_module_copy (module, $2);
		} else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("Spurious expression in \\score"));
	}
	| score_items
	{
		if (Score *score = unsmob<Score> ($1)) {
			if (!ly_is_module (score->get_header ()))
				score->set_header (ly_make_module ());
			parser->lexer_->add_scope (score->get_header ());
		} else {
			if (!scm_is_pair ($1) || !ly_is_module (scm_car ($1)))
				$1 = scm_cons (ly_make_module (), $1);
			parser->lexer_->add_scope (scm_car ($1));
		}
	} lilypond_header
	{
		$$ = $1;
	}
	;


/*
	OUTPUT DEF
*/

paper_block:
	output_def {
                Output_def *od = unsmob<Output_def> ($1);

		if (!scm_is_eq (od->lookup_variable (ly_symbol2scm ("output-def-kind")),
				ly_symbol2scm ("paper")))
		{
			parser->parser_error (@1, _ ("need \\paper for paper block"));
			$$ = get_paper (parser)->unprotect ();
		}
	}
	;


output_def:
	output_def_body '}' {
		if (scm_is_pair ($1))
			$$ = scm_car ($1);

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
	music_assign
	| context_def_spec_block
	;

output_def_body:
	output_def_head_with_mode_switch '{' {
		unsmob<Output_def> ($1)->input_origin_.set_spot (@$);
		// This is a stupid trick to mark the beginning of the
		// body for deciding whether to allow
		// embedded_scm_active to have an output definition
		$$ = ly_list ($1);
	}
	| output_def_body assignment  {
		if (scm_is_pair ($1))
			$$ = scm_car ($1);
	}
	| output_def_body embedded_scm_active
	{
		// We don't switch into note mode for Scheme functions
		// here.  Does not seem warranted/required in output
		// definitions.
		if (scm_is_pair ($1))
		{
			Output_def *o = unsmob<Output_def> ($2);
			if (o) {
				o->input_origin_.set_spot (@$);
				$1 = o->self_scm ();
				parser->lexer_->remove_scope ();
				parser->lexer_->add_scope (o->scope_);
				$2 = SCM_UNSPECIFIED;
			} else
				$1 = scm_car ($1);
		}
		if (unsmob<Context_def> ($2))
			assign_context_def (unsmob<Output_def> ($1), $2);
		// Seems unlikely, but let's be complete:
		else if (unsmob<Music> ($2))
		{
			SCM proc = parser->lexer_->lookup_identifier ("output-def-music-handler");
			ly_call (proc, $1, $2);
		} else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _("bad expression type"));
		$$ = $1;
	}
	| output_def_body SCM_TOKEN {
		if (scm_is_pair ($1))
			$$ = scm_car ($1);
		// Evaluate and ignore #xxx, as opposed to \xxx
		parser->lexer_->eval_scm_token ($2, @2);
	}
	| output_def_body
	{
		if (scm_is_pair ($1))
			$1 = scm_car ($1);
		parser->lexer_->push_note_state ();
	} music_or_context_def
	{
		parser->lexer_->pop_state ();
		if (unsmob<Context_def> ($3))
			assign_context_def (unsmob<Output_def> ($1), $3);
		else {

			SCM proc = parser->lexer_->lookup_identifier ("output-def-music-handler");
			ly_call (proc, $1, $3);
		}
		$$ = $1;
	}
	| output_def_body error {

	}
	;

tempo_event:
	TEMPO steno_duration '=' tempo_range	{
		$$ = MAKE_SYNTAX (tempo, @$, SCM_EOL, $2, $4);
	}
	| TEMPO text steno_duration '=' tempo_range	{
		$$ = MAKE_SYNTAX (tempo, @$, $2, $3, $5);
	}
	| TEMPO text {
		$$ = MAKE_SYNTAX (tempo, @$, $2);
	} %prec ':'
	;

/*
The representation of a  list is reversed to have efficient append.  */

music_list:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| music_list music_embedded {
		if (unsmob<Music> ($2))
			$$ = scm_cons ($2, $1);
	}
	| music_list error {
		Music *m = MY_MAKE_MUSIC("Music", @$);
		// ugh. code dup
		set_property (m, "error-found", SCM_BOOL_T);
		$$ = scm_cons (m->self_scm (), $1);
		m->unprotect (); /* UGH */
	}
	;

braced_music_list:
	'{' music_list '}'
	{
		$$ = reverse_music_list (parser, @$, $2, true, false);
	}
	;

music:	music_assign
	| lyric_element_music
	| pitch_as_music
	;

pitch_as_music:
	pitch_or_music
	{
	        $$ = make_music_from_simple (parser, @1, $1);
                if (!unsmob<Music> ($$))
		{
                        parser->parser_error (@1, _ ("music expected"));
			$$ = MAKE_SYNTAX (void_music, @$);
		}
	}
	;

music_embedded:
	music
	| post_event
	| music_embedded_backup
	{
		$$ = $1;
	}
	| music_embedded_backup BACKUP lyric_element_music
	{
		$$ = $3;
	}
	| duration post_events %prec ':'
	{
		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);

		parser->default_duration_ = *unsmob<Duration> ($1);
		set_property (n, "duration", $1);

		if (scm_is_pair ($2))
			set_property (n, "articulations",
					 scm_reverse_x ($2, SCM_EOL));
		$$ = n->unprotect ();
	}
	;

music_embedded_backup:
	embedded_scm
	{
		if (scm_is_eq ($1, SCM_UNSPECIFIED)
		    || unsmob<Music> ($1))
			$$ = $1;
		else if (parser->lexer_->is_lyric_state ()
			   && Text_interface::is_markup ($1))
			MYBACKUP (LYRIC_ELEMENT, $1, @1);
		else {
			@$.warning (_ ("Ignoring non-music expression"));
			$$ = SCM_UNSPECIFIED;
		}
	}
	;

// music_assign does not need to contain lyrics: there are no
// assignments in lyricmode.
music_assign:
	simple_music
	| composite_music %prec COMPOSITE
	;

repeated_music:
	REPEAT simple_string unsigned_number music
	{
		$$ = MAKE_SYNTAX (repeat, @$, $2, $3, $4);
	}
	| REPEAT simple_string unsigned_number music sequential_alternative_music
	{
		$$ = MAKE_SYNTAX (repeat_alt, @$, $2, $3, $4, $5);
	}
	;

sequential_alternative_music:
	ALTERNATIVE braced_music_list {
		$$ = MAKE_SYNTAX (sequential_alternative_music, @$, $2);
	}
	|
	ALTERNATIVE MUSIC_IDENTIFIER { $$ = $2; }
	;

sequential_music:
	sequential_alternative_music {
		$$ = $1;
	}
	| SEQUENTIAL braced_music_list {
		$$ = MAKE_SYNTAX (sequential_music, @$, $2);
	}
	| braced_music_list {
		$$ = MAKE_SYNTAX (sequential_music, @$, $1);
	}
	;

simultaneous_music:
	SIMULTANEOUS braced_music_list {
		$$ = MAKE_SYNTAX (simultaneous_music, @$, $2);
	}
	| DOUBLE_ANGLE_OPEN music_list DOUBLE_ANGLE_CLOSE	{
		$$ = MAKE_SYNTAX (simultaneous_music, @$,
				  reverse_music_list (parser, @$, $2,
						      true, false));
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
		parser->lexer_->push_note_state ();
	} '{' context_mod_list '}'
        {
                parser->lexer_->pop_state ();
                $$ = $4;
        }
	| WITH context_modification_arg
	{
		if (unsmob<Music> ($2)) {
			SCM proc = parser->lexer_->lookup_identifier ("context-mod-music-handler");
			$2 = ly_call (proc, $2);
		}
		if (unsmob<Context_mod> ($2))
			$$ = $2;
		else {
			// let's permit \with #*unspecified* to go for
			// an empty context mod
			if (!scm_is_eq ($2, SCM_UNSPECIFIED))
				parser->parser_error (@2, _ ("not a context mod"));
			$$ = Context_mod ().smobbed_copy ();
		}
	}
        ;

context_modification_arg:
	embedded_scm
	| MUSIC_IDENTIFIER
	;

/* A list of single mods collected from a (possibly empty) sequence of
 * context modifications, usually written as \with ... \with ...
 */

optional_context_mods:
	context_modification_mods_list
        {
		if (scm_is_pair ($1))
			$$ = scm_append_x (scm_reverse_x ($1, SCM_EOL));
        }
        ;

/* The worker for optional_context_mods conses a (reversed) list where
 * each element contains the list of single context mods from one
 * context modification block.  Context_mod::get_mods creates fresh
 * copies, so it's okay to use append! on them.
 */

context_modification_mods_list:
	/**/ {
		$$ = SCM_EOL;
	}
	| context_modification_mods_list context_modification
	{
		if (Context_mod *m = unsmob<Context_mod> ($2))
			$$ = scm_cons (m->get_mods (), $1);
	}
	;

/* A Context_mod is a container for a list of context mods like
 * \consists ...  \override ... .  context_mod_list produces a
 * Context_mod from the inside of a \with { ... } statement.
 */

context_mod_list:
        /**/ {
            $$ = Context_mod ().smobbed_copy ();
        }
        | context_mod_list context_mod  {
		if (!SCM_UNBNDP ($2))
			unsmob<Context_mod> ($1)->add_context_mod ($2);
        }
	| context_mod_list context_mod_arg {
		if (unsmob<Music> ($2)) {
			SCM proc = parser->lexer_->lookup_identifier ("context-mod-music-handler");
			$2 = ly_call (proc, $2);
		}
		if (unsmob<Context_mod> ($2))
			unsmob<Context_mod> ($$)->add_context_mods
				(unsmob<Context_mod> ($2)->get_mods ());
		else if (!scm_is_eq ($2, SCM_UNSPECIFIED))
			parser->parser_error (@2, _ ("not a context mod"));
        }
        ;

context_prefix:
	CONTEXT symbol optional_id optional_context_mods {
		$$ = START_MAKE_SYNTAX (context_find_or_create, $2, $3, $4);
	}
	| NEWCONTEXT symbol optional_id optional_context_mods {
		$$ = START_MAKE_SYNTAX (context_create, $2, $3, $4);
	}
	;

new_lyrics:
	ADDLYRICS optional_context_mods lyric_mode_music {
		$$ = scm_acons ($3, $2, SCM_EOL);
	}
	| new_lyrics ADDLYRICS optional_context_mods lyric_mode_music {
		$$ = scm_acons ($4, $3, $1);
	}
	;

/* basic_music is basically the same as composite_music but with
 * context-prefixed music and lyricized music explicitly removed.  The
 * reason is that in a sequence
 *
 *   \new Staff \new Voice { ... } \addlyrics { ... } \addlyrics { ... }
 *
 * we need to group both \addlyrics together (as they go with the same
 * voice) but then combine them with \new Voice { ... }, meaning that
 * combining \new Voice { ... } needs higher priority than
 * { ... } \addlyrics, and *not* have \new Staff \new Voice { ... }
 * combine before combining \new Voice { ... } \addlyrics: only one
 * layer of context-prefixed music should assemble before \addlyrics
 * is integrated.  Total mess, and we sort this mess out with explicit
 * rules preferring a single context-prefix.
 */

basic_music:
	music_function_call
	| repeated_music
	| music_bare
	| LYRICSTO simple_string lyric_mode_music {
		$$ = MAKE_SYNTAX (lyric_combine, @$, $2, SCM_EOL, $3);
	}
	| LYRICSTO symbol '=' simple_string lyric_mode_music
	{
		$$ = MAKE_SYNTAX (lyric_combine, @$, $4, $2, $5);
	}
	;

contextable_music:
	basic_music
	| pitch_as_music
	| event_chord
	;

contexted_basic_music:
	context_prefix contextable_music new_lyrics
	{
		Input i;
		i.set_location (@1, @2);
		$$ = FINISH_MAKE_SYNTAX ($1, i, $2);
		$$ = MAKE_SYNTAX (add_lyrics, @$, $$, scm_reverse_x ($3, SCM_EOL));
	} %prec COMPOSITE
	| context_prefix contextable_music
	{
		$$ = FINISH_MAKE_SYNTAX ($1, @$, $2);
	} %prec COMPOSITE
	| context_prefix contexted_basic_music
	{
		$$ = FINISH_MAKE_SYNTAX ($1, @$, $2);
	}
	;

composite_music:
	basic_music %prec COMPOSITE
	| contexted_basic_music
	| basic_music new_lyrics
	{
		$$ = MAKE_SYNTAX (add_lyrics, @$, $1, scm_reverse_x ($2, SCM_EOL));
	} %prec COMPOSITE
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

symbol_list_arg:
	SYMBOL_LIST
	| SYMBOL_LIST '.' symbol_list_rev
	{
		$$ = ly_append ($1, scm_reverse_x ($3, SCM_EOL));
	}
	| SYMBOL_LIST ',' symbol_list_rev
	{
		$$ = ly_append ($1, scm_reverse_x ($3, SCM_EOL));
	}
	;

symbol_list_rev:
	symbol_list_part
	| symbol_list_rev '.' symbol_list_part
	{
		$$ = scm_append_x (ly_list ($3, $1));
	}
	| symbol_list_rev ',' symbol_list_part
	{
		$$ = scm_append_x (ly_list ($3, $1));
	}
	;

// symbol_list_part delivers elements in reverse copy, no lookahead

symbol_list_part:
	symbol_list_part_bare
	| embedded_scm_bare
	{
		$$ = make_reverse_key_list ($1);
		if (SCM_UNBNDP ($$)) {
			parser->parser_error (@1, _("not a key"));
			$$ = SCM_EOL;
		}
	}
	;


symbol_list_element:
	STRING
	{
		$$ = scm_string_to_symbol ($1);
	}
	| UNSIGNED
	;


symbol_list_part_bare:
	SYMBOL
	{
		$$ = try_word_variants (Lily::key_list_p, $1);
		if (SCM_UNBNDP ($$)) {
			parser->parser_error (@1, _("not a key"));
			$$ = SCM_EOL;
		} else
			$$ = scm_reverse ($$);
	}
	| symbol_list_element
	{
		$$ = ly_list ($1);
	}
	;

function_arglist_nonbackup:
	function_arglist_common
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup '-' UNSIGNED
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (ly_call ($2, n)))
			$$ = scm_cons (n, $3);
		else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @5);
			set_property (t, "digit", $5);
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
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup embedded_scm_arg
	{
		if (scm_is_true (ly_call ($2, $4)))
			$$ = scm_cons ($4, $3);
		else
			$$ = check_scheme_arg (parser, @4,
					       make_music_from_simple
					       (parser, @4, $4),
					       $3, $2, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup bare_number_common
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE pitch_or_music
	{
		if (scm_is_true (ly_call ($2, $3)))
			$$ = scm_cons ($3, $1);
		else
			$$ = check_scheme_arg (parser, @3,
					       make_music_from_simple
					       (parser, @3, $3),
					       $1, $2, $3);
	}
	| function_arglist_nonbackup_reparse REPARSE duration
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	| function_arglist_nonbackup_reparse REPARSE reparsed_rhythm
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


reparsed_rhythm:
	DURATION_ARG dots multipliers post_events
	{
		SCM d = make_duration ($1, from_scm<int> ($2), $3);
		parser->default_duration_ = *unsmob<Duration> (d);
		$$ = make_music_from_simple (parser, @$, d);
		Music *m = unsmob<Music> ($$);
		assert (m);
		if (scm_is_pair ($4))
			set_property (m, "articulations",
					 scm_reverse_x ($4, SCM_EOL));
	} %prec ':'
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
			 (ly_call
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup pitch
	{
		$$ = $3;
		if (scm_is_true
		    (ly_call
		     ($2, make_music_from_simple
		      (parser, @4, $4))))
			MYREPARSE (@4, $2, PITCH_IDENTIFIER, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup steno_tonic_pitch
	{
		$$ = $3;
		if (scm_is_true
		    (ly_call
		     ($2, make_music_from_simple
		      (parser, @4, $4))))
			MYREPARSE (@4, $2, TONICNAME_PITCH, $4);
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
			 (ly_call
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup SYMBOL
	{
		$$ = $3;
		SCM res = try_word_variants ($2, $4);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res))
				MYREPARSE (@4, $2, SYMBOL_LIST, res);
			else
				MYREPARSE (@4, $2, SCM_ARG, res);
		else if (scm_is_true
			 (ly_call
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup full_markup
	{
		$$ = $3;
		if (scm_is_true (ly_call ($2, $4)))
			MYREPARSE (@4, $2, SCM_ARG, $4);
		else if (scm_is_true
			 (ly_call
			  ($2, make_music_from_simple
			   (parser, @4, $4))))
			MYREPARSE (@4, $2, STRING, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup UNSIGNED
	{
		$$ = $3;
		if (scm_is_true (ly_call ($2, $4)))
			// May be 3 \cm or similar
			MYREPARSE (@4, $2, REAL, $4);
		else if (scm_is_true (ly_call ($2, ly_list ($4))))
			MYREPARSE (@4, $2, SYMBOL_LIST, ly_list ($4));
		else {
			SCM d = make_duration ($4);
			if (!SCM_UNBNDP (d)) {
				if (scm_is_true (ly_call ($2, d)))
					MYREPARSE (@4, $2, DURATION_IDENTIFIER, d);
				else if (scm_is_true
					 (ly_call
					  ($2, make_music_from_simple (parser, @4, d))))
					MYREPARSE (@4, $2, DURATION_ARG, d);
				else
					MYREPARSE (@4, $2, SCM_ARG, $4); // trigger error
			} else
				MYREPARSE (@4, $2, SCM_ARG, $4); // trigger error
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup DURATION_IDENTIFIER
	{
		$$ = $3;
		if (scm_is_true (ly_call ($2, $4)))
			MYREPARSE (@4, $2, DURATION_IDENTIFIER, $4);
		else if (scm_is_true
			 (ly_call
			  ($2, make_music_from_simple (parser, @4, $4))))
			MYREPARSE (@4, $2, DURATION_ARG, $4);
		else
			MYREPARSE (@4, $2, SCM_ARG, $4); // trigger error
	}
	;


// function_arglist_backup can't occur at the end of an argument
// list.
function_arglist_backup:
	function_arglist_common
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup embedded_scm_arg
	{
		if (scm_is_true (ly_call ($2, $4)))
			$$ = scm_cons ($4, $3);
		else {
			$$ = make_music_from_simple (parser, @4, $4);
			if (scm_is_true (ly_call ($2, $$)))
				$$ = scm_cons ($$, $3);
			else
			{
				$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
				MYBACKUP (SCM_ARG, $4, @4);
			}
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup post_event_nofinger
	{
		if (scm_is_true (ly_call ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (EVENT_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup pitch
	{
		if (scm_is_true
		    (ly_call
		     ($2, make_music_from_simple
		      (parser, @4, $4))))
		{
			$$ = $3;
			MYREPARSE (@4, $2, PITCH_IDENTIFIER, $4);
		} else if (scm_is_true (ly_call ($2, $4)))
			$$ = scm_cons ($4, $3);
		else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (PITCH_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup steno_tonic_pitch
	{
		if (scm_is_true
		    (ly_call
		     ($2, make_music_from_simple
		      (parser, @4, $4))))
		{
			$$ = $3;
			MYREPARSE (@4, $2, TONICNAME_PITCH, $4);
		} else if (scm_is_true (ly_call ($2, $4)))
			$$ = scm_cons ($4, $3);
		else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (TONICNAME_PITCH, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup full_markup
	{
		if (scm_is_true (ly_call ($2, $4)))
			$$ = scm_cons ($4, $3);
		else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (SCM_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup UNSIGNED
	{
		$$ = $3;
		if (scm_is_true (ly_call ($2, $4)))
			// May be 3 \cm or similar
			MYREPARSE (@4, $2, REAL, $4);
		else if (scm_is_true (ly_call ($2, ly_list ($4))))
			MYREPARSE (@4, $2, SYMBOL_LIST, ly_list ($4));
		else {
			SCM d = make_duration ($4);
			if (!SCM_UNBNDP (d)) {
				if (scm_is_true (ly_call ($2, d)))
					MYREPARSE (@4, $2, DURATION_IDENTIFIER, d);
				else if (scm_is_true
					 (ly_call
					  ($2, make_music_from_simple (parser, @4, d))))
					MYREPARSE (@4, $2, DURATION_ARG, d);
				else {
					$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
					MYBACKUP (UNSIGNED, $4, @4);
				}
			} else {
				$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
				MYBACKUP (UNSIGNED, $4, @4);
			}
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup REAL
	{
		if (scm_is_true (ly_call ($2, $4)))
		{
			$$ = $3;
			MYREPARSE (@4, $2, REAL, $4);
		} else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (REAL, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup NUMBER_IDENTIFIER
	{
		if (scm_is_true (ly_call ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (NUMBER_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup '-' UNSIGNED
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (ly_call ($2, n))) {
			$$ = $3;
			MYREPARSE (@5, $2, REAL, n);
		} else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @5);
			set_property (t, "digit", $5);
			$$ = t->unprotect ();
			if (scm_is_true (ly_call ($2, $$)))
				$$ = scm_cons ($$, $3);
			else {
				$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
				MYBACKUP (UNSIGNED, $5, @5);
				parser->lexer_->push_extra_token (@4, '-');
			}
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup '-' REAL
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (ly_call ($2, n))) {
			MYREPARSE (@5, $2, REAL, n);
			$$ = $3;
		} else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (REAL, n, @5);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup '-' NUMBER_IDENTIFIER
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (ly_call ($2, n))) {
			$$ = scm_cons (n, $3);
		} else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (NUMBER_IDENTIFIER, n, @5);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup DURATION_IDENTIFIER
	{
		$$ = $3;
		if (scm_is_true (ly_call ($2, $4)))
			MYREPARSE (@4, $2, DURATION_IDENTIFIER, $4);
		else if (scm_is_true
			 (ly_call
			  ($2, make_music_from_simple (parser, @4, $4))))
			MYREPARSE (@4, $2, DURATION_ARG, $4);
		else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
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
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
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
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (STRING, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup SYMBOL
	{
		SCM res = try_word_variants ($2, $4);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res)) {
				$$ = $3;
				MYREPARSE (@4, $2, SYMBOL_LIST, res);
			}
			else
				$$ = scm_cons (res, $3);
		else {
			$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
			MYBACKUP (STRING, $4, @4);
		}
	}
	| function_arglist_backup REPARSE pitch_or_music
	{
		if (scm_is_true (ly_call ($2, $3)))
			$$ = scm_cons ($3, $1);
		else
			$$ = check_scheme_arg (parser, @3,
					       make_music_from_simple
					       (parser, @3, $3),
					       $1, $2);
	}
	| function_arglist_backup REPARSE bare_number_common
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_backup REPARSE duration
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_backup REPARSE reparsed_rhythm
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
		$$ = scm_cons (loc_on_copy (parser, @4, $1), $3);
	}
	;

function_arglist_skip_nonbackup:
	function_arglist_nonbackup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_nonbackup
	{
		$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
	}
	;

// Partial function arglists are returned just in their incomplete
// state: when combined with the music function, the missing parts of
// the signature can be reconstructed
//
// To serve as a partial arglist, the argument list must absolutely
// _not_ be in "skipping optional arguments" mode since then there is
// some backup token that has nowhere to go before \etc.
//
// So we can skim off an arbitrary number of arguments from the end of
// the argument list.  The argument list remaining afterwards has to
// be in not-skipping-optional-arguments mode.

function_arglist_partial:
	EXPECT_SCM function_arglist_optional
	{
		$$ = $2;
	}
	| EXPECT_SCM function_arglist_partial_optional
	{
		$$ = $2;
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_nonbackup
	{
		$$ = $3;
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_partial
	{
		$$ = $3;
	}
	;

function_arglist_partial_optional:
	EXPECT_SCM function_arglist_optional
	{
		$$ = $2;
	}
	| EXPECT_SCM function_arglist_partial_optional
	{
		$$ = $2;
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup
	{
		$$ = $3;
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_partial_optional
	{
		$$ = $3;
	}
	;

function_arglist_common:
	EXPECT_NO_MORE_ARGS {
		$$ = SCM_EOL;
	}
	| EXPECT_SCM function_arglist_optional embedded_scm_arg
	{
		if (scm_is_true (ly_call ($1, $3)))
			$$ = scm_cons ($3, $2);
		else
			$$ = check_scheme_arg (parser, @3,
					       make_music_from_simple
					       (parser, @3, $3),
					       $2, $1, $3);
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
	| function_arglist_common_reparse REPARSE pitch_or_music
	{
		if (scm_is_true (ly_call ($2, $3)))
			$$ = scm_cons ($3, $1);
		else
			$$ = check_scheme_arg (parser, @3,
					       make_music_from_simple
					       (parser, @3, $3),
					       $1, $2, $3);
	}
	| function_arglist_common_reparse REPARSE bare_number_common
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE duration
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_common_reparse REPARSE reparsed_rhythm
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
			 (ly_call
			  ($1, make_music_from_simple (parser, @3, $3))))
			MYREPARSE (@3, $1, LYRIC_ELEMENT, $3);
		else
			// This is going to flag a syntax error, we
			// know the predicate to be false.
			MYREPARSE (@3, $1, SCM_ARG, $3);
	}
	| EXPECT_SCM function_arglist_optional pitch
	{
		$$ = $2;
		if (scm_is_true
		    (ly_call
		     ($1, make_music_from_simple
		      (parser, @3, $3))))
			MYREPARSE (@3, $1, PITCH_IDENTIFIER, $3);
		else
			MYREPARSE (@3, $1, SCM_ARG, $3);
	}
	| EXPECT_SCM function_arglist_optional steno_tonic_pitch
	{
		$$ = $2;
		if (scm_is_true
		    (ly_call
		     ($1, make_music_from_simple
		      (parser, @3, $3))))
			MYREPARSE (@3, $1, TONICNAME_PITCH, $3);
		else
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
			 (ly_call
			  ($1, make_music_from_simple (parser, @3, $3))))
			MYREPARSE (@3, $1, LYRIC_ELEMENT, $3);
		else
			// This is going to flag a syntax error, we
			// know the predicate to be false.
			MYREPARSE (@3, $1, SCM_ARG, $3);
	}
	| EXPECT_SCM function_arglist_optional SYMBOL
	{
		$$ = $2;
		SCM res = try_word_variants ($1, $3);
		if (!SCM_UNBNDP (res))
			if (scm_is_pair (res))
				MYREPARSE (@3, $1, SYMBOL_LIST, res);
			else
				MYREPARSE (@3, $1, SCM_ARG, res);
		else if (scm_is_true
			 (ly_call
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
		if (scm_is_true (ly_call ($1, $3)))
			MYREPARSE (@3, $1, SCM_ARG, $3);
		else if (scm_is_true
			 (ly_call
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
		if (scm_is_true (ly_call ($1, $3)))
			// May be 3 \cm or similar
			MYREPARSE (@3, $1, REAL, $3);
		else if (scm_is_true (ly_call ($1, ly_list ($3))))
			MYREPARSE (@3, $1, SYMBOL_LIST, ly_list ($3));
		else {
			SCM d = make_duration ($3);
			if (!SCM_UNBNDP (d)) {
				if (scm_is_true (ly_call ($1, d)))
					MYREPARSE (@3, $1, DURATION_IDENTIFIER, d);
				else if (scm_is_true
					 (ly_call
					  ($1, make_music_from_simple (parser, @3, d))))
					MYREPARSE (@3, $1, DURATION_ARG, d);
				else
					MYREPARSE (@3, $1, SCM_ARG, $3); // trigger error
			} else
				MYREPARSE (@3, $1, SCM_ARG, $3); // trigger error
		}
	}
	| EXPECT_SCM function_arglist_optional DURATION_IDENTIFIER
	{
		$$ = $2;
		if (scm_is_true (ly_call ($1, $3)))
			MYREPARSE (@3, $1, DURATION_IDENTIFIER, $3);
		else if (scm_is_true
			 (ly_call
			  ($1, make_music_from_simple (parser, @3, $3))))
			MYREPARSE (@3, $1, DURATION_ARG, $3);
		else
			MYREPARSE (@3, $1, SCM_ARG, $3); // trigger error
	}
	| EXPECT_SCM function_arglist_optional '-' UNSIGNED
	{
		$$ = $2;
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		if (scm_is_true (ly_call ($1, n)))
			MYREPARSE (@4, $1, REAL, n);
		else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @4);
			set_property (t, "digit", $4);
			SCM m = t->unprotect ();
			if (scm_is_true (ly_call ($1, m)))
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

function_arglist_optional:
	function_arglist_backup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_backup DEFAULT
	{
		$$ = scm_cons (loc_on_copy (parser, @4, $1), $3);
	}
	| function_arglist_skip_backup BACKUP
	;

function_arglist_skip_backup:
	function_arglist_backup
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip_backup
	{
		$$ = scm_cons (loc_on_copy (parser, @3, $1), $3);
	}
	;

music_function_call:
	MUSIC_FUNCTION function_arglist {
		$$ = MAKE_SYNTAX (music_function, @$,
				  $1, $2);
	}
	;


optional_id:
	/**/ { $$ = SCM_EOL; }
	| '=' simple_string {
		$$ = $2;
	}
	;

// We must not have lookahead tokens parsed in lyric mode.  In order
// to save confusion, we take almost the same set as permitted with
// \lyricmode and/or \lyrics.  However, music identifiers are also
// allowed, and they obviously do not require switching into lyrics
// mode for parsing.

lyric_mode_music:
	{
		parser->lexer_->push_lyric_state ();
	} grouped_music_list
	{
		parser->lexer_->pop_state ();
		$$ = $2;
	}
	| MUSIC_IDENTIFIER
	;

mode_changed_music:
	mode_changing_head grouped_music_list {
		if (scm_is_eq ($1, ly_symbol2scm ("chords")))
		{
		  $$ = MAKE_SYNTAX (unrelativable_music, @$, $2);
		}
		else
		{
		  $$ = $2;
		}
		parser->lexer_->pop_state ();
	}
	| mode_changing_head_with_context optional_context_mods grouped_music_list {
		$$ = MAKE_SYNTAX (context_create, @$, $1, SCM_EOL, $2, $3);
		if (scm_is_eq ($1, ly_symbol2scm ("ChordNames")))
		{
		  $$ = MAKE_SYNTAX (unrelativable_music, @$, $$);
		}
		parser->lexer_->pop_state ();
	}
	;

mode_changing_head:
	NOTEMODE {
		parser->lexer_->push_note_state ();

		$$ = ly_symbol2scm ("notes");
	}
	| DRUMMODE
		{
		parser->lexer_->push_drum_state ();
		$$ = ly_symbol2scm ("drums");
	}
	| FIGUREMODE {
		parser->lexer_->push_figuredbass_state ();

		$$ = ly_symbol2scm ("figures");
	}
	| CHORDMODE {
		SCM mods = parser->lexer_->lookup_identifier_symbol (ly_symbol2scm ("chordmodifiers"));
		parser->lexer_->chordmodifier_tab_ = alist_to_hashq (mods);
		parser->lexer_->push_chord_state ();
		$$ = ly_symbol2scm ("chords");

	}
	| LYRICMODE
		{ parser->lexer_->push_lyric_state ();
		$$ = ly_symbol2scm ("lyrics");
	}
	;

mode_changing_head_with_context:
	DRUMS {
		parser->lexer_->push_drum_state();
		$$ = ly_symbol2scm ("DrumStaff");
	}
	| FIGURES {
		parser->lexer_->push_figuredbass_state ();

		$$ = ly_symbol2scm ("FiguredBass");
	}
	| CHORDS {
		SCM mods = parser->lexer_->lookup_identifier_symbol (ly_symbol2scm ("chordmodifiers"));
		parser->lexer_->chordmodifier_tab_ = alist_to_hashq (mods);
		parser->lexer_->push_chord_state ();
		$$ = ly_symbol2scm ("ChordNames");
	}
	| LYRICS
		{ parser->lexer_->push_lyric_state ();
		$$ = ly_symbol2scm ("Lyrics");
	}
	;

context_change:
	CHANGE symbol '=' simple_string  {
		$$ = MAKE_SYNTAX (context_change, @$, $2, $4);
	}
	;


property_path:
	symbol_list_rev  {
		$$ = scm_reverse_x ($1, SCM_EOL);
	}
	;

property_operation:
	symbol '=' scalar {
		$$ = ly_list (ly_symbol2scm ("assign"), $1, $3);
	}
	| UNSET symbol {
		$$ = ly_list (ly_symbol2scm ("unset"), $2);
	}
	| OVERRIDE revert_arg '=' scalar {
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
		$$ = scm_append_x (ly_list ($5, $3));
	}
	| revert_arg_backup BACKUP SCM_ARG ',' symbol_list_part
	{
		$$ = scm_append_x (ly_list ($5, $3));
	}
	| revert_arg_backup BACKUP SCM_ARG symbol_list_part
	{
		$$ = scm_append_x (ly_list ($4, $3));
		property_path_dot_warning (@4, scm_reverse ($$));
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
		$$ = ly_list ($1, $2);
	}
	| context_def_mod SYMBOL {
		$$ = ly_list ($1, $2);
	}
	| context_def_mod embedded_scm
	{
		if (!scm_is_string ($2)
		    && !scm_is_eq ($1, ly_symbol2scm ("consists"))
		    && !scm_is_eq ($1, ly_symbol2scm ("remove")))
		{
			$$ = SCM_EOL;
			parser->parser_error (@1, _ ("only \\consists and \\remove take non-string argument."));
		}
		else
		{
			$$ = ly_list ($1, $2);
		}
	}
	;

grob_prop_spec:
	symbol_list_rev
	{
		$$ = scm_reverse_x ($1, SCM_EOL);
	}
	;

// If defined, at least three members
grob_prop_path:
	grob_prop_spec
	{
		if (scm_is_pair ($1)
		    && from_scm<bool>
		    (scm_object_property (scm_car ($1),
					  ly_symbol2scm ("is-grob?"))))
			$$ = scm_cons (ly_symbol2scm ("Bottom"), $1);
		if (!scm_is_pair ($$)
		    || !scm_is_pair (scm_cdr ($$))
		    || !scm_is_pair (scm_cddr ($$)))
		{
			parser->parser_error (@1, _ ("bad grob property path"));
			$$ = SCM_UNDEFINED;
		}
	}
	| grob_prop_spec property_path
	{
		if (scm_is_pair ($1)
		    && from_scm<bool>
		    (scm_object_property (scm_car ($1),
					  ly_symbol2scm ("is-grob?"))))
			$$ = scm_cons (ly_symbol2scm ("Bottom"), $1);
		if (!scm_is_pair ($$)
		    || !scm_is_pair (scm_cdr ($$))
		    || scm_is_pair (scm_cddr ($$))
		    || !scm_is_pair ($2))
		{
			parser->parser_error (@1, _ ("bad grob property path"));
			$$ = SCM_UNDEFINED;
		} else {
			property_path_dot_warning (@2, ly_append ($1, $2));
			$$ = scm_append_x (ly_list ($$, $2));
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
		    || from_scm<bool>
		    (scm_object_property (scm_car ($1),
					  ly_symbol2scm ("is-grob?")))) {
			$$ = ly_symbol2scm ("Bottom");
			parser->lexer_->push_extra_token (@1, SCM_IDENTIFIER, $1);
		} else {
			$$ = scm_car ($1);
			parser->lexer_->push_extra_token (@1, SCM_IDENTIFIER,
							  scm_cdr ($1));
		}
	}
	;

music_property_def:
	OVERRIDE grob_prop_path '=' scalar {
		if (SCM_UNBNDP ($2))
			$$ = MAKE_SYNTAX (void_music, @$);
		else
			$$ = MAKE_SYNTAX (property_override, @$,
					  scm_car ($2),
					  scm_cdr ($2),
					  $4);
	}
	| REVERT simple_revert_context revert_arg {
		$$ = MAKE_SYNTAX (property_revert, @$, $2, $3);
	}
	| SET context_prop_spec '=' scalar {
		if (SCM_UNBNDP ($2))
			$$ = MAKE_SYNTAX (void_music, @$);
		else
			$$ = MAKE_SYNTAX (property_set, @$,
					  scm_car ($2),
					  scm_cadr ($2),
					  $4);
	}
	| UNSET context_prop_spec {
		if (SCM_UNBNDP ($2))
			$$ = MAKE_SYNTAX (void_music, @$);
		else
			$$ = MAKE_SYNTAX (property_unset, @$,
					  scm_car ($2),
					  scm_cadr ($2));
	}
	;

string:
	STRING
	| SYMBOL
	| full_markup
	;

text:
	STRING
	| SYMBOL
	| full_markup
	| embedded_scm_bare
	{
		if (Text_interface::is_markup ($1)) {
			$$ = $1;
		} else {
			parser->parser_error (@1, (_ ("markup expected")));
			$$ = scm_string (SCM_EOL);
		}
	}
	;

simple_string: STRING
	| SYMBOL
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
	| SYMBOL
	{
		if (!is_regular_identifier ($1, false))
			parser->parser_error (@1, (_ ("symbol expected")));
		$$ = scm_string_to_symbol ($1);
	}
	| embedded_scm_bare
	{
		// This is a bit of overkill but makes the same
		// routine responsible for all symbol interpretations.
		$$ = try_string_variants (Guile_user::symbol_p, $1);
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
	| pitch_or_music
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
	| string
	| symbol_list_part_bare '.' property_path
	{
		$$ = scm_reverse_x ($1, $3);
	}
	| symbol_list_part_bare ',' property_path
	{
		$$ = scm_reverse_x ($1, $3);
	}
	;

event_chord:
	simple_element post_events {
		// Let the rhythmic music iterator sort this mess out.
		if (scm_is_pair ($2)) {
			set_property (unsmob<Music> ($$), "articulations",
							 scm_reverse_x ($2, SCM_EOL));
		}
	} %prec ':'
	| CHORD_REPETITION optional_notemode_duration post_events {
		$$ = MAKE_SYNTAX (repetition_chord, @$,
				  $2, scm_reverse_x ($3, SCM_EOL));
	} %prec ':'
	| MULTI_MEASURE_REST optional_notemode_duration post_events {
		$$ = MAKE_SYNTAX (multi_measure_rest, @$, $2,
				  scm_reverse_x ($3, SCM_EOL));
	} %prec ':'
	| tempo_event
	| note_chord_element
	;


note_chord_element:
	chord_body optional_notemode_duration post_events
	{
		Music *m = unsmob<Music> ($1);
		SCM dur = unsmob<Duration> ($2)->smobbed_copy ();
		SCM es = get_property (m, "elements");
		SCM postevs = scm_reverse_x ($3, SCM_EOL);

		for (SCM s = es; scm_is_pair (s); s = scm_cdr (s))
			set_property (unsmob<Music> (scm_car (s)), "duration", dur);
		es = ly_append (es, postevs);

		set_property (m, "elements", es);
		m->set_spot (parser->lexer_->override_input (@$));
		$$ = m->self_scm ();
	} %prec ':'
	;

chord_body:
	ANGLE_OPEN chord_body_elements ANGLE_CLOSE
	{
		$$ = MAKE_SYNTAX (event_chord, @$,
				  reverse_music_list (parser, @$,
						      $2, false, false));
	}
	| FIGURE_OPEN figure_list FIGURE_CLOSE
	{
		$$ = MAKE_SYNTAX (event_chord, @$, scm_reverse_x ($2, SCM_EOL));
	}
	;

chord_body_elements:
	/* empty */ 		{ $$ = SCM_EOL; }
	| chord_body_elements chord_body_element {
		if (unsmob<Music> ($2))
			$$ = scm_cons ($2, $1);
	}
	;

chord_body_element:
	pitch_or_tonic_pitch exclamations questions octave_check post_events %prec ':'
	{
		bool q = from_scm<bool> ($3);
		bool ex = from_scm<bool> ($2);
		SCM check = $4;
		SCM post = $5;

		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		set_property (n, "pitch", $1);
		if (q)
			set_property (n, "cautionary", SCM_BOOL_T);
                if (ex || q)
			set_property (n, "force-accidental", SCM_BOOL_T);

		if (scm_is_pair (post)) {
			SCM arts = scm_reverse_x (post, SCM_EOL);
			set_property (n, "articulations", arts);
		}
		if (scm_is_number (check))
		{
			int q = from_scm<int> (check);
			set_property (n, "absolute-octave", to_scm (q-1));
		}

		$$ = n->unprotect ();
	}
	| DRUM_PITCH post_events %prec ':' {
		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		set_property (n, "drum-type", $1);

		if (scm_is_pair ($2)) {
			SCM arts = scm_reverse_x ($2, SCM_EOL);
			set_property (n, "articulations", arts);
		}
		$$ = n->unprotect ();
	}
	| music_function_chord_body
	{
		Music *m = unsmob<Music> ($1);

		if (m && !m->is_mus_type ("post-event")) {
			while (m && m->is_mus_type ("music-wrapper-music")) {
				$$ = get_property (m, "element");
				m = unsmob<Music> ($$);
			}

			if (!(m && m->is_mus_type ("rhythmic-event"))) {
				parser->parser_error (@$, _ ("not a rhythmic event"));
				$$ = SCM_UNSPECIFIED;
			}
		}
	}
	| post_event
	;

music_function_chord_body:
	music_function_call
	| MUSIC_IDENTIFIER
	| embedded_scm
	;

event_function_event:
	EVENT_FUNCTION function_arglist {
		$$ = MAKE_SYNTAX (music_function, @$,
				  $1, $2);
	}
	;

post_events:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| post_events post_event {
		$$ = post_event_cons ($2, $1);
	}
	;

post_event_nofinger:
	direction_less_event {
		$$ = $1;
	}
	| script_dir music_function_call {
		Music *m = unsmob<Music> ($2);
		if (!m->is_mus_type ("post-event")) {
			parser->parser_error (@2, _ ("post-event expected"));
			$$ = SCM_UNSPECIFIED;
		} else {
			m->set_spot (parser->lexer_->override_input (@$));
			if (!SCM_UNBNDP ($1))
				set_property (m, "direction", $1);
			$$ = $2;
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
		if (Music *m = unsmob<Music> ($2)) {
			m->set_spot (parser->lexer_->override_input (@$));
			if (!SCM_UNBNDP ($1))
			{
				set_property (m, "direction", $1);
			}
		}
		$$ = $2;
	}
	| script_dir direction_less_event {
		Music *m = unsmob<Music> ($2);
		m->set_spot (parser->lexer_->override_input (@$));
		if (!SCM_UNBNDP ($1))
			set_property (m, "direction", $1);
		$$ = $2;
	}
	| '^' fingering
	{
		Music *m = unsmob<Music> ($2);
		m->set_spot (parser->lexer_->override_input (@$));
		set_property (m, "direction", to_scm (UP));
		$$ = $2;
	}
	| '_' fingering
	{
		Music *m = unsmob<Music> ($2);
		m->set_spot (parser->lexer_->override_input (@$));
		set_property (m, "direction", to_scm (DOWN));
		$$ = $2;
	}
	;

post_event:
	post_event_nofinger
	| '-' fingering {
		unsmob<Music> ($2)->set_spot (parser->lexer_->override_input (@$));
		$$ = $2;
	}
	;

string_number_event:
	E_UNSIGNED {
		Music *s = MY_MAKE_MUSIC ("StringNumberEvent", @$);
		set_property (s, "string-number", $1);
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
               set_property (a, "tremolo-type", $1);
               $$ = a->unprotect ();
        }
	| event_function_event
	;

direction_reqd_event:
	gen_text_def {
		$$ = $1;
	}
	| script_abbreviation {
		SCM sym = ly_symbol2scm ("dash" + ly_scm2string ($1));
		SCM s = parser->lexer_->lookup_identifier_symbol (sym);
		Music *original = unsmob<Music> (s);
		if (original && original->is_mus_type ("post-event")) {
			Music *a = original->clone ();
			// origin will be set by post_event_nofinger
			$$ = a->unprotect ();
// -----------------------------------------------------------------
// obsoletion handling, may be removed at some point (e.g. for 2.26)
		} else if (scm_is_string (s)) {
			string s_string = ly_scm2string (s);
			@$.warning (_f ("Re-defining dash%s using a string is deprecated. \
Please try replacing \"%s\" by \\%s or run convert-ly.",
				ly_scm2string ($1), s_string, s_string));
			Music *a = MY_MAKE_MUSIC ("ArticulationEvent", @$);
			set_property (a, "articulation-type", scm_string_to_symbol (s));
			$$ = a->unprotect ();
// -----------------------------------------------------------------
		} else {
			parser->parser_error (@1, _ ("expecting post-event as script definition"));
			$$ = SCM_UNSPECIFIED;
		}
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
	} %prec ':'
	| sub_quotes %prec ':'
	| sup_quotes %prec ':'
        ;

// no quotes, no error: pass *undefined* in that case.
erroneous_quotes:
	quotes {
		if (scm_is_eq (SCM_INUM0, $1))
			$$ = SCM_UNDEFINED;
	}
	;

sup_quotes:
	'\'' {
		$$ = to_scm (1);
	}
	| sup_quotes '\'' {
		$$ = scm_oneplus ($1);
	}
	;

sub_quotes:
	',' {
		$$ = to_scm (-1);
	}
	| sub_quotes ',' {
		$$ = scm_oneminus ($1);
	}
	;

steno_pitch:
	NOTENAME_PITCH quotes {
                if (!scm_is_eq (SCM_INUM0, $2))
                {
                        Pitch p = *unsmob<Pitch> ($1);
                        p = p.transposed (Pitch (from_scm<int> ($2), 0));
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
                        Pitch p = *unsmob<Pitch> ($1);
                        p = p.transposed (Pitch (from_scm<int> ($2), 0));
                        $$ = p.smobbed_copy ();
                }
	}
	;

pitch:
	steno_pitch
	| PITCH_IDENTIFIER quotes {
                if (!scm_is_eq (SCM_INUM0, $2))
                {
                        Pitch p = *unsmob<Pitch> ($1);
                        p = p.transposed (Pitch (from_scm<int> ($2), 0));
                        $$ = p.smobbed_copy ();
                }
	}
	;

pitch_or_tonic_pitch:
	pitch
	| steno_tonic_pitch
	;

gen_text_def:
	full_markup {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		set_property (t, "text", $1);
		$$ = t->unprotect ();
	}
	| STRING {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		set_property (t, "text",
			make_simple_markup ($1));
		$$ = t->unprotect ();
	}
	| SYMBOL {
		// Flag a warning? could be unintentional
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		set_property (t, "text",
			make_simple_markup ($1));
		$$ = t->unprotect ();
	}
	| embedded_scm
	{
		// Could be using this for every gen_text_def but for speed
		$$ = MAKE_SYNTAX (create_script, @1, $1);
	}
	;

fingering:
	UNSIGNED {
		Music *t = MY_MAKE_MUSIC ("FingeringEvent", @$);
		set_property (t, "digit", $1);
		$$ = t->unprotect ();
	}
	;

script_abbreviation:
	'^'		{
		$$ = scm_from_latin1_string ("Hat");
	}
	| '+'		{
		$$ = scm_from_latin1_string ("Plus");
	}
	| '-' 		{
		$$ = scm_from_latin1_string ("Dash");
	}
 	| '!'		{
		$$ = scm_from_latin1_string ("Bang");
	}
	| ANGLE_CLOSE	{
		$$ = scm_from_latin1_string ("Larger");
	}
	| '.' 		{
		$$ = scm_from_latin1_string ("Dot");
	}
	| '_' {
		$$ = scm_from_latin1_string ("Underscore");
	}
	;

script_dir:
	'_'	{ $$ = to_scm (DOWN); }
	| '^'	{ $$ = to_scm (UP); }
	| '-'	{ $$ = SCM_UNDEFINED; }
	;

maybe_notemode_duration:
	{
		$$ = SCM_UNDEFINED;
	} %prec ':'
	| duration	{
		$$ = $1;
		parser->default_duration_ = *unsmob<Duration> ($$);
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
		$$ = make_duration ($1, from_scm<int> ($2));
		if (SCM_UNBNDP ($$))
		{
			parser->parser_error (@1, _ ("not a duration"));
			$$ = Duration ().smobbed_copy ();
		}
	}
	| DURATION_IDENTIFIER dots	{
		$$ = make_duration ($1, from_scm<int> ($2));
	}
	;

duration:
	steno_duration multipliers {
		$$ = make_duration ($1, 0, $2);
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

multiplier_scm:
	NUMBER_IDENTIFIER
	| embedded_scm_bare
	;

multipliers:
	/* empty */
	{
		$$ = SCM_UNDEFINED;
	}
	| multipliers '*' UNSIGNED
	{
		if (!SCM_UNBNDP ($1))
			$$ = scm_product ($1, $3);
		else
			$$ = $3;
	}
	| multipliers '*' FRACTION
	{
		if (!SCM_UNBNDP ($1))
			$$ = scm_product ($1, scm_divide (scm_car ($3),
							  scm_cdr ($3)));
		else
			$$ = scm_divide (scm_car ($3), scm_cdr ($3));
	}
	| multipliers '*' multiplier_scm
	{
		if (scm_is_false (Lily::scale_p ($3)))
		{
			parser->parser_error (@3, _ ("not a multiplier"));
		} else if (SCM_UNBNDP ($1))
			$$ = Lily::scale_to_factor ($3);
		else
			$$ = scm_product ($1, Lily::scale_to_factor ($3));
	}
	;

tremolo_type:
	':'	{
		$$ = to_scm (parser->default_tremolo_type_);
	}
	| ':' UNSIGNED {
		if (SCM_UNBNDP (make_duration ($2))) {
			parser->parser_error (@2, _ ("not a duration"));
			$$ = to_scm (parser->default_tremolo_type_);
		} else {
			$$ = $2;
			parser->default_tremolo_type_ = from_scm<int> ($2);
		}
	}
	;

bass_number:
	UNSIGNED
	| STRING
	| SYMBOL
	| full_markup
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

bass_figure:
	FIGURE_SPACE {
		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent", @$);
		$$ = bfr->unprotect ();
	}
	| bass_number  {
		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent", @$);
		$$ = bfr->self_scm ();

		if (scm_is_number ($1))
			set_property (bfr, "figure", $1);
		else if (Text_interface::is_markup ($1))
			set_property (bfr, "text", $1);

		bfr->unprotect ();
	}
	| bass_figure ']' {
		$$ = $1;
		set_property (unsmob<Music> ($1), "bracket-stop", SCM_BOOL_T);
	}
	| bass_figure FIGURE_ALTERATION_EXPR {
		Music *m = unsmob<Music> ($1);

		if (scm_is_number (get_property (m, "alteration")))
			m->warning (_f ("Dropping surplus alteration symbols for bass figure."));
		else {
			string alter_expr = ly_scm2string ($2);
			Rational alter (0);
			bool bracket = false;

			for (string::iterator it=alter_expr.begin(); it != alter_expr.end (); it++)
			{
				int c = *it & 0xff;

				/* The friendly lexer guarantees that '[' has its matching ']',
				   so we don't have to check here. */
				if (c == '[')		bracket = true;

				/* "!" resets the counter: we mimic this traditional (pre-2.23.4) behavior. */
				else if (c == '!')	alter = 0;
				else if (c == '+')	alter += SHARP_ALTERATION;
				else if (c == '-')	alter += FLAT_ALTERATION;
			}

			set_property (m, "alteration", to_scm (alter));
			if (bracket)
				set_property (m, "alteration-bracket", SCM_BOOL_T);
		}
	}
	| bass_figure figured_bass_modification  {
		Music *m = unsmob<Music> ($1);
		set_property (m, $2, SCM_BOOL_T);
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
		set_property (unsmob<Music> ($$), "bracket-start", SCM_BOOL_T);
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

optional_rest:
	/**/   { $$ = SCM_BOOL_F; }
	| REST { $$ = SCM_BOOL_T; }
	;

pitch_or_music:
// The erroneous_quotes element is for input such as a1'' which is a
// typical note entry error that we don't want the parser to get
// confused about.  The resulting grammar, however, is inconsistent
// enough that accepting it is not doing anybody a favor.
	pitch exclamations questions octave_check maybe_notemode_duration erroneous_quotes optional_rest post_events {
		if (!parser->lexer_->is_note_state ())
			parser->parser_error (@1, _ ("have to be in Note mode for notes"));
		if (!SCM_UNBNDP ($6))
		{
			// It's possible to get here without a
			// duration, like when there is no
			// octave_check but a question mark.  But we
			// point out the most frequent error of an
			// interspersed duration specifically
			if (!SCM_UNBNDP ($5))
				parser->parser_error (@6, _ ("octave marks must precede duration"));
			else
				parser->parser_error (@6, _ ("badly placed octave marks"));
			// Try sorting the quotes to where they likely belong
			if (scm_is_number ($4)) {
				$4 = scm_sum ($4, $6);
			} else {
				$1 = unsmob<Pitch> ($1)->transposed
					(Pitch (from_scm<int> ($6), 0)).smobbed_copy ();
			}
		}

		if (!SCM_UNBNDP ($2)
                    || !SCM_UNBNDP ($3)
                    || scm_is_number ($4)
                    || !SCM_UNBNDP ($5)
		    || scm_is_true ($7)
		    || scm_is_pair ($8))
		{
			Music *n = 0;
			if (scm_is_true ($7))
				n = MY_MAKE_MUSIC ("RestEvent", @$);
			else
				n = MY_MAKE_MUSIC ("NoteEvent", @$);

			set_property (n, "pitch", $1);
			if (SCM_UNBNDP ($5))
				set_property (n, "duration",
						 parser->default_duration_.smobbed_copy ());
			else
				set_property (n, "duration", $5);

			if (scm_is_number ($4))
			{
				int q = from_scm<int> ($4);
				set_property (n, "absolute-octave", to_scm (q-1));
			}

			if (from_scm<bool> ($3))
				set_property (n, "cautionary", SCM_BOOL_T);
			if (from_scm<bool> ($2) || from_scm<bool> ($3))
				set_property (n, "force-accidental", SCM_BOOL_T);
			if (scm_is_pair ($8))
				set_property (n, "articulations",
						 scm_reverse_x ($8, SCM_EOL));
			$$ = n->unprotect ();
		}
	} %prec ':'
	| new_chord post_events {
		if (!parser->lexer_->is_chord_state ())
                        parser->parser_error (@1, _ ("have to be in Chord mode for chords"));
		if (scm_is_pair ($2)) {
			if (unsmob<Pitch> ($1))
				$1 = make_chord_elements (@1,
							  $1,
							  parser->default_duration_.smobbed_copy (),
							  SCM_EOL);

			SCM elts = ly_append ($1, scm_reverse_x ($2, SCM_EOL));

			$$ = MAKE_SYNTAX (event_chord, @1, elts);
		} else if (!unsmob<Pitch> ($1))
			$$ = MAKE_SYNTAX (event_chord, @1, $1);
		// A mere pitch drops through.
	} %prec ':'
	;

simple_element:
	DRUM_PITCH optional_notemode_duration {
		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		set_property (n, "duration", $2);
		set_property (n, "drum-type", $1);

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
		set_property (ev, "duration", $2);
 		$$ = ev->unprotect ();
	}
	;

lyric_element:
	full_markup {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _ ("markup outside of text script or \\lyricmode"));
		$$ = $1;
	}
	| SYMBOL {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _f ("not a note name: %s", ly_scm2string ($1)));
		$$ = $1;
	}
	| STRING {
		if (!parser->lexer_->is_lyric_state ())
			parser->parser_error (@1, _ ("string outside of text script or \\lyricmode"));
		$$ = $1;
	}
	| LYRIC_ELEMENT
	;

lyric_element_music:
	lyric_element optional_notemode_duration post_events {
		$$ = MAKE_SYNTAX (lyric_event, @$, $1, $2);
		if (scm_is_pair ($3))
			set_property
				(unsmob<Music> ($$), "articulations", scm_reverse_x ($3, SCM_EOL));
	} %prec ':'
	;

// Can return a single pitch rather than a list.
new_chord:
	steno_tonic_pitch maybe_notemode_duration   {
		if (SCM_UNBNDP ($2))
			$$ = $1;
		else
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
 		$$ = ly_list (ly_symbol2scm ("chord-slash"), $2);
	}
	| CHORD_BASS steno_tonic_pitch {
		$$ = ly_list (ly_symbol2scm ("chord-bass"), $2);
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
	unsigned_number {
		$$ = $1;
	} %prec ':'
	| unsigned_number '-' unsigned_number {
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
	REAL
	| NUMBER_IDENTIFIER
	| REAL NUMBER_IDENTIFIER
	{
		$$ = scm_product ($1, $2);
	}
	;

bare_number:
	bare_number_common
	| UNSIGNED
	| UNSIGNED NUMBER_IDENTIFIER	{
		$$ = scm_product ($1, $2);
	}
	;

unsigned_number:
	UNSIGNED
	| NUMBER_IDENTIFIER
	{
		if (!scm_is_integer ($1)
		    || scm_is_true (scm_negative_p ($1)))
		{
			parser->parser_error (@1, _("not an unsigned integer"));
			$$ = SCM_INUM0;
		}
	}
	| embedded_scm
	{
		if (!scm_is_integer ($1)
		    || scm_is_true (scm_negative_p ($1)))
		{
			parser->parser_error (@1, _("not an unsigned integer"));
			$$ = SCM_INUM0;
		}
	}
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
// This precedence rule is rather weird.  It triggers when '!' is
// encountered after a pitch, and is used for deciding whether to save
// this instead for a figure modification.  This should not actually
// occur in practice as pitches and figures are generated in different
// modes.  Using a greedy (%right) precedence makes sure that we don't
// get stuck in a wrong interpretation.
	{ $$ = SCM_UNDEFINED; } %prec ':'
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

markup_mode:
	MARKUP
	{
		parser->lexer_->push_markup_state ();
	}
	;

// Sort-of ugly: We need this as markup of its own as well as in
// markup function assignments, without triggering lookahead or the
// '=' for assignments will be parsed in markup mode and not
// recognized.  Worse: the next token following something like
// \markup "string" would be parsed in markup mode as well.
//
// So we make a single production here that's used either in markup or
// in assignment.

markup_mode_word:
	markup_mode markup_word
	{
		$$ = $2;
		parser->lexer_->pop_state ();
	}
	;


full_markup:
	markup_mode markup_top {
		$$ = $2;
		parser->lexer_->pop_state ();
	}
	| markup_mode_word
	{
		$$ = make_simple_markup ($1);
	}
	;

partial_markup:
	markup_mode markup_partial_function ETC
	{
		$$ = MAKE_SYNTAX (partial_markup, @2, $2);
		parser->lexer_->pop_state ();
	}
	;

markup_top:
	markup_list {
		$$ = ly_list (Lily::line_markup,  $1);
	}
	| markup_head_1_list simple_markup
	{
		$$ = scm_car (MAKE_SYNTAX (composed_markup_list,
					   @2, $1, ly_list ($2)));
	}
	| simple_markup_noword {
		$$ = $1;
	}
	;

markup_scm:
	embedded_scm
	{
		if (Text_interface::is_markup ($1))
			MYBACKUP (MARKUP_IDENTIFIER, $1, @1);
		else if (Text_interface::is_markup_list ($1))
			MYBACKUP (MARKUPLIST_IDENTIFIER, $1, @1);
		else if (scm_is_eq ($1, SCM_UNSPECIFIED))
			MYBACKUP (MARKUPLIST_IDENTIFIER, SCM_EOL, @1);
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
		$$ = ly_list ($1);
	}
	| markup_scm MARKUPLIST_IDENTIFIER
	{
		$$ = $2;
	}
	| SCORELINES {
		parser->lexer_->push_note_state ();
	} '{' score_body '}' {
		Score *sc = unsmob<Score> ($4);
		sc->origin ()->set_spot (@$);
		if (sc->defs_.empty ()) {
			Output_def *od = get_layout (parser);
			sc->add_output_def (od);
			od->unprotect ();
		}
		$$ = ly_list (ly_list (Lily::score_lines_markup_list, $4));
		parser->lexer_->pop_state ();
	}
	;

markup_composed_list:
	markup_head_1_list markup_uncomposed_list {
		$$ = MAKE_SYNTAX (composed_markup_list,
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
		$$ = Srfi_1::append_reverse ($2, $1);
	}
	;

markup_command_list:
	MARKUP_LIST_FUNCTION markup_command_list_arguments {
	  $$ = scm_cons ($1, scm_reverse_x ($2, SCM_EOL));
	}
	;

markup_command_embedded_lilypond:
	'{' {
		parser->lexer_->push_note_state ();
	} embedded_lilypond '}' {
		parser->lexer_->pop_state ();
                $$ = $3;
	}
	;


markup_command_basic_arguments:
	EXPECT_MARKUP_LIST markup_command_list_arguments markup_list {
		$$ = scm_cons ($3, $2);
	}
	| EXPECT_SCM markup_command_list_arguments embedded_scm {
		$$ = check_scheme_arg (parser, @3, $3, $2, $1);
	}
	| EXPECT_SCM markup_command_list_arguments markup_command_embedded_lilypond
	{
		$$ = check_scheme_arg (parser, @3, $3, $2, $1);
	}
	| EXPECT_SCM markup_command_list_arguments mode_changed_music {
		$$ = check_scheme_arg (parser, @3, $3, $2, $1);
	}
	| EXPECT_SCM markup_command_list_arguments MUSIC_IDENTIFIER {
		$$ = check_scheme_arg (parser, @3, $3, $2, $1);
	}
	| EXPECT_SCM markup_command_list_arguments STRING {
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

markup_partial_function:
	MARKUP_FUNCTION markup_arglist_partial
	{
		$$ = ly_list (scm_cons ($1, scm_reverse_x ($2, SCM_EOL)));
	}
	| markup_head_1_list MARKUP_FUNCTION markup_arglist_partial
	{
		$$ = scm_cons (scm_cons ($2, scm_reverse_x ($3, SCM_EOL)),
			       $1);
	}
	;

markup_arglist_partial:
	EXPECT_MARKUP markup_arglist_partial
	{
		$$ = $2;
	}
	| EXPECT_SCM markup_arglist_partial
	{
		$$= $2;
	}
	| EXPECT_MARKUP markup_command_list_arguments
	{
		$$ = $2;
	}
	| EXPECT_SCM markup_command_list_arguments
	{
		$$ = $2;
	}
	;

markup_head_1_item:
	MARKUP_FUNCTION EXPECT_MARKUP markup_command_list_arguments {
	  $$ = scm_cons ($1, scm_reverse_x ($3, SCM_EOL));
	}
	;

markup_head_1_list:
	markup_head_1_item	{
		$$ = ly_list ($1);
	}
	| markup_head_1_list markup_head_1_item	{
		$$ = scm_cons ($2, $1);
	}
	;

markup_word:
	STRING
	| SYMBOL
	;

simple_markup:
	markup_word
	{
		$$ = make_simple_markup ($1);
	}
	| simple_markup_noword
	;

simple_markup_noword:
	SCORE {
		parser->lexer_->push_note_state ();
	} '{' score_body '}' {
		Score *sc = unsmob<Score> ($4);
		sc->origin ()->set_spot (@$);
		if (sc->defs_.empty ()) {
			Output_def *od = get_layout (parser);
			sc->add_output_def (od);
			od->unprotect ();
		}
		$$ = ly_list (Lily::score_markup, $4);
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
		$$ = scm_car (MAKE_SYNTAX (composed_markup_list,
					   @2, $1, ly_list ($2)));
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
	return scm_c_with_fluid (Lily::f_parser,
				 self_scm (),
				 do_yyparse_trampoline,
				 static_cast <void *>(this));
}

SCM
Lily_parser::do_yyparse_trampoline (void *parser)
{
	SCM retval = SCM_UNDEFINED;
	yyparse (static_cast <Lily_parser *>(parser), &retval);
	return retval;
}



/*

It is a little strange to have this function in this file, but
otherwise, we have to import music classes into the lexer.

*/
int
Lily_lexer::try_special_identifiers (SCM *destination, SCM sid)
{
	if (unsmob<Book> (sid)) {
		Book *book =  unsmob<Book> (sid)->clone ();
		*destination = book->self_scm ();
		book->unprotect ();

		return BOOK_IDENTIFIER;
	} else if (scm_is_number (sid)) {
		*destination = sid;
		return NUMBER_IDENTIFIER;
	} else if (unsmob<Context_def> (sid))
	{
		*destination = unsmob<Context_def> (sid)->clone ()->unprotect ();
		return SCM_IDENTIFIER;
        } else if (unsmob<Context_mod> (sid)) {
                *destination = unsmob<Context_mod> (sid)->smobbed_copy ();
                return SCM_IDENTIFIER;
	} else if (Music *mus = unsmob<Music> (sid)) {
		mus = mus->clone ();
		*destination = mus->self_scm ();
		bool is_event = mus->is_mus_type ("post-event");
		mus->unprotect ();
		return is_event ? EVENT_IDENTIFIER : MUSIC_IDENTIFIER;
	} else if (unsmob<Pitch> (sid)) {
		*destination = unsmob<Pitch> (sid)->smobbed_copy ();
		return PITCH_IDENTIFIER;
	} else if (unsmob<Duration> (sid)) {
		*destination = unsmob<Duration> (sid)->smobbed_copy ();
		return DURATION_IDENTIFIER;
	} else if (unsmob<Output_def> (sid)) {
		*destination = unsmob<Output_def> (sid)->clone ()->unprotect ();
		return SCM_IDENTIFIER;
	} else if (unsmob<Score> (sid)) {
		*destination = unsmob<Score> (sid)->clone ()->unprotect ();
		return SCM_IDENTIFIER;
	} else if (scm_is_pair (sid)
		   && scm_is_pair (scm_car (sid))
		   && scm_is_true (Lily::key_p (scm_caar (sid)))) {
		*destination = sid;
		return LOOKUP_IDENTIFIER;
	}
	return -1;
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
		if (scm_is_true (ly_call (pred, arg)))
			return args;
	}
	scm_set_cdr_x (scm_last_pair (args), SCM_EOL);
	MAKE_SYNTAX (argument_error, loc, scm_length (args), pred,
		     SCM_UNBNDP (disp) ? arg : disp);
	scm_set_cdr_x (scm_last_pair (args), SCM_BOOL_F);
	return args;
}

SCM loc_on_copy (Lily_parser *parser, Input loc, SCM arg)
{
	if (Music *m = unsmob<Music> (arg))
	{
		m = m->clone ();
		m->set_spot (parser->lexer_->override_input (loc));
		return m->unprotect ();
	}
	if (Book *b = unsmob<Book> (arg))
	{
		b = b->clone ();
		b->origin ()->set_spot (parser->lexer_->override_input (loc));
		return b->unprotect ();
	}
	if (Context_def *cd = unsmob<Context_def> (arg))
	{
		cd = cd->clone ();
		cd->origin ()->set_spot (parser->lexer_->override_input (loc));
		return cd->unprotect ();
	}
	if (Output_def *od = unsmob<Output_def> (arg))
	{
		od = od->clone ();
		od->input_origin_ = parser->lexer_->override_input (loc);
		return od->unprotect ();
	}
	if (Score *s = unsmob<Score> (arg))
	{
		s = s->clone ();
		s->origin ()->set_spot (parser->lexer_->override_input (loc));
		return s->unprotect ();
	}
	if (Context_mod *cm = unsmob<Context_mod> (arg))
	{
		return cm->smobbed_copy ();
	}
	return arg;
}

SCM
make_reverse_key_list (SCM keys)
{
	if (scm_is_true (Lily::key_p (keys)))
		return ly_list (keys);
	if (scm_is_string (keys))
		return ly_list (scm_string_to_symbol (keys));
	if (!ly_is_list (keys))
		return SCM_UNDEFINED;
	SCM res = SCM_EOL;
	for (; scm_is_pair (keys); keys = scm_cdr (keys))
	{
		SCM elt = scm_car (keys);
		if (scm_is_true (Lily::key_p (elt)))
			res = scm_cons (elt, res);
		else if (scm_is_string (elt))
			res = scm_cons (scm_string_to_symbol (elt), res);
		else return SCM_UNDEFINED;
	}
	return res;
}

SCM
try_string_variants (SCM pred, SCM str)
{
	// a matching predicate is always ok
	if (scm_is_true (ly_call (pred, str)))
		return str;
	// a key may be interpreted as a list of keys if it helps
	if (scm_is_true (Lily::key_p (str))) {
		str = ly_list (str);
		if (scm_is_true (ly_call (pred, str)))
			return str;
		return SCM_UNDEFINED;
	}

	if (!scm_is_string (str))
		return SCM_UNDEFINED;

	// Let's attempt the symbol list interpretation first.

	str = scm_string_to_symbol (str);

	SCM lst = ly_list (str);

	if (scm_is_true (ly_call (pred, lst)))
		return lst;

	// Try the single symbol interpretation

	if (scm_is_true (ly_call (pred, str)))
		return str;

	return SCM_UNDEFINED;
}

SCM
try_word_variants (SCM pred, SCM str)
{
	// str is always a string when we come here

	if (scm_is_true (ly_call (pred, str)))
		return str;

	// If this cannot be a string representation of a symbol list,
	// we are through.

	if (!is_regular_identifier (str, true))
		return SCM_UNDEFINED;

	str = scm_string_split (str, SCM_MAKE_CHAR ('.'));
	for (SCM &p : as_ly_scm_list (str))
		p = scm_string_split (p, SCM_MAKE_CHAR (','));
	str = scm_append_x (str);
	for (SCM &p : as_ly_scm_list (str))
		p = scm_string_to_symbol (p);

	// Let's attempt the symbol list interpretation first.

	if (scm_is_true (ly_call (pred, str)))
		return str;

	// If there is just one symbol in the list, we might interpret
	// it as a single symbol

	if (scm_is_null (scm_cdr (str)))
	{
		str = scm_car (str);
		if (scm_is_true (ly_call (pred, str)))
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
	  else if (middle && (c == '-' || c == '_' || (multiple &&
						       (c == '.' || c == ','))))
		  middle = false;
	  else
		  return false;
  }
  return middle;
}

SCM
make_music_from_simple (Lily_parser *parser, Input loc, SCM simple)
{
	if (unsmob<Music> (simple))
		return simple;

	if (scm_is_symbol (simple))
	{
		SCM out = SCM_UNDEFINED;
		switch (parser->lexer_->scan_word (out, simple))
		{
		case DRUM_PITCH:
		{
			Music *n = MY_MAKE_MUSIC ("NoteEvent", loc);
			set_property (n, "duration", parser->default_duration_.smobbed_copy ());
			set_property (n, "drum-type", out);
			return n->unprotect ();
		}
		case NOTENAME_PITCH:
		case TONICNAME_PITCH:
			// Take the parsed pitch
			simple = out;
			break;
		// Don't scan CHORD_MODIFIER etc.
		}
	}

	if (parser->lexer_->is_note_state ()) {
		if (unsmob<Pitch> (simple)) {
			Music *n = MY_MAKE_MUSIC ("NoteEvent", loc);
			set_property (n, "duration", parser->default_duration_.smobbed_copy ());
			set_property (n, "pitch", simple);
			return n->unprotect ();
		}
		SCM d = simple;
		if (scm_is_integer (simple))
			d = make_duration (simple);
		if (unsmob<Duration> (d)) {
			Music *n = MY_MAKE_MUSIC ("NoteEvent", loc);
			set_property (n, "duration", d);
			return n->unprotect ();
		}
		return simple;
	} else if (parser->lexer_->is_lyric_state ()) {
		if (Text_interface::is_markup (simple))
			return MAKE_SYNTAX (lyric_event, loc, simple,
					    parser->default_duration_.smobbed_copy ());
	} else if (parser->lexer_->is_chord_state ()) {
		if (unsmob<Pitch> (simple))
			return MAKE_SYNTAX
				(event_chord,
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
make_duration (SCM d, int dots, SCM factor)
{
	Duration k;

	if (Duration *dur = unsmob<Duration> (d)) {
		if (!dots && SCM_UNBNDP (factor))
			return d;
		k = *dur;
		if (dots)
			k = Duration (k.duration_log (), k.dot_count () + dots)
				.compressed (k.factor ());
	} else {
		int t = from_scm<int> (d);
		if (t > 0 && (t & (t-1)) == 0)
			k = Duration (intlog2 (t), dots);
		else
			return SCM_UNDEFINED;
	}

	if (!SCM_UNBNDP (factor))
		k = k.compressed (from_scm<Rational> (factor));

	return k.smobbed_copy ();
}

SCM
make_chord_step (SCM step_scm, Rational alter)
{
	Pitch m (0, from_scm<int> (step_scm) - 1, alter);

	// Notename/octave are normalized
	if (m.get_notename () == 6)
		m = m.transposed (Pitch (0, 0, FLAT_ALTERATION));

	return m.smobbed_copy ();
}


SCM
make_chord_elements (Input loc, SCM pitch, SCM dur, SCM modification_list)
{
	SCM res = Lily::construct_chord_elements (pitch, dur, modification_list);
	for (SCM s = res; scm_is_pair (s); s = scm_cdr (s))
	{
		unsmob<Music> (scm_car (s))->set_spot (loc);
	}
	return res;
}

// Return true if there are post events unaccounted for
bool
add_post_events (Music *m, SCM events)
{
	if (!scm_is_pair (events))
		return false;	// successfully added -- nothing

	while (m) {
		if (m->is_mus_type ("rhythmic-event")
		    || m->is_mus_type ("caesura-event")) {
			set_property
				(m, "articulations",
				 scm_append_x (ly_list
					       (get_property (m, "articulations"),
						events)));
			return false;
		}
		if (m->is_mus_type ("event-chord")) {
			set_property
				(m, "elements",
				 scm_append_x (ly_list
					       (get_property (m, "elements"),
						events)));
			return false;
		}
		if (m->is_mus_type ("sequential-music")) {
			SCM lp = scm_last_pair (get_property (m, "elements"));
			if (scm_is_pair (lp)) {
				m = unsmob<Music> (scm_car (lp));
				continue;
			}
			return true;
		}
		if (m->is_mus_type ("music-wrapper-music")
		    || m->is_mus_type ("time-scaled-music")) {
			m = unsmob<Music> (get_property (m, "element"));
			continue;
		}
		break;
	}
	return true;
}

// Returns either a list or a post-event
//
// If PRESERVE is true, unattachable post-events are not thrown away
// but rather added attached to empty chords.  If COMPRESS is true, a
// sequence consisting only of post-events may be returned as a single
// post-event.
SCM reverse_music_list (Lily_parser *parser, Input loc, SCM lst, bool preserve, bool compress)
{
	SCM res = SCM_EOL;	// Resulting reversed list
	SCM bad = SCM_EOL;	// Bad post events
	SCM post = SCM_EOL;	// current unattached events
	for (; scm_is_pair (lst); lst = scm_cdr (lst)) {
		SCM elt = scm_car (lst);
		Music *m = unsmob<Music> (elt);
		assert (m);
		if (m->is_mus_type ("post-event")) {
			post = post_event_cons (elt, post);
			continue;
		}
		if (add_post_events (m, post)) {
			bad = scm_cons (scm_car (post), bad);
			if (preserve) {
				Music *p = unsmob<Music> (scm_car (post));
				res = scm_cons (MAKE_SYNTAX (event_chord,
							     *p->origin (),
							     post),
						res);
			}
		}
		post = SCM_EOL;
		res = scm_cons (elt, res);
	}
	if (scm_is_pair (post)) {
		if (scm_is_null (res) && compress) { // pure postevent list
			if (scm_is_null (scm_cdr (post)))
				return scm_car (post);
			Music *m = MY_MAKE_MUSIC ("PostEvents", loc);
			set_property (m, "elements", post);
			return m->unprotect ();
		}
		bad = ly_append (post, bad);
		if (preserve) {
			Music *p = unsmob<Music> (scm_car (post));
			res = scm_cons (MAKE_SYNTAX (event_chord,
						     *p->origin (),
						     post),
					res);
		}
	}
	for (; scm_is_pair (bad); bad = scm_cdr (bad))
	{
		Music *what = unsmob<Music> (scm_car (bad));
		if (preserve)
			what->warning (_f ("Unattached %s", what->name ()));
		else
			what->warning (_f ("Dropping unattachable %s", what->name ()));
	}
	return res;
}

SCM post_event_cons (SCM post_event, SCM tail)
{
	Music *ev = unsmob<Music> (post_event);
	if (!ev)
		return tail;
	if (!ev->is_mus_type ("post-event-wrapper"))
		return scm_cons (post_event, tail);
	SCM elts = SCM_UNDEFINED;
	SCM props = SCM_EOL;
	SCM tweaks = SCM_UNDEFINED;
	for (SCM p = ev->get_property_alist (true);
	     scm_is_pair (p);
	     p = scm_cdr (p))
	{
		SCM pair = scm_car (p);
		SCM sym = scm_car (pair);
		if (scm_is_eq (sym, ly_symbol2scm ("origin")))
			continue;
		else if (scm_is_eq (sym, ly_symbol2scm ("elements"))
			 && SCM_UNBNDP (elts))
			elts = scm_cdr (pair);
		else if (scm_is_eq (sym, ly_symbol2scm ("tweaks"))
			 && SCM_UNBNDP (tweaks))
			tweaks = scm_cdr (pair);
		else
			props = scm_cons (pair, props);
	}
	if (!scm_is_pair (elts))
		return tail;
	elts = scm_reverse_x (elts, SCM_EOL);
	for (SCM p = elts; scm_is_pair (p); p = scm_cdr (p))
	{
		Music *ev = unsmob<Music> (scm_car (p));
		// tweaks are always collected in-order, newer tweaks
		// nearer to the front of the list
		if (scm_is_pair (tweaks))
			set_property (ev,
			              "tweaks",
			              Srfi_1::append_reverse (tweaks,
				                              get_property (ev, "tweaks")));
		// other properties are applied last to first so that
		// in case of duplicate properties, the actually
		// current one survives
		for (SCM q = props; scm_is_pair (q); q = scm_cdr (q))
			set_property (ev, scm_caar (q), scm_cdar (q));
	}
	return ly_append (elts, tail);
}

void property_path_dot_warning (Input loc, SCM lst)
{
	// if lst is empty, don't even venture a guess...
	if (scm_is_pair (lst)) {
		std::string out = ly_symbol2string (scm_car (lst));
		for (lst = scm_cdr (lst); scm_is_pair (lst); lst = scm_cdr (lst)) {
			out += ".";
			out += ly_symbol2string (scm_car (lst));
		}
		loc.warning (_f ("deprecated: missing `.' in property path %s", out));
	}
}


int
yylex (YYSTYPE *s, YYLTYPE *loc, Lily_parser *parser)
{
	Lily_lexer *lex = parser->lexer_;

	lex->lexval_ = s;
	lex->lexloc_ = loc;
	int tok = lex->pop_extra_token ();
	if (tok >= 0)
		return tok;
	lex->prepare_for_next_token ();
	return lex->yylex ();
}
