/* -*- mode: c++; c-file-style: "linux" -*- */
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

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

#define yyerror Lily_parser::parser_error

/* We use custom location type: Input objects */
#define YYLTYPE Input
#define YYLLOC_DEFAULT(Current,Rhs,N) \
	((Current).set_location ((Rhs)[1], (Rhs)[N]))


%}

%parse-param {Lily_parser *parser}
%lex-param {Lily_parser *parser}

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

 /* ADDLYRICS needs to have lower precedence than argument scanning,
  * or we won't be able to tell music apart from closed_music without
  * lookahead in the context of function calls.
  */

%nonassoc DEFAULT

 /* \default is only applied after exhausting function arguments */

%nonassoc FUNCTION_ARGLIST

 /* expressions with units are permitted into argument lists */

%right PITCH_IDENTIFIER NOTENAME_PITCH TONICNAME_PITCH
      UNSIGNED REAL DURATION_IDENTIFIER ':'

 /* The above are the symbols that can start optional function arguments
    that are recognized in the grammar rather than by predicate
 */

%nonassoc NUMBER_IDENTIFIER '/'

 /* Number-unit expressions, where permitted, are concatenated into
  * function arguments, just like fractions and tremoli.  Tremoli must
  * not have higher precedence than UNSIGNED, or Lilypond will not
  * join ':' with a following optional number.
  */

%left PREC_TOP




%pure_parser
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
Lily_parser::parser_error (Input const *i, Lily_parser *parser, string s)
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


%union {
	Book *book;
	Output_def *outputdef;
	SCM scm;
	std::string *string;
 	Score *score;
 	int i;
}

%{

#define MY_MAKE_MUSIC(x, spot)  make_music_with_input (ly_symbol2scm (x), spot)

/* ES TODO:
- Don't use lily module, create a new module instead.
- delay application of the function
*/
#define LOWLEVEL_MAKE_SYNTAX(proc, args)	\
  scm_apply_0 (proc, args)
/* Syntactic Sugar. */
#define MAKE_SYNTAX(name, location, ...)	\
  LOWLEVEL_MAKE_SYNTAX (ly_lily_module_constant (name), scm_list_n (parser->self_scm (), make_input (location) , ##__VA_ARGS__, SCM_UNDEFINED))
#define START_MAKE_SYNTAX(name, ...)					\
	scm_list_n (ly_lily_module_constant (name) , ##__VA_ARGS__, SCM_UNDEFINED)
#define FINISH_MAKE_SYNTAX(start, location, ...)			\
	LOWLEVEL_MAKE_SYNTAX (scm_car (start), scm_cons2 (parser->self_scm (), make_input (location), scm_append_x (scm_list_2 (scm_cdr (start), scm_list_n (__VA_ARGS__, SCM_UNDEFINED)))))

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
		      SCM arg, SCM args, SCM pred);
SCM loc_on_music (Input loc, SCM arg);
SCM make_chord_elements (SCM pitch, SCM dur, SCM modification_list);
SCM make_chord_step (int step, Rational alter);
SCM make_simple_markup (SCM a);
bool is_duration (int t);
bool is_regular_identifier (SCM id);
int yylex (YYSTYPE *s, YYLTYPE *loc, Lily_parser *parser);
void set_music_properties (Music *p, SCM a);

%}

/* The third option is an alias that will be used to display the
   syntax error.  Bison CVS now correctly handles backslash escapes.

   FIXME: Bison needs to translate some of these, eg, STRING.

*/

/* Keyword tokens with plain escaped name.  */
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
%token E_ANGLE_CLOSE "\\>"
%token E_CHAR "\\C[haracter]"
%token E_CLOSE "\\)"
%token E_EXCLAMATION "\\!"
%token E_BRACKET_OPEN "\\["
%token E_OPEN "\\("
%token E_BRACKET_CLOSE "\\]"
%token E_ANGLE_OPEN "\\<"
%token E_PLUS "\\+"
%token E_TILDE "\\~"
%token EXTENDER "__"

/*
If we give names, Bison complains.
*/
%token FIGURE_CLOSE /* "\\>" */
%token FIGURE_OPEN /* "\\<" */
%token FIGURE_SPACE "_"
%token HYPHEN "--"

%token CHORDMODIFIERS
%token LYRIC_MARKUP
%token MULTI_MEASURE_REST


%token <i> E_UNSIGNED
%token <scm> UNSIGNED

/* Artificial tokens, for more generic function syntax */
%token <i> EXPECT_MARKUP "markup?"
%token <i> EXPECT_PITCH "ly:pitch?"
%token <i> EXPECT_DURATION "ly:duration?"
%token <scm> EXPECT_SCM "scheme?"
%token <scm> BACKUP "(backed-up?)"
%token <scm> REPARSE "(reparsed?)"
%token <i> EXPECT_MARKUP_LIST "markup-list?"
%token <scm> EXPECT_OPTIONAL "optional?"
/* After the last argument. */
%token <i> EXPECT_NO_MORE_ARGS;

/* An artificial token for parsing embedded Lilypond */
%token <i> EMBEDDED_LILY "#{"

%token <scm> BOOK_IDENTIFIER
%token <scm> CHORDMODIFIER_PITCH
%token <scm> CHORD_MODIFIER
%token <scm> CHORD_REPETITION
%token <scm> CONTEXT_DEF_IDENTIFIER
%token <scm> CONTEXT_MOD_IDENTIFIER
%token <scm> DRUM_PITCH
%token <scm> PITCH_IDENTIFIER
%token <scm> DURATION_IDENTIFIER
%token <scm> EVENT_IDENTIFIER
%token <scm> EVENT_FUNCTION
%token <scm> FRACTION
%token <scm> LYRICS_STRING
%token <scm> LYRIC_ELEMENT
%token <scm> LYRIC_MARKUP_IDENTIFIER
%token <scm> MARKUP_FUNCTION
%token <scm> MARKUP_LIST_FUNCTION
%token <scm> MARKUP_IDENTIFIER
%token <scm> MARKUPLIST_IDENTIFIER
%token <scm> MUSIC_FUNCTION
%token <scm> MUSIC_IDENTIFIER
%token <scm> NOTENAME_PITCH
%token <scm> NUMBER_IDENTIFIER
%token <scm> OUTPUT_DEF_IDENTIFIER
%token <scm> REAL
%token <scm> RESTNAME
%token <scm> SCM_FUNCTION
%token <scm> SCM_IDENTIFIER
%token <scm> SCM_TOKEN
%token <scm> SCORE_IDENTIFIER
%token <scm> STRING
%token <scm> STRING_IDENTIFIER
%token <scm> TONICNAME_PITCH


%type <book> book_block
%type <book> book_body
%type <book> bookpart_block
%type <book> bookpart_body

%type <i> bare_unsigned
%type <scm> figured_bass_alteration
%type <i> dots
%type <i> exclamations
%type <i> optional_rest
%type <i> questions
%type <i> script_dir
%type <i> sub_quotes
%type <i> sup_quotes
%type <i> tremolo_type

/* Music */
%type <scm> composite_music
%type <scm> grouped_music_list
%type <scm> braced_music_list
%type <scm> closed_music
%type <scm> music
%type <scm> music_bare
%type <scm> music_arg
%type <scm> complex_music
%type <scm> complex_music_prefix
%type <scm> mode_changed_music
%type <scm> repeated_music
%type <scm> sequential_music
%type <scm> simple_music
%type <scm> simultaneous_music
%type <scm> chord_body
%type <scm> chord_body_element
%type <scm> command_element
%type <scm> command_event
%type <scm> context_modification
%type <scm> context_change
%type <scm> direction_less_event
%type <scm> direction_reqd_event
%type <scm> embedded_lilypond
%type <scm> event_chord
%type <scm> fingering
%type <scm> gen_text_def
%type <scm> music_property_def
%type <scm> note_chord_element
%type <scm> post_event
%type <scm> post_event_nofinger
%type <scm> re_rhythmed_music
%type <scm> simple_element
%type <scm> simple_music_property_def
%type <scm> start_symbol
%type <scm> string_number_event
%type <scm> tempo_event

%type <outputdef> output_def_body
%type <outputdef> output_def_head
%type <outputdef> output_def_head_with_mode_switch
%type <outputdef> output_def
%type <outputdef> paper_block

%type <scm> music_function_call
%type <scm> music_list
%type <scm> assignment_id
%type <scm> bare_number
%type <scm> bare_number_closed
%type <scm> unsigned_number
%type <scm> bass_figure
%type <scm> figured_bass_modification
%type <scm> br_bass_figure
%type <scm> bass_number
%type <scm> chord_body_elements
%type <scm> chord_item
%type <scm> chord_items
%type <scm> chord_separator
%type <scm> context_def_mod
%type <scm> context_def_spec_block
%type <scm> context_def_spec_body
%type <scm> context_mod
%type <scm> context_mod_list
%type <scm> context_prop_spec
%type <scm> direction_less_char
%type <scm> duration_length
%type <scm> embedded_scm
%type <scm> embedded_scm_arg
%type <scm> embedded_scm_arg_closed
%type <scm> embedded_scm_bare
%type <scm> embedded_scm_bare_arg
%type <scm> embedded_scm_closed
%type <scm> event_function_event
%type <scm> figure_list
%type <scm> figure_spec
%type <scm> fraction
%type <scm> full_markup
%type <scm> full_markup_list
%type <scm> function_arglist
%type <scm> function_arglist_optional
%type <scm> function_arglist_backup
%type <scm> function_arglist_nonbackup
%type <scm> function_arglist_skip
%type <scm> function_arglist_bare
%type <scm> function_arglist_closed
%type <scm> function_arglist_closed_optional
%type <scm> function_arglist_common
%type <scm> function_arglist_common_lyric
%type <scm> function_arglist_common_minus
%type <scm> function_arglist_closed_common
%type <scm> function_arglist_keep
%type <scm> function_arglist_closed_keep
%type <scm> identifier_init
%type <scm> lilypond
%type <scm> lilypond_header
%type <scm> lilypond_header_body
%type <scm> lyric_element
%type <scm> lyric_element_arg
%type <scm> lyric_element_music
%type <scm> lyric_markup
%type <scm> markup
%type <scm> markup_braced_list
%type <scm> markup_braced_list_body
%type <scm> markup_composed_list
%type <scm> markup_command_list
%type <scm> markup_command_list_arguments
%type <scm> markup_command_basic_arguments
%type <scm> markup_head_1_item
%type <scm> markup_head_1_list
%type <scm> markup_list
%type <scm> markup_top
%type <scm> mode_changing_head
%type <scm> mode_changing_head_with_context
%type <scm> multiplied_duration
%type <scm> music_function_event
%type <scm> music_function_chord_body
%type <scm> new_chord
%type <scm> new_lyrics
%type <scm> number_expression
%type <scm> number_factor
%type <scm> number_term
%type <scm> octave_check
%type <scm> optional_context_mod
%type <scm> optional_id
%type <scm> optional_notemode_duration
%type <scm> pitch
%type <scm> pitch_also_in_chords
%type <scm> post_events
%type <scm> property_operation
%type <scm> property_path property_path_revved
%type <scm> scalar
%type <scm> scalar_closed
%type <scm> scm_function_call
%type <scm> scm_function_call_closed
%type <scm> script_abbreviation
%type <scm> simple_chord_elements
%type <scm> simple_markup
%type <scm> simple_string
%type <scm> steno_duration
%type <scm> steno_pitch
%type <scm> steno_tonic_pitch
%type <scm> step_number
%type <scm> step_numbers
%type <scm> string
%type <scm> tempo_range

%type <score> score_block
%type <score> score_body


%left '-' '+'

/* We don't assign precedence to / and *, because we might need varied
prec levels in different prods */

%left UNARY_MINUS

%%

start_symbol:
	lilypond
	| EMBEDDED_LILY {
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (alist_to_hashq (nn));
	} embedded_lilypond {
		parser->lexer_->pop_state ();
		parser->lexer_->set_identifier (ly_symbol2scm ("parseStringResult"), $3);
 	}
	;

lilypond:	/* empty */ { }
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
	lilypond_header {
		parser->lexer_->set_identifier (ly_symbol2scm ("$defaultheader"), $1);
	}
	| book_block {
		Book *book = $1;
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-book-handler");
		scm_call_2 (proc, parser->self_scm (), book->self_scm ());
		book->unprotect ();
	}
	| bookpart_block {
		Book *bookpart = $1;
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-bookpart-handler");
		scm_call_2 (proc, parser->self_scm (), bookpart->self_scm ());
		bookpart->unprotect ();
	}
	| score_block {
		Score *score = $1;

		SCM proc = parser->lexer_->lookup_identifier ("toplevel-score-handler");
		scm_call_2 (proc, parser->self_scm (), score->self_scm ());
		score->unprotect ();
	}
	| composite_music {
		Music *music = unsmob_music ($1);
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-music-handler");
		scm_call_2 (proc, parser->self_scm (), music->self_scm ());
	}
	| full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
		scm_call_2 (proc, parser->self_scm (), scm_list_1 ($1));
	}
	| full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("toplevel-text-handler");
		scm_call_2 (proc, parser->self_scm (), $1);
	}
	| output_def {
		SCM id = SCM_EOL;
		Output_def * od = $1;

		if ($1->c_variable ("is-paper") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultpaper");
		else if ($1->c_variable ("is-midi") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultmidi");
		else if ($1->c_variable ("is-layout") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultlayout");

		parser->lexer_->set_identifier (id, od->self_scm ());
		od->unprotect();
	}
	;

embedded_scm_bare:
	SCM_TOKEN
	{
		$$ = parser->lexer_->eval_scm ($1);
	}
	| SCM_IDENTIFIER
	;

embedded_scm_bare_arg:
	embedded_scm_bare
	| STRING
	| STRING_IDENTIFIER
	| full_markup
	| full_markup_list
	| context_modification
	| score_block
	{
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| context_def_spec_block
	| book_block
	{
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| bookpart_block
	{
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| output_def
	{
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	;

/* The generic version may end in music, or not */

embedded_scm:
	embedded_scm_bare
	| scm_function_call
	;

embedded_scm_arg:
	embedded_scm_bare_arg
	| scm_function_call
	| music_arg
	;

scm_function_call:
	SCM_FUNCTION function_arglist {
		$$ = MAKE_SYNTAX ("music-function", @$,
					 $1, $2);
	}
	;

embedded_lilypond:
	/* empty */
	{
		$$ = MAKE_SYNTAX ("void-music", @$);
	}
	| identifier_init
	| music music music_list {
		$$ = MAKE_SYNTAX ("sequential-music", @$,	
				  scm_cons2 ($1, $2, scm_reverse_x ($3, SCM_EOL)));
	}
	| error {
		parser->error_level_ = 1;
	}
	| INVALID embedded_lilypond {
		parser->error_level_ = 1;
	}
	;


lilypond_header_body:
	{
		$$ = get_header (parser);
		parser->lexer_->add_scope ($$);
	}
	| lilypond_header_body assignment  {

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
	| LYRICS_STRING { $$ = $1; }
	;

assignment:
	assignment_id '=' identifier_init  {
	        parser->lexer_->set_identifier ($1, $3);
	}
	| assignment_id property_path '=' identifier_init {
		SCM path = scm_cons (scm_string_to_symbol ($1), $2);
		parser->lexer_->set_identifier (path, $4);
	;
/*
 TODO: devise standard for protection in parser.

  The parser stack lives on the C-stack, which means that
all objects can be unprotected as soon as they're here.

*/
	}
	| embedded_scm { }
	;


identifier_init:
	score_block {
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| book_block {
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| bookpart_block {
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| output_def {
		$$ = $1->self_scm ();
		$1->unprotect ();
	}
	| context_def_spec_block {
		$$ = $1;
	}
	| music  {
		$$ = $1;
	}
	| post_event_nofinger {
		$$ = $1;
	}
	| number_expression {
 		$$ = $1;
	}
	| string {
		$$ = $1;
	}
        | embedded_scm {
		$$ = $1;
	}
	| full_markup {
		$$ = $1;
	}
	| full_markup_list {
		$$ = $1;
	}
        | context_modification {
                $$ = $1;
        }
	;

context_def_spec_block:
	CONTEXT '{' context_def_spec_body '}'
		{
		$$ = $3;
	}
	;

context_def_spec_body:
	/**/ {
		$$ = Context_def::make_scm ();
		unsmob_context_def ($$)->origin ()->set_spot (@$);
	}
	| CONTEXT_DEF_IDENTIFIER {
		$$ = $1;
		unsmob_context_def ($$)->origin ()->set_spot (@$);
	}
	| context_def_spec_body embedded_scm {
		if (Context_mod *cm = unsmob_context_mod ($2)) {
			SCM p = cm->get_mods ();
			Context_def*td = unsmob_context_def ($$);

			for (; scm_is_pair (p); p = scm_cdr (p)) {
				td->add_context_mod (scm_car (p));
			}
		} else {
			parser->parser_error (@2, _ ("not a context mod"));
		}
	}
	| context_def_spec_body context_mod {
		unsmob_context_def ($$)->add_context_mod ($2);
	}
	| context_def_spec_body context_modification {
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
		pop_paper (parser);
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-book"), SCM_BOOL_F);
	}
	;

/* FIXME:
   * Use 'handlers' like for toplevel-* stuff?
   * grok \layout and \midi?  */
book_body:
	{
		$$ = new Book;
		init_papers (parser);
		$$->origin ()->set_spot (@$);
		$$->paper_ = dynamic_cast<Output_def*> (unsmob_output_def (parser->lexer_->lookup_identifier ("$defaultpaper"))->clone ());
		$$->paper_->unprotect ();
		push_paper (parser, $$->paper_);
		$$->header_ = parser->lexer_->lookup_identifier ("$defaultheader");
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-book"), $$->self_scm ());
		parser->lexer_->set_identifier (ly_symbol2scm ("book-output-suffix"), SCM_BOOL_F);
		parser->lexer_->set_identifier (ly_symbol2scm ("book-filename"), SCM_BOOL_F);
	}
	| BOOK_IDENTIFIER {
		$$ = unsmob_book ($1);
		$$->protect ();
		$$->origin ()->set_spot (@$);
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-book"), $1);
	}
	| book_body paper_block {
		$$->paper_ = $2;
		$2->unprotect ();
		set_paper (parser, $2);
	}
	| book_body bookpart_block {
		Book *bookpart = $2;
		SCM proc = parser->lexer_->lookup_identifier ("book-bookpart-handler");
		scm_call_2 (proc, $$->self_scm (), bookpart->self_scm ());
		bookpart->unprotect ();
	}
	| book_body score_block {
		Score *score = $2;
		SCM proc = parser->lexer_->lookup_identifier ("book-score-handler");
		scm_call_2 (proc, $$->self_scm (), score->self_scm ());
		score->unprotect ();
	}
	| book_body composite_music {
		Music *music = unsmob_music ($2);
		SCM proc = parser->lexer_->lookup_identifier ("book-music-handler");
		scm_call_3 (proc, parser->self_scm (), $$->self_scm (), music->self_scm ());
	}
	| book_body full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
		scm_call_2 (proc, $$->self_scm (), scm_list_1 ($2));
	}
	| book_body full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("book-text-handler");
		scm_call_2 (proc, $$->self_scm (), $2);
	}
	| book_body lilypond_header {
		$$->header_ = $2;
	}
	| book_body embedded_scm { }
	| book_body error {
		$$->paper_ = 0;
		$$->scores_ = SCM_EOL;
		$$->bookparts_ = SCM_EOL;
	}
	;

bookpart_block:
	BOOKPART '{' bookpart_body '}' {
		$$ = $3;
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), SCM_BOOL_F);
	}
	;

bookpart_body:
	{
		$$ = new Book;
		$$->origin ()->set_spot (@$);
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), $$->self_scm ());
	}
	| BOOK_IDENTIFIER {
		$$ = unsmob_book ($1);
		$$->protect ();
		$$->origin ()->set_spot (@$);
		parser->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), $1);
	}
	| bookpart_body paper_block {
		$$->paper_ = $2;
		$2->unprotect ();
	}
	| bookpart_body score_block {
		Score *score = $2;
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-score-handler");
		scm_call_2 (proc, $$->self_scm (), score->self_scm ());
		score->unprotect ();
	}
	| bookpart_body composite_music {
		Music *music = unsmob_music ($2);
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-music-handler");
		scm_call_3 (proc, parser->self_scm (), $$->self_scm (), music->self_scm ());
	}
	| bookpart_body full_markup {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
		scm_call_2 (proc, $$->self_scm (), scm_list_1 ($2));
	}
	| bookpart_body full_markup_list {
		SCM proc = parser->lexer_->lookup_identifier ("bookpart-text-handler");
		scm_call_2 (proc, $$->self_scm (), $2);
	}
	| bookpart_body lilypond_header {
		$$->header_ = $2;
	}
	| bookpart_body embedded_scm { }
	| bookpart_body error {
		$$->paper_ = 0;
		$$->scores_ = SCM_EOL;
	}
	;

score_block:
	SCORE '{' score_body '}' 	{
		$$ = $3;
	}
	;

score_body:
	music {
		SCM m = $1;
		SCM scorify = ly_lily_module_constant ("scorify-music");
		SCM score = scm_call_2 (scorify, m, parser->self_scm ());

		// pass ownernship to C++ again.
		$$ = unsmob_score (score);
		$$->protect ();
		$$->origin ()->set_spot (@$);
	}
	| SCORE_IDENTIFIER {
		$$ = unsmob_score ($1);
		$$->protect ();
		$$->origin ()->set_spot (@$);
	}
	| score_body lilypond_header 	{
		$$->set_header ($2);
	}
	| score_body output_def {
		if ($2->lookup_variable (ly_symbol2scm ("is-paper")) == SCM_BOOL_T)
		{
			parser->parser_error (@2, _("\\paper cannot be used in \\score, use \\layout instead"));

		}
		else
		{
			$$->add_output_def ($2);
		}
		$2->unprotect ();
	}
	| score_body error {
		$$->error_found_ = true;
	}
	;


/*
	OUTPUT DEF
*/

paper_block:
	output_def {
		$$ = $1;
		if ($$->lookup_variable (ly_symbol2scm ("is-paper")) != SCM_BOOL_T)
		{
			parser->parser_error (@1, _ ("need \\paper for paper block"));
			$1->unprotect ();
			$$ = get_paper (parser);
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
		$$ = get_paper (parser);
		$$->input_origin_ = @$;
		parser->lexer_->add_scope ($$->scope_);
	}
	| MIDI    {
		Output_def *p = get_midi (parser);
		$$ = p;
		parser->lexer_->add_scope (p->scope_);
	}
	| LAYOUT 	{
		Output_def *p = get_layout (parser);

		parser->lexer_->add_scope (p->scope_);
		$$ = p;
	}
	;

output_def_head_with_mode_switch:
	output_def_head {
		parser->lexer_->push_initial_state ();
		$$ = $1;
	}
	;

output_def_body:
	output_def_head_with_mode_switch '{' {
		$$ = $1;
		$$->input_origin_.set_spot (@$);
	}
	| output_def_head_with_mode_switch '{' OUTPUT_DEF_IDENTIFIER 	{
		$1->unprotect ();

		Output_def *o = unsmob_output_def ($3);
		o->input_origin_.set_spot (@$);
		$$ = o;
		$$->protect ();
		parser->lexer_->remove_scope ();
		parser->lexer_->add_scope (o->scope_);
	}
	| output_def_body assignment  {

	}
	| output_def_body context_def_spec_block	{
		assign_context_def ($$, $2);
	}
	| output_def_body error {

	}
	;

tempo_event:
	TEMPO steno_duration '=' tempo_range	{
		$$ = MAKE_SYNTAX ("tempo", @$, SCM_EOL, $2, $4);
	}
	| TEMPO scalar_closed steno_duration '=' tempo_range	{
		$$ = MAKE_SYNTAX ("tempo", @$, $2, $3, $5);
	}
	| TEMPO scalar {
		$$ = MAKE_SYNTAX ("tempo", @$, $2);
	}
	;

/*
The representation of a  list is reversed to have efficient append.  */

music_list:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| music_list music {
		$$ = scm_cons ($2, $1);
	}
	| music_list embedded_scm {

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

music:	simple_music
	| lyric_element_music
	| composite_music %prec COMPOSITE
	;

music_arg:
	simple_music
	| composite_music %prec COMPOSITE
	;

repeated_music:
	REPEAT simple_string unsigned_number music
	{
		$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, SCM_EOL);
	}
	| REPEAT simple_string unsigned_number music ALTERNATIVE braced_music_list
	{
		$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, $6);
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
        WITH { parser->lexer_->push_initial_state (); } '{' context_mod_list '}'
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
	| WITH embedded_scm_closed
	{
		if (unsmob_context_mod ($2))
			$$ = $2;
		else {
			parser->parser_error (@2, _ ("not a context mod"));
			$$ = Context_mod ().smobbed_copy ();
		}
	}
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
                 unsmob_context_mod ($1)->add_context_mod ($2);
        }
        | context_mod_list CONTEXT_MOD_IDENTIFIER {
                 Context_mod *md = unsmob_context_mod ($2);
                 if (md)
                     unsmob_context_mod ($1)->add_context_mods (md->get_mods ());
        }
	| context_mod_list embedded_scm {
		Context_mod *md = unsmob_context_mod ($2);
		if (md)
			unsmob_context_mod ($1)->add_context_mods (md->get_mods ());
		else
			parser->parser_error (@2, _ ("not a context mod"));
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

/* An argument list. If a function \foo expects scm scm pitch, then the lexer expands \foo into the token sequence:
 MUSIC_FUNCTION EXPECT_PITCH EXPECT_SCM EXPECT_SCM EXPECT_NO_MORE_ARGS
and this rule returns the reversed list of arguments. */

function_arglist_skip:
	function_arglist_common
	| EXPECT_OPTIONAL EXPECT_PITCH function_arglist_skip
	{
		$$ = scm_cons ($1, $3);
	} %prec FUNCTION_ARGLIST
	| EXPECT_OPTIONAL EXPECT_DURATION function_arglist_skip
	{
		$$ = scm_cons ($1, $3);
	} %prec FUNCTION_ARGLIST
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip
	{
		$$ = scm_cons ($1, $3);
	} %prec FUNCTION_ARGLIST
	;


function_arglist_nonbackup:
	EXPECT_OPTIONAL EXPECT_PITCH function_arglist pitch_also_in_chords {
		$$ = scm_cons ($4, $3);
	}
	| EXPECT_OPTIONAL EXPECT_DURATION function_arglist_closed duration_length {
		$$ = scm_cons ($4, $3);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist embedded_scm_arg_closed
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed bare_number_closed
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed FRACTION
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @4, $4, $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed '-' UNSIGNED
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n)))
			$$ = scm_cons (n, $3);
		else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @5);
			t->set_property ("digit", $5);
			$$ = t->unprotect ();
			if (scm_is_true (scm_call_1 ($2, $$)))
				$$ = scm_cons ($$, $3);
			else
				$$ = check_scheme_arg (parser, @4, n, $3, $2);
		}
		
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed '-' REAL
	{
		$$ = check_scheme_arg (parser, @4,
				       scm_difference ($5, SCM_UNDEFINED),
				       $3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed '-' NUMBER_IDENTIFIER
	{
		$$ = check_scheme_arg (parser, @4,
				       scm_difference ($5, SCM_UNDEFINED),
				       $3, $2);
	}
	;


function_arglist_keep:
	function_arglist_common
	| function_arglist_backup
	;

function_arglist_closed_keep:
	function_arglist_closed_common
	| function_arglist_backup
	;

function_arglist_backup:
	EXPECT_OPTIONAL EXPECT_SCM function_arglist_keep embedded_scm_arg_closed
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (SCM_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep post_event_nofinger
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (EVENT_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_keep lyric_element
	{
		// There is no point interpreting a lyrics string as
		// an event, since we don't allow music possibly
		// followed by durations or postevent into closed
		// music, and we only accept closed music in optional
		// arguments at the moment.  If this changes, more
		// complex schemes might become interesting here as
		// well: see how we do this at the mandatory argument
		// point.
		if (scm_is_true (scm_call_1 ($2, $4)))
			$$ = scm_cons ($4, $3);
		else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (LYRICS_STRING, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep UNSIGNED
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = $3;
			MYREPARSE (@4, $2, UNSIGNED, $4);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (UNSIGNED, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep REAL
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
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep NUMBER_IDENTIFIER
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (NUMBER_IDENTIFIER, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep FRACTION
	{
		if (scm_is_true (scm_call_1 ($2, $4)))
		{
			$$ = scm_cons ($4, $3);
		} else {
			$$ = scm_cons (loc_on_music (@3, $1), $3);
			MYBACKUP (FRACTION, $4, @4);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep '-' UNSIGNED
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
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep '-' REAL
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n))) {
			MYREPARSE (@5, $2, REAL, n);
			$$ = $3;
		} else {
			MYBACKUP (REAL, n, @5);
		}
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_closed_keep '-' NUMBER_IDENTIFIER
	{
		SCM n = scm_difference ($5, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($2, n))) {
			$$ = scm_cons (n, $3);
		} else {
			MYBACKUP (NUMBER_IDENTIFIER, n, @5);
		}
	}
	| EXPECT_OPTIONAL EXPECT_PITCH function_arglist_keep pitch_also_in_chords
	{
		$$ = scm_cons ($4, $3);
	}
	| EXPECT_OPTIONAL EXPECT_DURATION function_arglist_closed_keep duration_length
	{
		$$ = scm_cons ($4, $3);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_backup BACKUP
	{
		$$ = scm_cons ($1, $3);
		MYBACKUP(0, SCM_UNDEFINED, @3);
	}
	| function_arglist_backup REPARSE embedded_scm_arg_closed
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_backup REPARSE bare_number
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	| function_arglist_backup REPARSE fraction
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	;

function_arglist:
	function_arglist_common
	| function_arglist_nonbackup
	;

function_arglist_common:
	function_arglist_bare
	| EXPECT_SCM function_arglist_optional embedded_scm_arg
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional bare_number
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional fraction
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| function_arglist_common_minus
	| function_arglist_common_lyric
	;

function_arglist_common_lyric:
	EXPECT_SCM function_arglist_optional lyric_element
	{
		// We check how the predicate thinks about a lyrics
		// event or about a markup.  If it accepts neither, we
		// backup the original token.  Otherwise we commit to
		// taking the token.  Depending on what the predicate
		// is willing to accept, we interpret as a string, as
		// a lyric event, or ambiguously (meaning that if
		// something looking like a duration or post event
		// follows, we take the event, otherwise the string).
		SCM lyric_event = MAKE_SYNTAX ("lyric-event", @3, $3,
					       parser->default_duration_.smobbed_copy ());
		if (scm_is_true (scm_call_1 ($1, $3)))
			if (scm_is_true (scm_call_1 ($1, lyric_event)))
			{
				$$ = $2;
				MYREPARSE (@3, $1, LYRICS_STRING, $3);
			} else {
				$$ = scm_cons ($3, $2);
			}
		else if (scm_is_true (scm_call_1 ($1, lyric_event)))
		{
			$$ = $2;
			MYREPARSE (@3, $1, LYRIC_ELEMENT, $3);
		} else {
			// This is going to flag a syntax error, we
			// know the predicate to be false.
			check_scheme_arg (parser, @3,
					  $3, $2, $1);
		}
	}
	| function_arglist_common_lyric REPARSE lyric_element_arg
	{
		// This should never be false
		$$ = check_scheme_arg (parser, @3,
				       $3, $1, $2);
	}
	;

function_arglist_common_minus:
	EXPECT_SCM function_arglist_closed_optional '-' UNSIGNED
	{
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($1, n))) {
			$$ = $2;
			MYREPARSE (@4, $1, REAL, n);
		} else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @4);
			t->set_property ("digit", $4);
			$$ = t->unprotect ();
			if (scm_is_true (scm_call_1 ($1, $$)))
				$$ = scm_cons ($$, $2);
			else
				$$ = check_scheme_arg (parser, @3, n, $2, $1);
		}
		
	}
	| EXPECT_SCM function_arglist_closed_optional '-' REAL
	{
		$$ = $2;
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		MYREPARSE (@4, $1, REAL, n);
	}
	| EXPECT_SCM function_arglist_closed_optional '-' NUMBER_IDENTIFIER
	{
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		$$ = check_scheme_arg (parser, @4, n, $2, $1);
	}
	| function_arglist_common_minus REPARSE bare_number
	{
		$$ = check_scheme_arg (parser, @3, $3, $1, $2);
	}
	;

function_arglist_closed:
	function_arglist_closed_common
	| function_arglist_nonbackup
	;

function_arglist_closed_common:
	function_arglist_bare
	| EXPECT_SCM function_arglist_optional embedded_scm_arg_closed
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional bare_number
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional '-' UNSIGNED
	{
		SCM n = scm_difference ($4, SCM_UNDEFINED);
		if (scm_is_true (scm_call_1 ($1, n))) {
			$$ = scm_cons (n, $2);
		} else {
			Music *t = MY_MAKE_MUSIC ("FingeringEvent", @4);
			t->set_property ("digit", $4);
			$$ = t->unprotect ();
			if (scm_is_true (scm_call_1 ($1, $$)))
				$$ = scm_cons ($$, $2);
			else
				$$ = check_scheme_arg (parser, @3, n, $2, $1);
		}
		
	}
	| EXPECT_SCM function_arglist_closed_optional '-' REAL
	{
		$$ = check_scheme_arg (parser, @3,
				       scm_difference ($4, SCM_UNDEFINED),
				       $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional '-' NUMBER_IDENTIFIER
	{
		$$ = check_scheme_arg (parser, @3,
				       scm_difference ($4, SCM_UNDEFINED),
				       $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional post_event_nofinger
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_closed_optional fraction
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	| EXPECT_SCM function_arglist_optional lyric_element
	{
		$$ = check_scheme_arg (parser, @3,
				       $3, $2, $1);
	}
	;

function_arglist_optional:
	function_arglist_keep %prec FUNCTION_ARGLIST
	| function_arglist_backup BACKUP
	| EXPECT_OPTIONAL EXPECT_PITCH function_arglist_optional
	{
		$$ = scm_cons ($1, $3);
	}
	| EXPECT_OPTIONAL EXPECT_DURATION function_arglist_optional
	{
		$$ = scm_cons ($1, $3);
	}
	;

function_arglist_closed_optional:
	function_arglist_closed_keep %prec FUNCTION_ARGLIST
	| function_arglist_backup BACKUP
	| EXPECT_OPTIONAL EXPECT_PITCH function_arglist_closed_optional
	{
		$$ = scm_cons ($1, $3);
	}
	| EXPECT_OPTIONAL EXPECT_DURATION function_arglist_closed_optional
	{
		$$ = scm_cons ($1, $3);
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
	} %prec FUNCTION_ARGLIST
	;

function_arglist_bare:
	EXPECT_NO_MORE_ARGS {
		$$ = SCM_EOL;
	}
	| EXPECT_PITCH function_arglist_optional pitch_also_in_chords {
		$$ = scm_cons ($3, $2);
	}
	| EXPECT_DURATION function_arglist_closed_optional duration_length {
		$$ = scm_cons ($3, $2);
	}
	| EXPECT_OPTIONAL EXPECT_PITCH function_arglist_skip DEFAULT {
		$$ = scm_cons ($1, $3);
	}
	| EXPECT_OPTIONAL EXPECT_DURATION function_arglist_skip DEFAULT {
		$$ = scm_cons ($1, $3);
	}
	| EXPECT_OPTIONAL EXPECT_SCM function_arglist_skip DEFAULT {
		$$ = scm_cons ($1, $3);
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
	CONTEXT simple_string optional_id optional_context_mod {
                Context_mod *ctxmod = unsmob_context_mod ($4);
                SCM mods = SCM_EOL;
                if (ctxmod)
                        mods = ctxmod->get_mods ();
		$$ = START_MAKE_SYNTAX ("context-specification", $2, $3, mods, SCM_BOOL_F);
	}
	| NEWCONTEXT simple_string optional_id optional_context_mod {
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
		parser->lexer_->push_note_state (alist_to_hashq (nn));

		$$ = ly_symbol2scm ("notes");
	}
	| DRUMMODE
		{
		SCM nn = parser->lexer_->lookup_identifier ("drumPitchNames");
		parser->lexer_->push_note_state (alist_to_hashq (nn));

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
		parser->lexer_->push_chord_state (alist_to_hashq (nn));
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
		parser->lexer_->push_note_state (alist_to_hashq (nn));

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
		parser->lexer_->push_chord_state (alist_to_hashq (nn));
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


property_path_revved:
	embedded_scm_closed {
		$$ = scm_cons ($1, SCM_EOL);
	}
	| property_path_revved embedded_scm_closed {
		$$ = scm_cons ($2, $1);
	}
	;

property_path:
	property_path_revved  {
		$$ = scm_reverse_x ($1, SCM_EOL);
	}
	;

property_operation:
	STRING '=' scalar {
		$$ = scm_list_3 (ly_symbol2scm ("assign"),
			scm_string_to_symbol ($1), $3);
	}
	| UNSET simple_string {
		$$ = scm_list_2 (ly_symbol2scm ("unset"),
			scm_string_to_symbol ($2));
	}
	| OVERRIDE simple_string property_path '=' scalar {
		$$ = scm_append (scm_list_2 (scm_list_3 (ly_symbol2scm ("push"),
							scm_string_to_symbol ($2), $5),
					     $3));
	}
	| REVERT simple_string embedded_scm {
		$$ = scm_list_3 (ly_symbol2scm ("pop"),
			scm_string_to_symbol ($2), $3);
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
	| context_def_mod embedded_scm {
	   if (ly_symbol2scm ("consists") != $1)
	   {
	     $$ = SCM_EOL;
             parser->parser_error (@1, _ ("only \\consists takes non-string argument."));
	   }
	   else
	   {
 	     $$ = scm_list_2 ($1, $2);
	   }
	}
	;

context_prop_spec:
	simple_string {
		if (!is_regular_identifier ($1))
		{
			@$.error (_("Grob name should be alphanumeric"));
		}

		$$ = scm_list_2 (ly_symbol2scm ("Bottom"),
			scm_string_to_symbol ($1));
	}
	| simple_string '.' simple_string {
		$$ = scm_list_2 (scm_string_to_symbol ($1),
			scm_string_to_symbol ($3));
	}
	;

simple_music_property_def:
	OVERRIDE context_prop_spec property_path '=' scalar {
		$$ = scm_append (scm_list_2 (scm_list_n (scm_car ($2),
				ly_symbol2scm ("OverrideProperty"),
				scm_cadr ($2),
				$5, SCM_UNDEFINED),
				$3));
	}
	| REVERT context_prop_spec embedded_scm {
		$$ = scm_list_4 (scm_car ($2),
			ly_symbol2scm ("RevertProperty"),
			scm_cadr ($2),
			$3);
	}
	| SET context_prop_spec '=' scalar {
		$$ = scm_list_4 (scm_car ($2),
			ly_symbol2scm ("PropertySet"),
			scm_cadr ($2),
			$4);
	}
	| UNSET context_prop_spec {
		$$ = scm_list_3 (scm_car ($2),
			ly_symbol2scm ("PropertyUnset"),
			scm_cadr ($2));
	}
	;

music_property_def:
	simple_music_property_def {
		$$ = LOWLEVEL_MAKE_SYNTAX (ly_lily_module_constant ("property-operation"), scm_cons2 (parser->self_scm (), make_input (@$), $1));
	}
	;

string:
	STRING {
		$$ = $1;
	}
	| STRING_IDENTIFIER {
		$$ = $1;
	}
	| string '+' string {
		$$ = scm_string_append (scm_list_2 ($1, $3));
	}
	;

simple_string: STRING {
		$$ = $1;
	}
	| LYRICS_STRING {
		$$ = $1;
	}
	| STRING_IDENTIFIER {
		$$ = $1;
	}
	;

scalar:
	embedded_scm_arg
	| bare_number
	| lyric_element
	;

scalar_closed:
	embedded_scm_arg_closed
	| bare_number
	| lyric_element
	;


event_chord:
	simple_element post_events {
		// Let the rhythmic music iterator sort this mess out.
		if (scm_is_pair ($2))
			unsmob_music ($1)->set_property ("articulations",
							 scm_reverse_x ($2, SCM_EOL));
	}
	| simple_chord_elements post_events	{
		SCM elts = ly_append2 ($1, scm_reverse_x ($2, SCM_EOL));

		Input i;
		/* why is this giving wrong start location? -ns
		 * i = @$; */
		i.set_location (@1, @2);
		$$ = MAKE_SYNTAX ("event-chord", i, elts);
	}
	| CHORD_REPETITION optional_notemode_duration post_events {
		Input i;
		i.set_location (@1, @3);
		$$ = MAKE_SYNTAX ("repetition-chord", i,
				  parser->lexer_->chord_repetition_.last_chord_,
				  parser->lexer_->chord_repetition_.repetition_function_,
				  $2, scm_reverse_x ($3, SCM_EOL));
	}
	| MULTI_MEASURE_REST optional_notemode_duration post_events {
		Input i;
		i.set_location (@1, @3);
		$$ = MAKE_SYNTAX ("multi-measure-rest", i, $2,
				  scm_reverse_x ($3, SCM_EOL));
	}
	| command_element
	/* note chord elements are memorized into
	   parser->lexer_->chord_repetition_ so that the chord repetition
	   mechanism copy them when a chord repetition symbol is found
	*/
	| note_chord_element	{
		parser->lexer_->chord_repetition_.last_chord_ = $$;
	}
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
	}
	;

chord_body:
	ANGLE_OPEN chord_body_elements ANGLE_CLOSE
	{
		$$ = MAKE_SYNTAX ("event-chord", @$, scm_reverse_x ($2, SCM_EOL));
	}
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
		int q = $3;
		int ex = $2;
		SCM check = $4;
		SCM post = $5;

		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
		n->set_property ("pitch", $1);
		if (q % 2)
			n->set_property ("cautionary", SCM_BOOL_T);
		if (ex % 2 || q % 2)
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

music_function_event:
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
	| E_BRACKET_OPEN {
		Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$);
		m->set_property ("span-direction", scm_from_int (START));
		$$ = m->unprotect();
	}
	| E_BRACKET_CLOSE {
		Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$);
		m->set_property ("span-direction", scm_from_int (STOP));
		$$ = m->unprotect ();
	}
	| E_BACKSLASH {
		$$ = MAKE_SYNTAX ("voice-separator", @$);
	}
	| '|'      {
		SCM pipe = parser->lexer_->lookup_identifier ("pipeSymbol");

		Music *m = unsmob_music (pipe);
		if (m)
		{
			m = m->clone ();
			m->set_spot (@$);
			$$ = m->unprotect ();
		}
		else
			$$ = MAKE_SYNTAX ("bar-check", @$);

	}
	;

command_event:
	E_TILDE {
		$$ = MY_MAKE_MUSIC ("PesOrFlexaEvent", @$)->unprotect ();
	}
	| tempo_event {
		$$ = $1;
	}
	;


post_events:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| post_events post_event {
		unsmob_music ($2)->set_spot (@2);
		$$ = scm_cons ($2, $$);
	}
	;

post_event_nofinger:
	direction_less_event {
		$$ = $1;
	}
	| script_dir music_function_event {
		$$ = $2;
		if ($1)
		{
			unsmob_music ($$)->set_property ("direction", scm_from_int ($1));
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
		if ($1)
		{
			Music *m = unsmob_music ($2);
			m->set_property ("direction", scm_from_int ($1));
		}
		$$ = $2;
	}
	| script_dir direction_less_event {
		if ($1)
		{
			Music *m = unsmob_music ($2);
			m->set_property ("direction", scm_from_int ($1));
		}
		$$ = $2;
	}
	| string_number_event
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
		s->set_property ("string-number", scm_from_int ($1));
		$$ = s->unprotect ();
	}
	;

direction_less_char:
	'['  {
		$$ = ly_symbol2scm ("bracketOpenSymbol");
	}
	| ']'  {
		$$ = ly_symbol2scm ("bracketCloseSymbol");
	}
	| '~'  {
		$$ = ly_symbol2scm ("tildeSymbol");
	}
	| '('  {
		$$ = ly_symbol2scm ("parenthesisOpenSymbol");
	}
	| ')'  {
		$$ = ly_symbol2scm ("parenthesisCloseSymbol");
	}
	| E_EXCLAMATION  {
		$$ = ly_symbol2scm ("escapedExclamationSymbol");
	}
	| E_OPEN  {
		$$ = ly_symbol2scm ("escapedParenthesisOpenSymbol");
	}
	| E_CLOSE  {
		$$ = ly_symbol2scm ("escapedParenthesisCloseSymbol");
	}
	| E_ANGLE_CLOSE  {
		$$ = ly_symbol2scm ("escapedBiggerSymbol");
	}
	| E_ANGLE_OPEN  {
		$$ = ly_symbol2scm ("escapedSmallerSymbol");
	}
	;

direction_less_event:
	direction_less_char {
		SCM predefd = parser->lexer_->lookup_identifier_symbol ($1);
		Music *m = 0;
		if (unsmob_music (predefd))
		{
			m = unsmob_music (predefd)->clone ();
			m->set_spot (@$);
		}
		else
		{
			m = MY_MAKE_MUSIC ("Music", @$);
		}
		$$ = m->unprotect ();
	}
	| EVENT_IDENTIFIER	{
		$$ = $1;
	}
	| tremolo_type  {
               Music *a = MY_MAKE_MUSIC ("TremoloEvent", @$);
               a->set_property ("tremolo-type", scm_from_int ($1));
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
	| '='  { $$ = scm_from_int (0); }
	| '=' sub_quotes { $$ = scm_from_int (-$2); }
	| '=' sup_quotes { $$ = scm_from_int ($2); }
	;

sup_quotes:
	'\'' {
		$$ = 1;
	}
	| sup_quotes '\'' {
		$$ ++;
	}
	;

sub_quotes:
	',' {
		$$ = 1;
	}
	| sub_quotes ',' {
		$$++;
	}
	;

steno_pitch:
	NOTENAME_PITCH	{
		$$ = $1;
	}
	| NOTENAME_PITCH sup_quotes 	{
		Pitch p = *unsmob_pitch ($1);
		p = p.transposed (Pitch ($2,0,0));
		$$ = p.smobbed_copy ();
	}
	| NOTENAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);
		p = p.transposed (Pitch (-$2,0,0));
		$$ = p.smobbed_copy ();
	}
	;

/*
ugh. duplication
*/

steno_tonic_pitch:
	TONICNAME_PITCH	{
		$$ = $1;
	}
	| TONICNAME_PITCH sup_quotes 	{
		Pitch p = *unsmob_pitch ($1);
		p = p.transposed (Pitch ($2,0,0));
		$$ = p.smobbed_copy ();
	}
	| TONICNAME_PITCH sub_quotes	 {
		Pitch p = *unsmob_pitch ($1);

		p = p.transposed (Pitch (-$2,0,0));
		$$ = p.smobbed_copy ();
	}
	;

pitch:
	steno_pitch {
		$$ = $1;
	}
	| PITCH_IDENTIFIER
	;

pitch_also_in_chords:
	pitch
	| steno_tonic_pitch
	;

gen_text_def:
	full_markup {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		t->set_property ("text", $1);
		$$ = t->unprotect ();
	}
	| simple_string {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
		t->set_property ("text",
			make_simple_markup ($1));
		$$ = t->unprotect ();
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
 	| '|'		{
		$$ = scm_from_locale_string ("Bar");
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
	'_'	{ $$ = DOWN; }
	| '^'	{ $$ = UP; }
	| '-'	{ $$ = CENTER; }
	;

duration_length:
	multiplied_duration {
		$$ = $1;
	}
	;

optional_notemode_duration:
	{
		Duration dd = parser->default_duration_;
		$$ = dd.smobbed_copy ();
	}
	| multiplied_duration	{
		$$ = $1;
		parser->default_duration_ = *unsmob_duration ($$);
	}
	;

steno_duration:
	bare_unsigned dots		{
		int len = 0;
		if (!is_duration ($1))
			parser->parser_error (@1, _f ("not a duration: %d", $1));
		else
			len = intlog2 ($1);

		$$ = Duration (len, $2).smobbed_copy ();
	}
	| DURATION_IDENTIFIER dots	{
		Duration *d = unsmob_duration ($1);
		Duration k (d->duration_log (), d->dot_count () + $2);
		k = k.compressed (d->factor ());
		*d = k;
		$$ = $1;
	}
	;

multiplied_duration:
	steno_duration {
		$$ = $1;
	}
	| multiplied_duration '*' bare_unsigned {
		$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
	}
	| multiplied_duration '*' FRACTION {
		Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3)));

		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
	}
	;

fraction:
	FRACTION { $$ = $1; }
	| UNSIGNED '/' UNSIGNED {
		$$ = scm_cons ($1, $3);
	}
	;

dots:
	/* empty */ 	{
		$$ = 0;
	}
	| dots '.' {
		$$ ++;
	}
	;

tremolo_type:
	':'	{
		$$ = 0;
	}
	| ':' bare_unsigned {
		if (!is_duration ($2))
			parser->parser_error (@2, _f ("not a duration: %d", $2));
		$$ = $2;
	}
	;

bass_number:
	UNSIGNED { $$ = $1; }
	| STRING { $$ = $1; }
	| full_markup { $$ = $1; }
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
		if ($2 == ly_symbol2scm ("plus"))
			{
			m->set_property ("augmented", SCM_BOOL_T);
			}
		else if ($2 == ly_symbol2scm ("slash"))
			{
			m->set_property ("diminished", SCM_BOOL_T);
			}
		else if ($2 == ly_symbol2scm ("exclamation"))
			{
			m->set_property ("no-continuation", SCM_BOOL_T);
			}
		else if ($2 == ly_symbol2scm ("backslash"))
			{
			m->set_property ("augmented-slash", SCM_BOOL_T);
			}
	}
	;


figured_bass_modification:
	E_PLUS		{
		$$ = ly_symbol2scm ("plus");
	}
	| E_EXCLAMATION {
		$$ = ly_symbol2scm ("exclamation");
	}
	| '/'		{
		$$ = ly_symbol2scm ("slash");
	}
	| E_BACKSLASH {
		$$ = ly_symbol2scm ("backslash");
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
	/**/   { $$ = 0; }
	| REST { $$ = 1; }
	;

simple_element:
	pitch exclamations questions octave_check optional_notemode_duration optional_rest {
		if (!parser->lexer_->is_note_state ())
			parser->parser_error (@1, _ ("have to be in Note mode for notes"));

		Music *n = 0;
		if ($6)
			n = MY_MAKE_MUSIC ("RestEvent", @$);
		else
			n = MY_MAKE_MUSIC ("NoteEvent", @$);

		n->set_property ("pitch", $1);
		n->set_property ("duration", $5);

		if (scm_is_number ($4))
		{
			int q = scm_to_int ($4);
			n->set_property ("absolute-octave", scm_from_int (q-1));
		}

		if ($3 % 2)
			n->set_property ("cautionary", SCM_BOOL_T);
		if ($2 % 2 || $3 % 2)
			n->set_property ("force-accidental", SCM_BOOL_T);

		$$ = n->unprotect ();
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
	lyric_markup {
		$$ = $1;
	}
	| LYRICS_STRING {
		$$ = $1;
	}
	;

lyric_element_arg:
	lyric_element
	| lyric_element multiplied_duration post_events {
		$$ = MAKE_SYNTAX ("lyric-event", @$, $1, $2);
		if (scm_is_pair ($3))
			unsmob_music ($$)->set_property
				("articulations", scm_reverse_x ($3, SCM_EOL));
	}
	| lyric_element post_event post_events {
		$$ = MAKE_SYNTAX ("lyric-event", @$, $1,
				  parser->default_duration_.smobbed_copy ());
		unsmob_music ($$)->set_property
			("articulations", scm_cons ($2, scm_reverse_x ($3, SCM_EOL)));
	}
	| LYRIC_ELEMENT optional_notemode_duration post_events {
		$$ = MAKE_SYNTAX ("lyric-event", @$, $1, $2);
		if (scm_is_pair ($3))
			unsmob_music ($$)->set_property
				("articulations", scm_reverse_x ($3, SCM_EOL));
	}
	;


lyric_element_music:
	lyric_element optional_notemode_duration post_events {
		$$ = MAKE_SYNTAX ("lyric-event", @$, $1, $2);
		if (scm_is_pair ($3))
			unsmob_music ($$)->set_property
				("articulations", scm_reverse_x ($3, SCM_EOL));
	}
	;

new_chord:
	steno_tonic_pitch optional_notemode_duration   {
		$$ = make_chord_elements ($1, $2, SCM_EOL);
	}
	| steno_tonic_pitch optional_notemode_duration chord_separator chord_items {
		SCM its = scm_reverse_x ($4, SCM_EOL);
		$$ = make_chord_elements ($1, $2, scm_cons ($3, its));
	}
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
	bare_unsigned {
		$$ = make_chord_step ($1, 0);
        }
	| bare_unsigned '+' {
		$$ = make_chord_step ($1, SHARP_ALTERATION);
	}
	| bare_unsigned CHORD_MINUS {
		$$ = make_chord_step ($1, FLAT_ALTERATION);
	}
	;

tempo_range:
	bare_unsigned {
		$$ = scm_from_int ($1);
	}
	| bare_unsigned '~' bare_unsigned {
		$$ = scm_cons (scm_from_int ($1), scm_from_int ($3));
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


bare_number:
	bare_number_closed
	| UNSIGNED NUMBER_IDENTIFIER	{
		$$ = scm_product ($1, $2);
	}
	| REAL NUMBER_IDENTIFIER	{
		$$ = scm_product ($1, $2);
	}
	;

bare_number_closed:
	UNSIGNED
	| REAL
	| NUMBER_IDENTIFIER
	;

bare_unsigned:
	UNSIGNED {
		$$ = scm_to_int ($1);
	}
	;

unsigned_number:
	UNSIGNED
	| NUMBER_IDENTIFIER
	;

exclamations:
		{ $$ = 0; }
	| exclamations '!'	{ $$ ++; }
	;

questions:
		{ $$ = 0; }
	| questions '?'	{ $$ ++; }
	;

/*
This should be done more dynamically if possible.
*/

lyric_markup:
	LYRIC_MARKUP_IDENTIFIER {
		$$ = $1;
	}
	| LYRIC_MARKUP
		{ parser->lexer_->push_markup_state (); }
	markup_top {
		$$ = $3;
		parser->lexer_->pop_state ();
	}
	;

full_markup_list:
	MARKUPLIST_IDENTIFIER {
		$$ = $1;
	}
	| MARKUPLIST
		{ parser->lexer_->push_markup_state (); }
	markup_list {
		$$ = $3;
		parser->lexer_->pop_state ();
	}
	;

full_markup:
	MARKUP_IDENTIFIER {
		$$ = $1;
	}
	| MARKUP
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
	| markup_head_1_list simple_markup	{
		$$ = scm_car (scm_call_2 (ly_lily_module_constant ("map-markup-command-list"), $1, scm_list_1 ($2)));
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
	MARKUPLIST_IDENTIFIER {
		$$ = $1;
	}
	| markup_composed_list {
		$$ = $1;
	}
	| markup_braced_list {
		$$ = $1;
	}
	| markup_command_list {
		$$ = scm_list_1 ($1);
	}
	| markup_scm MARKUPLIST_IDENTIFIER
	{
		$$ = $2;
	}
	;

markup_composed_list:
	markup_head_1_list markup_braced_list {
		$$ = scm_call_2 (ly_lily_module_constant ("map-markup-command-list"), $1, $2);

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
	| MARKUP_IDENTIFIER {
		$$ = $1;
	}
	| LYRIC_MARKUP_IDENTIFIER {
		$$ = $1;
	}
	| STRING_IDENTIFIER {
		$$ = $1;
	}
	| SCORE {
		SCM nn = parser->lexer_->lookup_identifier ("pitchnames");
		parser->lexer_->push_note_state (alist_to_hashq (nn));
	} '{' score_body '}' {
		Score * sc = $4;
		$$ = scm_list_2 (ly_lily_module_constant ("score-markup"), sc->self_scm ());
		sc->unprotect ();
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
	markup_head_1_list simple_markup	{
		SCM mapper = ly_lily_module_constant ("map-markup-command-list");
		$$ = scm_car (scm_call_2 (mapper, $1, scm_list_1 ($2)));
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

void
Lily_parser::do_yyparse ()
{
	yyparse (this);
}





/*

It is a little strange to have this function in this file, but
otherwise, we have to import music classes into the lexer.

*/
int
Lily_lexer::try_special_identifiers (SCM *destination, SCM sid)
{
	if (scm_is_string (sid)) {
		*destination = sid;
		return STRING_IDENTIFIER;
	} else if (unsmob_book (sid)) {
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
	} else if (unsmob_score (sid)) {
		Score *score = new Score (*unsmob_score (sid));
		*destination = score->self_scm ();

		score->unprotect ();
		return SCORE_IDENTIFIER;
	} else if (Music *mus = unsmob_music (sid)) {
		mus = mus->clone ();
		*destination = mus->self_scm ();
		unsmob_music (*destination)->
			set_property ("origin", make_input (last_input_));

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
	} else if (Text_interface::is_markup (sid)) {
		*destination = sid;
		if (is_lyric_state ())
			return LYRIC_MARKUP_IDENTIFIER;
		return MARKUP_IDENTIFIER;
	} else if (Text_interface::is_markup_list (sid)) {
		*destination = sid;
		return MARKUPLIST_IDENTIFIER;
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

SCM check_scheme_arg (Lily_parser *parser, Input loc,
		      SCM arg, SCM args, SCM pred)
{
	args = scm_cons (arg, args);
	if (scm_is_true (scm_call_1 (pred, arg)))
		return args;
	scm_set_cdr_x (scm_last_pair (args), SCM_EOL);
	MAKE_SYNTAX ("argument-error", loc, scm_length (args), pred, arg);
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

bool
is_regular_identifier (SCM id)
{
  string str = ly_scm2string (id);
  char const *s = str.c_str ();

  bool v = true;
#if 0
  isalpha (*s);
  s++;
#endif
  while (*s && v)
   {
        v = v && isalnum (*s);
        s++;
   }
  return v;
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

bool
is_duration (int t)
{
  return t && t == 1 << intlog2 (t);
}

void
set_music_properties (Music *p, SCM a)
{
  for (SCM k = a; scm_is_pair (k); k = scm_cdr (k))
 	p->set_property (scm_caar (k), scm_cdar (k));
}


SCM
make_chord_step (int step, Rational alter)
{
	if (step == 7)
		alter += FLAT_ALTERATION;

	while (step < 0)
		step += 7;
	Pitch m ((step -1) / 7, (step - 1) % 7, alter);
	return m.smobbed_copy ();
}


SCM
make_chord_elements (SCM pitch, SCM dur, SCM modification_list)
{
	SCM chord_ctor = ly_lily_module_constant ("construct-chord-elements");
	return scm_call_3 (chord_ctor, pitch, dur, modification_list);
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
