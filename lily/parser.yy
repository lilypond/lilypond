%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for lilypond

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  Two shift/reduce problems:
    -
    -
 */

/*

the rules for who is protecting what are very shady. TODO: uniformise
this.


*/

#include <ctype.h>

#include "translator-def.hh"
#include "lily-guile.hh"
#include "change-iterator.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "file-path.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "my-lily-parser.hh"
#include "context-specced-music.hh"
#include "score.hh"
#include "music-list.hh"
#include "output-property-music-iterator.hh"
#include "property-iterator.hh"
#include "input-file-results.hh"
#include "input.hh"
#include "relative-music.hh"
#include "lyric-combine-music.hh"
#include "transposed-music.hh"
#include "time-scaled-music.hh"
#include "repeated-music.hh"
#include "untransposable-music.hh"
#include "lilypond-input-version.hh"
#include "grace-music.hh"
#include "part-combine-music.hh"
#include "scm-hash.hh"
#include "auto-change-iterator.hh"
#include "un-relativable-music.hh"
#include "chord.hh"

bool
regular_identifier_b (SCM id)
{
  String str = ly_scm2string (id);
  char const *s = str.to_str0 () ;

  bool v = true;
  while (*s && v)
   {
        v = v && isalpha (*s);
        s++;
   }
  return v;
}


Music* 
set_property_music (SCM sym, SCM value)
{
	Music * p = new Music (SCM_EOL);
	p->set_mus_property ("symbol", sym);
	p->set_mus_property ("iterator-ctor",
	Property_iterator::constructor_cxx_function);

	p->set_mus_property ("value", value);
	return p;
}

bool
is_duration_b (int t)
{
  return t && t == 1 << intlog2 (t);
}

void
set_music_properties (Music *p, SCM a)
{
  for (SCM k = a; gh_pair_p (k); k = ly_cdr (k))
	{
	p->internal_set_mus_property (ly_caar (k), ly_cdar (k));
	}
}







// needed for bison.simple's malloc () and free ()

#include <malloc.h>
#include <stdlib.h>



#define YYERROR_VERBOSE 1

#define YYPARSE_PARAM my_lily_parser
#define YYLEX_PARAM my_lily_parser
#define THIS\
	((My_lily_parser *) my_lily_parser)

#define yyerror THIS->parser_error

%}

/* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesnt trigger a move
of the parse stack onto the heap. */


%union {

    Link_array<Request> *reqvec;

    String *string; // needed by the lexer as temporary scratch area.
    Music *music;
    Score *score;
    Scheme_hash_table *scmhash;
    Music_output_def * outputdef;

    Request * request;

    SCM scm;

    Tempo_req *tempo;
    int i;
}
%{

int
yylex (YYSTYPE *s,  void * v)
{
	My_lily_parser	 *pars = (My_lily_parser*) v;
	My_lily_lexer * lex = pars->lexer_;

	lex->lexval = (void*) s;
	return lex->yylex ();
}


%}

%pure_parser

/* tokens which are not keywords */
%token AUTOCHANGE
%token ALIAS
%token APPLY
%token ARPEGGIO
%token DYNAMICSCRIPT
%token ACCEPTS
%token ALTERNATIVE
%token BAR
%token BREATHE
%token CHORDMODIFIERS
%token CHORDS
%token CHAR_T
%token CLEF
%token CM_T
%token CONSISTS
%token DURATION
%token SEQUENTIAL
%token GROBDESCRIPTIONS
%token SIMULTANEOUS
%token CONSISTSEND
%token DENIES
%token DURATION
%token EXTENDER
%token FIGURES FIGURE_OPEN FIGURE_CLOSE
%token FIGURE_BRACKET_CLOSE FIGURE_BRACKET_OPEN
%token GLISSANDO
%token GRACE 
%token HEADER
%token HYPHEN
%token IN_T
%token INVALID
%token KEY
%token LYRICS
%token MARK
%token MULTI_MEASURE_REST
%token MIDI
%token MM_T
%token PITCH
%token DEFAULT
%token NAME
%token PITCHNAMES
%token NOTES
%token PAPER
%token PARTIAL
%token PENALTY
%token PROPERTY
%token OVERRIDE SET REVERT 
%token PT_T
%token RELATIVE
%token REMOVE
%token REPEAT
%token ADDLYRICS
%token PARTCOMBINE
%token SCM_T
%token SCORE
%token SCRIPT
%token SKIP
%token SPANREQUEST
%token STYLESHEET
%token COMMANDSPANREQUEST
%token TEMPO
%token OUTPUTPROPERTY
%token TIME_T
%token TIMES
%token TRANSLATOR
%token TRANSPOSE
%token TYPE
%token UNSET
%token CONTEXT
%token REST

/* escaped */
%token E_CHAR E_EXCLAMATION E_SMALLER E_BIGGER E_OPEN E_CLOSE
%token E_LEFTSQUARE E_RIGHTSQUARE E_TILDE
%token E_BACKSLASH
%token CHORD_BASS CHORD_COLON CHORD_MINUS CHORD_CARET
%token FIGURE_SPACE


%type <i>	exclamations questions dots optional_rest
%type <i>  	bass_number bass_mod
%type <scm> 	br_bass_figure bass_figure figure_list figure_spec
%token <i>	DIGIT
%token <scm>	NOTENAME_PITCH
%token <scm>	TONICNAME_PITCH
%token <scm>	CHORDMODIFIER_PITCH
%token <scm>	DURATION_IDENTIFIER
%token <scm>    FRACTION
%token <id>	IDENTIFIER


%token <scm>	SCORE_IDENTIFIER
%token <scm>	MUSIC_OUTPUT_DEF_IDENTIFIER

%token <scm>	NUMBER_IDENTIFIER
%token <scm>	REQUEST_IDENTIFIER
%token <scm>	MUSIC_IDENTIFIER TRANSLATOR_IDENTIFIER
%token <scm>	STRING_IDENTIFIER SCM_IDENTIFIER 
%token <scm>	RESTNAME
%token <scm>	STRING   
%token <scm>	SCM_T
%token <i>	UNSIGNED
%token <scm>   REAL

%type <outputdef> output_def
%type <scmhash> 	lilypond_header lilypond_header_body
%type <request>	open_request_parens close_request_parens open_request close_request
%type <request> request_with_dir request_that_take_dir verbose_request
%type <i>	sub_quotes sup_quotes
%type <music>	simple_element  request_chord command_element Simple_music  Composite_music 
%type <music>	Repeated_music
%type <scm>     Alternative_music
%type <i>	tremolo_type
%type <i>	bare_int  bare_unsigned
%type <i>	script_dir

%type <scm>	identifier_init 

%type <scm> steno_duration optional_notemode_duration multiplied_duration
%type <scm>  verbose_duration
	
%type <reqvec>  pre_requests post_requests
%type <request> gen_text_def
%type <scm>   steno_pitch pitch absolute_pitch
%type <scm>   explicit_pitch steno_tonic_pitch

%type <scm>	chord_additions chord_subtractions chord_notes chord_step
%type <music>	chord
%type <scm>	chord_note chord_inversion chord_bass
%type <scm>	duration_length fraction

%type <scm>  embedded_scm scalar
%type <music>	Music Sequential_music Simultaneous_music 
%type <music>	relative_music re_rhythmed_music part_combined_music
%type <music>	property_def translator_change
%type <scm> Music_list
%type <outputdef>  music_output_def_body
%type <request> shorthand_command_req
%type <request>	post_request 
%type <music> command_req verbose_command_req
%type <request>	extender_req
%type <request> hyphen_req
%type <scm>	string bare_number number_expression number_term number_factor 

%type <score>	score_block score_body

%type <scm>	translator_spec_block translator_spec_body
%type <tempo> 	tempo_request
%type <scm> notenames_body notenames_block chordmodifiers_block
%type <scm>	script_abbreviation



%left '-' '+'

/* We don't assign precedence to / and *, because we might need varied
prec levels in different prods */

%left UNARY_MINUS

%%

lilypond:	/* empty */
	| lilypond toplevel_expression {}
	| lilypond assignment  { }
	| lilypond error {
		THIS->error_level_  = 1;
	}
	| lilypond INVALID	{
		THIS->error_level_  = 1;
	}
	;

toplevel_expression:
	notenames_block			{
		THIS->lexer_->pitchname_tab_ =  $1;
	}
	| chordmodifiers_block			{
		THIS->lexer_->chordmodifier_tab_  = $1;
	}
	| lilypond_header {
		if (THIS->input_file_->header_)
			scm_gc_unprotect_object (THIS->input_file_->header_->self_scm ());
		THIS->input_file_->header_ = $1;
	}
	| score_block {
		THIS->input_file_->scores_.push ($1);
	}
	| output_def {
		if (dynamic_cast<Paper_def*> ($1))
			THIS->lexer_->set_identifier (scm_makfrom0str ("$defaultpaper"), $1->self_scm ());
		else if (dynamic_cast<Midi_def*> ($1))
			THIS->lexer_->set_identifier (scm_makfrom0str ("$defaultmidi"), $1->self_scm ());
	}
	| embedded_scm {
		// junk value
	}	
	;

embedded_scm:
	SCM_T
	| SCM_IDENTIFIER 
	;


chordmodifiers_block:
	CHORDMODIFIERS notenames_body   {  $$ = $2; }
	;

notenames_block:
	PITCHNAMES notenames_body   {  $$ = $2; }
	;

notenames_body:
	embedded_scm	{
	  int i = scm_ilength ($1);

	  SCM tab = scm_make_vector (gh_int2scm (i), SCM_EOL);
	  for (SCM s = $1; gh_pair_p (s); s = ly_cdr (s)) {
		SCM pt = ly_cdar (s);
		if (!unsmob_pitch (pt))
			THIS->parser_error ("Need pitch object.");
		else
			scm_hashq_set_x (tab, ly_caar (s), pt);
	  }

	  $$ = tab;
	}
	;

lilypond_header_body:
	{
		$$ = new Scheme_hash_table;
		THIS->lexer_-> scopes_.push ($$);
	}
	| lilypond_header_body assignment  { 

	}
	;

lilypond_header:
	HEADER '{' lilypond_header_body '}'	{
		$$ = $3;
		THIS->lexer_->scopes_.pop ();
	}
	;


/*
	DECLARATIONS
*/
assignment:
	STRING {
		THIS->push_spot ();
	}
	/* cont */ '=' identifier_init  {

	/*
		Should find generic way of associating input with objects.
	*/
		Input ip = THIS->pop_spot ();

		if (! regular_identifier_b ($1))
		{
			ip.warning (_ ("Identifier should have alphabetic characters only"));
		}

	        THIS->lexer_->set_identifier ($1, $4);

/*
 TODO: devise standard for protection in parser.

  The parser stack lives on the C-stack, which means that
all objects can be unprotected as soon as they're here.

*/
	}
	;



identifier_init:
	score_block {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| output_def {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| translator_spec_block {
		$$ = $1;
	}
	| Music  {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| post_request {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| verbose_duration {
		$$ = $1;
	}
	| number_expression {
		$$ = $1;
	}
	| string {
		$$ = $1;
	}
	| embedded_scm	{
		$$ = $1;
	}
	;

translator_spec_block:
	TRANSLATOR '{' translator_spec_body '}'
		{
		$$ = $3;
	}
	;

translator_spec_body:
	TRANSLATOR_IDENTIFIER	{
		$$ = unsmob_translator_def ($1)->clone_scm ();
		unsmob_translator_def ($$)-> set_spot (THIS->here_input ());
	}
	| TYPE STRING 	{
		$$ = Translator_def::make_scm ();
		Translator_def*td =  unsmob_translator_def ($$);
		td->translator_group_type_ = $2;
		td->set_spot (THIS->here_input ());
	}
	| translator_spec_body STRING '=' embedded_scm			{
		unsmob_translator_def ($$)->add_property_assign ($2, $4);
	}
	| translator_spec_body STRING OVERRIDE embedded_scm '=' embedded_scm {
		unsmob_translator_def ($$)
			->add_push_property (scm_string_to_symbol ($2), $4, $6);
	}
	| translator_spec_body STRING SET embedded_scm '=' embedded_scm {
		unsmob_translator_def ($$)
			->add_push_property (scm_string_to_symbol ($2), $4, $6);
	}
	| translator_spec_body STRING REVERT embedded_scm  {
	  unsmob_translator_def ($$)->add_pop_property (
		scm_string_to_symbol ($2), $4);
	}
	| translator_spec_body NAME STRING  {
		unsmob_translator_def ($$)->type_name_ = $3;
	}
	| translator_spec_body CONSISTS STRING  {
		unsmob_translator_def ($$)->add_element ($3);
	}
	| translator_spec_body ALIAS STRING  {
		Translator_def*td = unsmob_translator_def ($$);
		td->type_aliases_ = gh_cons ($3, td->type_aliases_);
	}
	| translator_spec_body GROBDESCRIPTIONS embedded_scm {
		Translator_def*td = unsmob_translator_def($$);
		// td->add_property_assign (ly_symbol2scm ("allGrobDescriptions"), $3);
		for (SCM p = $3; gh_pair_p (p); p = ly_cdr (p))
			td->add_property_assign (scm_symbol_to_string (ly_caar (p)), ly_cdar (p));
	}
	| translator_spec_body CONSISTSEND STRING  {
		unsmob_translator_def ($$)->add_last_element ( $3);
	}
	| translator_spec_body ACCEPTS STRING  {
		unsmob_translator_def ($$)->set_acceptor ($3,true);
	}
	| translator_spec_body DENIES STRING  {
		unsmob_translator_def ($$)->set_acceptor ($3,false);
	}
	| translator_spec_body REMOVE STRING  {
		unsmob_translator_def ($$)->remove_element ($3);
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { 
		THIS->push_spot ();
	}
	/*cont*/ '{' score_body '}' 	{
		THIS->pop_spot ();
		$$ = $4;
		if (!$$->defs_.size ())
		{
		  Music_output_def *id =
			unsmob_music_output_def (THIS->lexer_->lookup_identifier ("$defaultpaper"));
		  $$->add_output (id ? id->clone () :  new Paper_def );
		}
	}
	;

score_body:
	Music	{
		$$ = new Score;
	
		$$->set_spot (THIS->here_input ());
		SCM m = $1->self_scm ();
		scm_gc_unprotect_object (m);

		/*
			guh.
		*/
		SCM check_funcs = scm_c_eval_string ("toplevel-music-functions");
		for (; gh_pair_p (check_funcs); check_funcs = gh_cdr (check_funcs))
			m = gh_call1 (gh_car (check_funcs), m);
		$$->music_ = m;

	}
	| SCORE_IDENTIFIER {
		$$ = new Score (*unsmob_score ($1));
		$$->set_spot (THIS->here_input ());
	}
	| score_body lilypond_header 	{
		scm_gc_unprotect_object ($2->self_scm ()); 
		$$->header_ = $2;
	}
	| score_body output_def {
		$$->add_output ($2);
	}
	| score_body error {

	}
	;


/*
	MIDI
*/
output_def:
	music_output_def_body '}' {
		$$ = $1;
		THIS-> lexer_-> scopes_.pop ();
	}
	;

music_output_def_body:
	MIDI '{'    {
	   Music_output_def *id = unsmob_music_output_def (THIS->lexer_->lookup_identifier ("$defaultmidi"));

		
	 Midi_def* p =0;
	if (id)
		p = dynamic_cast<Midi_def*> (id->clone ());
	else
		p = new Midi_def;

	 $$ = p;
	 THIS->lexer_->scopes_.push (p->variable_tab_);
	}
	| PAPER '{' 	{
		Music_output_def *id = unsmob_music_output_def (THIS->lexer_->lookup_identifier ("$defaultpaper"));
		  Paper_def *p = 0;
		if (id)
			p = dynamic_cast<Paper_def*> (id->clone ());
		else
			p = new Paper_def;
		THIS-> lexer_-> scopes_.push (p->variable_tab_);
		$$ = p;
	}
	| PAPER '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def *p = unsmob_music_output_def ($3);
		p = p->clone ();
		THIS->lexer_->scopes_.push (p->variable_tab_);
		$$ = p;
	}
	| MIDI '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def *p = unsmob_music_output_def ($3);
		p = p->clone ();

		THIS->lexer_->scopes_.push (p->variable_tab_);
		$$ = p;
	}
	| music_output_def_body assignment  {

	}
	| music_output_def_body translator_spec_block	{
		$$->assign_translator ($2);
	}
	| music_output_def_body STYLESHEET embedded_scm {
		dynamic_cast<Paper_def*> ($$)-> style_sheet_ = $3;
	}
	| music_output_def_body tempo_request  {
		/*
			junk this ? there already is tempo stuff in
			music.
		*/
		int m = gh_scm2int ( $2->get_mus_property ("metronome-count"));
		Duration *d = unsmob_duration ($2->get_mus_property ("duration"));
		Midi_def * md = dynamic_cast<Midi_def*> ($$);
		if (md)
			md->set_tempo (d->length_mom (), m);
	}
	| music_output_def_body error {

	}
	;

tempo_request:
	TEMPO steno_duration '=' bare_unsigned	{
		$$ = new Tempo_req;
		$$->set_mus_property ("duration", $2);
		$$->set_mus_property ("metronome-count", gh_int2scm ( $4));
	}
	;

/*
The representation of a  list is the

  (LIST . LAST-CONS)

 to have  efficient append.
*/
Music_list: /* empty */ {
		$$ = gh_cons (SCM_EOL, SCM_EOL);
	}
	| Music_list Music {
		SCM s = $$;
		SCM c = gh_cons ($2->self_scm (), SCM_EOL);
		scm_gc_unprotect_object ($2->self_scm ()); /* UGH */
		if (gh_pair_p (ly_cdr (s)))
			gh_set_cdr_x (ly_cdr (s), c); /* append */
		else
			gh_set_car_x (s, c); /* set first cons */
		gh_set_cdr_x (s, c) ;  /* remember last cell */ 
	}
	| Music_list error {
	}
	;


Music:
	Simple_music
	| Composite_music
	;

Alternative_music:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| ALTERNATIVE '{' Music_list '}' {
		$$ = $3;
	}
	;

Repeated_music:
	REPEAT string bare_unsigned Music Alternative_music
	{
		Music *beg = $4;
		int times = $3;
		SCM alts = gh_pair_p ($5) ? gh_car ($5) : SCM_EOL;
		if (times < scm_ilength (alts)) {
		  unsmob_music (gh_car (alts))
		    ->origin ()->warning (
		    _("More alternatives than repeats.  Junking excess alternatives."));
		  alts = ly_truncate_list (times, alts);
		}

		Repeated_music * r = new Repeated_music (SCM_EOL);
		if (beg)
			{
			r-> set_mus_property ("element", beg->self_scm ());
			scm_gc_unprotect_object (beg->self_scm ());
			}
		r->set_mus_property ("repeat-count", gh_int2scm (times >? 1));

		r-> set_mus_property ("elements",alts);
		SCM func = scm_primitive_eval (ly_symbol2scm ("repeat-name-to-ctor"));
		SCM result = gh_call1 (func, $2);

		if (gh_equal_p ($2, scm_makfrom0str ("tremolo")))
		{
		/*
		we can not get durations and other stuff correct down the line, so we have to
		add to the duration log here.

		TODO: do dots.
		*/
			SCM func = scm_primitive_eval (ly_symbol2scm ("shift-duration-log"));
			gh_call2 (func, r->self_scm (), gh_int2scm(-intlog2 ($3)));
		}

		set_music_properties (r, result);

		r->set_spot (*$4->origin ());

		$$ = r;
	}
	;

Sequential_music:
	SEQUENTIAL '{' Music_list '}'		{
		$$ = new Sequential_music (SCM_EOL);
		$$->set_mus_property ("elements", ly_car ($3));
		$$->set_spot(THIS->here_input());
	}
	| '{' Music_list '}'		{
		$$ = new Sequential_music (SCM_EOL);
		$$->set_mus_property ("elements", ly_car ($2));
		$$->set_spot(THIS->here_input());
	}
	;

Simultaneous_music:
	SIMULTANEOUS '{' Music_list '}'{
		$$ = new Simultaneous_music (SCM_EOL);
		$$->set_mus_property ("elements", ly_car ($3));
		$$->set_spot(THIS->here_input());

	}
	| '<' Music_list '>'	{
		$$ = new Simultaneous_music (SCM_EOL);
		$$->set_mus_property ("elements", ly_car ($2));
		$$->set_spot(THIS->here_input());
	}
	;

Simple_music:
	request_chord		{ $$ = $1; }
	| OUTPUTPROPERTY embedded_scm embedded_scm '=' embedded_scm	{
		SCM pred = $2;
		if (!gh_symbol_p ($3))
		{
			THIS->parser_error (_ ("Second argument must be a symbol")); 
		}
		/* Should check # args */
		if (!gh_procedure_p (pred))
		{
			THIS->parser_error (_ ("First argument must be a procedure taking one argument"));
		}

		Music *m = new Music (SCM_EOL);
		m->set_mus_property ("predicate", pred);
		m->set_mus_property ("grob-property", $3);
		m->set_mus_property ("grob-value",  $5);
		m->set_mus_property ("iterator-ctor",
		Output_property_music_iterator::constructor_cxx_function);

		$$ = m;
	}
	| MUSIC_IDENTIFIER {
		$$ = unsmob_music ($1)->clone ();

		$$->set_spot (THIS->here_input());
	}
	| property_def
	| translator_change
	;


Composite_music:
	CONTEXT STRING Music	{
		Context_specced_music *csm =  new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", $3->self_scm ());
		scm_gc_unprotect_object ($3->self_scm ());

		csm->set_mus_property ("context-type",$2);
		csm->set_mus_property ("context-id", scm_makfrom0str (""));

		$$ = csm;
	}
	| AUTOCHANGE STRING Music	{
		Music * chm = new Music_wrapper (SCM_EOL);
		chm->set_mus_property ("element", $3->self_scm ());
		chm->set_mus_property ("iterator-ctor", Auto_change_iterator::constructor_cxx_function);

		scm_gc_unprotect_object ($3->self_scm ());
		chm->set_mus_property ("what", $2); 

		$$ = chm;
		chm->set_spot (*$3->origin ());
	}
	| GRACE Music {
#if 1
	/*
		The other version is for easier debugging  of
		Sequential_music_iterator in combination with grace notes.
	*/

		SCM start = THIS->lexer_->lookup_identifier ("startGraceMusic");
		SCM stop = THIS->lexer_->lookup_identifier ("stopGraceMusic");
		Music *startm = unsmob_music (start);
		Music *stopm = unsmob_music (stop);

		SCM ms = SCM_EOL;
		if (stopm) {
			stopm = stopm->clone ();
			ms = gh_cons (stopm->self_scm (), ms);
			scm_gc_unprotect_object (stopm->self_scm ());
		}
		ms = gh_cons ($2->self_scm (), ms);
		scm_gc_unprotect_object ($2->self_scm());
		if (startm) {
			startm = startm->clone ();
			ms = gh_cons (startm->self_scm () , ms);
			scm_gc_unprotect_object (startm->self_scm ());
		}

		Music* seq = new Sequential_music (SCM_EOL);
		seq->set_mus_property ("elements", ms);

		$$ = new Grace_music (SCM_EOL);
		$$->set_mus_property ("element", seq->self_scm ());
		scm_gc_unprotect_object (seq->self_scm ());
#else
		$$ = new Grace_music (SCM_EOL);
		$$->set_mus_property ("element", $2->self_scm ());
		scm_gc_unprotect_object ($2->self_scm ());
#endif
	}
	| CONTEXT string '=' string Music {
		Context_specced_music *csm =  new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", $5->self_scm ());
		scm_gc_unprotect_object ($5->self_scm ());

		csm->set_mus_property ("context-type", $2);
		csm->set_mus_property ("context-id", $4);

		$$ = csm;
	}
	| TIMES {
		THIS->push_spot ();
	}
	/* CONTINUED */ 
		fraction Music 	

	{
		int n = gh_scm2int (ly_car ($3)); int d = gh_scm2int (ly_cdr ($3));
		Music *mp = $4;
		$$ = new Time_scaled_music (SCM_EOL);
		$$->set_spot (THIS->pop_spot ());


		$$->set_mus_property ("element", mp->self_scm ());
		scm_gc_unprotect_object (mp->self_scm ());
		$$->set_mus_property ("numerator", gh_int2scm (n));
		$$->set_mus_property ("denominator", gh_int2scm (d));
		$$->compress (Moment (Rational (n,d)));

	}
	| Repeated_music		{ $$ = $1; }
	| Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	| TRANSPOSE pitch Music {
		$$ = new Transposed_music (SCM_EOL);
		Music *p = $3;
		Pitch pit = *unsmob_pitch ($2);

		p->transpose (pit);
		$$->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());
	}
	| TRANSPOSE steno_tonic_pitch Music {
		$$ = new Transposed_music (SCM_EOL);
		Music *p = $3;
		Pitch pit = *unsmob_pitch ($2);

		p->transpose (pit);
		$$->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());
	
	}
	| APPLY embedded_scm Music  {
		SCM ret = gh_call1 ($2, $3->self_scm ());
		Music *m = unsmob_music (ret);
		if (!m) {
			THIS->parser_error ("\\apply must return a Music");
			m = new Music (SCM_EOL);
			}
		$$ = m;
	}
	| NOTES
		{ THIS->lexer_->push_note_state (); }
	Music
		{ $$ = $3;
		  THIS->lexer_->pop_state ();
		}
	| FIGURES
		{ THIS->lexer_->push_figuredbass_state (); }
	Music
		{
		  Music * chm = new Untransposable_music () ;
		  chm->set_mus_property ("element", $3->self_scm ());
		  $$ = chm;
		  scm_gc_unprotect_object ($3->self_scm());

		  THIS->lexer_->pop_state ();
	}
	| CHORDS
		{ THIS->lexer_->push_chord_state (); }
	Music
		{
		  Music * chm = new Un_relativable_music ;
		  chm->set_mus_property ("element", $3->self_scm ());
		  scm_gc_unprotect_object ($3->self_scm());
		  $$ = chm;

		  THIS->lexer_->pop_state ();
	}
	| LYRICS
		{ THIS->lexer_->push_lyric_state (); }
	Music
		{
		  $$ = $3;
		  THIS->lexer_->pop_state ();
	}
	| relative_music	{ $$ = $1; }
	| re_rhythmed_music	{ $$ = $1; } 
	| part_combined_music	{ $$ = $1; } 
	;

relative_music:
	RELATIVE absolute_pitch Music {
		Music * p = $3;
		Pitch pit = *unsmob_pitch ($2);
		$$ = new Relative_octave_music (SCM_EOL);

		$$->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());

		$$->set_mus_property ("last-pitch", p->to_relative_octave (pit).smobbed_copy ());

	}
	;

re_rhythmed_music:
	ADDLYRICS Music Music {
	  Lyric_combine_music * l = new Lyric_combine_music (SCM_EOL);
	  l->set_mus_property ("elements", gh_list ($2->self_scm (), $3->self_scm (), SCM_UNDEFINED));
	  scm_gc_unprotect_object ($3->self_scm ());
	  scm_gc_unprotect_object ($2->self_scm ());
	  $$ = l;
	}
	;

part_combined_music:
	PARTCOMBINE STRING Music Music {
		Part_combine_music * p = new Part_combine_music (SCM_EOL);

		p->set_mus_property ("what", $2);
		p->set_mus_property ("elements", gh_list ($3->self_scm (),$4->self_scm (), SCM_UNDEFINED));  

		scm_gc_unprotect_object ($3->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());  

		$$ = p;
	}
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Music * t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Change_iterator::constructor_cxx_function);
		t-> set_mus_property ("change-to-type", $2);
		t-> set_mus_property ("change-to-id", $4);

		$$ = t;
		$$->set_spot (THIS->here_input ());
	}
	;

property_def:
	PROPERTY STRING '.' STRING '='  scalar {
		
		Music *t = set_property_music (scm_string_to_symbol ($4), $6);
		Context_specced_music *csm = new Context_specced_music (SCM_EOL);

		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING UNSET {
		Music *t = new Music (SCM_EOL);

		t->set_mus_property ("iterator-ctor",
			Property_unset_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING SET embedded_scm '=' embedded_scm {
		bool autobeam
		  = gh_equal_p ($4, scm_makfrom0str ("autoBeamSettings"));
		bool itc = internal_type_checking_global_b;
		Music *t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Push_property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		t->set_mus_property ("pop-first", SCM_BOOL_T);
		if (autobeam)
			internal_type_checking_global_b = false;
		t->set_mus_property ("grob-property", $6);
		if (autobeam)
			internal_type_checking_global_b = itc;
		t->set_mus_property ("grob-value", $8);
		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING OVERRIDE embedded_scm '=' embedded_scm {
		/*
			UGH UGH UGH UGH.
		*/
		bool autobeam
		  = gh_equal_p ($4, scm_makfrom0str ("autoBeamSettings"));
		bool itc = internal_type_checking_global_b;

		Music *t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Push_property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));

		if (autobeam)
			internal_type_checking_global_b = false;
		t->set_mus_property ("grob-property", $6);
		t->set_mus_property ("grob-value", $8);
		if (autobeam)
			internal_type_checking_global_b = itc;

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING REVERT embedded_scm {
		Music *t = new Music (SCM_EOL);
		bool autobeam
		  = gh_equal_p ($4, scm_makfrom0str ("autoBeamSettings"));
		bool itc = internal_type_checking_global_b;

		t->set_mus_property ("iterator-ctor",
			Pop_property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		if (autobeam)
			internal_type_checking_global_b = false;
		t->set_mus_property ("grob-property", $6);
		if (autobeam)
			internal_type_checking_global_b = itc;
	
		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	;


scalar:
        string          { $$ = $1; }
        | bare_int      { $$ = gh_int2scm ($1); }
        | embedded_scm  { $$ = $1; }
        ;


request_chord:
	pre_requests {
		THIS->push_spot ();
	} /*cont */ simple_element post_requests	{
		Music_sequence *l = dynamic_cast<Music_sequence*> ($3);
		
		$1->concat (*$4);
	        for (int i=0; i < $1->size (); i++) {
		  Music * m = $1->elem (i);
		  l->append_music (m);
		}
 		$$ = $3;

		delete $1;
		delete $4;
	}
	| command_element
	;

command_element:
	command_req {
		$$ = new Request_chord (SCM_EOL);
		$$->set_mus_property ("elements", gh_cons ($1->self_scm (), SCM_EOL));
  	  scm_gc_unprotect_object ($1->self_scm());

		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
	}
	| E_LEFTSQUARE {
		Span_req *l = new Span_req;
		l->set_span_dir (START);
		l->set_mus_property ("span-type", scm_makfrom0str ("ligature"));
		l->set_spot (THIS->here_input ());

		$$ = new Request_chord (SCM_EOL);
		$$->set_mus_property ("elements", gh_cons (l->self_scm (), SCM_EOL));
  	  scm_gc_unprotect_object (l->self_scm());
		$$->set_spot (THIS->here_input ());
	}
	| E_RIGHTSQUARE {
		Span_req *l = new Span_req;
		l->set_span_dir (STOP);
		l->set_mus_property ("span-type", scm_makfrom0str ("ligature"));
		l->set_spot (THIS->here_input ());

		$$ = new Request_chord (SCM_EOL);
		$$->set_mus_property ("elements", gh_cons (l->self_scm (), SCM_EOL));
		$$->set_spot (THIS->here_input ());
	  scm_gc_unprotect_object (l->self_scm());

	}
	| E_BACKSLASH {
		$$ = new Music (gh_list (gh_cons (ly_symbol2scm ("name"), ly_symbol2scm ("separator")), SCM_UNDEFINED));
		$$->set_spot (THIS->here_input ());
	}
	| '|'      {

		extern Music * get_barcheck();
		$$ = get_barcheck ();
		$$->set_spot (THIS->here_input ());
	}
	| BAR STRING  			{
		Music *t = set_property_music (ly_symbol2scm ("whichBar"), $2);

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm->set_mus_property ("context-type", scm_makfrom0str ("Score"));
	}
	| PARTIAL duration_length  	{
		Moment m = - unsmob_duration ($2)->length_mom ();
		Music * p = set_property_music (ly_symbol2scm ( "measurePosition"),m.smobbed_copy ());

		Context_specced_music * sp = new Context_specced_music (SCM_EOL);
		sp->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());

		$$ =sp ;
		sp-> set_mus_property ("context-type", scm_makfrom0str ( "Score"));
	}
	| CLEF STRING  {
		SCM func = scm_primitive_eval (ly_symbol2scm ("clef-name-to-properties"));
		SCM result = gh_call1 (func, $2);

		SCM l = SCM_EOL;
		for (SCM s = result ; gh_pair_p (s); s = ly_cdr (s)) {
			Music * p = new Music (SCM_EOL);
			set_music_properties (p, ly_car (s));
			l = gh_cons (p->self_scm (), l);
			scm_gc_unprotect_object (p->self_scm ());
		}
		Sequential_music * seq = new Sequential_music (SCM_EOL);
		seq->set_mus_property ("elements", l);

		Context_specced_music * sp = new Context_specced_music (SCM_EOL);
		sp->set_mus_property ("element", seq->self_scm ());
		scm_gc_unprotect_object (seq->self_scm ());

		$$ =sp ;
		sp-> set_mus_property ("context-type", scm_makfrom0str ("Staff"));
	}
	| TIME_T fraction  {
		Music * p1 = set_property_music (ly_symbol2scm ( "timeSignatureFraction"), $2);

		int l = gh_scm2int (ly_car ($2));
		int o = gh_scm2int (ly_cdr ($2));

		Moment one_beat = Moment (1)/Moment (o);
		Moment len = Moment (l) * one_beat;


		Music *p2 = set_property_music (ly_symbol2scm ("measureLength"), len.smobbed_copy ());
		Music *p3 = set_property_music (ly_symbol2scm ("beatLength"), one_beat.smobbed_copy ());

		SCM list = scm_list_n (p1->self_scm (), p2->self_scm (), p3->self_scm(), SCM_UNDEFINED);
		Sequential_music *seq = new Sequential_music (SCM_EOL);
		seq->set_mus_property ("elements", list);
		

		Context_specced_music * sp = new Context_specced_music (SCM_EOL);
		sp->set_mus_property ("element", seq->self_scm ());

		

		scm_gc_unprotect_object (p3->self_scm ());
		scm_gc_unprotect_object (p2->self_scm ());
		scm_gc_unprotect_object (p1->self_scm ());
		scm_gc_unprotect_object (seq->self_scm ());

		$$ = sp;

/*
 TODO: should make alias TimingContext for Score
*/

		sp-> set_mus_property ("context-type", scm_makfrom0str ( "Score"));
	}
	;

command_req:
	shorthand_command_req  	{ $$ = $1; }
	| verbose_command_req 	{ $$ = $1; }
	;

shorthand_command_req:
	extender_req {
		$$ = $1;
	}
	| hyphen_req {
		$$ = $1;
	}
	| '~'	{
		$$ = new Tie_req;
	}
	| '['		{
		Span_req*b= new Span_req;
		b->set_span_dir (START);
		b->set_mus_property ("span-type", scm_makfrom0str ("beam"));
		$$ =b;


		THIS->last_beam_start_ = b->self_scm ();
	}
	| ']'		{
		Span_req*b= new Span_req;
		b->set_span_dir ( STOP);
		b->set_mus_property ("span-type", scm_makfrom0str ("beam"));
		$$ = b;
	}
	| BREATHE {
		$$ = new Breathing_sign_req;
	}
	| E_TILDE {
		$$ = new Porrectus_req;
	}
	;

verbose_command_req:
	COMMANDSPANREQUEST bare_int STRING { /*TODO: junkme */
		Span_req * sp = new Span_req;
		sp-> set_span_dir ( Direction ($2));
		sp->set_mus_property ("span-type",$3);
		sp->set_spot (THIS->here_input ());
		$$ = sp;
	}
	| MARK DEFAULT  {
		Mark_req * m = new Mark_req;
		$$ = m;
	}
	| MARK scalar {
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label", $2);
		$$ = m;
	}
	| PENALTY SCM_T 	{
		Break_req * b = new Break_req;
		SCM s = $2;
		if (!gh_number_p (s))
			s  =gh_int2scm (0);

		b->set_mus_property ("penalty", s);
		b->set_spot (THIS->here_input ());
		$$ = b;
	}
	| SKIP duration_length {
		Skip_req * skip = new Skip_req;
		skip->set_mus_property ("duration", $2);

		$$ = skip;
	}
	| tempo_request {
		$$ = $1;
	}
	| KEY DEFAULT {
		Key_change_req *key= new Key_change_req;
		$$ = key;
	}
	| KEY NOTENAME_PITCH SCM_IDENTIFIER 	{
		Key_change_req *key= new Key_change_req;
		
		key->set_mus_property ("pitch-alist", $3);
 		((Music*)key)->transpose (* unsmob_pitch ($2));
		$$ = key; 
	}
	;

post_requests:
	{
		$$ = new Link_array<Request>;
	}
	| post_requests post_request {
		$2->set_spot (THIS->here_input ());
		$$->push ($2);
	}
	;

post_request:
	verbose_request
	| request_with_dir
	| close_request
	;


request_that_take_dir:
	gen_text_def
	| verbose_request
	| script_abbreviation {
		SCM s = THIS->lexer_->lookup_identifier ("dash" + ly_scm2string ($1));
		Articulation_req *a = new Articulation_req;
		if (gh_string_p (s))
			a->set_mus_property ("articulation-type", s);
		else THIS->parser_error (_ ("Expecting string as script definition"));
		$$ = a;
	}
	;

request_with_dir:
	script_dir request_that_take_dir	{
		if (Script_req * gs = dynamic_cast<Script_req*> ($2))
			gs->set_direction (Direction ($1));
		else if ($1)
			$2->origin ()->warning (_ ("Can't specify direction for this request"));
		$$ = $2;
	}
	;
	
verbose_request:
	REQUEST_IDENTIFIER	{
		$$ = dynamic_cast<Request*> (unsmob_music ($1)->clone ());
		$$->set_spot (THIS->here_input ());
	}
	| DYNAMICSCRIPT embedded_scm {
		/*
			TODO: junkme, use text-type == dynamic
		*/
		Text_script_req *d = new Text_script_req;
		SCM dyn = ly_symbol2scm ("dynamic");
		d->set_mus_property ("text-type" , dyn);
		d->set_mus_property ("text", $2);
		d->set_spot (THIS->here_input ());
		$$ = d;
	}
	| SPANREQUEST bare_int STRING {
		Span_req * sp = new Span_req;
		sp->set_span_dir ( Direction ($2));
		sp->set_mus_property ("span-type", $3);
		sp->set_spot (THIS->here_input ());
		$$ = sp;
	}
	| tremolo_type  {
               Tremolo_req* a = new Tremolo_req;
               a->set_spot (THIS->here_input ());
               a->set_mus_property ("tremolo-type", gh_int2scm ($1));
               $$ = a;
        }
	| SCRIPT STRING 	{ 
		Articulation_req * a = new Articulation_req;
		a->set_mus_property ("articulation-type", $2);
		a->set_spot (THIS->here_input ());
		$$ = a;
	}
	/*
duh, junk this syntax from the parser, if possible. 
	*/
	| ARPEGGIO {
		Arpeggio_req *a = new Arpeggio_req;
		a->set_spot (THIS->here_input ());
		$$ = a;
	}
	| GLISSANDO {
		Glissando_req *g = new Glissando_req;
		g->set_spot /* No pun intended */ (THIS->here_input ());
		$$ = g;
	}	
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
		$$ ++ ;
	}
	;

steno_pitch:
	NOTENAME_PITCH	{
		$$ = $1;
	}
	| NOTENAME_PITCH sup_quotes 	{
		Pitch p = *unsmob_pitch ($1);
		p.octave_ +=  $2;
		$$ = p.smobbed_copy ();
	}
	| NOTENAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p.octave_ +=  -$2;
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
		p.octave_ +=  $2;
		$$ = p.smobbed_copy ();
	}
	| TONICNAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p.octave_ +=  -$2;
		$$ = p.smobbed_copy ();

	}
	;

pitch:
	steno_pitch {
		$$ = $1;
	}
	| explicit_pitch {
		$$ = $1;
	}
	;

explicit_pitch:
	PITCH embedded_scm {
		$$ = $2;
		if (!unsmob_pitch ($2)) {
			THIS->parser_error (_f ("Expecting musical-pitch value", 3));
			 $$ = Pitch ().smobbed_copy ();
		}
	}
	;

verbose_duration:
	DURATION embedded_scm 	{
		$$ = $2;
		if (!unsmob_duration ($2))
		{
			THIS->parser_error (_ ("Must have duration object"));
			$$ = Duration ().smobbed_copy ();
		}
	}
	;

extender_req:
	EXTENDER {
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = new Extender_req;
	}
	;

hyphen_req:
	HYPHEN {
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = new Hyphen_req;
	}
	;

close_request:
	close_request_parens {
		$$ = $1;
		dynamic_cast<Span_req*> ($$)->set_span_dir ( START);
	}
	;
 
close_request_parens:
	'('	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "slur"));
		s->set_spot (THIS->here_input());
	}
	| E_OPEN	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "phrasing-slur"));
		s->set_spot (THIS->here_input());
	}
	| E_SMALLER {
		Span_req*s =new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "crescendo"));
		s->set_spot (THIS->here_input());
	}
	| E_BIGGER {
		Span_req*s =new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ("decrescendo"));
		s->set_spot (THIS->here_input());
	}
	;


open_request:
	open_request_parens {
		$$ = $1;
		dynamic_cast<Span_req*> ($$)->set_span_dir (STOP);
	}
	;

open_request_parens:
	E_EXCLAMATION 	{
		Span_req *s =  new Span_req;
		s->set_mus_property ("span-type", scm_makfrom0str ( "crescendo"));
		s->set_spot (THIS->here_input());

		$$ = s;
	}
	| ')'	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "slur"));
		s->set_spot (THIS->here_input());

	}
	| E_CLOSE	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "phrasing-slur"));
		s->set_spot (THIS->here_input());
	}
	;

gen_text_def:
	embedded_scm {
		Text_script_req *t = new Text_script_req;
		t->set_mus_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	| string {
		Text_script_req *t = new Text_script_req;
		t->set_mus_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	| DIGIT {
		String ds = to_string ($1);
		Text_script_req* t = new Text_script_req;
		SCM finger = ly_symbol2scm ("finger");
		t->set_mus_property ("text",  scm_makfrom0str (ds.to_str0 ()));
		t->set_mus_property ("text-type" , finger);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	;

script_abbreviation:
	'^'		{
		$$ = scm_makfrom0str ("Hat");
	}
	| '+'		{
		$$ = scm_makfrom0str ("Plus");
	}
	| '-' 		{
		$$ = scm_makfrom0str ("Dash");
	}
 	| '|'		{
		$$ = scm_makfrom0str ("Bar");
	}
	| '>'		{
		$$ = scm_makfrom0str ("Larger");
	}
	| '.' 		{
		$$ = scm_makfrom0str ("Dot");
	}
	| '_' {
		$$ = scm_makfrom0str ("Underscore");
	}
	;

script_dir:
	'_'	{ $$ = DOWN; }
	| '^'	{ $$ = UP; }
	| '-'	{ $$ = CENTER; }
	;

pre_requests:
	{
		$$ = new Link_array<Request>;
	}
	| pre_requests open_request {
		$$->push ($2);
	}
	;

absolute_pitch:
	steno_pitch	{
		$$ = $1;
	}
	;

duration_length:
	multiplied_duration {
		$$ = $1;
	}
	| verbose_duration {
		$$ = $1;
	}	
	;

optional_notemode_duration:
	{
		Duration dd = THIS->default_duration_;
		$$ = dd.smobbed_copy ();

		THIS->beam_check ($$);
	}
	| multiplied_duration	{
		$$ = $1;
		THIS->default_duration_ = *unsmob_duration ($$);

		THIS->beam_check ($$);
	}
	| verbose_duration {
		$$ = $1;
		THIS->default_duration_ = *unsmob_duration ($$);
	}	
	;

steno_duration:
	bare_unsigned dots		{
		int l = 0;
		if (!is_duration_b ($1))
			THIS->parser_error (_f ("not a duration: %d", $1));
		else
			l =  intlog2 ($1);

		$$ = Duration (l, $2).smobbed_copy ();
	}
	| DURATION_IDENTIFIER dots	{
		Duration *d =unsmob_duration ($1);
		Duration k (d->duration_log (),d->dot_count () + $2);
		$$ = k.smobbed_copy ();
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
		Rational  m (gh_scm2int (ly_car ($3)), gh_scm2int (ly_cdr ($3)));

		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
	}
	;

fraction:
	FRACTION { $$ = $1; }
	| UNSIGNED '/' UNSIGNED {
		$$ = gh_cons (gh_int2scm ($1), gh_int2scm ($3));
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
		$$ =0;
	}
	| ':' bare_unsigned {
		if (!is_duration_b ($2))
			THIS->parser_error (_f ("not a duration: %d", $2));
		$$ = $2;
	}
	;


bass_number:
	DIGIT
	| UNSIGNED 
	;

bass_mod:
	'-' 	{ $$ = -1; }
	| '+'	{ $$ = 1; } 
	| '!'	{ $$ = 0; }
	;

bass_figure:
	FIGURE_SPACE {
		Bass_figure_req *bfr = new Bass_figure_req;
		$$ = bfr->self_scm();
		scm_gc_unprotect_object ($$);
	}
	| bass_number  {
		Bass_figure_req *bfr = new Bass_figure_req;
		$$ = bfr->self_scm();

		bfr->set_mus_property ("figure", gh_int2scm ($1));

		scm_gc_unprotect_object ($$);
	}
	| bass_figure bass_mod {
		Music *m = unsmob_music ($1);
		if ($2) {
			SCM salter =m->get_mus_property ("alteration");
			int alter = gh_number_p ( salter) ? gh_scm2int (salter) : 0;
			m->set_mus_property ("alteration",
				gh_int2scm (alter + $2));
		} else {
			m->set_mus_property ("alteration", gh_int2scm (0));
		}
	}
	;

br_bass_figure:
	'[' bass_figure {
		$$ = $2;
		unsmob_music ($$)->set_mus_property ("bracket-start", SCM_BOOL_T);
	}
	| bass_figure	{
		$$ = $1;
	}
	| br_bass_figure ']' {
		$$ = $1;
		unsmob_music ($1)->set_mus_property ("bracket-stop", SCM_BOOL_T);
	}
	;

figure_list:
	/**/		{
		$$ = SCM_EOL;
	}
	| figure_list br_bass_figure {
		$$ = gh_cons ($2, $1); 
	}
	;

figure_spec:
	FIGURE_OPEN figure_list FIGURE_CLOSE {
		Music * m = new Request_chord (SCM_EOL);
		$2 = scm_reverse_x ($2, SCM_EOL);
		m->set_mus_property ("elements",  $2);
		$$ = m->self_scm ();
	}
	;


optional_rest:
	/**/   { $$ = 0; }
	| REST { $$ = 1; }
	;

simple_element:
	pitch exclamations questions optional_notemode_duration optional_rest {

		Input i = THIS->pop_spot ();
		if (!THIS->lexer_->note_state_b ())
			THIS->parser_error (_ ("Have to be in Note mode for notes"));

		Music *n = 0;
		if ($5)
			n =  new Rest_req ;
		else
			n =  new Note_req;
		
		n->set_mus_property ("pitch", $1);
		n->set_mus_property ("duration", $4);


		if ($3 % 2)
			n->set_mus_property ("cautionary", SCM_BOOL_T);
		if ($2 % 2 || $3 % 2)
			n->set_mus_property ("force-accidental", SCM_BOOL_T);

		Simultaneous_music*v = new Request_chord (SCM_EOL);
		v->set_mus_property ("elements", scm_list_n (n->self_scm (), SCM_UNDEFINED));
		scm_gc_unprotect_object (n->self_scm());

		v->set_spot (i);
		n->set_spot (i);
		$$ = v;
	}
	| figure_spec optional_notemode_duration {
		Music * m = unsmob_music ($1);
		Input i = THIS->pop_spot (); 
		m->set_spot (i);
		for (SCM s = m->get_mus_property ("elements"); gh_pair_p (s); s = ly_cdr (s))
			{
				unsmob_music (ly_car (s))->set_mus_property ("duration", $2);
			}
		$$ = m;
	}	
 	| RESTNAME optional_notemode_duration		{

		Input i = THIS->pop_spot ();
 		SCM e = SCM_UNDEFINED;
 		if (ly_scm2string ($1) =="s") {
			/* Space */
			Skip_req * skip = new Skip_req;
			skip->set_mus_property ("duration" ,$2);
			skip->set_spot (i);
			e = skip->self_scm ();
		  }
		  else {
 			Rest_req * rest_req = new Rest_req;
		      	rest_req->set_mus_property ("duration", $2);
		      	rest_req->set_spot (i);
			e = rest_req->self_scm ();
		    }
 		Simultaneous_music* velt = new Request_chord (SCM_EOL);
		velt-> set_mus_property ("elements", scm_list_n (e,SCM_UNDEFINED));
		velt->set_spot (i);

 		$$ = velt;
	}
	| MULTI_MEASURE_REST optional_notemode_duration  	{
		Input i = THIS->pop_spot ();

		Skip_req * sk = new Skip_req;
		sk->set_mus_property ("duration", $2);
		Span_req *sp1 = new Span_req;
		Span_req *sp2 = new Span_req;
		sp1-> set_span_dir ( START);
		sp2-> set_span_dir ( STOP);
		SCM r = scm_makfrom0str ("rest");
		sp1->set_mus_property ("span-type", r);
		sp2->set_mus_property ("span-type", r);

		Request_chord * rqc1 = new Request_chord (SCM_EOL);
		rqc1->set_mus_property ("elements", scm_list_n (sp1->self_scm (), SCM_UNDEFINED));
		Request_chord * rqc2 = new Request_chord (SCM_EOL);
		rqc2->set_mus_property ("elements", scm_list_n (sk->self_scm (), SCM_UNDEFINED));;
		Request_chord * rqc3 = new Request_chord (SCM_EOL);
		rqc3->set_mus_property ("elements", scm_list_n (sp2->self_scm (), SCM_UNDEFINED));;

		SCM ms = scm_list_n (rqc1->self_scm (), rqc2->self_scm (), rqc3->self_scm (), SCM_UNDEFINED);

		$$ = new Sequential_music (SCM_EOL);
		$$->set_mus_property ("elements", ms);
	}
	| STRING optional_notemode_duration 	{
		Input i = THIS->pop_spot ();

		Lyric_req* lreq = new Lyric_req;
                lreq->set_mus_property ("text", $1);
		lreq->set_mus_property ("duration",$2);
		lreq->set_spot (i);
		Simultaneous_music* velt = new Request_chord (SCM_EOL);
		velt->set_mus_property ("elements", scm_list_n (lreq->self_scm (), SCM_UNDEFINED));

		$$= velt;
	}
	| chord {
		Input i = THIS->pop_spot ();

		if (!THIS->lexer_->chord_state_b ())
			THIS->parser_error (_ ("Have to be in Chord mode for chords"));
		$$ = $1;
	}
	;


chord:
	steno_tonic_pitch optional_notemode_duration chord_additions chord_subtractions chord_inversion chord_bass {
                $$ = Chord::get_chord ($1, $3, $4, $5, $6, $2);
		$$->set_spot (THIS->here_input ());
        };

chord_additions: 
	{
		$$ = SCM_EOL;
	} 
	| CHORD_COLON chord_notes {
		$$ = $2;
	}
	;

chord_notes:
	chord_step {
		$$ = $1;
	}
	| chord_notes '.' chord_step {
		$$ = gh_append2 ($$, $3);
	}
	;

chord_subtractions: 
	{
		$$ = SCM_EOL;
	} 
	| CHORD_CARET chord_notes {
		$$ = $2;
	}
	;


chord_inversion:
	{
		$$ = SCM_EOL;
	}
	| '/' steno_tonic_pitch {
		$$ = $2;
	}
	;

chord_bass:
	{
		$$ = SCM_EOL;
	}
	| CHORD_BASS steno_tonic_pitch {
		$$ = $2;
	}
	;

chord_step:
	chord_note {
		$$ = gh_cons ($1, SCM_EOL);
	}
	| CHORDMODIFIER_PITCH {
		$$ = gh_cons (unsmob_pitch ($1)->smobbed_copy (), SCM_EOL);
	}
	| CHORDMODIFIER_PITCH chord_note { /* Ugh. */
		$$ = scm_list_n (unsmob_pitch ($1)->smobbed_copy (),
			$2, SCM_UNDEFINED);
	}
	;

chord_note:
	bare_unsigned {
		 Pitch m;
		m.notename_ = ($1 - 1) % 7;
		m.octave_ = $1 > 7 ? 1 : 0;
		m.alteration_ = 0;

		$$ = m.smobbed_copy ();
        } 
	| bare_unsigned '+' {
		Pitch m;
		m.notename_ = ($1 - 1) % 7;
		m.octave_ = $1 > 7 ? 1 : 0;
		m.alteration_ = 1;


		$$ = m.smobbed_copy ();
	}
	| bare_unsigned CHORD_MINUS {
		Pitch m;
		m.notename_ = ($1 - 1) % 7;
		m.octave_ = $1 > 7 ? 1 : 0;
		m.alteration_ = -1;

		$$ = m.smobbed_copy ();
	}
        ;

/*
	UTILITIES
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
	'(' number_expression ')'	{
		$$ = $2;
	}
	| '-'  number_factor { /* %prec UNARY_MINUS */
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| bare_number
	;


bare_number:
	UNSIGNED	{
		$$ = gh_int2scm ($1);
	}
	| REAL		{
		$$ = $1;
	}
	| NUMBER_IDENTIFIER		{
		$$ = $1;
	}
	| REAL CM_T	{
		$$ = gh_double2scm (gh_scm2double ($1) CM );
	}
	| REAL PT_T	{
		$$ = gh_double2scm (gh_scm2double ($1) PT);
	}
	| REAL IN_T	{
		$$ = gh_double2scm (gh_scm2double ($1) INCH);
	}
	| REAL MM_T	{
		$$ = gh_double2scm (gh_scm2double ($1) MM);
	}
	| REAL CHAR_T	{
		$$ = gh_double2scm (gh_scm2double ($1) CHAR);
	}
	;


bare_unsigned:
	UNSIGNED {
			$$ = $1;
	}
	| DIGIT {
		$$ = $1;
	}
	;

bare_int:
	bare_number {
		if (scm_integer_p ($1) == SCM_BOOL_T)
		{
			int k = gh_scm2int ($1);
			$$ = k;
		} else
		{
			THIS->parser_error (_ ("need integer number arg"));
			$$ = 0;
		}
	}
	| '-' bare_int {
		$$ = -$2;
	}
	;


string:
	STRING		{
		$$ = $1;
	}
	| STRING_IDENTIFIER	{
		$$ = $1;
	}
	| string '+' string {
		$$ = scm_string_append (scm_list_n ($1, $3, SCM_UNDEFINED));
	}
	;


exclamations:
		{ $$ = 0; }
	| exclamations '!'	{ $$ ++; }
	;

questions:
		{ $$ = 0; }
	| questions '?'	{ $$ ++; }
	;


%%

void
My_lily_parser::set_yydebug (bool )
{
#if 0
	yydebug = b;
#endif
}

extern My_lily_parser * current_parser;

void
My_lily_parser::do_yyparse ()
{

	current_parser = this;;
	yyparse ((void*)this);
}


/*
Should make this optional?    It will also complain when you do

	[s4]

which is entirely legitimate.

Or we can scrap it. Barchecks should detect wrong durations, and
skipTypesetting speeds it up a lot.
*/
void
My_lily_parser::beam_check (SCM dur)
{
  Duration *d = unsmob_duration (dur);
  if (unsmob_music (last_beam_start_) && d->duration_log () <= 2)
    {
      Music * m = unsmob_music (last_beam_start_);
      m->origin ()->warning (_("Suspect duration found following this beam"));
    }
  last_beam_start_ = SCM_EOL;
}
