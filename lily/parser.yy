%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for mudela

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <iostream.h>
#include "translator-def.hh"
#include "lily-guile.hh"

#include "lookup.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "file-path.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "identifier.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "my-lily-parser.hh"
#include "context-specced-music.hh"
#include "score.hh"
#include "music-list.hh"

#include "file-results.hh"
#include "input.hh"
#include "scope.hh"
#include "relative-music.hh"
#include "lyric-combine-music.hh"
#include "transposed-music.hh"
#include "time-scaled-music.hh"
#include "repeated-music.hh"
#include "mudela-version.hh"
#include "grace-music.hh"
#include "auto-change-music.hh"
#include "part-combine-music.hh"

#include "chord.hh"

bool
is_duration_b (int t)
{
  return t && t == 1 << intlog2(t);
}


// mmm JUNKME ?
Mudela_version oldest_version ("1.3.59");

void
print_mudela_versions (ostream &os)
{
  os << _f ("Oldest supported input version: %s", oldest_version.str ()) 
    << endl;
}


// needed for bison.simple's malloc() and free()
#include <malloc.h>

#ifndef NDEBUG
#define YYDEBUG 1
#endif

#define YYERROR_VERBOSE 1

#define YYPARSE_PARAM my_lily_parser_l
#define YYLEX_PARAM my_lily_parser_l
#define THIS ((My_lily_parser *) my_lily_parser_l)

#define yyerror THIS->parser_error
#define ARRAY_SIZE(a,s)   if (a.size () != s) THIS->parser_error (_f ("Expecting %d arguments", s))

%}


%union {
    Array<Musical_pitch> *pitch_arr;
    Link_array<Request> *reqvec;
    Duration *duration;
    Identifier *id;
    String * string;
    Music *music;
    Score *score;
    Scope *scope;

    Musical_req* musreq;
    Music_output_def * outputdef;
    Musical_pitch * pitch;
    Midi_def* midi;
    Real real;
    Request * request;

    /* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesnt trigger a move
of the parse stack onto the heap. */

    SCM scm;

    Tempo_req *tempo;
    int i;
}
%{

int
yylex (YYSTYPE *s,  void * v_l)
{
	My_lily_parser	 *pars_l = (My_lily_parser*) v_l;
	My_lily_lexer * lex_l = pars_l->lexer_p_;

	lex_l->lexval_l = (void*) s;
	return lex_l->yylex ();
}


%}

%pure_parser

/* tokens which are not keywords */
%token AUTOCHANGE
%token ARPEGGIO
%token TEXTSCRIPT
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
%token SEQUENTIAL
%token SIMULTANEOUS
%token CONSISTSEND
%token DENIES
%token DURATION
%token EXTENDER
%token FONT
%token GRACE
%token HEADER
%token HYPHEN
%token IN_T
%token INVALID
%token KEY
%token LYRICS
%token MARK
%token MEASURES
%token MIDI
%token MM_T
%token MUSICAL_PITCH
%token NAME
%token NOTENAMES
%token NOTES
%token PAPER
%token PARTIAL
%token PENALTY
%token PROPERTY
%token PUSH POP 
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
%token COMMANDSPANREQUEST
%token TEMPO
%token OUTPUTPROPERTY
%token TIME_T
%token TIMES
%token TRANSLATOR
%token TRANSPOSE
%token TYPE
%token CONTEXT

/* escaped */
%token E_CHAR E_EXCLAMATION E_SMALLER E_BIGGER 
%token CHORD_BASS CHORD_COLON CHORD_MINUS CHORD_CARET 

%type <i>	exclamations questions
%token <i>	DIGIT
%token <pitch>	NOTENAME_PITCH
%token <pitch>	TONICNAME_PITCH
%token <pitch>	CHORDMODIFIER_PITCH
%token <id>	DURATION_IDENTIFIER
%token <id>	IDENTIFIER


%token <id>	SCORE_IDENTIFIER
%token <id>	MUSIC_OUTPUT_DEF_IDENTIFIER

%token <scm>	NUMBER_IDENTIFIER
%token <scm>	REQUEST_IDENTIFIER
%token <scm>	MUSIC_IDENTIFIER TRANSLATOR_IDENTIFIER
%token <scm>	STRING_IDENTIFIER SCM_IDENTIFIER 
%token <scm>	DURATION RESTNAME
%token <scm>	STRING 
%token <scm>	SCM_T
%token <i>	UNSIGNED
%token <real>   REAL

%type <outputdef> output_def
%type <scope> 	mudela_header mudela_header_body
%type <request>	open_request_parens close_request_parens open_request close_request
%type <request> request_with_dir request_that_take_dir verbose_request
%type <i>	sub_quotes sup_quotes
%type <music>	simple_element  request_chord command_element Simple_music  Composite_music 
%type <music>	Alternative_music Repeated_music
%type <i>	tremolo_type
%type <i>	bare_int  bare_unsigned
%type <i>	script_dir

%type <scm>	identifier_init 

%type <duration> steno_duration optional_notemode_duration
%type <duration> entered_notemode_duration explicit_duration
	
%type <reqvec>  pre_requests post_requests
%type <request> gen_text_def
%type <pitch>   steno_musical_pitch musical_pitch absolute_musical_pitch
%type <pitch>   steno_tonic_pitch

%type <pitch_arr>	chord_additions chord_subtractions chord_notes chord_step
%type <music>	chord
%type <pitch>	chord_note chord_inversion chord_bass
%type <duration>	duration_length

%type <scm>  embedded_scm scalar
%type <music>	Music Sequential_music Simultaneous_music Music_sequence
%type <music>	relative_music re_rhythmed_music part_combined_music
%type <music>	property_def translator_change
%type <scm> Music_list
%type <outputdef>  music_output_def_body
%type <request> shorthand_command_req
%type <request>	post_request 
%type <request> command_req verbose_command_req
%type <request>	extender_req
%type <request> hyphen_req
%type <scm>	string bare_number number_expression
%type <score>	score_block score_body

%type <scm>	translator_spec_block translator_spec_body
%type <tempo> 	tempo_request
%type <scm> notenames_body notenames_block chordmodifiers_block
%type <scm>	script_abbreviation



%left '-' '+'
%left '*' '/'
%left UNARY_MINUS

%%

mudela:	/* empty */
	| mudela toplevel_expression {}
	| mudela assignment  { }
	| mudela error {
		THIS->error_level_i_  = 1;
		//THIS->parser_error (_ ("ly invalid"));
	}
	| mudela INVALID	{
		THIS->error_level_i_  = 1;
		//THIS->parser_error (_ ("ly invalid"));
	}
	;

toplevel_expression:
	notenames_block			{
		THIS->lexer_p_->pitchname_tab_ =  $1;
	}
	| chordmodifiers_block			{
		THIS->lexer_p_->chordmodifier_tab_  = $1;
	}
	| mudela_header {
		delete header_global_p;
		header_global_p = $1;
	}
	| score_block {
		score_global_array.push ($1);
	}
	| output_def {
		Identifier * id = new
			Music_output_def_identifier ($1, MUSIC_OUTPUT_DEF_IDENTIFIER);
		if (dynamic_cast<Paper_def*> ($1))
			THIS->lexer_p_->set_identifier ("$defaultpaper", id->self_scm ());
		else if (dynamic_cast<Midi_def*> ($1))
			THIS->lexer_p_->set_identifier ("$defaultmidi", id->self_scm ());
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
	NOTENAMES notenames_body   {  $$ = $2; }
	;



notenames_body:
	embedded_scm	{
	  int i = scm_ilength ($1);

	  SCM tab = scm_make_vector (gh_int2scm (i), SCM_EOL);
	  for (SCM s = $1; s != SCM_EOL; s = gh_cdr (s)) {
		SCM pt = gh_cdar (s);
		if (scm_ilength (pt) != 3)
			THIS->parser_error ("Need three args");
		scm_hashq_set_x (tab, gh_caar(s), pt);
	  }

	  $$ = tab;
	}
	;

mudela_header_body:
	{
		$$ = new Scope;
		THIS->lexer_p_-> scope_l_arr_.push ($$);
	}
	| mudela_header_body assignment semicolon { 

	}
	;

mudela_header:
	HEADER '{' mudela_header_body '}'	{
		$$ = $3;
		THIS->lexer_p_-> scope_l_arr_.pop ();		
	}
	;


/*
	DECLARATIONS
*/
assignment:
	STRING {
		THIS->remember_spot ();
	}
	/* cont */ '=' identifier_init  {
	        THIS->lexer_p_->set_identifier (ly_scm2string ($1), $4);

/*
 TODO: devise standard for protection in parser.

  The parser stack lives on the C-stack, which means that
all objects can be unprotected as soon as they're here.

*/
		Identifier * id =unsmob_identifier ($4);
		Input spot = THIS->pop_spot ();
		if (id) id->set_spot (spot);
	}
	;



identifier_init:
	score_block {
		$$ = (new Score_identifier ($1, SCORE_IDENTIFIER))->self_scm();
	}
	| output_def {
		$$ = (new Music_output_def_identifier ($1, MUSIC_OUTPUT_DEF_IDENTIFIER))->self_scm();
	}
	| translator_spec_block {
		$$ = $1;
	}
	| Music  {
		$$ = $1->self_scm ();
		scm_unprotect_object ($$);
	}
	| post_request {
		$$ = $1->self_scm ();
		scm_unprotect_object ($$);
	}
	| explicit_duration {
		$$ = (new Duration_identifier ($1, DURATION_IDENTIFIER))->self_scm ();
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
	| TYPE STRING semicolon	{
		$$ = Translator_def::make_scm ();
		Translator_def*td =  unsmob_translator_def ($$);
		td->translator_group_type_ = $2;
		td->set_spot (THIS->here_input ());
	}
	| translator_spec_body STRING '=' embedded_scm			{
		unsmob_translator_def ($$)->add_property_assign ($2, $4);
	}
	| translator_spec_body STRING PUSH embedded_scm '=' embedded_scm {
		unsmob_translator_def ($$)
			->add_push_property (scm_string_to_symbol ($2), $4, $6);
	}
	| translator_spec_body STRING POP embedded_scm  {
	  unsmob_translator_def($$)->add_pop_property (
		scm_string_to_symbol ($2), $4);
	}
	| translator_spec_body STRING '=' identifier_init semicolon	{ 
		SCM v = gh_int2scm (0);
		if (gh_string_p ($4) || gh_number_p ($4) || gh_boolean_p ($4))
			v = $4;
		else 
			THIS->parser_error (_("Wrong type for property value"));

		/* ugh*/
		unsmob_translator_def($$)->add_property_assign ($2, v);
	}
	| translator_spec_body NAME STRING semicolon {
		unsmob_translator_def ($$)->type_name_ = $3;
	}
	| translator_spec_body CONSISTS STRING semicolon {
		unsmob_translator_def ($$)->add_element ($3);
	}
	| translator_spec_body CONSISTSEND STRING semicolon {
		unsmob_translator_def ($$)->add_last_element ( $3);
	}
	| translator_spec_body ACCEPTS STRING semicolon {
		unsmob_translator_def ($$)->set_acceptor ($3,true);
	}
	| translator_spec_body DENIES STRING semicolon {
		unsmob_translator_def ($$)->set_acceptor ($3,false);
	}
	| translator_spec_body REMOVE STRING semicolon {
		unsmob_translator_def ($$)->remove_element ($3);
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { 
		THIS->remember_spot ();
	}
	/*cont*/ '{' score_body '}' 	{
		THIS->pop_spot ();
		$$ = $4;
		if (!$$->def_p_arr_.size ())
		{
		  Identifier *id =
			unsmob_identifier (THIS->lexer_p_->lookup_identifier ("$defaultpaper"));
		  $$->add_output (id ? id->access_content_Music_output_def (true) : new Paper_def );
		}
	}
/*
	| SCORE '{' score_body error {
		$$ = $3
		$$->set_spot (THIS->here_input ());
		// THIS->here_input ().error ("SCORE INVALID");
		$$->error ("SCORE INVALID");
		THIS->parser_error (_f ("SCORE ERROR"));
	}
*/
	;

score_body:
	Music	{
		$$ = new Score;

		$$->set_spot (THIS->here_input ());
		SCM m = $1->self_scm ();
		scm_unprotect_object (m);
		$$->music_ = m;
	}
	| SCORE_IDENTIFIER {
		$$ = $1->access_content_Score (true);
	}
	| score_body mudela_header 	{
		$$->header_p_ = $2;
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
		THIS-> lexer_p_-> scope_l_arr_.pop();
	}
	;

music_output_def_body:
	MIDI '{'    {
	 Identifier *id = unsmob_identifier (THIS->lexer_p_->lookup_identifier ("$defaultmidi"));

		
	 Midi_def* p =0;
	if (id)
		p = dynamic_cast<Midi_def*> (id->access_content_Music_output_def (true));
	else
		p = new Midi_def;

	 $$ = p;
	 THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
	}
	| PAPER '{' 	{
		  Identifier *id = unsmob_identifier (THIS->lexer_p_->lookup_identifier ("$defaultpaper"));
		  Paper_def *p = 0;
		if (id)
			p = dynamic_cast<Paper_def*> (id->access_content_Music_output_def (true));
		else
			p = new Paper_def;
		THIS-> lexer_p_-> scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| PAPER '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def *p = $3->access_content_Music_output_def (true);
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| MIDI '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def *p = $3->access_content_Music_output_def (true);
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| music_output_def_body assignment semicolon {

	}
	| music_output_def_body translator_spec_block	{
		$$->assign_translator ($2);
	}
	| music_output_def_body tempo_request semicolon {
		/*
			junk this ? there already is tempo stuff in
			music.
		*/
		dynamic_cast<Midi_def*> ($$)->set_tempo ($2->dur_.length_mom (), $2->metronome_i_);
	}
	| music_output_def_body bare_int '=' FONT STRING		{ // ugh, what a syntax
		SCM sl = Lookup::make_lookup();
		Lookup * l =unsmob_lookup (sl);
		l->font_name_ = ly_scm2string ($5);
		dynamic_cast<Paper_def*> ($$)->set_lookup ($2, sl);
	}
	| music_output_def_body error {

	}
	;

tempo_request:
	TEMPO steno_duration '=' bare_unsigned	{
		$$ = new Tempo_req;
		$$->dur_ = *$2;
		delete $2;
		$$-> metronome_i_ = $4;
	}
	;

Music_list: /* empty */ {
		$$ = gh_cons (SCM_EOL, SCM_EOL);
	}
	| Music_list Music {
		SCM s = $$;
		SCM c = gh_cons ($2->self_scm (), SCM_EOL);
		scm_unprotect_object ($2->self_scm ()); /* UGH */

	
		if (gh_pair_p (gh_cdr (s)))
			gh_set_cdr_x (gh_cdr (s), c); /* append */
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
		$$ = 0;
	}
	| ALTERNATIVE Music_sequence {
		$$ = $2;
		$2->set_spot (THIS->here_input ());
	}
	;




Repeated_music:
	REPEAT STRING bare_unsigned Music Alternative_music
	{
		Music_sequence* m = dynamic_cast <Music_sequence*> ($5);
		if (m && $3 < m->length_i ())
			$5->origin ()->warning (_ ("More alternatives than repeats.  Junking excess alternatives."));

		Repeated_music * r = new Repeated_music ($4, $3 >? 1, m);
		$$ = r;
		r->type_ = ly_scm2string ($2);
		r->fold_b_ = (r->type_ == "fold");
		r->volta_fold_b_ =  (r->type_ == "volta");
		r->set_spot (*$4->origin ());
	}
	;

Music_sequence: '{' Music_list '}'	{
		$$ = new Music_sequence (gh_car ($2));
	}
	;

Sequential_music:
	SEQUENTIAL '{' Music_list '}'		{
		$$ = new Sequential_music (gh_car ($3));
	}
	| '{' Music_list '}'		{
		$$ = new Sequential_music (gh_car ($2));
	}
	;

Simultaneous_music:
	SIMULTANEOUS '{' Music_list '}'{
		$$ = new Simultaneous_music (gh_car ($3));
	}
	| '<' Music_list '>'	{
		$$ = new Simultaneous_music (gh_car ($2));
	}
	;

Simple_music:
	request_chord		{ $$ = $1; }
	| OUTPUTPROPERTY embedded_scm embedded_scm '=' embedded_scm	{
		SCM pred = $2;
		if (!gh_symbol_p ($3))
		{
			THIS->parser_error (_("Second argument must be a symbol")); 
		}
		/*hould check # args */
		if (!gh_procedure_p (pred))
		{
			THIS->parser_error (_("First argument must be a procedure taking 1 argument"));
		}

	  Music *m = new Music;
	  m->set_mus_property ("predicate", pred);
	  m->set_mus_property ("symbol", $3);
	  m->set_mus_property ("value",  $5);
	  m->set_mus_property ("type", ly_symbol2scm ("output-property"));

		$$ = m;
	}
	| MUSIC_IDENTIFIER {
		$$ = unsmob_music ($1)->clone ();
	}
	| property_def
	| translator_change
	| Simple_music '*' bare_unsigned '/' bare_unsigned 	{
		$$ = $1;
		$$->compress (Moment($3, $5 ));
	}
	| Simple_music '*' bare_unsigned		 {
		$$ = $1;
		$$->compress (Moment ($3, 1));
	}
	;


Composite_music:
	CONTEXT STRING Music	{
		Context_specced_music *csm =  new Context_specced_music ($3);

		csm->translator_type_str_ = ly_scm2string ($2);
		csm->translator_id_str_ = "";


		$$ = csm;
	}
	| AUTOCHANGE STRING Music	{
		Auto_change_music * chm = new Auto_change_music ($3);
		chm->set_mus_property ("what", $2); 

		$$ = chm;
		chm->set_spot (*$3->origin ());
	}
	| GRACE Music {
		$$ = new Grace_music ($2);
	}
	| CONTEXT STRING '=' STRING Music {
		Context_specced_music *csm =  new Context_specced_music ($5);

		csm->translator_type_str_ = ly_scm2string ($2);
		csm->translator_id_str_ = ly_scm2string ($4);

		$$ = csm;
	}
	| TIMES {
		THIS->remember_spot ();
	}
	/* CONTINUED */ 
		bare_unsigned '/' bare_unsigned Music 	

	{
		$$ = new Time_scaled_music ($3, $5, $6);
		$$->set_spot (THIS->pop_spot ());
	}
	| Repeated_music		{ $$ = $1; }
	| Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	| TRANSPOSE musical_pitch Music {
		$$ = new Transposed_music ($3, *$2);
		delete $2; // ugh
	}
	| TRANSPOSE steno_tonic_pitch Music {
		$$ = new Transposed_music ($3, *$2);
		delete $2; // ugh
	}
	| NOTES
		{ THIS->lexer_p_->push_note_state (); }
	Music
		{ $$ = $3;
		  THIS->lexer_p_->pop_state ();
		}
	| CHORDS
		{ THIS->lexer_p_->push_chord_state (); }
	Music
		{
		  $$ = $3;
		  THIS->lexer_p_->pop_state ();
	}
	| LYRICS
		{ THIS->lexer_p_->push_lyric_state (); }
	Music
		{
		  $$ = $3;
		  THIS->lexer_p_->pop_state ();
	}
	| relative_music	{ $$ = $1; }
	| re_rhythmed_music	{ $$ = $1; } 
	| part_combined_music	{ $$ = $1; } 
	;

relative_music:
	RELATIVE absolute_musical_pitch Music {
		$$ = new Relative_octave_music ($3, *$2);
		delete $2; // ugh
	}
	;

re_rhythmed_music:
	ADDLYRICS Music Music {
		Lyric_combine_music * l = new Lyric_combine_music ($2, $3);
		$$ = l;
	}
	;

part_combined_music:
	PARTCOMBINE STRING Music Music {
		Part_combine_music * p = new Part_combine_music (ly_scm2string ($2), $3, $4);
		$$ = p;
	}
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Music * t = new Music;
		t->set_mus_property ("type",
			ly_symbol2scm ("change-translator"));
		t-> set_mus_property ("change-to-type", $2);
		t-> set_mus_property ("change-to-id", $4);

		$$ = t;
		$$->set_spot (THIS->here_input ());
	}
	;

property_def:
	PROPERTY STRING '.' STRING '='  scalar {
		Music *t = new Music;

		t->set_mus_property ("type", ly_symbol2scm ("property-set"));
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		t->set_mus_property ("value", $6);

		Context_specced_music *csm = new Context_specced_music (t);
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> translator_type_str_ = ly_scm2string ($2);
	}
	| PROPERTY STRING '.' STRING PUSH embedded_scm '=' embedded_scm {
		Music *t = new Music;
		t->set_mus_property ("type", ly_symbol2scm ("property-push"));
		t->set_mus_property ("symbols", scm_string_to_symbol ($4));
		t->set_mus_property ("element-property", $6);
		t->set_mus_property ("element-value", $8);
		Context_specced_music *csm = new Context_specced_music (t);
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> translator_type_str_ = ly_scm2string ($2);
	}
	| PROPERTY STRING '.' STRING POP embedded_scm {
		Music *t = new Music;
		t->set_mus_property ("type", ly_symbol2scm ("property-pop"));
		t->set_mus_property ("symbols", scm_string_to_symbol ($4));
		t->set_mus_property ("element-property", $6);

		Context_specced_music *csm = new Context_specced_music (t);
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> translator_type_str_ = ly_scm2string ($2);
	}
	;

scalar:
	string		{ $$ = $1; }
	| bare_int	{ $$ = gh_int2scm ($1); }
	| embedded_scm 	{ $$ = $1; }
	;


request_chord:
	pre_requests simple_element post_requests	{
		Music_sequence *l = dynamic_cast<Music_sequence*>($2);
		if (l) {
			for (int i=0; i < $1->size(); i++)
				l->append_music ($1->elem(i));
			for (int i=0; i < $3->size(); i++)
				l->append_music ($3->elem(i));
			}
		else
			programming_error ("Need Sequence to add music to");
 		$$ = $2;
		
	}
	| command_element
	;

command_element:
	command_req {
		$$ = new Request_chord (gh_cons ($1->self_scm (), SCM_EOL));
		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
	}
	| PARTIAL duration_length ';' 	{
		Music * p = new Music;
		p->set_mus_property ("symbol", ly_symbol2scm ( "measurePosition"));
		p->set_mus_property ("type", ly_symbol2scm ("property-set"));

		Moment m = - $2->length_mom ();
		p->set_mus_property ("value", m.make_scm());
		delete $2; // ugh
		Context_specced_music * sp = new Context_specced_music (p);
		$$ =sp ;
		sp-> translator_type_str_ = "Score";
	}
	;

command_req:
	shorthand_command_req
	| verbose_command_req semicolon	{ $$ = $1; }
	;

shorthand_command_req:
	extender_req {
		$$ = $1;
	}
	| hyphen_req {
		$$ = $1;
	}
	| '|'				{
		$$ = new Barcheck_req;
	}
	| '~'	{
		$$ = new Tie_req;
	}
	| '['		{
		Span_req*b= new Span_req;
		b->span_dir_ = START;
		b->span_type_str_ = "beam";
		$$ =b;
	}
	| ']'		{
	     Span_req*b= new Span_req;
	     b->span_dir_ = STOP;
	     b->span_type_str_ = "beam";
	     $$ = b;
	}
	| BREATHE {
		$$ = new Breathing_sign_req;
	}
	;


verbose_command_req:
	
	BAR STRING 			{
		$$ = new Bar_req (ly_scm2string ($2));
	}
	| COMMANDSPANREQUEST bare_int STRING {
		Span_req * sp_p = new Span_req;
		sp_p-> span_dir_  = Direction($2);
		sp_p->span_type_str_ = ly_scm2string ($3);
		sp_p->set_spot (THIS->here_input ());
		$$ = sp_p;
	}
	| MARK  {
		Mark_req * m = new Mark_req;
		$$ = m;
	}
	| MARK STRING {
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label", $2);
		$$ = m;

	}
	| MARK bare_unsigned {
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label",  gh_int2scm ($2));
		$$ = m;
	}

	| TIME_T bare_unsigned '/' bare_unsigned 	{
		Time_signature_change_req *m = new Time_signature_change_req;
		m->beats_i_ = $2;
		m->one_beat_i_=$4;
		$$ = m;
	}
	| PENALTY bare_int 	{
		Break_req * b = new Break_req;
		b->penalty_f_ = $2 / 100.0;
		b->set_spot (THIS->here_input ());
		$$ = b;
	}
	| SKIP duration_length {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_ = *$2;
		delete $2; // ugh
		$$ = skip_p;
	}
	| tempo_request {
		$$ = $1;
	}
	| CLEF STRING {
		$$ = new Clef_change_req (ly_scm2string ($2));

	}
	| KEY {
		Key_change_req *key_p= new Key_change_req;
		$$ = key_p;
	}
	| KEY NOTENAME_PITCH SCM_IDENTIFIER 	{
		Key_change_req *key_p= new Key_change_req;
		
		key_p->set_mus_property ("pitch-alist", $3);
		((Music* )key_p)->transpose (* $2);
		$$ = key_p; 
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
		SCM s = THIS->lexer_p_->lookup_identifier ("dash-" + ly_scm2string ($1));
		Articulation_req *a = new Articulation_req;
		if (gh_string_p (s))
			a->articulation_str_ = ly_scm2string (s);
		else THIS->parser_error (_ ("Expecting string as script definition"));
		$$ = a;
	}
	;

request_with_dir:
	script_dir request_that_take_dir	{
		if (Script_req * gs = dynamic_cast<Script_req*> ($2))
			gs->dir_ = Direction ($1);
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
	| TEXTSCRIPT STRING STRING 	{
		Text_script_req *ts_p = new Text_script_req;
		ts_p-> text_str_ = ly_scm2string ($2);
		ts_p-> style_str_ = ly_scm2string ($3);
		ts_p->set_spot (THIS->here_input ());

		$$ = ts_p;
	}
	| SPANREQUEST bare_int STRING {
		Span_req * sp_p = new Span_req;
		sp_p->span_dir_  = Direction($2);
		sp_p->span_type_str_ = ly_scm2string ($3);
		sp_p->set_spot (THIS->here_input ());
		$$ = sp_p;
	}
	| tremolo_type	{
		Tremolo_req* a = new Tremolo_req;
		a->set_spot (THIS->here_input ());
		a->type_i_ = $1;
		$$ = a;
	}
	| SCRIPT STRING 	{ 
		Articulation_req * a = new Articulation_req;
		a->articulation_str_ = ly_scm2string ($2);
		a->set_spot (THIS->here_input ());
		$$ = a;
	}
	| ARPEGGIO {
		Arpeggio_req *a = new Arpeggio_req;
		a->set_spot (THIS->here_input ());
		$$ = a;
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

steno_musical_pitch:
	NOTENAME_PITCH	{
		$$ = $1;
	}
	| NOTENAME_PITCH sup_quotes 	{
		$$ = $1;
		$$->octave_i_ +=  $2;
	}
	| NOTENAME_PITCH sub_quotes	 {
		$$ = $1;
		$$->octave_i_ += - $2;
	}
	;

steno_tonic_pitch:
	TONICNAME_PITCH	{
		$$ = $1;
	}
	| TONICNAME_PITCH sup_quotes 	{
		$$ = $1;
		$$->octave_i_ +=  $2;
	}
	| TONICNAME_PITCH sub_quotes	 {
		$$ = $1;
		$$->octave_i_ += - $2;
	}
	;

musical_pitch:
	steno_musical_pitch {
		$$ = $1;
	}
	| MUSICAL_PITCH embedded_scm {
		int sz = scm_ilength ($2);
		if (sz != 3) {
			THIS->parser_error (_f ("Expecting %d arguments", 3));
			$2 = gh_list (gh_int2scm (0), gh_int2scm (0), gh_int2scm (0), SCM_UNDEFINED);
		}
		$$ = new Musical_pitch ($2);
	}
	;

explicit_duration:
	DURATION embedded_scm 	{
		$$ = new Duration;
		if (scm_ilength ($2) == 2)
			{
			$$-> durlog_i_ = gh_scm2int (gh_car($2));
			$$-> dots_i_ = gh_scm2int (gh_cadr($2));
			}
		else
			THIS->parser_error (_("Must have 2 arguments for duration"));
	}
	;

extender_req:
	EXTENDER {
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = new Extender_req;
	}
	;

hyphen_req:
	HYPHEN {
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = new Hyphen_req;
	}
	;

close_request:
	close_request_parens {
		$$ = $1;
		dynamic_cast<Span_req*> ($$)->span_dir_ = START;
	}
	
close_request_parens:
	'('	{
		Span_req* s= new Span_req;
		$$ = s;
		s->span_type_str_ = "slur";
	}
	| E_SMALLER {
		Span_req*s =new Span_req;
		$$ = s;
		s->span_type_str_ = "crescendo";
	}
	| E_BIGGER {
		Span_req*s =new Span_req;
		$$ = s;
		s->span_type_str_ = "decrescendo";
	}
	;


open_request:
	open_request_parens {
		$$ = $1;
		dynamic_cast<Span_req*> ($$)->span_dir_ = STOP;
	}
	;

open_request_parens:
	E_EXCLAMATION 	{
		Span_req *s =  new Span_req;
		s->span_type_str_ = "crescendo";
		$$ = s;
	}
	| ')'	{
		Span_req* s= new Span_req;
		$$ = s;
		s->span_type_str_ = "slur";
	}
	;

gen_text_def:
	string {
		Text_script_req *t  = new Text_script_req;
		$$ = t;
		t->text_str_ = ly_scm2string ($1);

		$$->set_spot (THIS->here_input ());
	}
	| DIGIT {
		Text_script_req* t  = new Text_script_req;
		$$ = t;
		t->text_str_ = to_str ($1);
		t->style_str_ = "finger";
		$$->set_spot (THIS->here_input ());
	}
	;

script_abbreviation:
	'^'		{
		$$ = gh_str02scm  ("hat");
	}
	| '+'		{
		$$ = gh_str02scm("plus");
	}
	| '-' 		{
		$$ = gh_str02scm ("dash");
	}
 	| '|'		{
		$$ = gh_str02scm ("bar");
	}
	| '>'		{
		$$ = gh_str02scm ("larger");
	}
	| '.' 		{
		$$ = gh_str02scm ("dot");
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

absolute_musical_pitch:
	steno_musical_pitch	{
		$$ = $1;
	}
	;

duration_length:
	steno_duration {
		$$ = $1;
	}
	| duration_length '*' bare_unsigned {
		$$->tuplet_iso_i_ *= $3;
	}
	| duration_length '/' bare_unsigned {
		$$->tuplet_type_i_ *= $3;
	}
	;

entered_notemode_duration:
	steno_duration	{
		THIS->set_last_duration ($1);
	}
	;

optional_notemode_duration:
	{
		$$ = new Duration (THIS->default_duration_);
	}
	| entered_notemode_duration {
		$$ = $1;
	}
	;

steno_duration:
	bare_unsigned		{
		$$ = new Duration;
		if (!is_duration_b ($1))
			THIS->parser_error (_f ("not a duration: %d", $1));
		else {
			$$->durlog_i_ = intlog2 ($1);
		     }
	}
	| DURATION_IDENTIFIER	{
		$$ = $1->access_content_Duration (true);
	}
	| steno_duration '.' 	{
		$$->dots_i_ ++;
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


simple_element:
	musical_pitch exclamations questions optional_notemode_duration {
		if (!THIS->lexer_p_->note_state_b ())
			THIS->parser_error (_ ("Have to be in Note mode for notes"));


		Note_req *n = new Note_req;
		
		n->pitch_ = *$1;
		n->duration_ = *$4;

		n->cautionary_b_ = $3 % 2;
		n->forceacc_b_ = $2 % 2 || n->cautionary_b_;


		Simultaneous_music*v = new Request_chord (gh_list (n->self_scm (), SCM_UNDEFINED));
		v->set_spot ($1->spot ());
		n->set_spot ($1->spot ());
		$$ = v;

		delete $1;
		delete $4;
	}
	| RESTNAME optional_notemode_duration		{

		SCM e = SCM_UNDEFINED;
		  if (ly_scm2string ($1) =="s")
		    { /* Space */
		      Skip_req * skip_p = new Skip_req;
		      skip_p->duration_ = *$2;

		      skip_p->set_spot (THIS->here_input());
			e = skip_p->self_scm ();
		    }
		  else
		    {
		      Rest_req * rest_req_p = new Rest_req;
		      rest_req_p->duration_ = *$2;
		      rest_req_p->set_spot (THIS->here_input());
			e = rest_req_p->self_scm ();
		    }
		  Simultaneous_music* velt_p = new Request_chord (gh_list (e,SCM_UNDEFINED));
		  velt_p->set_spot (THIS->here_input());

		  delete $2; // ugh
		  $$ = velt_p;
	}
	| MEASURES optional_notemode_duration  	{
		Skip_req * sk = new Skip_req;
		sk->duration_ = *$2;
		

		Span_req *sp1 = new Span_req;
		Span_req *sp2 = new Span_req;
		sp1-> span_dir_ = START;
		sp2-> span_dir_ = STOP;
		sp1->span_type_str_ = sp2->span_type_str_ = "rest";

		Request_chord * rqc1 = new Request_chord (gh_list (sp1->self_scm (), SCM_UNDEFINED));
		Request_chord * rqc2 = new Request_chord (gh_list (sk->self_scm (), SCM_UNDEFINED));;
		Request_chord * rqc3 = new Request_chord(gh_list (sp2->self_scm (), SCM_UNDEFINED));;

		SCM ms = gh_list (rqc1->self_scm (), rqc2->self_scm (), rqc3->self_scm (), SCM_UNDEFINED);

		$$ = new Sequential_music (ms);
	}
	| STRING { 
	 	THIS->remember_spot ();
	} 
	/* cont */
	optional_notemode_duration 	{
		if (!THIS->lexer_p_->lyric_state_b ()) {
			THIS->pop_spot ().error (_ ("Have to be in Lyric mode for lyrics"));
			THIS->error_level_i_  = 1;
			THIS->parser_error (_ ("Giving up"));
		} 
		else
			THIS->pop_spot ();
		Lyric_req* lreq_p = new Lyric_req;
		lreq_p ->text_str_ = ly_scm2string ($1);
		lreq_p->duration_ = *$3;
		lreq_p->set_spot (THIS->here_input());
		Simultaneous_music* velt_p = new Request_chord (gh_list (lreq_p->self_scm (), SCM_UNDEFINED));

		delete  $3; // ugh
		$$= velt_p;

	}
	| chord {
		if (!THIS->lexer_p_->chord_state_b ())
			THIS->parser_error (_ ("Have to be in Chord mode for chords"));
		$$ = $1;
	}
	;


chord:
	steno_tonic_pitch optional_notemode_duration chord_additions chord_subtractions chord_inversion chord_bass {
                $$ = get_chord (*$1, $3, $4, $5, $6, *$2);
		$$->set_spot (THIS->here_input ());
        };

chord_additions: 
	{
		$$ = new Array<Musical_pitch>;
	} 
	| CHORD_COLON chord_notes {
		$$ = $2;
	}
	;

chord_notes:
	chord_step {
		$$ = $1
	}
	| chord_notes '.' chord_step {
		$$ = $1;
		$$->concat (*$3);
	}
	;

chord_subtractions: 
	{
		$$ = new Array<Musical_pitch>;
	} 
	| CHORD_CARET chord_notes {
		$$ = $2;
	}
	;


chord_inversion:
	{
		$$ = 0;
	}
	| '/' steno_tonic_pitch {
		$$ = $2;
		$$->set_spot (THIS->here_input ());
	}
	;

chord_bass:
	{
		$$ = 0;
	}
	| CHORD_BASS steno_tonic_pitch {
		$$ = $2;
		$$->set_spot (THIS->here_input ());
	}
	;

chord_step:
	chord_note {
		$$ = new Array<Musical_pitch>;
		$$->push (*$1);
	}
	| CHORDMODIFIER_PITCH {
		$$ = new Array<Musical_pitch>;
		$$->push (*$1);
	}
	| CHORDMODIFIER_PITCH chord_note { /* Ugh. */
		$$ = new Array<Musical_pitch>;
		$$->push (*$1);
		$$->push (*$2);
	}
	;

chord_note:
	bare_unsigned {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = 0;
        } 
	| bare_unsigned '+' {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = 1;
	}
	| bare_unsigned CHORD_MINUS {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = -1;
	}
        ;

/*
	UTILITIES
 */
number_expression:
	bare_number {
		$$ = $1;
	}
	| '-'  number_expression %prec UNARY_MINUS {
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| number_expression '*' number_expression {
		$$ = scm_product ($1, $3);
	}
	| number_expression '/' number_expression {
		$$ = scm_divide ($1, $3);
	}
	| number_expression '+' number_expression {
		$$ = scm_sum ($1, $3);
	}
	| number_expression '-' number_expression {
		$$ = scm_difference ($1, $3);
	}
	| '(' number_expression ')'	{
		$$ = $2;
	}
	;

bare_number:
	UNSIGNED	{
		$$ = gh_int2scm ($1);
	}
	| DIGIT		{
		$$ = gh_int2scm ($1);
	}
	| REAL		{
		$$ = gh_double2scm ($1);
	}
	| NUMBER_IDENTIFIER		{
		$$ = $1;
	}
	| REAL CM_T	{
		$$ = gh_double2scm ($1 CM);
	}
	| REAL PT_T	{
		$$ = gh_double2scm ($1 PT);
	}
	| REAL IN_T	{
		$$ = gh_double2scm ($1 INCH);
	}
	| REAL MM_T	{
		$$ = gh_double2scm ($1 MM);
	}
	| REAL CHAR_T	{
		$$ = gh_double2scm ($1 CHAR);
	}
	;


bare_unsigned:
	bare_number {
		if (scm_integer_p ($1) == SCM_BOOL_T) {
			$$ = gh_scm2int ($1);

		} else {
			THIS->parser_error (_("need integer number arg"));
			$$ = 0;
		}
		if ($$ < 0) {
			THIS->parser_error (_("Must be positive integer"));
			$$ = -$$;
			}

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
			THIS->parser_error (_("need integer number arg"));
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
		$$ = scm_string_append (scm_listify ($1, $3, SCM_UNDEFINED));
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


semicolon:
	';'
	;

%%

void
My_lily_parser::set_yydebug (bool b)
{
#ifdef YYDEBUG
	yydebug = b;
#endif
}
void
My_lily_parser::do_yyparse ()
{
	yyparse ((void*)this);
}


