%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for mudela

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <iostream.h>
#include "lily-guile.hh"
#include "notename-table.hh"
#include "translation-property.hh"
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
#include "translator-group.hh"
#include "score.hh"
#include "music-list.hh"
#include "change-translator.hh"
#include "file-results.hh"
#include "scope.hh"
#include "relative-music.hh"
#include "lyric-combine-music.hh"
#include "transposed-music.hh"
#include "time-scaled-music.hh"
#include "repeated-music.hh"
#include "mudela-version.hh"
#include "grace-music.hh"
#include "auto-change-music.hh"
#include "output-property.hh"

// mmm
Mudela_version oldest_version ("1.3.4");

bool
is_duration_b (int t)
{
  return t == 1 << intlog2(t);
}




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
    Array<int> *intvec;
    Notename_table *chordmodifiertab;
    Duration *duration;
    Identifier *id;
    String * string;
    Music *music;
    Music_list *music_list;
    Score *score;
    Scope *scope;
    Interval *interval;
    Musical_req* musreq;
    Music_output_def * outputdef;
    Musical_pitch * pitch;
    Midi_def* midi;
    Moment *moment;
    Notename_table *notenametab;
    Paper_def *paper;
    Real real;
    Request * request;

 /* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesnt trigger a move
of the parse stack onto the heap. */
    SCM scm;

    Tempo_req *tempo;
    Translator_group* trans;
    char c;
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
%token DURATION
%token EXTENDER
%token FONT
%token GRACE
%token HEADER
%token HYPHEN
%token IN_T
%token INVALID
%token KEY
%token KEYSIGNATURE
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
%token PT_T
%token RELATIVE
%token REMOVE
%token REPEAT
%token REPETITIONS
%token ADDLYRICS
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
%token <id>	NOTENAME_TABLE_IDENTIFIER
%token <id>	MUSIC_IDENTIFIER
%token <id>	REQUEST_IDENTIFIER
%token <id>	REAL_IDENTIFIER
%token <id>	STRING_IDENTIFIER
%token <id>	TRANS_IDENTIFIER
%token <id>	INT_IDENTIFIER
%token <id>	SCORE_IDENTIFIER
%token <id>	MIDI_IDENTIFIER
%token <id>	PAPER_IDENTIFIER
%token <real>	REAL
%token <scm>	DURATION RESTNAME
%token <scm>	STRING
%token <scm>	SCM_T
%token <i>	UNSIGNED


%type <outputdef> output_def
%type <scope> 	mudela_header mudela_header_body
%type <request>	open_request_parens close_request_parens open_request close_request
%type <request> request_with_dir request_that_take_dir verbose_request
%type <i>	sub_quotes sup_quotes
%type <music>	simple_element  request_chord command_element Simple_music  Composite_music 
%type <music>	Alternative_music Repeated_music
%type <i>	abbrev_type
%type <i>	int unsigned
%type <i>	script_dir
%type <i>	optional_modality 
%type <id>	identifier_init  
%type <duration> steno_duration optional_notemode_duration
%type <duration> entered_notemode_duration explicit_duration
%type <intvec>	 int_list
%type <reqvec>  pre_requests post_requests
%type <request> gen_text_def
%type <pitch>   explicit_musical_pitch steno_musical_pitch musical_pitch absolute_musical_pitch
%type <pitch>   steno_tonic_pitch

%type <pitch_arr>	pitch_list
%type <music>	chord
%type <pitch_arr>	chord_additions chord_subtractions chord_notes chord_step
%type <pitch>	chord_note chord_inversion chord_bass
%type <midi>	midi_block midi_body
%type <duration>	duration_length

%type <scm>  embedded_scm scalar
%type <music>	Music Sequential_music Simultaneous_music Music_sequence
%type <music>	relative_music re_rhythmed_music
%type <music>	property_def translator_change
%type <music_list> Music_list
%type <paper>	paper_block paper_def_body
%type <real>	real real_with_dimension
%type <request> abbrev_command_req
%type <request>	post_request 
%type <request> command_req verbose_command_req
%type <request>	extender_req
%type <request> hyphen_req
%type <scm>	string
%type <score>	score_block score_body

%type <scm>	script_abbreviation
%type <trans>	translator_spec_block translator_spec_body
%type <tempo> 	tempo_request
%type <notenametab> notenames_body notenames_block chordmodifiers_block



%left '-' '+'
%left '*' '/'
%left UNARY_MINUS

%%

mudela:	/* empty */
	| mudela toplevel_expression {}
	| mudela assignment  { }
	| mudela error
	| mudela INVALID	{
		THIS->error_level_i_  =1;
	}
	;

toplevel_expression:
	notenames_block			{
		THIS->lexer_p_->set_notename_table ($1);
	}
	| chordmodifiers_block			{
		THIS->lexer_p_->set_chordmodifier_table ($1);
	}
	| mudela_header {
		delete header_global_p;
		header_global_p = $1;
	}
	| score_block {
		score_global_array.push ($1);
	}
	| paper_block {
		Identifier * id = new
			Paper_def_identifier ($1, PAPER_IDENTIFIER);
		THIS->lexer_p_->set_identifier ("$defaultpaper", id)
	}
	| midi_block {
		Identifier * id = new
			Midi_def_identifier ($1, MIDI_IDENTIFIER);
		THIS->lexer_p_->set_identifier ("$defaultmidi", id)
	}
	| embedded_scm {
		// junk value
	}	
	;

embedded_scm:
	SCM_T
	;


chordmodifiers_block:
	CHORDMODIFIERS '{' notenames_body '}'  {  $$ = $3; }
	;


notenames_block:
	NOTENAMES '{' notenames_body '}'  {  $$ = $3; }
	;



notenames_body:
	/**/	{
		$$ = new Notename_table;
	}
	| NOTENAME_TABLE_IDENTIFIER	{
		$$ = $1-> access_content_Notename_table(true);
	}
	| notenames_body STRING '=' explicit_musical_pitch {
		$$->add_note_name (ly_scm2string ($2), $4);
		delete $4;
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
	    $4->init_b_ = THIS->init_parse_b_;
	    $4->set_spot (THIS->pop_spot ());
	}
	;



identifier_init:
	score_block {
		$$ = new Score_identifier ($1, SCORE_IDENTIFIER);

	}
	| chordmodifiers_block {
		$$ = new Notename_table_identifier ($1, NOTENAME_TABLE_IDENTIFIER);
	}
	| notenames_block {
		$$ = new Notename_table_identifier ($1, NOTENAME_TABLE_IDENTIFIER);
	}
	| paper_block {
		$$ = new Paper_def_identifier ($1, PAPER_IDENTIFIER);
	}
	| midi_block {
		$$ = new Midi_def_identifier ($1, MIDI_IDENTIFIER);

	}
	| translator_spec_block {
		$$ = new Translator_group_identifier ($1, TRANS_IDENTIFIER);
	}
	| Music  {
		$$ = new Music_identifier ($1, MUSIC_IDENTIFIER);
	}

	| post_request {
		$$ = new Request_identifier ($1, REQUEST_IDENTIFIER);
	}
	| explicit_duration {
		$$ = new Duration_identifier ($1, DURATION_IDENTIFIER);
	}
	| real {
		$$ = new Real_identifier (new Real ($1), REAL_IDENTIFIER);
	}
	| string {
		$$ = new String_identifier (new String (ly_scm2string ($1)), STRING_IDENTIFIER);
	}
	| int	{
		$$ = new int_identifier (new int ($1), INT_IDENTIFIER);
	}
	;

translator_spec_block:
	TRANSLATOR '{' translator_spec_body '}'
		{
		$$ = $3;
	}
	;

translator_spec_body:
	TRANS_IDENTIFIER	{
		$$ = $1->access_content_Translator_group (true);
		$$-> set_spot (THIS->here_input ());
	}
	| TYPE STRING semicolon	{
		Translator* t = get_translator_l (ly_scm2string ($2));
		Translator_group * tg = dynamic_cast<Translator_group*> (t);

		if (!tg)
			THIS->parser_error (_("Need a translator group for a context"));
		
		tg = dynamic_cast<Translator_group*> (t->clone ());
		tg->set_spot (THIS->here_input ());
		$$ = tg;
	}
	| translator_spec_body STRING '=' embedded_scm			{
		Translator_group* tg = dynamic_cast<Translator_group*> ($$);
		tg->set_property (ly_scm2string ($2), $4);
	}
	| translator_spec_body STRING '=' identifier_init semicolon	{ 
		Identifier* id = $4;
		String_identifier *s = dynamic_cast<String_identifier*> (id);
		Real_identifier *r= dynamic_cast<Real_identifier*>(id);
		int_identifier *i = dynamic_cast<int_identifier*> (id);
	
		SCM v;
		if (s) v = ly_str02scm (s->access_content_String (false)->ch_C());
		if (i) v = gh_int2scm (*i->access_content_int (false));
		if (r) v = gh_double2scm (*r->access_content_Real (false));
		if (!s && !i && !r)
			THIS->parser_error (_("Wrong type for property value"));

		delete $4;
		/* ugh*/
		Translator_group* tg = dynamic_cast<Translator_group*> ($$);
		
		tg->set_property (ly_scm2string ($2), v);
	}
	| translator_spec_body NAME STRING semicolon {
		$$->type_str_ = ly_scm2string ($3);
	}
	| translator_spec_body CONSISTS STRING semicolon {
		dynamic_cast<Translator_group*> ($$)-> set_element (ly_scm2string ($3), true);
	}
	| translator_spec_body CONSISTSEND STRING semicolon {
		dynamic_cast<Translator_group*> ($$)-> set_element (ly_scm2string ($3), true);
	}
	| translator_spec_body ACCEPTS STRING semicolon {
		dynamic_cast<Translator_group*> ($$)-> set_acceptor (ly_scm2string ($3), true);
	}
	| translator_spec_body REMOVE STRING semicolon {
		dynamic_cast<Translator_group*> ($$)-> set_element (ly_scm2string ($3), false);
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { 
	}
	/*cont*/ '{' score_body '}' 	{
		$$ = $4;
		if (!$$->def_p_arr_.size ())
			$$->add_output (THIS->default_paper_p ());
	}
	;

score_body:		{
		$$ = new Score;
		$$->set_spot (THIS->here_input ());
	}
	| SCORE_IDENTIFIER {
		$$ = $1->access_content_Score (true);
	}
	| score_body mudela_header 	{
		$$->header_p_ = $2;
	}
	| score_body Music	{
		if ($$->music_p_)
			$2->warning (_ ("More than one music block"));	
		$$->music_p_ = $2;
	}
	| score_body output_def {
		$$->add_output ($2);
	}
	| score_body error {

	}
	;

output_def:
	paper_block {
		$$ = $1;
	}
	|  midi_block		{
		$$= $1;
	}
	;


/*
	PAPER
*/
paper_block:
	PAPER '{' paper_def_body '}' 	{ 
		$$ = $3;
		THIS-> lexer_p_->scope_l_arr_.pop ();
	}
	;


paper_def_body:
	/* empty */		 	{
		Paper_def *p = THIS->default_paper_p ();
		THIS-> lexer_p_-> scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| PAPER_IDENTIFIER 	{
		Paper_def *p = $1->access_content_Paper_def (true);
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| paper_def_body int '=' FONT STRING		{ // ugh, what a syntax
		Lookup * l = new Lookup;
		l->font_name_ = ly_scm2string ($5);
		$$->set_lookup ($2, l);
	}
	| paper_def_body assignment semicolon {
		
	}
	| paper_def_body SCM_T '=' SCM_T {
		if (!gh_symbol_p ($2))
			THIS->parser_error ("expect a symbol as lvalue");
		else
			$$->default_properties_.set ($2, $4);
	}
	| paper_def_body SCM_T '=' real semicolon {
		if (!gh_symbol_p ($2))
			THIS->parser_error ("expect a symbol as lvalue");
		else
			$$->default_properties_.set ($2, gh_double2scm ($4));
	}
	| paper_def_body translator_spec_block {
		$$->assign_translator ($2);
	}
	| paper_def_body error {

	}
	;



real_with_dimension:
	REAL CM_T	{
		$$ = $1 CM;
	}
	| REAL PT_T	{
		$$ = $1 PT;
	}
	| REAL IN_T	{
		$$ = $1 INCH;
	}
	| REAL MM_T	{
		$$ = $1 MM;
	}
	| REAL CHAR_T	{
		$$ = $1 CHAR;
	}
	;

real:
	REAL		{
		$$ = $1;
	}
	| real_with_dimension
	| REAL_IDENTIFIER		{
		$$= *$1->access_content_Real (false);
	}
	| '-'  real %prec UNARY_MINUS {
		$$ = -$2;
	}
	| real '*' real {
		$$ = $1 * $3;
	}
	| real '/' real {
		$$ = $1 / $3;
	}
	| real '+' real {
		$$ = $1  + $3;
	}
	| real '-' real {
		$$ = $1 - $3;
	}
	| '(' real ')'	{
		$$ = $2;
	}
	;
		
/*
	MIDI
*/
midi_block:
	MIDI
	'{' midi_body '}' 	{
		$$ = $3;
		THIS-> lexer_p_-> scope_l_arr_.pop();
	}
	;

midi_body: /* empty */ 		{
		Midi_def * p =THIS->default_midi_p ();
		$$ = p;
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
	}
	| MIDI_IDENTIFIER	{
		Midi_def * p =$1-> access_content_Midi_def (true);
		$$ = p;
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
	}
	| midi_body assignment semicolon {

	}
	| midi_body translator_spec_block	{
		$$-> assign_translator ($2);
	}
	| midi_body tempo_request semicolon {
		$$->set_tempo ($2->dur_.length_mom (), $2->metronome_i_);
		delete $2;
	}
	| midi_body error {

	}
	;

tempo_request:
	TEMPO steno_duration '=' unsigned	{
		$$ = new Tempo_req;
		$$->dur_ = *$2;
		delete $2;
		$$-> metronome_i_ = $4;
	}
	;

Music_list: /* empty */ {
		$$ = new Music_list;
		$$->set_spot (THIS->here_input ());
	}
	| Music_list Music {
		$$->add_music ($2);
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
	REPEAT STRING unsigned Music Alternative_music
	{
		Music_sequence* m = dynamic_cast <Music_sequence*> ($5);
		if (m && $3 < m->length_i ())
			$5->warning (_ ("More alternatives than repeats.  Junking excess alternatives."));

		Repeated_music * r = new Repeated_music ($4, $3 >? 1, m);
		$$ = r;
		r->fold_b_ = (ly_scm2string ($2) == "fold");
		r->volta_fold_b_ =  (ly_scm2string ($2) == "volta");
		r->set_spot ($4->spot  ());
	}
	;

Music_sequence: '{' Music_list '}'	{
		$$ = new Music_sequence ($2);
		$$->set_spot ($2->spot ());
	}
	;

Sequential_music:
	SEQUENTIAL '{' Music_list '}'		{
		$$ = new Sequential_music ($3);
		$$->set_spot ($3->spot ());
	}
	| '{' Music_list '}'		{
		$$ = new Sequential_music ($2);
		$$->set_spot ($2->spot ());
	}
	;

Simultaneous_music:
	SIMULTANEOUS '{' Music_list '}'{
		$$ = new Simultaneous_music ($3);
		$$->set_spot ($3->spot ());
	}
	| '<' Music_list '>'	{
		$$ = new Simultaneous_music ($2);
		$$->set_spot ($2->spot ());
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
	
		$$ = new Output_property (pred,$3, $5);
	}
	| MUSIC_IDENTIFIER { $$ = $1->access_content_Music (true); }
	| property_def
	| translator_change
	| Simple_music '*' unsigned '/' unsigned 	{
		$$ = $1;
		$$->compress (Moment($3, $5 ));
	}
	| Simple_music '*' unsigned		 {
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
		Auto_change_music * chm = new Auto_change_music (ly_scm2string ($2), $3);

		$$ = chm;
		chm->set_spot ($3->spot ());
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
		unsigned '/' unsigned Music 	

	{
		$$ = new Time_scaled_music ($3, $5, $6);
		$$->set_spot (THIS->pop_spot ());
	}
	| Repeated_music		{ $$ = $1; }
	| Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	| TRANSPOSE musical_pitch Music {
		$$ = new Transposed_music ($3, *$2);
		delete $2;
	}
	| TRANSPOSE steno_tonic_pitch Music {
		$$ = new Transposed_music ($3, *$2);
		delete $2;
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
	;

relative_music:
	RELATIVE absolute_musical_pitch Music {
		$$ = new Relative_octave_music ($3, *$2);
		delete $2;
	}
	;

re_rhythmed_music:
	ADDLYRICS Music Music {
		Lyric_combine_music * l = new Lyric_combine_music ($2, $3);
		$$ = l;
	}
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Change_translator * t = new Change_translator;
		t-> change_to_type_str_ = ly_scm2string ($2);
		t-> change_to_id_str_ = ly_scm2string ($4);

		$$ = t;
		$$->set_spot (THIS->here_input ());
	}
	;

property_def:
	PROPERTY STRING '.' STRING '='  scalar {
		Translation_property *t = new Translation_property;

		t->var_str_ = ly_scm2string ($4);
		t->value_ = $6;

		Context_specced_music *csm = new Context_specced_music (t);
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> translator_type_str_ = ly_scm2string ($2);
	}
	;

scalar:
	string		{ $$ = $1; }
	| int		{ $$ = gh_int2scm ($1); }
	| embedded_scm 	{ $$ = $1; }
	;


request_chord:
	pre_requests simple_element post_requests	{
		Music_sequence *l = dynamic_cast<Music_sequence*>($2);
		if (l) {
			for (int i=0; i < $1->size(); i++)
				l->add_music ($1->elem(i));
			for (int i=0; i < $3->size(); i++)
				l->add_music ($3->elem(i));
			}
		else
			programming_error ("Need Sequence to add music to");
 		$$ = $2;
		
	}
	| command_element
	;

command_element:
	command_req {
		$$ = new Request_chord;
		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
		((Simultaneous_music*)$$) ->add_music ($1);//ugh
	}
	| PARTIAL duration_length ';' 	{
		Translation_property * p = new Translation_property;
		p->var_str_ = "measurePosition";
		p->value_ =  (new Moment (-$2->length_mom ()))->smobify_self ();
		delete $2;
		Context_specced_music * sp = new Context_specced_music (p);
		$$ =sp ;
		sp-> translator_type_str_ = "Score";
	}
	;

command_req:
	abbrev_command_req
	| verbose_command_req semicolon	{ $$ = $1; }
	;

abbrev_command_req:
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
	| '[' ':' unsigned {
		if (!is_duration_b ($3))
		  THIS->parser_error (_f ("not a duration: %d", $3));
		else if ($3 < 8)
		  THIS->parser_error (_ ("Can't abbreviate"));
		else
		  THIS->set_abbrev_beam ($3);

		Chord_tremolo_req* a = new Chord_tremolo_req;
		a->span_dir_ = START;
		a->type_i_ = THIS->abbrev_beam_type_i_;
		$$=a;
	}
	| ']'		{
		if (!THIS->abbrev_beam_type_i_)
		  {
		     Span_req*b= new Span_req;
		     b->span_dir_ = STOP;
		     b->span_type_str_ = "beam";
		     $$ = b;
		   }
		else
		  {
		    Chord_tremolo_req* a = new Chord_tremolo_req;
		    a->span_dir_ = STOP;
		    a->type_i_ = THIS->abbrev_beam_type_i_;
		    THIS->set_abbrev_beam (0);
		    $$ = a;
		  }
	}
	| BREATHE {
		$$ = new Breathing_sign_req;
	}
	;


verbose_command_req:
	
	BAR STRING 			{
		$$ = new Bar_req (ly_scm2string ($2));
	}
	| COMMANDSPANREQUEST int STRING {
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
		m->mark_label_ = $2;
		$$ = m;

	}
	| MARK unsigned {
		Mark_req *m = new Mark_req;
		m->mark_label_ =  gh_int2scm ($2);
		$$ = m;
	}

	| TIME_T unsigned '/' unsigned 	{
		Time_signature_change_req *m = new Time_signature_change_req;
		m->beats_i_ = $2;
		m->one_beat_i_=$4;
		$$ = m;
	}
	| PENALTY int 	{
		Break_req * b = new Break_req;
		b->penalty_f_ = $2 / 100.0;
		b->set_spot (THIS->here_input ());
		$$ = b;
	}
	| SKIP duration_length {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_ = *$2;
		delete $2;
		$$ = skip_p;
	}
	| tempo_request {
		$$ = $1;
	}
	| CLEF STRING {
		$$ = new Clef_change_req (ly_scm2string ($2));

	}
/* UGH. optional.  */
	| KEY NOTENAME_PITCH optional_modality	{
		Key_change_req *key_p= new Key_change_req;
		key_p->key_.pitch_arr_.push (*$2);
		key_p->key_.ordinary_key_b_ = true;
		key_p->key_.modality_i_ = $3;
		$$ = key_p;
		delete $2;
	}
	| KEYSIGNATURE pitch_list {
		Key_change_req *key_p= new Key_change_req;
		key_p->key_.pitch_arr_ = *$2;
		key_p->key_.ordinary_key_b_ = false;
		$$ = key_p;
		delete $2;
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
		Identifier*i = THIS->lexer_p_->lookup_identifier ("dash-" + ly_scm2string ($1));
		Articulation_req *a = new Articulation_req;
		a->articulation_str_ = *i->access_content_String (false);
		$$ = a;
	}
	;

request_with_dir:
	script_dir request_that_take_dir	{
		if (Script_req * gs = dynamic_cast<Script_req*> ($2))
			gs->dir_ = Direction ($1);
		else if ($1)
			$2->warning (_ ("Can't specify direction for this request"));
		$$ = $2;
	}
	;
	
verbose_request:
	REQUEST_IDENTIFIER	{
		$$ = (Request*)$1->access_content_Request (true);
		$$->set_spot (THIS->here_input ());
	}
	| TEXTSCRIPT STRING STRING 	{
		Text_script_req *ts_p = new Text_script_req;
		ts_p-> text_str_ = ly_scm2string ($2);
		ts_p-> style_str_ = ly_scm2string ($3);
		ts_p->set_spot (THIS->here_input ());

		$$ = ts_p;
	}
	| SPANREQUEST int STRING {
		Span_req * sp_p = new Span_req;
		sp_p-> span_dir_  = Direction($2);
		sp_p->span_type_str_ = ly_scm2string ($3);
		sp_p->set_spot (THIS->here_input ());
		$$ = sp_p;
	}
	| abbrev_type	{
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
	;

optional_modality:
	/* empty */	{
		$$ = 0;
	}
	| int	{
		$$ = $1;
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

explicit_musical_pitch:
	MUSICAL_PITCH '{' int_list '}'	{/* ugh */
		Array<int> &a = *$3;
		ARRAY_SIZE(a,3);
		$$ = new Musical_pitch;
		$$->octave_i_ = a[0];
		$$->notename_i_ = a[1];
		$$->accidental_i_ = a[2];
		delete &a;
	}
	;

musical_pitch:
	steno_musical_pitch {
		$$ = $1;
	}
	| explicit_musical_pitch {
		$$ = $1;
	}
	;

explicit_duration:
	DURATION '{' int_list '}'	{
		$$ = new Duration;
		Array<int> &a = *$3;
		ARRAY_SIZE(a,2);
			
		$$-> durlog_i_ = a[0];
		$$-> dots_i_ = a[1];

		delete &a;		
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
	| duration_length '*' unsigned {
		$$->tuplet_iso_i_ *= $3;
	}
	| duration_length '/' unsigned {
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
	unsigned		{
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


abbrev_type: 
	':'	{
		$$ =0;
	}
	| ':' unsigned {
		if (!is_duration_b ($2))
			THIS->parser_error (_f ("not a duration: %d", $2));
		else if ($2 < 8)
			THIS->parser_error (_ ("Can't abbreviate"));
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
		if (THIS->abbrev_beam_type_i_)
		  {
		    if (n->duration_.plet_b ())
		      THIS->parser_error (_ ("Can't abbreviate tuplet"));
		    else
		      n->duration_.set_plet (1, 2);
		  }
		n->cautionary_b_ = $3 % 2;
		n->forceacc_b_ = $2 % 2 || n->cautionary_b_;

		Simultaneous_music*v = new Request_chord;
		v->set_spot ($1->spot ());
		n->set_spot ($1->spot ());

		v->add_music (n);

		$$ = v;

		delete $1;
		delete $4;

	}
	| RESTNAME optional_notemode_duration		{
		  Simultaneous_music* velt_p = new Request_chord;
		  velt_p->set_spot (THIS->here_input());

		  if (ly_scm2string ($1) =="s")
		    { /* Space */
		      Skip_req * skip_p = new Skip_req;
		      skip_p->duration_ = *$2;

		      skip_p->set_spot (THIS->here_input());
		      velt_p->add_music (skip_p);
		    }
		  else
		    {
		      Rest_req * rest_req_p = new Rest_req;
		      rest_req_p->duration_ = *$2;
		      rest_req_p->set_spot (THIS->here_input());

		      velt_p->add_music (rest_req_p);
		    }

		  delete $2;
		  $$ = velt_p;
	}
	| MEASURES optional_notemode_duration  	{
		Skip_req * sk = new Skip_req;
		sk->duration_ = *$2;
		Music_list * ms = new Music_list;
		Request_chord * rqc1 = new Request_chord;
		Request_chord * rqc2 = new Request_chord;
		Request_chord * rqc3 = new Request_chord;

		Span_req *sp1 = new Span_req;
		Span_req *sp2 = new Span_req;
		sp1-> span_dir_ = START;
		sp2-> span_dir_ = STOP;
		sp1->span_type_str_ = sp2->span_type_str_ = "rest";
		rqc1->add_music (sp1);
		rqc2->add_music (sk);
		rqc3->add_music (sp2);
		
		ms->add_music (rqc1);
		ms->add_music (rqc2);
		ms->add_music (rqc3);

		$$ = new Sequential_music (ms);
	}
	| STRING optional_notemode_duration 	{
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		  Simultaneous_music* velt_p = new Request_chord;

		  Lyric_req* lreq_p = new Lyric_req;
		  lreq_p ->text_str_ = ly_scm2string ($1);
		  lreq_p->duration_ = *$2;
		  lreq_p->set_spot (THIS->here_input());

		  velt_p->add_music (lreq_p);

		  delete  $2;
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
                $$ = THIS->get_chord (*$1, $3, $4, $5, $6, *$2);
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
	unsigned {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = 0;
        } 
	| unsigned '+' {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = 1;
	}
	| unsigned CHORD_MINUS {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = -1;
	}
        ;

/*
	UTILITIES
 */
pitch_list:			{
		$$ = new Array<Musical_pitch>;
	}
	| pitch_list musical_pitch	{
		$$->push (*$2);
		delete $2;
	}
	;


int_list:
	/**/			{
		$$ = new Array<int>
	}
	| int_list int 		{
		$$->push ($2);		
	}
	;

unsigned:
	UNSIGNED	{
		$$ = $1;
	}
	| DIGIT		{
		$$ = $1;
	}
	;

int:
	unsigned {
		$$ = $1;
	}
	| '-' unsigned {
		$$ = -$2;
	}
	| INT_IDENTIFIER	{
		$$ = *$1->access_content_int (false);
	}
	;


string:
	STRING		{
		$$ = $1;
	}
	| STRING_IDENTIFIER	{
		$$ = ly_str02scm ($1->access_content_String (true)->ch_C ());
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


