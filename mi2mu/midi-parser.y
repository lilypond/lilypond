%{

#include "mi2mu.hh"

#ifndef NDEBUG
#define YYDEBUG 1
#endif

%}

%union {
    Byte byte;
    char c;
    int i;
    String* str_p;
    Request* request_p;
    Midi_event* midi_event_p;	// Voice_element* ?
    Midi_score* midi_score_p; 	// Input_score* ?
    Midi_track* midi_track_p;	// Input_music* ?
}

%token HEADER TRACK
%token SYSEX_EVENT1 SYSEX_EVENT2
%token META_EVENT
%token SEQUENCE
%token END_OF_TRACK TEMPO SMPTE_OFFSET TIME KEY SSME

%token<i> I8 U8 INT16 INT32 INT7_8UNSET INT7_8SET VARINT
%token<i> RUNNING_STATUS DATA_ENTRY ALL_NOTES_OFF
%token<i> NOTE_OFF NOTE_ON 
%token<i> POLYPHONIC_AFTERTOUCH CONTROLMODE_CHANGE PROGRAM_CHANGE 
%token<i> CHANNEL_AFTERTOUCH PITCHWHEEL_RANGE
%token<i> YYTEXT YYCOPYRIGHT YYTRACK_NAME YYINSTRUMENT_NAME YYLYRIC YYMARKER YYCUE_POINT
%token<str_p> DATA

%type <i> varint
%type <midi_score_p> header midi_score
%type <midi_track_p> track
%type <midi_event_p> event
%type <midi_event_p> the_event meta_event the_meta_event text_event midi_event sysex_event
%type <midi_event_p> running_status data_entry all_notes_off
%type <midi_event_p> note_off note_on
%type <midi_event_p> polyphonic_aftertouch controlmode_change program_change
%type <midi_event_p> channel_aftertouch pitchwheel_range

%%

midi:	/* empty */
	| midi midi_score {
		midi_parser_l_g->add_score( $2 );		
	}
	;

midi_score:
	header {
	}
	| midi_score track {
		$$->add_track( $2 );
		// ugh
		$2->set_tempo( midi_parser_l_g->midi_tempo_p_->useconds_per_4_i() );
		$2->set_time( midi_parser_l_g->midi_time_p_->num_i(), 
			midi_parser_l_g->midi_time_p_->den_i(), 
			midi_parser_l_g->midi_time_p_->clocks_1_i(), 
			8 );
		if ( midi_parser_l_g->copyright_str_.length_i() )
			$2->copyright_str_ = midi_parser_l_g->copyright_str_;
		if ( midi_parser_l_g->track_name_str_.length_i() )
			$2->name_str_ = midi_parser_l_g->track_name_str_;
		if ( midi_parser_l_g->instrument_str_.length_i() )
			$2->instrument_str_ = midi_parser_l_g->instrument_str_;
		midi_parser_l_g->reset();
	}
	;

header:	
	HEADER INT32 INT16 INT16 INT16 {
		$$ = new Midi_score( $3, $4, $5 );
		midi_parser_l_g->set_division_4( $5 );
	}
	;

track: 
	TRACK INT32 {
		mtor << "\ntrack " << midi_parser_l_g->track_i_ << ": " << flush;
		$$ = new Midi_track( midi_parser_l_g->track_i_++,
			// silly, cause not set yet!
			midi_parser_l_g->copyright_str_,
			midi_parser_l_g->track_name_str_,
			midi_parser_l_g->instrument_str_ );
	}
	| track event {
		$$->add_event( midi_parser_l_g->mom(), $2 );
	}
	;

event:	
	varint the_event {
		$$ = $2;
		if ( $2 ) {
			String str = $2->mudela_str( false );
			if ( str.length_i() )
				dtor << str << " " << flush;
		}
	}
	;
	
varint:
	VARINT {
		midi_parser_l_g->forward( $1 );
		if ( $1 ) {
			int bars_i = (int)( midi_parser_l_g->mom() / midi_parser_l_g->midi_time_p_->bar_mom() );
			if ( bars_i > midi_parser_l_g->bar_i_ )
				mtor << '[' << midi_parser_l_g->bar_i_++ 
					<< ']' << flush; 
		}
	}
	;

the_event: 
	meta_event { 
	}
	| midi_event {
	}
	| sysex_event {
	}
	;

meta_event:
	META_EVENT the_meta_event {
		$$ = $2;
	}
	|
	META_EVENT U8 U8 U8 {
		$$ = 0;
	}
	;

the_meta_event:
	SEQUENCE INT16 {
	}
	| text_event DATA {
		Midi_text::Type type = (Midi_text::Type)$1;
		$$ = 0;
		switch ( type )
			{
			case Midi_text::COPYRIGHT:
			        midi_parser_l_g->copyright_str_ = *$2;
				break;
			case Midi_text::TRACK_NAME:
			        midi_parser_l_g->track_name_str_ = *$2;
				while ( midi_parser_l_g->track_name_str_.index_i( ' ' ) != -1 )
			        	*(midi_parser_l_g->track_name_str_.ch_l() + midi_parser_l_g->track_name_str_.index_i( ' ' ) ) = '_';
				break;
			case Midi_text::INSTRUMENT_NAME:
			        midi_parser_l_g->instrument_str_ = *$2;
				break;
			default:
				$$ = new Midi_text( type, *$2 );
				break;
			}
		dtor << *$2 << endl;
		delete $2;
	}
	| END_OF_TRACK {
		$$ = 0;
	}
	| TEMPO U8 U8 U8 { 
		$$ = new Midi_tempo( ( $2 << 16 ) + ( $3 << 8 ) + $4 );
		dtor << $$->mudela_str( false ) << endl;
		midi_parser_l_g->set_tempo( ( $2 << 16 ) + ( $3 << 8 ) + $4 );
	}
	| SMPTE_OFFSET U8 U8 U8 U8 U8 { 
		$$ = 0;
	}
	| TIME U8 U8 U8 U8 { 
		$$ = new Midi_time( $2, $3, $4, $5 );
		dtor << $$->mudela_str( true ) << endl;
		midi_parser_l_g->set_time( $2, $3, $4, $5 );
	}
	| KEY I8 I8 { 
		$$ = new Midi_key( $2, $3 );
		midi_parser_l_g->set_key( $2, $3  );
	}
	| SSME DATA {
		$$ = new Midi_text( (Midi_text::Type)0, *$2 );
		delete $2;
	}
	;

text_event: 
	YYTEXT {
		dtor << "\n% Text: ";
	}
	| YYCOPYRIGHT {
		dtor << "\n% Copyright: ";
	}
	| YYTRACK_NAME {
		dtor << "\n% Track  name: ";
	}
	| YYINSTRUMENT_NAME {
		dtor << "\n% Instrument  name: ";
	}
	| YYLYRIC {
		dtor << "\n% Lyric: ";
	}
	| YYMARKER {
		dtor << "\n% Marker: ";
	}
	| YYCUE_POINT {
		dtor << "\n% Cue point: ";
	}
	;

midi_event: 
	running_status {
	}
	| data_entry {
	}
	| all_notes_off {
	}
	| note_off {
	}
	| note_on {
	}
	| polyphonic_aftertouch {
	}
	| controlmode_change {
	}
	| program_change {
	}
	| channel_aftertouch {
	}
	| pitchwheel_range {
	}
	;

running_status:
	RUNNING_STATUS U8 { //U8 {
		$$ = 0;
	}
	;

data_entry:
	DATA_ENTRY U8 {
		$$ = 0;
	}
	;

all_notes_off:
	ALL_NOTES_OFF U8 U8 {
		$$ = 0;
	}
	;

note_off:
	NOTE_OFF U8 U8 {
		int i = $1;
		i = i & ~0x80;
		$$ = midi_parser_l_g->note_end_midi_event_p( $1 & ~0x80, $2, $3 );
	}
	;

note_on:
	NOTE_ON U8 U8 {
		int i = $1;
		i = i & ~0x90;
		$$ = 0;
		midi_parser_l_g->note_begin( $1 & ~0x90, $2, $3 );
	}
	;

polyphonic_aftertouch:
	POLYPHONIC_AFTERTOUCH U8 U8 {
		$$ = 0;
	}
	;

controlmode_change:
	CONTROLMODE_CHANGE U8 U8 {
		$$ = 0;
	}
	;

program_change:
	PROGRAM_CHANGE U8 {
		$$ = 0;
	}
	;

channel_aftertouch:
	CHANNEL_AFTERTOUCH U8 U8 {
		$$ = 0;
	}
	;

pitchwheel_range:
	PITCHWHEEL_RANGE U8 U8 {
		$$ = 0;
	}
	;

sysex_event:
	SYSEX_EVENT1 DATA {
		$$ = 0;
	}
	| SYSEX_EVENT2 DATA { // U8 ?
		$$ = 0;
	}
	;
