%{
// midi-lexer.l

#include <stdio.h>

#include "string.hh"
#include "proto.hh"
#include "my-midi-lexer.hh"
#include "midi-parser.hh"

#ifndef MIDI_LEX_DEBUG
#define puts( x )
#endif

%}

%option c++
%option noyywrap
%option nodefault
%option yylineno
%option debug
%option yyclass="My_midi_lexer"
%option stack

%x data
%x event
%x int8
%x int16
%x int32
%x meta_event
%x track

INT8		[\x00-\xff]
INT16		{INT8}{INT8}
INT32		{INT16}{INT16}
INT7_8UNSET	[\x00-\x7f]
INT7_8SET	[\x80-\xff]
VARINT		{INT7_8SET}{0,3}{INT7_8UNSET}

HEADER		MThd
TRACK		MTrk

RUNNING_STATUS	[\x30-\x4f]
NOTE_OFF	[\x80-\x8f]
NOTE_ON		[\x90-\x9f]
PROGRAM_CHANGE	[\xc0-\xcf]

SYSEX_EVENT1	[\xf0]
SYSEX_EVENT2	[\xf7]

META_EVENT	[\xff]

SEQUENCE	[\x00][\x02]
TEXT		[\x01] 
COPYRIGHT	[\x02]
TRACK_NAME	[\x03]
INSTRUMENT_NAME	[\x04]
LYRIC		[\x05]
MARKER		[\x06]
CUE_POINT	[\x07]

END_OF_TRACK	[\x2f][\x00]
TEMPO		[\x51][\x03]
SMPTE_OFFSET	[\x54][\x05]
TIME		[\x58][\x04]
KEY		[\x59][\x02]
SSME		[\0x7f][\x03]

%%

{HEADER}/{INT32}	{ // using /{INT32}; longer match than {INT32}
	puts( "lex: header" );
	yy_push_state( int16 ); 
	yy_push_state( int16 ); 
	yy_push_state( int16 ); 
	yy_push_state( int32 ); 
	return HEADER;
}

{TRACK}/{INT32}	{ // using /{INT32}; longer match than {INT32}
	puts( "lex: track" );
	yy_push_state( track ); 
	yy_push_state( int32 ); 
	return TRACK;
}

<int32>{INT32}	{
	puts( "lex: int32" );
	assert( YYLeng() == 4 );
	String str( (Byte const*)YYText(), YYLeng() );
	yylval.i = StringConversion::bin2int_i( str );
	yy_pop_state();
	return INT32;
}
<int16>{INT16}	{
	puts( "lex: int16" );
	assert( YYLeng() == 2 );
	String str( (Byte const*)YYText(), YYLeng() );
	yylval.i = StringConversion::bin2int_i( str );
	yy_pop_state();
	return INT16;
}
<int8>{INT8}	{
	puts( "lex: int8" );
	assert( YYLeng() == 1 );
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state(); 
	return INT8;
}

<track>{VARINT} {
	puts( "lex: track: varint" );
	String str( (Byte const*)YYText(), YYLeng() );
	yylval.i = My_midi_lexer::varint2int_i( str );
	yy_push_state( event ); 
	return VARINT;
}

<event>{RUNNING_STATUS}	{
	yylval.byte = *(Byte*)YYText();
	yy_pop_state(); 
	yy_push_state( int8 );
	return RUNNING_STATUS;
}
<event>{NOTE_OFF}	{
	puts( "lex: note off" );
	yylval.byte = *(Byte*)YYText();
	yy_pop_state(); 
	yy_push_state( int8 );
	yy_push_state( int8 );
	return NOTE_OFF;
}
<event>{NOTE_ON}	{
	puts( "lex: note on" );
	yylval.byte = *(Byte*)YYText();
	yy_pop_state(); 
	yy_push_state( int8 );
	yy_push_state( int8 );
	return NOTE_ON;
}
<event>{PROGRAM_CHANGE}	{
	yylval.byte = *(Byte*)YYText();
	yy_pop_state(); 
	yy_push_state( int8 );
	return PROGRAM_CHANGE;
}
<event>{META_EVENT}	{
	yy_push_state( meta_event );
	return META_EVENT;
}

<meta_event>{SEQUENCE}	{	// ssss sequence number
	yy_push_state( int16 );
	return SEQUENCE;
}
<meta_event>{TEXT}	{		// len data
	yy_push_state( data );
	return TEXT;
}
<meta_event>{COPYRIGHT}	{
	yy_push_state( data );
	return COPYRIGHT;
}
<meta_event>{TRACK_NAME}	{
	yy_push_state( data );
	return TRACK_NAME;
}
<meta_event>{INSTRUMENT_NAME}	{
	yy_push_state( data );
	return INSTRUMENT_NAME;
}
<meta_event>{LYRIC}	{
	yy_push_state( data );
	return LYRIC;
}
<meta_event>{MARKER}	{
	yy_push_state( data );
	return MARKER;
}
<meta_event>{CUE_POINT}	{
	yy_push_state( data );
	return CUE_POINT;
}
<meta_event>{TEMPO}	{ 	// tttttt usec
	puts( "lex: tempo" );
	yy_pop_state();
	yy_pop_state();
	yy_push_state( int8 );
	yy_push_state( int8 );
	yy_push_state( int8 );
	return TEMPO;
}
<meta_event>{SMPTE_OFFSET}	{ 		// hr mn se fr ff
	yy_pop_state();
	yy_pop_state();
	yy_push_state( int8 );
	yy_push_state( int8 );
	yy_push_state( int8 );
	yy_push_state( int8 );
	yy_push_state( int8 );
	return SMPTE_OFFSET;
}
<meta_event>{TIME}	{		// nn dd cc bb
	puts( "lex: time" );
	yy_pop_state();
	yy_pop_state();
	yy_push_state( int8 );
	yy_push_state( int8 );
	yy_push_state( int8 );
	yy_push_state( int8 );
	return TIME;
}
<meta_event>{KEY}	{	// sf mi
	puts( "lex: key" );
	yy_pop_state();
	yy_pop_state();
	yy_push_state( int8 );
	yy_push_state( int8 );
	return KEY;
}
<meta_event>{SSME}	{	// len data
	yy_pop_state();
	yy_pop_state();
	yy_push_state( data );
	return SSME;
}
<meta_event>{END_OF_TRACK} {
	puts( "lex: end of track" );
	yy_pop_state();
	yy_pop_state();
	yy_pop_state();
	return END_OF_TRACK;
}
<meta_event>{INT8} {
	yylval.byte = *(Byte*)YYText();
	return INT8;
}

<data>{VARINT} {
	String str( (Byte const*)YYText(), YYLeng() );
	int i = My_midi_lexer::varint2int_i( str );
	String* str_p = new String;
	while ( i-- )
		*str_p += (char)yyinput();
	yylval.str_p = str_p;
	yy_pop_state();
	yy_pop_state();
	return DATA;
}


<<EOF>> {
//	mtor << "<<EOF>>";

	if ( !close_i() )
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
}

%%

