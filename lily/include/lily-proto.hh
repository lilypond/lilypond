/*
  lily-proto.hh -- declare class names.

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LILY_PROTO_HH
#define LILY_PROTO_HH
#include "proto.hh"

struct    My_lily_lexer;
struct My_lily_parser;
struct Notename_table;
struct Absolute_dynamic_req;
struct Barcheck_req;
struct Bar_req;
struct Beam_req;
struct Blank_req;
struct Bracket_req;
struct Cadenza_req;
struct Clef_change_req;
struct Cresc_req;
struct Decresc_req;
struct Durational_req;
struct Dynamic_req;
struct Group_change_req;
struct Feature;
struct Group_feature_req;
struct Key_change_req;
struct Lyric_req;
struct Melodic_req;
struct Measure_grouping_req;
struct Meter_change_req;
struct Musical_req;
struct Command_req;
struct Collision_register;
struct Collision;
struct Note_req;
struct Pulk_voices;
struct Pulk_voice;
struct Plet_req;
struct Partial_measure_req;
struct Request_column;
struct Rest_req;
struct Rhythmic_grouping_req;
struct Rhythmic_req;
struct Script_req;
struct Skip_req;
struct Slur_req;
struct Spacing_req ;
struct Span_req;
struct Span_dynamic_req;
struct Subtle_req;
struct Stem_req;
struct Terminate_voice_req;
struct Text_req;
struct Timing_req;
struct Note_column_register;
struct Note_column;
struct Staff_side;
struct Staff_symbol;

#endif // LILY_PROTO_HH
