/*
  parseconstruct.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef PARSECONSTRUCT_HH
#define PARSECONSTRUCT_HH

#include "lily-proto.hh"

String * get_scriptdef (char c);
Request* get_script_req (char);
Request*get_grouping_req (Array<int> i_arr);

#endif // PARSECONSTRUCT_HH

