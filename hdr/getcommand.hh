/*
  getcommand.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef GETCOMMAND_HH
#define GETCOMMAND_HH
#include "proto.hh"


Command* get_meter_command(int,int);
Command get_defaultbar_command();

void split_bar_command(Command &pre_com, Command &no_break_com, Command &post_com,String s);


#endif // GETCOMMAND_HH
