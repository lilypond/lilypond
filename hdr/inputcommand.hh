/*
  inputcommand.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INPUTCOMMAND_HH
#define INPUTCOMMAND_HH
#include "proto.hh"
#include "scalar.hh"
#include "varray.hh"
#include "moment.hh"

struct Input_command {
    /// analogous to argv[]
    Array<Scalar> args;
    void print()const;
    Input_command();

    operator Command();
};
Input_command *get_meterchange_command( int,int);
Input_command *get_key_interpret_command(Array<int >);
Input_command *get_clef_interpret_command(String w);
Input_command *get_reset_command();
Input_command *get_partial_command(Moment u);
Input_command *get_skip_command(int,Moment);
Input_command *get_grouping_command(Array<int>);
Input_command *get_bar_command(String);
Input_command *get_newmeasure_command();
Array<int> get_default_grouping(int count);
#endif // INPUTCOMMAND_HH

