/*
  inputcommand.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INPUTCOMMAND_HH
#define INPUTCOMMAND_HH
#include "proto.hh"
#include "scalar.hh"
#include "vray.hh"

struct Input_command {
    Real when;
    
    /// analogous to argv[]
    svec<Scalar> args;
    void print()const;
    Input_command();
    Input_command(Real);
    operator Command();
};

Input_command* get_meterchange_command( int,int);
Input_command* get_key_interpret_command(svec<String>);
Input_command* get_clef_interpret_command(String w);
Input_command *get_reset_command();
Input_command *get_partial_command(Real u);
Input_command* get_skip_command( int,Real);
Input_command* get_grouping_command( svec<int>);

void
interpret_meter(Input_command *c, int &beats_per_meas, int& one_beat,
		Real& whole_per_measure);

Input_command *get_bar_command(Real );
#endif // INPUTCOMMAND_HH

