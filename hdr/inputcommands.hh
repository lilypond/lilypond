/*
  inputcommands.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INPUTCOMMANDS_HH
#define INPUTCOMMANDS_HH
#include "pcursor.hh"
#include "proto.hh"
#include "plist.hh"
#include "real.hh"

struct Input_cursor : public PCursor<Command*>
{
    /// current measure info
    Real whole_per_measure;

    /// where am i 
    Real whole_in_measure;

    /// Real last when which was read
    Real last;
    
    int bars;
    
    Input_cursor(PCursor<Command*>);
    /// hmm. not safe. Should rethink cursor.
    void operator++(int);
    /** warning: no optor -- () defined.. */
    void reset();
    Real when()const;
    void add(Command*);
    void setpartial(Real);
    void addbot(Command*);
    void sync();
    void print()const;   
    void last_command_here();
};

/// the list of commands in Score
struct Input_commands : public IPointerList<Command*> {
    Input_cursor ptr;

    /****************/

    void find_moment(Real);
    void do_skip(int & bars, Real & wholes);
    void truncate(Real);
    
    Input_commands();
    Input_commands(Input_commands const&);
    void add(Command*);
    void reset();
    void print()const;
    Staff_commands *parse() const;
};


void
interpret_meter(Command *c, int &beats_per_meas, int& one_beat,
		Real& whole_per_measure);
#endif // INPUTCOMMANDS_HH

