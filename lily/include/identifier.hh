/*
  identifier.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH

#include "lily-proto.hh"
#include "string.hh"
#include "input.hh"
#include "virtual-methods.hh"

#define IDACCESSOR( Input_staff, staff)\
virtual Input_staff * staff () { error (#Input_staff); return 0; }

/**
   A declarable data structure in mudela. 


   */
struct Identifier : public Input {
  bool init_b_;
  bool accessed_b_;
  int token_code_i_;
    
  Identifier (int code) ;
  virtual ~Identifier() ;

  void print() const;
  DECLARE_MY_RUNTIME_TYPEINFO;
  void error (String);
  IDACCESSOR(Translator, translator)
    IDACCESSOR(Music, music)
    IDACCESSOR(General_script_def, script)
    IDACCESSOR(Symtables, symtables)
    IDACCESSOR(Midi_def, mididef)
    IDACCESSOR(Paper_def, paperdef)
    IDACCESSOR(Lookup,lookup)
    IDACCESSOR(Real,real)
    IDACCESSOR(Request, request)
    IDACCESSOR(Score, score)
    IDACCESSOR(int, intid)
    IDACCESSOR(Duration, duration)

    protected:
  virtual void do_print() const=0;
private:
  Identifier (Identifier const&);
};

#define DECLARE_ID_CLASS(Idclass, Class, accessor)	\
struct Idclass : Identifier {\
			     Class *data_p_;		     \
			     DECLARE_MY_RUNTIME_TYPEINFO; 			    \
			     Idclass (Class*st, int code);\
			     virtual Class* accessor ();\
			     ~Idclass();\
			     virtual void do_print() const; \
}\


DECLARE_ID_CLASS(Translator_id, Translator, translator);
DECLARE_ID_CLASS(Duration_id, Duration, duration);
DECLARE_ID_CLASS(Real_id, Real, real);
DECLARE_ID_CLASS(Script_id, General_script_def, script);
DECLARE_ID_CLASS(Lookup_id, Lookup, lookup);
DECLARE_ID_CLASS(Symtables_id, Symtables, symtables);
DECLARE_ID_CLASS(Music_id, Music, music);
DECLARE_ID_CLASS(Int_id, int, intid);
DECLARE_ID_CLASS(Score_id, Score, score);
DECLARE_ID_CLASS(Request_id, Request, request);
DECLARE_ID_CLASS(Paper_def_id,Paper_def, paperdef);
DECLARE_ID_CLASS(Midi_def_id,Midi_def, mididef);
#endif // IDENTIFIER_
