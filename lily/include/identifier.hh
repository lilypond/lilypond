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

#define IDACCESSOR(Class, accessor)\
virtual Class * accessor () const { error (#Class  + String ("_identifier")); return 0; }

/**
   A declarable data structure in mudela. 


   */
struct Identifier : public Input {
  bool init_b_;
  bool accessed_b_;
  int token_code_i_;
  Identifier (Identifier const&);    
  Identifier (int code) ;
  virtual ~Identifier() ;


  void print() const;
  DECLARE_MY_RUNTIME_TYPEINFO;
  void error (String) const;
  String str () const;
  IDACCESSOR(Translator, translator)
  IDACCESSOR(Music, music)
  IDACCESSOR(General_script_def, script)
  IDACCESSOR(Symtables, symtables)
  IDACCESSOR(Midi_def, mididef)
  IDACCESSOR(Paper_def, paperdef)
  IDACCESSOR(Lookup,lookup)
  IDACCESSOR(Real,real)
  IDACCESSOR(String,string)
  IDACCESSOR(Request, request)
  IDACCESSOR(Score, score)
  IDACCESSOR(int, intid)
  IDACCESSOR(Duration, duration)
  VIRTUAL_COPY_CONS (Identifier, Identifier);

protected:
  virtual void do_print () const;
  virtual String do_str () const;
};

#define DECLARE_ID_CLASS(Class, accessor)	\
struct Class ## _identifier : Identifier {\
			     Class *data_p_;		     \
			     DECLARE_MY_RUNTIME_TYPEINFO; \
			     Class ## _identifier (Class ## _identifier const&);\
			     Class ## _identifier (Class*st, int code);\
			     VIRTUAL_COPY_CONS (Class ## _identifier, Identifier);\
			     virtual Class* accessor () const;\
			     ~Class ## _identifier();\
			     virtual void do_print () const; \
			     virtual String do_str () const; \
}\


DECLARE_ID_CLASS(Translator, translator);
DECLARE_ID_CLASS(Duration, duration);
DECLARE_ID_CLASS(Real, real);
DECLARE_ID_CLASS(String, string);
DECLARE_ID_CLASS(General_script_def, script);
DECLARE_ID_CLASS(Lookup, lookup);
DECLARE_ID_CLASS(Symtables, symtables);
DECLARE_ID_CLASS(Music, music);
DECLARE_ID_CLASS(int, intid);
DECLARE_ID_CLASS(Score, score);
DECLARE_ID_CLASS(Request, request);
DECLARE_ID_CLASS(Paper_def, paperdef);
DECLARE_ID_CLASS(Midi_def, mididef);

#endif // IDENTIFIER_HH

