/*
  identifier.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH

#include "lily-proto.hh"
#include "string.hh"
#include "input.hh"
#include "virtual-methods.hh"

#define IDACCESSOR(Class)\
virtual Class *  access_ ## Class (bool=true) const { error (#Class  + String ("_identifier")); return 0; }

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
  IDACCESSOR(Translator)
  IDACCESSOR(Music)
  IDACCESSOR(General_script_def)
  IDACCESSOR(Symtables)
  IDACCESSOR(Midi_def)
  IDACCESSOR(Paper_def)
  IDACCESSOR(Lookup)
  IDACCESSOR(Real)
  IDACCESSOR(String)
  IDACCESSOR(Request)
  IDACCESSOR(Score)
  IDACCESSOR(int)
  IDACCESSOR(Duration)
  VIRTUAL_COPY_CONS (Identifier, Identifier);

protected:
  virtual void do_print () const;
  virtual String do_str () const;
};

#define DECLARE_ID_CLASS(Class)	\
struct Class ## _identifier : Identifier {\
			     Class *data_p_;		     \
			     DECLARE_MY_RUNTIME_TYPEINFO; \
			     Class ## _identifier (Class ## _identifier const&);\
			     Class ## _identifier (Class*st, int code);\
			     VIRTUAL_COPY_CONS (Class ## _identifier, Identifier);\
			     virtual Class* access_ ## Class (bool copy_b = true) const;\
			     ~Class ## _identifier();\
			     virtual void do_print () const; \
			     virtual String do_str () const; \
}\


DECLARE_ID_CLASS(Translator);
DECLARE_ID_CLASS(Duration);
DECLARE_ID_CLASS(Real);
DECLARE_ID_CLASS(String);
DECLARE_ID_CLASS(General_script_def);
DECLARE_ID_CLASS(Lookup);
DECLARE_ID_CLASS(Symtables);
DECLARE_ID_CLASS(Music);
DECLARE_ID_CLASS(int);
DECLARE_ID_CLASS(Score);
DECLARE_ID_CLASS(Request);
DECLARE_ID_CLASS(Paper_def);
DECLARE_ID_CLASS(Midi_def);

#endif // IDENTIFIER_HH

