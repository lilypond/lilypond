/*
  identifier.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "string.hh"
#include "input.hh"
#include "virtual-methods.hh"
#include "smobs.hh"


class Output_def_identifier;
class Score_identifier;
class Duration_identifier;



#define IDACCESSOR(Class)\
virtual Class *  access_content_ ## Class (bool) const { error (#Class  + String ("_identifier")); return 0; }\


/**
   A declarable data structure in mudela.

   TODO: use SMOBS for the union type, and junk all derived classes.
   */
struct Identifier : public Input {

  bool init_b_;
  bool accessed_b_;
  int token_code_i_;
  Identifier (Identifier const&);    
  Identifier (int code) ;

  void print() const;
  
  void error (String) const;
  String str () const;
  IDACCESSOR(Music_output_def)
  IDACCESSOR(Score)
  IDACCESSOR(Duration)
  VIRTUAL_COPY_CONS(Identifier);

  DECLARE_SMOBS(Identifier, foo);
protected:

  virtual String do_str () const;
};

#define DECLARE_ID_CLASS(Class)	\
struct Class ## _identifier : Identifier {\
			     Class *data_p_;		     \
			      \
			     Class ## _identifier (Class ## _identifier const&);\
			     Class ## _identifier (Class*st, int code);\
			     VIRTUAL_COPY_CONS(Identifier);\
			     virtual Class* access_content_ ## Class (bool copy_b) const;\
			     ~Class ## _identifier();\
			    \
			     virtual String do_str () const; \
}\


DECLARE_ID_CLASS(Duration);
DECLARE_ID_CLASS(Score);
DECLARE_ID_CLASS(Music_output_def);

Identifier * unsmob_identifier (SCM);
SCM smobify (Identifier*);

#endif // IDENTIFIER_HH

