/*
  identifier.cc -- implement identifier and derived classes

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


/*
  JUNKTHIS!
 */
#include <assert.h>

#include "music-output-def.hh"
#include "score.hh"
#include "identifier.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "ly-smobs.icc"


IMPLEMENT_UNSMOB(Identifier, identifier);
IMPLEMENT_SMOBS(Identifier);
IMPLEMENT_DEFAULT_EQUAL_P(Identifier);

Identifier::Identifier (int code)
{
  token_code_i_ = code;
  accessed_b_ = 0;
  smobify_self ();
}

Identifier::Identifier (Identifier const&s)
  : Input (s)
{
  smobify_self ();  
  token_code_i_ = s.token_code_i_;
  accessed_b_ = s.accessed_b_;
}

Identifier::~Identifier()
{
}

void
Identifier::error (String expect) const
{
  ::error (_f ("wrong identifier type, expected: `%s'", expect));
}

String
Identifier::str () const
{
  return do_str ();
}

String
Identifier::do_str () const
{
  return "";
}

void
Identifier::print () const
{
  DEBUG_OUT << "identifier ";
  do_print ();
}
void
Identifier::do_print () const
{
}

/* ugh. */
#define DEFAULT_PRINT(Class) \
void \
Class ## _identifier::do_print () const { \
  Class *cl = ((Class ## _identifier *)this)->access_content_ ## Class(false);\
  cl->print (); \
}


DEFAULT_PRINT(Score);
DEFAULT_PRINT(Music_output_def);

/* ugh. */
#define DUMMY_STR(Class) \
String \
Class ## _identifier::do_str () const { \
  return String (#Class); \
}

DUMMY_STR(Score);
DUMMY_STR(Music_output_def);
DUMMY_STR(Duration);

#define STRING_PRINT(Class) \
void \
Class ## _identifier::do_print () const\
{\
  DEBUG_OUT << do_str () << '\n';\
}\


STRING_PRINT(Duration);
  
#define DEFAULT_STR(Class) \
String \
Class ## _identifier::do_str () const\
{\
  return to_str (*data_p_);\
}

  

/*
  fucking C++ blows me.
 */

#define DEFAULT_ACCESSOR(Class)\
Class*\
Class ## _identifier::access_content_ ## Class (bool copy_b) const {\
  ((Class ## _identifier*)this)->accessed_b_ = true;\
  return copy_b ? new Class (*data_p_) : data_p_;\
}

#define VIRTUAL_ACCESSOR(Class)\
Class*\
Class ## _identifier::access_content_ ## Class (bool copy_b) const{\
  ((Class ## _identifier*)this)->accessed_b_ = true;\
  return copy_b ? dynamic_cast<Class*> (data_p_->clone()) : data_p_;\
}

#define IMPLEMENT_ID_CLASS(Class)	\
	Class ## _identifier::~Class ## _identifier() { delete data_p_; }\
	Class ## _identifier::Class ## _identifier (Class*st, int code) \
	  :Identifier (code)\
	{\
	  data_p_ = st;\
	}\
Class ## _identifier::Class ## _identifier (Class ## _identifier const &s) \
  : Identifier (s)\
{\
   data_p_ = s.access_content_ ## Class (true);\
} 


IMPLEMENT_ID_CLASS(Duration);
IMPLEMENT_ID_CLASS(Score);
IMPLEMENT_ID_CLASS(Music_output_def);
VIRTUAL_ACCESSOR(Music_output_def);
DEFAULT_ACCESSOR(Duration);
DEFAULT_ACCESSOR(Score);


int
Identifier::print_smob (SCM s, SCM p, scm_print_state*)
{
 return 1;  
}

SCM
Identifier::mark_smob (SCM s)
{
  return SCM_EOL;
}



