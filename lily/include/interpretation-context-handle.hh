/*   
  interpretation-context-handle.hh -- declare  Interpretation_context_handle
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef INTERPRETATION_CONTEXT_HANDLE_HH
#define INTERPRETATION_CONTEXT_HANDLE_HH
#include "lily-proto.hh"

class Interpretation_context_handle
{
public:
  ~Interpretation_context_handle ();
  Interpretation_context_handle ();
  Interpretation_context_handle* clone () const;
  void set_translator (Translator_group*);
  bool try_music (Music *);
  void operator =(Interpretation_context_handle const&);
  Interpretation_context_handle (Interpretation_context_handle const&);
  Translator_group * report_to_l () const;

private:
  Translator_group * report_to_l_;
  void down ();
  void up (Translator_group*);
};

#endif /* INTERPRETATION_CONTEXT_HANDLE_HH */

