/*   
  interpretation-context-handle.hh -- declare  Interpretation_context_handle
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef INTERPRETATION_CONTEXT_HANDLE_HH
#define INTERPRETATION_CONTEXT_HANDLE_HH
#include "lily-proto.hh"

/*
RENAME ME to Context_handle.
*/
   
class Interpretation_context_handle
{
public:
  ~Interpretation_context_handle ();
  Interpretation_context_handle ();

  void set_context (Context *);
  bool try_music (Music *);
  void operator = (Interpretation_context_handle const&);
  Interpretation_context_handle (Interpretation_context_handle const&);
  Context * get_outlet () const;

  int get_count () const;
  void quit ();
private:
  Context * outlet_;
  void down ();
  void up (Context *);
};

#endif /* INTERPRETATION_CONTEXT_HANDLE_HH */

