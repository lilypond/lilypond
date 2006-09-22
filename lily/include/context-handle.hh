/*
  context-handle.hh -- declare  Context_handle

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INTERPRETATION_CONTEXT_HANDLE_HH
#define INTERPRETATION_CONTEXT_HANDLE_HH
#include "lily-proto.hh"

/*
  Rename me to Context_handle.
*/

class Context_handle
{
public:
  ~Context_handle ();
  Context_handle ();

  void set_context (Context *);
  void operator = (Context_handle const &);
  Context_handle (Context_handle const &);
  Context *get_outlet () const;

  int get_count () const;
private:
  Context *outlet_;
  void down ();
  void up (Context *);
};

#endif /* INTERPRETATION_CONTEXT_HANDLE_HH */

