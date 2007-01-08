/*
  context-key-manager.hh -- declare Context_key_manager

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef CONTEXT_KEY_MANAGER_HH
#define CONTEXT_KEY_MANAGER_HH

#include "lily-proto.hh"

#include <map>
using namespace std;

class Context_key_manager
{
  Object_key const *key_;
  map<string, int> grob_counts_;
  map<string, int> context_counts_;
  

protected:
  friend class Context;
  
  Context_key_manager (Object_key const *);
  Context_key_manager (Context_key_manager const &src);


  void unprotect () const;
  void gc_mark () const;
  void clear ();
  Object_key const *key () const { return key_; }
  Object_key const *create_grob_key (Moment, string);
  Object_key const *get_grob_key (Moment, string);
  Object_key const *get_context_key (Moment, string, string);
};

#endif /* CONTEXT_KEY_MANAGER_HH */


