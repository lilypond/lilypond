/*
  object-key.hh -- declare Object_key

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef OBJECT_KEY_HH
#define OBJECT_KEY_HH

#include "smobs.hh"

/*
  Object_keys are read-only values, suitable for storing references to
  transient objects (such as grobs or contexts) on disk.

  In the future, they might also act as handles for external processes
  requesting notation to be drawn.
*/
class Object_key
{
  DECLARE_SMOBS (Object_key);

protected:
  Object_key ();
  virtual void derived_mark () const;
  virtual int get_type () const;
  virtual int do_compare (Object_key const *other) const;
public:
  virtual SCM as_scheme () const;
  static Object_key *from_scheme (SCM);
  static Object_key *undump (SCM);
  int compare (Object_key const *other) const;
  SCM dump () const;
};

enum Object_key_type
  {
    BASE_KEY,
    COPIED_KEY,
    GENERAL_KEY,
    GROB_KEY,
    CONTEXT_KEY,
    KEY_COUNT,
  };

class Copied_key : public Object_key
{

private:
  Object_key const *original_;
  int copy_count_;

protected:
  virtual void derived_mark () const;
  virtual int get_type () const;
  virtual int do_compare (Object_key const *other) const;
  virtual SCM as_scheme () const;
public:
  static Object_key *from_scheme (SCM);
  Copied_key (Object_key const *, int);
};

DECLARE_UNSMOB (Object_key, key);

struct Object_key_less
{
  bool operator () (Object_key const *const &t1, Object_key const *const &t2) const
  {
    return t1->compare (t2);
  }
};

#endif /* OBJECT_KEY_HH */
