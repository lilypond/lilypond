/*
  string-handle.hh -- declare String_handle

  source file of the LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STRINGHANDLE_HH
#define STRINGHANDLE_HH

#include "flower-proto.hh"


/**
  Reference counting for strings.
  
   handles ref. counting, and provides a very thin interface using
   Byte *

   */
class String_handle {
  String_data* data;
    
  /// decrease ref count. Named kind of like a Tanenbaum semafore 
  void down ();

  /// increase ref count
  void up (String_data *d);
    
  /** make sure data has only one reference.      
      POST: data->references == 1
  */
  void copy ();
    
public:
  String_handle ();
  ~String_handle ();
  String_handle (String_handle const & src);

  Byte const* to_bytes () const;
  char const* to_str0 () const;
  Byte* get_bytes ();
  char* get_str0 ();    
  bool is_binary_bo () const;
  void operator = (String_handle const &src);
  void operator += (char const *s);
  Byte operator[] (int j) const;

  /** Access elements. WARNING: NOT SAFE
      don't use this for loops. Use to_bytes ()
  */
  Byte &operator[] (int j);
  void append (Byte const* byte, int length_i);
  void set (Byte const* byte, int length_i);
  void operator = (char const *p);
  void trunc (int j);
  int length () const;
};

#ifdef STRING_UTILS_INLINED
#ifndef INLINE
#define INLINE inline
#endif
#include "string-handle.icc"
/* we should be resetting INLINE. oh well. */
#endif


#endif // STRINGHANDLE_HH
