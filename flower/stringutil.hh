#ifndef __STRING_UTIL_HH
#define __STRING_UTIL_HH
#if 0
#if !defined(NDEBUG)
#define NDEBUG BLONDE
#endif
#endif
const INITIALMAX=8;
class String_handle;


typedef unsigned char Byte;

#ifdef STRING_UTILS_INLINED
#define INLINE inline
#else
#define INLINE
#endif

/**Internal String struct.
   the data itself. Handles simple tasks (resizing, resetting)
   */
class StringData {
    // GNU malloc: storage overhead is 8 bytes anyway.

friend class String_handle;
    int maxlen;	// maxlen is arraysize-1
    
    int length_i_;
    Byte* data_by_p_;
    int references;

    /// init to ""
    INLINE StringData();

    /// init from src. Conservative allocation.
    INLINE StringData(StringData const &src); 
    
    ~StringData();

    /** POST: maxlen >= j.
      IN: j, maximum stringlength_i_.    
      contents thrown away.
    */
    INLINE void setmax(int j);
    
    /** POST: maxlen >= j.
      IN: j, maximum stringlength_i_.
      contents are kept if it grows.
      */
    INLINE void remax(int j);

    /// check if writeable.
    INLINE void OKW();

    /// check state.
    INLINE void OK();

    /// reduce memory usage.
    INLINE void tighten();

    // assignment.
    INLINE void set( Byte const* by_c_l, int length_i );

    INLINE void set( char const* ch_c_l );
    
    /// concatenation.
    INLINE void append( Byte const* by_c_l, int length_i );

    INLINE void operator += ( char const* ch_c_l );

    INLINE char const* ch_c_l() const; 

    INLINE char* ch_l();

    INLINE Byte const* by_c_l() const;

    // idem, non const
    INLINE Byte* by_l();

    INLINE void trunc(int j);

    /** not really safe. Can alter length_i_ without StringData knowing it.
      */
    INLINE Byte &operator [](int j);
    INLINE Byte operator [](int j) const;
};




/**
  Reference counting for strings.
  
   handles ref. counting, and provides a very thin interface using
   Byte *

   */
class String_handle {
    StringData* data;
    
    /// decrease ref count. Named kind of like a Tanenbaum semafore 
    INLINE void down();

    /// increase ref count
    INLINE void up(StringData *d);
    
    /** make sure data has only one reference.      
       POST: data->references == 1
      */
    INLINE void copy();
    
public:
    INLINE String_handle();
    INLINE ~String_handle();
    INLINE String_handle(String_handle const & src);

    INLINE Byte const* by_c_l() const;
    INLINE char const* ch_c_l() const;
    INLINE Byte* by_l();
    INLINE char* ch_l();

    INLINE void operator =(String_handle const &src);
    INLINE void operator += (char const *s);
    INLINE Byte operator[](int j) const;

    /** Access elements. WARNING: NOT SAFE
       don't use this for loops. Use by_c_l()
       */
    INLINE Byte &operator[](int j);
    INLINE void append( Byte const* by_c_l, int length_i );
    INLINE void set( Byte const* by_c_l, int length_i );
    INLINE void operator = (char const *p);
    INLINE void trunc(int j);
    INLINE int length_i() const;
};

#if 0
#ifdef NDEBUG
#if (NDEBUG == BLONDE)
#undef NDEBUG
#endif
#endif
#endif

#ifdef STRING_UTILS_INLINED
#include "stringutil.cc"
#endif

#endif // __STRING_UTIL_HH //
