/*
  string-data.hh -- declare  String_data

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STRINGDATA_HH
#define STRINGDATA_HH


/**Internal String struct.
   the data itself. Handles simple tasks (resizing, resetting)
   */
class String_data {
    // GNU malloc: storage overhead is 8 bytes anyway.

friend class String_handle;
    int maxlen;	// maxlen is arraysize-1
    
    int length_i_;
    Byte* data_byte_p_;
    int references;

    /// init to ""
    String_data();

    /// init from src. Conservative allocation.
    String_data (String_data const &src); 
    
    ~String_data();

    /** POST: maxlen >= j.
      @param j, maximum stringlength_i_.    
      contents thrown away.
    */
    void setmax (int j);
    
    /** POST: maxlen >= j.
      @param j, maximum stringlength_i_.
      contents are kept if it grows.
      */
    void remax (int j);

    /// check if writeable.
    void OKW();

    /// check state.
    void OK();

    /// reduce memory usage.
    void tighten();

    // assignment.
    void set (Byte const* byte_C, int length_i);

    void set (char const* ch_C);
    
    /// concatenation.
    void append (Byte const* byte_C, int length_i);

    void operator += (char const* ch_C);

    char const* ch_C() const; 

    char* ch_l();

    Byte const* byte_C() const;

    // idem, non const
    Byte* byte_l();

    void trunc (int j);

    /** access element. not really safe. Can alter length_i_ without
      #String_data# knowing it.  */
    Byte &operator [](int j);
    Byte operator [](int j) const;
    bool is_binary_bo() const;
};



#ifdef STRING_UTILS_INLINED
#ifndef INLINE
#define INLINE inline
#endif
#include "string-data.icc"

#endif


#endif // STRING_DATA_HH
