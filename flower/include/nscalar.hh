/*
  scalar.hh -- declare 

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCALAR_HH
#define SCALAR_HH

struct Scalar {
  String str_;
  // Real real_;
  int int_;
  //  Rational rational_;


  struct typebits {
    string_bit: 1;
    int_bit:1;
  };
private:

  
  //  operator Real();
  operator int();
  
  /**   perl -like string to bool conversion.
   */
  //  operator bool() const;
   
  //Scalar (Real r) : String (r) {}
  Scalar (int i) : String (i) {}
  //  Scalar (char c) : String (c) {}
  Scalar (char const *c) : String (c) {}    
  Scalar (String s):String (s) {}
  //Scalar (Rational);
  static Scalar undefined ();
};

#endif // SCALAR_HH
