#ifndef GETOPT_LONG_HH
#define GETOPT_LONG_HH

#include <ostream.h> /* gcc 3.0 */
#include "string.hh"

/**
  a struct this for initialising the commandline options.
 */
struct Long_option_init {
  char const * take_arg_sz_;
  char const * longname_sz_;
  char        shortname_ch_;

  char const * help_sz_;
  
  String str () const;
  String str_for_help () const;
  //   NO constructor!

  static int compare (Long_option_init const&,Long_option_init const&);
  static String table_str (Long_option_init *); 
};


/** C++ for version of long_getopt.  For processing GNU style command
  line arguments.  No pointer (return values, arguments) contents are
  copied.
  
  TODO: handle 
  command  - , and command --
  
  argument reordering
  */
class Getopt_long {

  /// the option info.
  const Long_option_init *option_a_;
  int table_len_i_;
    
  /// if doing short option, arg_value_ch_a_a_[optind][optindind] is processed next.
  int argument_index_i_;

  /// the option found
  const Long_option_init *found_option_l_;


public: 
  /** errorcodes: no error, argument expected, no argument expected,
    unknown option, illegal argument (eg. int expected).  */
  enum Errorcod { E_NOERROR = 0, E_ARGEXPECT, E_NOARGEXPECT, E_UNKNOWNOPTION,
		  E_ILLEGALARG } ;

  /// argument. Set to 0 if not present
  char const * optional_argument_ch_C_;

  /// current error status
  Errorcod error_;

  /// arg_value_ch_a_a_[array_index_i_] will be processed next.
  int array_index_i_;

  /// the arguments
  char **arg_value_ch_a_a_;

  /// the arg. count
  int argument_count_i_;

  ostream *error_ostream_l_;

public:
  /// get ready for processing next error.
  void next ();
  const Long_option_init *parselong ();
  const Long_option_init *parseshort ();
  void OK () const;
  bool ok () const;

  /// report an error and abort
  void report (Errorcod c);


  /// return an integer (with err. detect)
  long argument_to_i ();
 
    
  /**
    What to do with  errors. 
    report messages on  #*os#, and abort.
    if #os# is null, then do not report nor abort, just set #error#
    */
       
  void seterror (ostream *os);

  /// construct: pass arguments and option info.
  Getopt_long (int c,  char **v, Long_option_init *lo);

  /**  get the next option. 
    @return pointer to next option found.
    0 if error occurred, or next argument is no option.
    */
  const Long_option_init *operator () ();

  char const *current_arg ();
  char const * get_next_arg ();
};

#endif // GETOPT_LONG_HH
