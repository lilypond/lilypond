#ifndef GETOPT_LONG_HH
#define GETOPT_LONG_HH


#include "string.hh"

/**
  a struct this for initialising the commandline options.
 */
struct Long_option_init {
  char const * take_arg_str0_;
  char const * longname_str0_;
  char        shortname_char_;

  char const * help_str0_;
  
  String string () const;
  String str_for_help () const;
  //   NO constructor!

  static int compare (Long_option_init const&,Long_option_init const&);
  static String table_string (Long_option_init *); 
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
  int table_len_;
    
  /// if doing short option, arg_value_char_a_a_[optind][optindind] is processed next.
  int argument_index_;

  /// the option found
  const Long_option_init *found_option_;


public: 
  /** errorcodes: no error, argument expected, no argument expected,
    unknown option, illegal argument (eg. int expected).  */
  enum Errorcod { E_NOERROR = 0, E_ARGEXPECT, E_NOARGEXPECT, E_UNKNOWNOPTION,
		  E_ILLEGALARG } ;

  /// argument. Set to 0 if not present
  char const * optional_argument_str0_;

  /// current error status
  Errorcod error_;

  /// arg_value_char_a_a_[array_index_] will be processed next.
  int array_index_;

  /// the arguments
  char **arg_value_char_a_a_;

  /// the arg. count
  int argument_count_;

  FILE *error_out_;

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
  long get_argument_index ();
 
    
  /**
    What to do with  errors. 
    report messages on  #*os#, and abort.
    if #os# is null, then do not report nor abort, just set #error#
    */
       
  void seterror (FILE *os);

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
