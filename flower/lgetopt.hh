#ifndef LGETOPT_HH
#define LGETOPT_HH

#include <string.h>


class ostream;

struct long_option_init {
    bool take_arg;
    const char* longname;
    char        shortname;

    ostream &printon(ostream &errorout);
};


///
class Getopt_long {
public:
    ///
    enum Errorcod { E_NOERROR = 0, E_ARGEXPECT, E_NOARGEXPECT, E_UNKNOWNOPTION,
		E_ILLEGALARG } ;

    /** errorcodes: no error, argument expected, no argument expected,
      unknown option, illegal argument (eg. int expected).  */

private:

    /// the option info.
    long_option_init *the_opts;
    int table_len;
    
    /// if doing short option, argv[optind][optindind] is processed next.
    int optindind;

    /// the option found
    long_option_init *beet;

    /// get ready for processing next error.
    bool next();
    long_option_init *parselong();
    long_option_init *parseshort();
    
    ostream *errorout;

    /// report an error and abort
    void report(Errorcod c);
public:
    /// what to do with  errors
    void seterror(ostream *os);
    /**
       report messages on  #*os#, and abort.
       if #os# is null, then do not report nor abort, just set #error#
      */
       
    /// argument. Set to 0 if not present
    char* optarg;


    /// return an integer (with err. detect)
    long intarg();
    /// argv[optind] will be processed next.
    int optind;

    /// the arguments
    char **argv;

    /// the arg. count
    int argc;
    
    /// construct: pass arguments and option info.
    Getopt_long(int c,  char **v, long_option_init *lo);

    /// get the next option
    long_option_init *operator()();
    /**
      RETURN: pointer to next option found.
      0 if error occurred, or next argument is no option.
      */

    char *current_arg();
    char * get_next_arg();
    Errorcod error;
};
/**
  C++ for version of long_getopt. For processing GNU style command line arguments.
  No pointer  (return values, arguments) contents are copied.
  */
#endif
