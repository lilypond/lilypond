// stringtest.cc

#include <iostream.h>
#include "string.hh"

int 
main()
{
    String str( "hai" );
    cout <<  str << endl;
    cout << "left" << endl;
    cout << "	0:" << str.left_str( 0 ) << endl;
    cout << "	1:" << str.left_str( 1 ) << endl;
    cout << "	2:" << str.left_str( 2 ) << endl;
    cout << "	3:" << str.left_str( 3 ) << endl;
    cout << "	4:" << str.left_str( 4 ) << endl;
    cout << "right" << endl;
    cout << "	0:" << str.right_str( 0 ) << endl;
    cout << "	1:" << str.right_str( 1 ) << endl;
    cout << "	2:" << str.right_str( 2 ) << endl;
    cout << "	3:" << str.right_str( 3 ) << endl;
    cout << "	4:" << str.right_str( 4 ) << endl;

    str += " daar";
    cout << str << endl;

    str = String( "hallo" ) + " daar" + '!';
    cout << str << endl;

    if ( str == String( "" ) )
        cout << str << " is empty" << endl;
    String fn = "";
    if ( fn == "" )
        cout << fn << " is empty" << endl;

    fn = "";
    fn += "";
    delete fn.copy_byte_p();

    delete str.copy_byte_p();

    cout << String_convert::bin2hex_str( String( (char)0xff ) ) << endl;
}
