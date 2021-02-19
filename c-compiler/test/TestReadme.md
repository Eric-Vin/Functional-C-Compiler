# Test Documentatio

List of all tests and what they test

## Preprocessor Tests
* Continued_Line_Simple             : Tests a simple example of a continued line
* Continued_Line_Complex            : Tests a more complex example of a continued line
* Continued_Line_Comment            : Tests a continued line on a line comment
* Continued_Line_Include            : Tests a continued line on an include
* Continued_Line_Define             : Tests a continued line on a define
* Continued_Line_Undefine           : Tests a continued line on an undefine
* Continued_Line_Split_Whitespace   : Tests a continued line with whitespace between "\" and "\n"
* Continued_Line_Split_Comment_1    : Tests a continued line with line comment between "\" and "\n"
* Continued_Line_Split_Comment_2    : Tests a continued line with block comment between "\" and "\n"
* Continued_Line_Split_Complex      : Tests multiple continued lines with whitespace and comments between "\" and "\n"

* Comment_Line_Simple               : Tests that a line comment is removed and replaced with whitespace
* Comment_Block_Simple              : Tests that a block comment is removed and replaced with whitespace
* Comment_Block_Multiple_Open       : Tests that a block comment with multiple openings is interpreted correctly
* Comment_Block_Multiple_Close      : Tests that a block comment with multiple closes is interpreted correctly
* Comment_Block_Nested              : Tests that a block comment inside a block comment is interpreted correctly
* Comment_Nested_1                  : Tests that line comments in a block comment do not affect the block comment
* Comment_Nested_2                  : Tests that a block comment in a line comment does not affect the line comments
* Comment_In_String_Literal         : Tests that a comment inside a string literal is not replaced with whitespace

* New_Line_End_Of_File              : Tests that an input without a new line at the end of the file has a new line implicitly supplied

* Include_File_Double_Quotes        : Tests including a local file wrapped in " "
* Include_Lib                       : Tests including a standard library wrapped in < >
* Include_Space                     : Tests including a local file with a space in the filename
* Include_Split_Space               : Tests that an include directive with a space between the "#" and "include" is interpreted correctly
* Include_File_Single_Quotes        : Tests that including a local file wrapped in ' ' throws an error
* Include_No_File                   : Tests that including a non existant file throws an error
* Include_No_Lib                    : Tests that including a non existant library throws an error
* Include_Invalid_Wrapper           : Tests that including a file that is not wrapped in " " or < > throws an error
* Include_Invalid_Params            : Tests that an include directive with an incorrect number of parameters throws an error

* Define_Simple_Identifier          : Tests defining a simple identifier (Alphabetical all caps)
* Define_Complex_Identifier         : Tests defining a complex identifier (Contains underscores, numbers and letters lower and upper case)
* Define_Text                       : Tests defining a macro as text
* Define_Decimal                    : Tests defining a macro as a decimal number
* Define_String_Single_Quotes       : Tests defining a macro as a string literal wrapped in ' '
* Define_String_Double_Quotes       : Tests defining a macro as a string literal wrapped in " "
* Define_Token_List                 : Tests defining a macro as a list of tokens
* Define_Sequential                 : Tests that define directives are applied sequentially
* Define_Nested                     : Tests that nested macros expand properly
* Define_Nested_Redefine            : Tests that nested macros that are redefined expand properly
* Define_Infinite_Recursive         : Tests that recursively defined macros are applied only once and do not infinitely recurse
* Define_Split_Space                : Tests that a define directive with a space between the "#" and "define" is interpreted correctly
* Define_Invalid_Identifier         : Tests that defining an invalid identifier throws an error

* Undefine_Simple_Identifier        : Tests undefining a simple identifier (Alphabetical all caps)
* Undefine_Complex_Identifier       : Tests undefining a complex identifier (Contains underscores, numbers and letters)
* Undefine_Not_Defined              : Tests that undefining a non defined identifier does nothing
* Undefine_Split_Space              : Tests that an undefine directive with a space between the "#" and "undef" is interpreted correctly 
* Undefine_Invalid_Identifier       : Tests that undefining an invalid identifier throws an error

/ Ifdef_Simple                      : Tests a simple ifdef conditional
/ Ifdef_Empty                       : Tests an ifdef conditional with no controlled text inside it
/ Ifdef_Nested_Simple               : Tests 2 ifdef conditionals nested inside each other
/ Ifdef_Nested_Complex              : Tests 4 ifdef conditionals nested inside each other
/ Ifdef_Split_Space                 : Tests that an ifdef directive with a space between the "#" and "ifdef" is interpreted correctly 
/ Ifdef_Multiple_Files              : Tests that an ifdef that is started in one file and ended in another throws an error
/ Ifdef_Invalid_Identifier          : Tests that an ifdef with an invalid identifier throws an error
/ Ifdef_Unmatched                   : Tests that an ifdef without a closing endif throws an error
/ Ifdef_Nested_Unmatched            : Tests that 3 ifdef directives closed by 2 endif directives throws an error
/ Ifdef_Invalid_Params              : Tests that an ifdef directive with an incorrect number of parameters throws an error

/ Ifndef_Simple                     : Tests a simple ifndef conditional
/ Ifndef_Empty                      : Tests an ifndef conditional with no controlled text inside it
/ Ifndef_Nested_Simple              : Tests 2 ifndef conditionals nested inside each other
/ Ifndef_Nested_Complex             : Tests 4 ifndef conditionals nested inside each other
/ Ifndef_Split_Space                : Tests that an ifndef directive with a space between the "#" and "ifndef" is interpreted correctly 
/ Ifndef_Multiple_Files             : Tests that an ifndef that is started in one file and ended in another throws an error
/ Ifndef_Invalid_Identifier         : Tests that an ifndef with an invalid identifier throws an error
/ Ifndef_Unmatched                  : Tests that an ifndef without a closing endif throws an error
/ Ifndef_Nested_Unmatched           : Tests that 3 ifndef directives closed by 2 endif directives throws an error
/ Ifndef_Invalid_Params             : Tests that an ifndef directive with an incorrect number of parameters throws an error

/ Conditionals_No_Start             : Tests that an endif directive not preceded by a conditional directive throws an error
