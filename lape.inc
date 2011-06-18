{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Defines for the Lape project.
}
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

//Change {*} to runtime settings

{$DEFINE Lape_AlwaysInitialize}   {*}   //Always initialize (local) variables to 0/nil (slower, safer)
{.$DEFINE Lape_CaseSensitive}            //Case sentive in script
{$DEFINE Lape_DoubleKeywordsCache}      //Create a double cache for keywords to speed up parsing (more memory, faster)
{$DEFINE Lape_EmitPos}                  //Emit source position when compiling (more memory)
{.$DEFINE Lape_ForceBlock}         {*}   //Only allow declarations in the blocks where they belong (vars in var block, expressions in begin-end, etc.)
{.$DEFINE Lape_Inline}                   //Compile with function inlining where it could be benefitial (faster, harder debugging)
{$DEFINE Lape_NoWideString}             //Compile without widestrings (only unicodestrings)
{.$DEFINE Lape_PStkD}                    //Support operations to a pointer located on the stack
{.$DEFINE Lape_SmallCode}                //Try to keep compiled code as small as possible (packed records, no aligning -- less memory, slower)
{.$DEFINE Lape_TrackObjects}             //Track all created classes with a list to check for leaks
{.$DEFINE Lape_Unicode}                  //Use unicodestrings

{$DEFINE Lape_UnlimitedCallStackSize}   //Increase callstack size when needed at runtime (less memory, slower)
{$DEFINE Lape_UnlimitedStackSize}       //Increase stack size when needed at runtime (less memory, slower)
{$DEFINE Lape_UnlimitedTryStackSize}    //Increase trystack size when needed at runtime (less memory, slower)
{$DEFINE Lape_UnlimitedVarStackSize}    //Increase varstack size when needed at runtime (less memory, slower)
