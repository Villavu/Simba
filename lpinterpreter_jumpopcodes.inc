{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	These are the jump related opcodes for the bytecode.
}
{%MainUnit lpinterpreter.pas}

ocJmp,                                                     //Jmp TCodePos
ocJmpR,                                                    //JmpR TCodeOffset

//jump if
{
ocJmpIf8_Stk,                                              //JmpIf8_Stk TCodePos TStackOffset
ocJmpIf16_Stk,                                             //JmpIf16_Stk TCodePos TStackOffset
ocJmpIf32_Stk,                                             //JmpIf32_Stk TCodePos TStackOffset
ocJmpIf64_Stk,                                             //JmpIf64_Stk TCodePos TStackOffset
ocJmpIf8_Var,                                              //JmpIf8_Var TCodePos TVarStackOffset
ocJmpIf16_Var,                                             //JmpIf16_Var TCodePos TVarStackOffset
ocJmpIf32_Var,                                             //JmpIf32_Var TCodePos TVarStackOffset
ocJmpIf64_Var,                                             //JmpIf64_Var TCodePos TVarStackOffset
ocJmpIf8_Ptr,                                              //JmpIf8_Ptr TCodePos Pointer
ocJmpIf16_Ptr,                                             //JmpIf16_Ptr TCodePos Pointer
ocJmpIf32_Ptr,                                             //JmpIf32_Ptr TCodePos Pointer
ocJmpIf64_Ptr,                                             //JmpIf64_Ptr TCodePos Pointer
ocJmpIf8_PStk,                                             //JmpIf8_PStk TCodePos
ocJmpIf16_PStk,                                            //JmpIf16_PStk TCodePos
ocJmpIf32_PStk,                                            //JmpIf32_PStk TCodePos
ocJmpIf64_PStk,                                            //JmpIf64_PStk TCodePos
ocJmpIf8_PVar,                                             //JmpIf8_PVar TCodePos TVarStackOffset TPointerOffset
ocJmpIf16_PVar,                                            //JmpIf16_PVar TCodePos TVarStackOffset TPointerOffset
ocJmpIf32_PVar,                                            //JmpIf32_PVar TCodePos TVarStackOffset TPointerOffset
ocJmpIf64_PVar,                                            //JmpIf64_PVar TCodePos TVarStackOffset TPointerOffset
}

//jump relative if
ocJmpRIf8_Stk,                                              //JmpRIf8_Stk TCodeOffset TStackOffset
ocJmpRIf16_Stk,                                             //JmpRIf16_Stk TCodeOffset TStackOffset
ocJmpRIf32_Stk,                                             //JmpRIf32_Stk TCodeOffset TStackOffset
ocJmpRIf64_Stk,                                             //JmpRIf64_Stk TCodeOffset TStackOffset
ocJmpRIf8_Var,                                              //JmpRIf8_Var TCodeOffset TVarStackOffset
ocJmpRIf16_Var,                                             //JmpRIf16_Var TCodeOffset TVarStackOffset
ocJmpRIf32_Var,                                             //JmpRIf32_Var TCodeOffset TVarStackOffset
ocJmpRIf64_Var,                                             //JmpRIf64_Var TCodeOffset TVarStackOffset
ocJmpRIf8_Ptr,                                              //JmpRIf8_Ptr TCodeOffset Pointer
ocJmpRIf16_Ptr,                                             //JmpRIf16_Ptr TCodeOffset Pointer
ocJmpRIf32_Ptr,                                             //JmpRIf32_Ptr TCodeOffset Pointer
ocJmpRIf64_Ptr,                                             //JmpRIf64_Ptr TCodeOffset Pointer
ocJmpRIf8_PStk,                                             //JmpRIf8_PStk TCodeOffset
ocJmpRIf16_PStk,                                            //JmpRIf16_PStk TCodeOffset
ocJmpRIf32_PStk,                                            //JmpRIf32_PStk TCodeOffset
ocJmpRIf64_PStk,                                            //JmpRIf64_PStk TCodeOffset
ocJmpRIf8_PVar,                                             //JmpRIf8_PVar TCodeOffset TVarStackOffset TPointerOffset
ocJmpRIf16_PVar,                                            //JmpRIf16_PVar TCodeOffset TVarStackOffset TPointerOffset
ocJmpRIf32_PVar,                                            //JmpRIf32_PVar TCodeOffset TVarStackOffset TPointerOffset
ocJmpRIf64_PVar,                                            //JmpRIf64_PVar TCodeOffset TVarStackOffset TPointerOffset

//jump relative if not
ocJmpRIfNot8_Stk,                                           //JmpRIfNot8_Stk TCodeOffset TStackOffset
ocJmpRIfNot16_Stk,                                          //JmpRIfNot16_Stk TCodeOffset TStackOffset
ocJmpRIfNot32_Stk,                                          //JmpRIfNot32_Stk TCodeOffset TStackOffset
ocJmpRIfNot64_Stk,                                          //JmpRIfNot64_Stk TCodeOffset TStackOffset
ocJmpRIfNot8_Var,                                           //JmpRIfNot8_Var TCodeOffset TVarStackOffset
ocJmpRIfNot16_Var,                                          //JmpRIfNot16_Var TCodeOffset TVarStackOffset
ocJmpRIfNot32_Var,                                          //JmpRIfNot32_Var TCodeOffset TVarStackOffset
ocJmpRIfNot64_Var,                                          //JmpRIfNot64_Var TCodeOffset TVarStackOffset
ocJmpRIfNot8_Ptr,                                           //JmpRIfNot8_Ptr TCodeOffset Pointer
ocJmpRIfNot16_Ptr,                                          //JmpRIfNot16_Ptr TCodeOffset Pointer
ocJmpRIfNot32_Ptr,                                          //JmpRIfNot32_Ptr TCodeOffset Pointer
ocJmpRIfNot64_Ptr,                                          //JmpRIfNot64_Ptr TCodeOffset Pointer
ocJmpRIfNot8_PStk,                                          //JmpRIfNot8_PStk TCodeOffset
ocJmpRIfNot16_PStk,                                         //JmpRIfNot16_PStk TCodeOffset
ocJmpRIfNot32_PStk,                                         //JmpRIfNot32_PStk TCodeOffset
ocJmpRIfNot64_PStk,                                         //JmpRIfNot64_PStk TCodeOffset
ocJmpRIfNot8_PVar,                                          //JmpRIfNot8_PVar TCodeOffset TVarStackOffset TPointerOffset
ocJmpRIfNot16_PVar,                                         //JmpRIfNot16_PVar TCodeOffset TVarStackOffset TPointerOffset
ocJmpRIfNot32_PVar,                                         //JmpRIfNot32_PVar TCodeOffset TVarStackOffset TPointerOffset
ocJmpRIfNot64_PVar,                                         //JmpRIfNot64_PVar TCodeOffset TVarStackOffset TPointerOffset

