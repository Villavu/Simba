{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	These are the jump related opcodes for the bytecode.
}
{%MainUnit lpinterpreter.pas}

ocJmp,                                                     //Jmp UInt32
ocJmpR,                                                    //JmpR Int32

//jump if
{
ocJmpIf8_Stk,                                              //JmpIf8_Stk UInt32 TStackOffset
ocJmpIf16_Stk,                                             //JmpIf16_Stk UInt32 TStackOffset
ocJmpIf32_Stk,                                             //JmpIf32_Stk UInt32 TStackOffset
ocJmpIf64_Stk,                                             //JmpIf64_Stk UInt32 TStackOffset
ocJmpIf8_Var,                                              //JmpIf8_Var UInt32 TVarStackOffset
ocJmpIf16_Var,                                             //JmpIf16_Var UInt32 TVarStackOffset
ocJmpIf32_Var,                                             //JmpIf32_Var UInt32 TVarStackOffset
ocJmpIf64_Var,                                             //JmpIf64_Var UInt32 TVarStackOffset
ocJmpIf8_Ptr,                                              //JmpIf8_Ptr UInt32 Pointer
ocJmpIf16_Ptr,                                             //JmpIf16_Ptr UInt32 Pointer
ocJmpIf32_Ptr,                                             //JmpIf32_Ptr UInt32 Pointer
ocJmpIf64_Ptr,                                             //JmpIf64_Ptr UInt32 Pointer
ocJmpIf8_PStk,                                             //JmpIf8_PStk UInt32
ocJmpIf16_PStk,                                            //JmpIf16_PStk UInt32
ocJmpIf32_PStk,                                            //JmpIf32_PStk UInt32
ocJmpIf64_PStk,                                            //JmpIf64_PStk UInt32
ocJmpIf8_PVar,                                             //JmpIf8_PVar UInt32 TVarStackOffset TPointerOffset
ocJmpIf16_PVar,                                            //JmpIf16_PVar UInt32 TVarStackOffset TPointerOffset
ocJmpIf32_PVar,                                            //JmpIf32_PVar UInt32 TVarStackOffset TPointerOffset
ocJmpIf64_PVar,                                            //JmpIf64_PVar UInt32 TVarStackOffset TPointerOffset
}

//jump relative if
ocJmpRIf8_Stk,                                              //JmpRIf8_Stk Int32 TStackOffset
ocJmpRIf16_Stk,                                             //JmpRIf16_Stk Int32 TStackOffset
ocJmpRIf32_Stk,                                             //JmpRIf32_Stk Int32 TStackOffset
ocJmpRIf64_Stk,                                             //JmpRIf64_Stk Int32 TStackOffset
ocJmpRIf8_Var,                                              //JmpRIf8_Var Int32 TVarStackOffset
ocJmpRIf16_Var,                                             //JmpRIf16_Var Int32 TVarStackOffset
ocJmpRIf32_Var,                                             //JmpRIf32_Var Int32 TVarStackOffset
ocJmpRIf64_Var,                                             //JmpRIf64_Var Int32 TVarStackOffset
ocJmpRIf8_Ptr,                                              //JmpRIf8_Ptr Int32 Pointer
ocJmpRIf16_Ptr,                                             //JmpRIf16_Ptr Int32 Pointer
ocJmpRIf32_Ptr,                                             //JmpRIf32_Ptr Int32 Pointer
ocJmpRIf64_Ptr,                                             //JmpRIf64_Ptr Int32 Pointer
ocJmpRIf8_PStk,                                             //JmpRIf8_PStk Int32
ocJmpRIf16_PStk,                                            //JmpRIf16_PStk Int32
ocJmpRIf32_PStk,                                            //JmpRIf32_PStk Int32
ocJmpRIf64_PStk,                                            //JmpRIf64_PStk Int32
ocJmpRIf8_PVar,                                             //JmpRIf8_PVar Int32 TVarStackOffset TPointerOffset
ocJmpRIf16_PVar,                                            //JmpRIf16_PVar Int32 TVarStackOffset TPointerOffset
ocJmpRIf32_PVar,                                            //JmpRIf32_PVar Int32 TVarStackOffset TPointerOffset
ocJmpRIf64_PVar,                                            //JmpRIf64_PVar Int32 TVarStackOffset TPointerOffset

//jump relative if not
ocJmpRIfNot8_Stk,                                           //JmpRIfNot8_Stk Int32 TStackOffset
ocJmpRIfNot16_Stk,                                          //JmpRIfNot16_Stk Int32 TStackOffset
ocJmpRIfNot32_Stk,                                          //JmpRIfNot32_Stk Int32 TStackOffset
ocJmpRIfNot64_Stk,                                          //JmpRIfNot64_Stk Int32 TStackOffset
ocJmpRIfNot8_Var,                                           //JmpRIfNot8_Var Int32 TVarStackOffset
ocJmpRIfNot16_Var,                                          //JmpRIfNot16_Var Int32 TVarStackOffset
ocJmpRIfNot32_Var,                                          //JmpRIfNot32_Var Int32 TVarStackOffset
ocJmpRIfNot64_Var,                                          //JmpRIfNot64_Var Int32 TVarStackOffset
ocJmpRIfNot8_Ptr,                                           //JmpRIfNot8_Ptr Int32 Pointer
ocJmpRIfNot16_Ptr,                                          //JmpRIfNot16_Ptr Int32 Pointer
ocJmpRIfNot32_Ptr,                                          //JmpRIfNot32_Ptr Int32 Pointer
ocJmpRIfNot64_Ptr,                                          //JmpRIfNot64_Ptr Int32 Pointer
ocJmpRIfNot8_PStk,                                          //JmpRIfNot8_PStk Int32
ocJmpRIfNot16_PStk,                                         //JmpRIfNot16_PStk Int32
ocJmpRIfNot32_PStk,                                         //JmpRIfNot32_PStk Int32
ocJmpRIfNot64_PStk,                                         //JmpRIfNot64_PStk Int32
ocJmpRIfNot8_PVar,                                          //JmpRIfNot8_PVar Int32 TVarStackOffset TPointerOffset
ocJmpRIfNot16_PVar,                                         //JmpRIfNot16_PVar Int32 TVarStackOffset TPointerOffset
ocJmpRIfNot32_PVar,                                         //JmpRIfNot32_PVar Int32 TVarStackOffset TPointerOffset
ocJmpRIfNot64_PVar,                                         //JmpRIfNot64_PVar Int32 TVarStackOffset TPointerOffset

