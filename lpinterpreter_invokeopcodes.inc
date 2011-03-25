{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	These are the invoke related opcodes for the bytecode.
}
{%MainUnit lpinterpreter.pas}

//ocIncCall_Stk,                                             //IncCall_Stk TStackOffset UInt16
//ocIncCall_PStk,                                            //IncCall_PStk TStackOffset UInt16
ocIncCall_Var,                                             //IncCall_Var TVarStackOffset UInt16
ocIncCall_PVar,                                            //IncCall_PVar TVarStackOffset TPointerOffset UInt16
ocIncCall_Ptr,                                             //IncCall_Ptr Ptr UInt16

//ocInvokeImported_Stk,                                      //InvokeImported_Stk TStackOffset UInt16
//ocInvokeImported_PStk,                                     //InvokeImported_PStk TStackOffset UInt16
ocInvokeImported_Var,                                      //InvokeImported_Var TVarStackOffset UInt16
ocInvokeImported_PVar,                                     //InvokeImported_PVar TVarStackOffset TPointerOffset UInt16
ocInvokeImported_Ptr,                                      //InvokeImported_Ptr Ptr UInt16

{
ocInvokeImported_Stk_Stk,                                  //InvokeImported_Stk_Stk TStackOffset UInt16 TStackInc
ocInvokeImported_Stk_PStk,                                 //InvokeImported_Stk_Stk TStackOffset UInt16
ocInvokeImported_Stk_Var,                                  //InvokeImported_Stk_Var TStackOffset TVarStackOffset UInt16
ocInvokeImported_Stk_PVar,                                 //InvokeImported_Stk_PVar TStackOffset TVarStackOffset TPointerOffset UInt16
ocInvokeImported_Stk_Ptr,                                  //InvokeImported_Stk_Ptr TStackOffset Ptr UInt16
}

ocInvokeImported_Var_Stk,                                  //InvokeImported_Var_Stk TVarStackOffset UInt16 TStackInc
ocInvokeImported_Var_PStk,                                 //InvokeImported_Var_PStk TVarStackOffset UInt16
ocInvokeImported_Var_Var,                                  //InvokeImported_Var_Var TVarStackOffset TVarStackOffset UInt16
ocInvokeImported_Var_PVar,                                 //InvokeImported_Var_PVar TVarStackOffset TVarStackOffset TPointerOffset UInt16
ocInvokeImported_Var_Ptr,                                  //InvokeImported_Var_Ptr TVarStackOffset Ptr UInt16

ocInvokeImported_PVar_Stk,                                 //InvokeImported_PVar_Stk TVarStackOffset TPointerOffset UInt16 TStackInc
ocInvokeImported_PVar_PStk,                                //InvokeImported_PVar_PStk TVarStackOffset TPointerOffset UInt16
ocInvokeImported_PVar_Var,                                 //InvokeImported_PVar_Var TVarStackOffset TVarStackOffset TPointerOffset UInt16
ocInvokeImported_PVar_PVar,                                //InvokeImported_PVar_Var TVarStackOffset TVarStackOffset TPointerOffset TPointerOffset UInt16
ocInvokeImported_PVar_Ptr,                                 //InvokeImported_PVar_Ptr TVarStackOffset Ptr UInt16

ocInvokeImported_Ptr_Stk,                                  //InvokeImported_Ptr_Stk Ptr UInt16 TStackInc
ocInvokeImported_Ptr_PStk,                                 //InvokeImported_Ptr_PStk Ptr UInt16
ocInvokeImported_Ptr_Var,                                  //InvokeImported_Ptr_Var Ptr TVarStackOffset UInt16
ocInvokeImported_Ptr_PVar,                                 //InvokeImported_Ptr_PVar Ptr TVarStackOffset TPointerOffset UInt16
ocInvokeImported_Ptr_Ptr,                                  //InvokeImported_Ptr_Var Ptr Ptr UInt16
