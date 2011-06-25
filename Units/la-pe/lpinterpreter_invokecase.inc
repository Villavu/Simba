{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include directs the invoke opcodes to the related functions.
}
{%MainUnit lpinterpreter.pas}

//ocIncCall_Stk: DoIncCall_Stk();
//ocIncCall_PStk: DoIncCall_PStk();
ocIncCall_Var: DoIncCall_Var();
ocIncCall_PVar: DoIncCall_PVar();
ocIncCall_Ptr: DoIncCall_Ptr();

//ocInvokeImported_Stk: DoInvokeImported_Stk();
//ocInvokeImported_PStk: DoInvokeImported_PStk();
ocInvokeImported_Var: DoInvokeImported_Var();
ocInvokeImported_PVar: DoInvokeImported_PVar();
ocInvokeImported_Ptr: DoInvokeImported_Ptr();

{
ocInvokeImported_Stk_Stk: DoInvokeImported_Stk_Stk();
ocInvokeImported_Stk_PStk: DoInvokeImported_Stk_PStk();
ocInvokeImported_Stk_Var: DoInvokeImported_Stk_Var();
ocInvokeImported_Stk_PVar: DoInvokeImported_Stk_PVar();
ocInvokeImported_Stk_Ptr: DoInvokeImported_Stk_Ptr();
}

ocInvokeImported_Var_Stk: DoInvokeImported_Var_Stk();
ocInvokeImported_Var_PStk: DoInvokeImported_Var_PStk();
ocInvokeImported_Var_Var: DoInvokeImported_Var_Var();
ocInvokeImported_Var_PVar: DoInvokeImported_Var_PVar();
ocInvokeImported_Var_Ptr: DoInvokeImported_Var_Ptr();

ocInvokeImported_PVar_Stk: DoInvokeImported_PVar_Stk();
ocInvokeImported_PVar_PStk: DoInvokeImported_PVar_PStk();
ocInvokeImported_PVar_Var: DoInvokeImported_PVar_Var();
ocInvokeImported_PVar_PVar: DoInvokeImported_PVar_PVar();
ocInvokeImported_PVar_Ptr: DoInvokeImported_PVar_Ptr();

ocInvokeImported_Ptr_Stk: DoInvokeImported_Ptr_Stk();
ocInvokeImported_Ptr_PStk: DoInvokeImported_Ptr_PStk();
ocInvokeImported_Ptr_Var: DoInvokeImported_Ptr_Var();
ocInvokeImported_Ptr_PVar: DoInvokeImported_Ptr_PVar();
ocInvokeImported_Ptr_Ptr: DoInvokeImported_Ptr_Ptr();
