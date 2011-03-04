{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This is include directs the opcode to the correct function.
}
{%MainUnit lpinterpreter.pas}

case opCode(opCodeTypeP(Code)^) of
  ocNone: Break;
  ocInitStackLen: DoInitStackLen();
  ocInitVarLen: DoInitVarLen();
  ocInitStack: DoInitStack();
  ocExpandVar: DoExpandVar();
  ocExpandVarAndInit: DoExpandVarAndInit();
  ocGrowVar: DoGrowVar();
  ocGrowVarAndInit: DoGrowVarAndInit();
  ocPopVar: DoPopVar();

  ocIncTry: DoIncTry();
  ocDecTry: DoDecTry();
  ocEndTry: DoEndTry();
  ocCatchException: DoCatchException();

  ocIncCall: DoIncCall();
  ocDecCall: DoDecCall();
  ocDecCall_EndTry: DoDecCall_EndTry();

  ocInvokeExternalProc: DoInvokeExternalProc;
  ocInvokeExternalFunc: DoInvokeExternalFunc;

  {$I lpinterpreter_jumpcase.inc}
  {$I lpinterpreter_evalcase.inc}

  else
    LapeException(lpeUnknownOC);
end;
