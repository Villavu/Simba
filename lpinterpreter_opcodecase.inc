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
  ocGrowStack: DoGrowStack();
  ocExpandVar: DoExpandVar();
  ocExpandVarAndInit: DoExpandVarAndInit();
  ocGrowVar: DoGrowVar();
  ocGrowVarAndInit: DoGrowVarAndInit();
  ocPopVar: DoPopVar();
  ocJmpSafe: DoJmpSafe();
  ocJmpSafeR: DoJmpSafeR();

  ocIncTry: DoIncTry();
  ocDecTry: DoDecTry();
  ocEndTry: DoEndTry();
  ocCatchException: DoCatchException();

  ocDecCall: DoDecCall();
  ocDecCall_EndTry: DoDecCall_EndTry();

  {$I lpinterpreter_invokecase.inc}
  {$I lpinterpreter_jumpcase.inc}
  {$I lpinterpreter_evalcase.inc}

  else
    LapeException(lpeUnknownOC);
end;
