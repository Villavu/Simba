unit v_Constants;

interface

//Code Insight Constants
const
  {$IFNDEF FPC}
  LineEnding = #13#10;
  {$ENDIF}

  ci_RangeError = 'Array range error at "%s" in statement: "%s"';
  ci_UnknownMember = 'Unknown member "%s" in statement: "%s"';
  ci_UnknownStruct = 'Cannot find proper structure for "%s" in statement: "%s"';
  ci_UnknownInclude = 'Unknown include "%s"';

implementation

end.
