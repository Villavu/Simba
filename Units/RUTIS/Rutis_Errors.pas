Unit Rutis_Errors;

Interface

Resourcestring
  ERR_UNEXPECTED_ERROR        = 'E000: Unexpected error occurred. Please contect the support!';

  ERR_SCANNER_UNEXPECTED_CHAR = 'E010: Unexpected char found in code';
  ERR_UNIT_NOT_FOUND          = 'E011: Unit ''%s'' not found';
  ERR_FILENAME_NOT_UNITNAME   = 'E012: Name of the unit is not the same as the filename';
  ERR_NOT_IMPLEMENTED_YET     = 'E013: This function is not implemented in RUTIS at this time - Please wait for me to implement it';
  ERR_NOT_AVAILABLE           = 'E014: This function is not available in this Version of RUTIS';
  ERR_NOT_AVAILABLE_C_TYPE    = 'E015: This type of const is not available in RUTIS at this time';

  ERR_UNKNOWN_IDENT           = 'E020: Unknown Identifier ''%s''';
  ERR_INDENT_REDEFINED        = 'E021: Identifier redefined ''%s''';
  ERR_UNKNOWN_TYPE            = 'E022: Unknown type ''%s''';
  ERR_UNALLOWED_STATEMENT     = 'E023: Unallowed Statement';

  ERR_EXPECTED_FOUND          = 'E030: ''%s'' expected, ''%s'' found instead';
  ERR_NEEDED_FOUND            = 'E031: ''%s'' needed, ''%s'' found instead';
  ERR_OP_OR_SEMI_EXPECTED     = 'E032: Operator or semicolon expected';
  ERR_VAR_CONSTANT_EXPECTED   = 'E033: Variable or Constant expected';
  ERR_VAR_EXPECTED            = 'E034: Variable expected';
  ERR_REC_EXPECTED            = 'E035: Record expected';
  ERR_ARRAY_EXPECTED          = 'E036: Array expected';
  ERR_PROCEDURE_EXPECTED      = 'E037: Procedure expected';
  ERR_STRING_EXPECTED         = 'E038: String expected';
  ERR_TYPE_EXPECTED           = 'E039: Type expected';
  ERR_EXPECTED                = 'E040: ''%s'' expected';

  ERR_NOT_ENOUGH_PARAMETERS   = 'E070: Not enough actual parameters';
  ERR_TOO_MANY_PARAMETERS     = 'E071: Too many parameters';
  ERR_ONLY_PARAMLESS_PROCS    = 'E072: Only parameterless procedures are allowed';
  ERR_NO_CONST_ALLOWED        = 'E073: No constant allowed here';
  ERR_NO_OVERLOADED_FUNC      = 'E073: There is no overloaded function with these parameters';

  ERR_UNALLOWED_STRING_ACTION = 'E080: Unallowed String action';
  ERR_POINTER_ONLY_FOR_VAR    = 'E081: Pointers can only be created for vars';

  ERR_INCOMPATIBLE_TYPES      = 'E090: Incompatible types ''%s'' and ''%s''';
  ERR_UNALLOWED_DATATYPE      = 'E091: Unallowed type';

  ERR_CODE_AFTER_PROGRAM_END  = 'W001: Code after Program END. is ignored';
  ERR_CODE_AFTER_UNIT_END     = 'W002: Code after Unit END. is ignored';

Implementation

End.

