{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.type_matrix;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

// Boolean Matrix
{$DEFINE TMATRIXHELPER := TBooleanMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TBooleanMatrix}
{$DEFINE TMATRIXPOINTERTYPE := PBooleanMatrix}
{$DEFINE TMATRIXARRAYTYPE := TBooleanArray}
{$DEFINE TMATRIXARRAYPOINTERTYPE := PBooleanArray}
{$DEFINE TMATRIXVARTYPE := Boolean}

{$I matrixhelper_header.inc}

// Byte Matrix
{$DEFINE TMATRIXHELPER := TByteMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TByteMatrix}
{$DEFINE TMATRIXPOINTERTYPE := PByteMatrix}
{$DEFINE TMATRIXARRAYTYPE := TByteArray}
{$DEFINE TMATRIXARRAYPOINTERTYPE := PByteArray}
{$DEFINE TMATRIXVARTYPE := Byte}

{$I matrixhelper_header.inc}

// Integer Matrix
{$DEFINE TMATRIXHELPER := TIntegerMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TIntegerMatrix}
{$DEFINE TMATRIXPOINTERTYPE := PIntegerMatrix}
{$DEFINE TMATRIXARRAYTYPE := TIntegerArray}
{$DEFINE TMATRIXARRAYPOINTERTYPE := PIntegerArray}
{$DEFINE TMATRIXVARTYPE := Integer}

{$I matrixhelper_header.inc}

// Single Matrix
{$DEFINE TMATRIXHELPER := TSingleMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TSingleMatrix}
{$DEFINE TMATRIXPOINTERTYPE := PSingleMatrix}
{$DEFINE TMATRIXARRAYTYPE := TSingleArray}
{$DEFINE TMATRIXARRAYPOINTERTYPE := PSingleArray}
{$DEFINE TMATRIXVARTYPE := Single}

{$I matrixhelper_header.inc}

// Double Matrix
{$DEFINE TMATRIXHELPER := TDoubleMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TDoubleMatrix}
{$DEFINE TMATRIXPOINTERTYPE := PDoubleMatrix}
{$DEFINE TMATRIXARRAYTYPE := TDoubleArray}
{$DEFINE TMATRIXARRAYPOINTERTYPE := PDoubleArray}
{$DEFINE TMATRIXVARTYPE := Double}

{$I matrixhelper_header.inc}

// Complex Matrix
{$DEFINE TMATRIXHELPER := TComplexMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TComplexMatrix}
{$DEFINE TMATRIXPOINTERTYPE := PComplexMatrix}
{$DEFINE TMATRIXARRAYTYPE := TComplexArray}
{$DEFINE TMATRIXARRAYPOINTERTYPE := PComplexArray}
{$DEFINE TMATRIXVARTYPE := TComplex}

{$I matrixhelper_header.inc}

implementation

uses
  math;

// Boolean Matrix
{$DEFINE TMATRIXHELPER := TBooleanMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TBooleanMatrix}
{$DEFINE TMATRIXARRAYTYPE := TBooleanArray}
{$DEFINE TMATRIXVARTYPE := Boolean}

{$I matrixhelper_body.inc}

// Byte Matrix
{$DEFINE TMATRIXHELPER := TByteMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TByteMatrix}
{$DEFINE TMATRIXARRAYTYPE := TByteArray}
{$DEFINE TMATRIXVARTYPE := Byte}

{$I matrixhelper_body.inc}

// Integer Matrix
{$DEFINE TMATRIXHELPER := TIntegerMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TIntegerMatrix}
{$DEFINE TMATRIXARRAYTYPE := TIntegerArray}
{$DEFINE TMATRIXVARTYPE := Integer}

{$I matrixhelper_body.inc}

// Single Matrix
{$DEFINE TMATRIXHELPER := TSingleMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TSingleMatrix}
{$DEFINE TMATRIXARRAYTYPE := TSingleArray}
{$DEFINE TMATRIXVARTYPE := Single}

{$I matrixhelper_body.inc}

// Double Matrix
{$DEFINE TMATRIXHELPER := TDoubleMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TDoubleMatrix}
{$DEFINE TMATRIXARRAYTYPE := TDoubleArray}
{$DEFINE TMATRIXVARTYPE := Double}

{$I matrixhelper_body.inc}

// Complex Matrix
{$DEFINE TMATRIXHELPER := TComplexMatrix_BaseHelper}
{$DEFINE TMATRIXTYPE := TComplexMatrix}
{$DEFINE TMATRIXARRAYTYPE := TComplexArray}
{$DEFINE TMATRIXVARTYPE := TComplex}

{$I matrixhelper_body.inc}

end.

