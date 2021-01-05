unit simba.script_import_types;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Types(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

procedure Lape_Import_Types(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Types';

    addGlobalType('array of String', 'TStringArray');
    addGlobalType('array of TStringArray', 'T2DStringArray');
    addGlobalType('array of Int32', 'TIntegerArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of TIntegerArray', 'T2DIntArray');
    addGlobalType('array of T2DIntegerArray', 'T3DIntegerArray');
    addGlobalType('array of Char', 'TCharArray');
    addGlobalType('array of TCharArray', 'T2DCharArray');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of TByteArray', 'T2DByteArray');
    addGlobalType('array of Single', 'TSingleArray');
    addGlobalType('array of TSingleArray', 'T2DSingleArray');
    addGlobalType('array of Double', 'TDoubleArray');
    addGlobalType('array of TDoubleArray', 'T2DDoubleArray');
    addGlobalType('array of Extended', 'TExtendedArray');
    addGlobalType('array of TExtendedArray', 'T2DExtendedArray');
    addGlobalType('array of Boolean', 'TBoolArray');
    addGlobalType('array of TBoolArray', 'T2DBoolArray');
    addGlobalType('array of Variant', 'TVariantArray');
    addGlobalType('array of TVariantArray', 'T2DVariantArray');

    addGlobalType('record X1, Y1, X2, Y2: Int32; end', 'TBox');
    addGlobalType('^TBox', 'PBox');
    addGlobalType('array of TBox', 'TBoxArray');
    addGlobalType('array of TBoxArray', 'T2DBoxArray');

    addGlobalType('record X, Y: Int32; end', 'TPoint');
    addGlobalType('^TPoint', 'PPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record R, T: Extended; end', 'TPolarPoint');
    addGlobalType('^TPolarPoint', 'PPolarPoint');

    addGlobalType('Int32', 'TColor');
    addGlobalType('UInt32', 'DWord');

    addGlobalType('array of Boolean', 'TBooleanMatrix');
    addGlobalType('array of TIntegerArray', 'TIntegerMatrix');
    addGlobalType('array of TByteArray', 'TByteMatrix');
    addGlobalType('array of TSingleArray', 'TSingleMatrix');

    addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');
  end;
end;

end.

