program test;

{$mode objfpc}{$H+}
{$assertions on}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  simba.ide_codetools_parser, simba.ide_codetools_insight, simba.ide_codetools_exprparser;

function Test(ci: TCodeinsight; Expr: String; Script: array of String): TDeclaration;
begin
  ci.SetScript(String.Join(LineEnding, Script), 'main');
  ci.Run();

  Result := ci.ParseExpr(Expr);
end;

procedure Check(d: TDeclaration; what: String);
begin
  Assert(d <> nil);
  Assert(d.Dump = what);
end;

var
  BaseParser: TCodeParser;
  ci: TCodeinsight;
  d: TDeclaration;
begin
  BaseParser := TCodeParser.Create();
  BaseParser.SetFile('base.pas');
  BaseParser.Run();

  TCodeinsight.AddBaseParser(BaseParser);
  ci := TCodeinsight.Create();

  // type copy
  d := Test(ci, 'Colors[0]', [
    'type',
    '  TColor = type Integer;',
    '  TColorArray = array of TColor;',
    '',
    'function TColor.R: Byte;',
    'begin',
    'end;',
    '',
    'var Colors: TColorArray;'
  ]);
  check(d, 'TDeclaration_TypeCopy (TColor)');

  d := Test(ci, 'Colors[0].R', [
    'type',
    '  TColor = type Integer;',
    '',
    'function TColor.R: Byte;',
    'begin',
    'end;',
    '',
    'var Colors: array of TColor;'
  ]);
  check(d, 'TDeclaration_MethodOfType (R) [function]');

  // string indexing
  d := Test(ci, 'Str[1]', [
    'var Str: String;'
  ]);
  check(d, 'TDeclaration_TypeAlias (Char) [Char]');

  d := Test(ci, 'Strings[0]', [
    'var Strings: array of array of String;'
  ]);
  check(d, 'TDeclaration_TypeArray [String] <1>');

  d := Test(ci, 'Strings[0][1]', [
    'var Strings: array of array of String;'
  ]);
  check(d, 'TDeclaration_TypeAlias (Char) [Char]');

  // pointer dereference
  d := Test(ci, 'Pixels[0]^.R', [
    'var Pixels: array of PColorBGRA;'
  ]);
  check(d, 'TDeclaration_Field (R) [TColorBGRA]');

  d := Test(ci, 'GetPixel()^.R', [
    'function GetPixel: PColorBGRA; begin end;'
  ]);
  check(d, 'TDeclaration_Field (R) [TColorBGRA]');

  // methods
  d := Test(ci, 'TPoint.Rand', [
    ''
  ]);
  check(d, 'TDeclaration_MethodOfType (Rand) [function]');

  d := Test(ci, 'GetAnonRec().p.Rand', [
    'function GetAnonRec: record p: TPoint; b: TBox; end;',
    'begin',
    'end;'
  ]);
  check(d, 'TDeclaration_MethodOfType (Rand) [function]');

  // fields
  d := Test(ci, 'TPoint.X', [
    ''
  ]);
  check(d, 'TDeclaration_Field (X) [TPoint]');

  d := Test(ci, 'NewBox().Center.X', [
    'function NewBox: TBox; begin end;'
  ]);
  check(d, 'TDeclaration_Field (X) [TPoint]');

  d := Test(ci, 'AnonRecord.NestedAnonRecord.NestedField2', [
    'var',
    '  AnonRecord: record',
    '    NestedAnonRecord: record',
    '      NestedField1, NestedField2: Integer;',
    '    end;',
    '    Field1: Integer;',
    '  end;'
  ]);
  check(d, 'TDeclaration_Field (NestedField2) []');

  // access parent type method
  d := Test(ci, 'form.obj', [
    'type',
    '  TLazObject = type Pointer;',
    '  TLazForm = type TLazObject;',
    '',
    '  procedure TLazObject.obj;',
    '  begin',
    '  end;',
    '  procedure TLazForm.form;',
    '  begin',
    '  end;',
    '',
    'var form: TLazForm;'
  ]);
  check(d, 'TDeclaration_MethodOfType (obj) [procedure]');

  // array indexing
  d := Test(ci, 'GetTPA()[0].X', [
    'function GetTPA: TPointArray; begin end;'
  ]);
  check(d, 'TDeclaration_Field (X) [TPoint]');

  d := Test(ci, 'GetATPA()[0,0].X', [
    'function GetATPA: T2DPointArray; begin end;'
  ]);
  check(d, 'TDeclaration_Field (X) [TPoint]');

  d := Test(ci, 'GetATPA()[0]', [
    'function GetATPA: T2DPointArray; begin end;'
  ]);
  check(d, 'TDeclaration_TypeArray (TPointArray) [TPoint] <1>');

  ReadLn;
end.

