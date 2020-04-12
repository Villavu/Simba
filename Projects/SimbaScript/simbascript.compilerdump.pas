unit simbascript.compilerdump;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  lptypes, lpvartypes, lptree,
  simbascript.compiler;

type
  TSimbaScript_CompilerDump = class(TSimbaScript_Compiler)
  protected
    FDump: TStringList;

    procedure Add(Header: String; TrailingSemiColon: Boolean = True; IsMethod: Boolean = False);
  public
    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; overload; override;

    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; override;

    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; override;

    function addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar; override;

    function addDelayedCode(Code: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;

    procedure addBaseDefine(Define: lpString); override;
    procedure addBaseDefine(Define: lpString; Value: lpString); override;

    property Dump: TStringList read FDump;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  lpparser;

procedure TSimbaScript_CompilerDump.addBaseDefine(Define: lpString);
begin
  inherited addBaseDefine(Define);

  Add(Format('{$DEFINE %s}', [Define]), False, False);
end;

procedure TSimbaScript_CompilerDump.addBaseDefine(Define: lpString; Value: lpString);
begin
  FBaseDefines[Define] := Trim(Value);

  Add(Format('{$DEFINE %s}', [Define]), False, False);
end;

function TSimbaScript_CompilerDump.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);

  Add(Header, True, True);
end;

function TSimbaScript_CompilerDump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  Add(Format('type %s = %s', [AName, Str]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);

  Add(Format('var %s: %s', [AName, Typ]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: Int32 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: UInt32 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: Int64 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: UInt64 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: Extended = %s', [AName, FloatToStr(Value)]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: EvalBool = %s', [AName, BoolToStr(Value, True)]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: String = "%s"', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('var %s: UnicodeString = "%s"', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);

  Add(Format('const %s: %s', [AName, Typ]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: Int32 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: UInt32 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: Int64 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: UInt64 = %d', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: Extended = %s', [AName, FloatToStr(Value)]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: EvalBool = %s', [AName, BoolToStr(Value, True)]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Add(Format('const %s: String = "%s"', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  Add(Format('const %s: UnicodeString = "%s"', [AName, Value]));
end;

function TSimbaScript_CompilerDump.addDelayedCode(Code: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(Code, AFileName, AfterCompilation, IsGlobal);

  if (not AFileName.StartsWith('!')) then
    Add(Code);
end;

procedure TSimbaScript_CompilerDump.Add(Header: String; TrailingSemiColon: Boolean; IsMethod: Boolean);
begin
  if (Dump = nil) then
    Exit;

  Header := Trim(Header);

  if TrailingSemiColon and (not Header.EndsWith(';')) then
    Header := Header + ';';

  if IsMethod then
  begin
    Header := Header + LineEnding + 'begin';
    Header := Header + LineEnding + '  // Internal method in Simba';
    Header := Header + LineEnding + 'end;';
  end;

  FDump.Values[FSection] := FDump.Values[FSection] + Header + LineEnding;
end;

constructor TSimbaScript_CompilerDump.Create;
begin
  inherited Create(TLapeTokenizerString.Create('begin end.'));

  FDump := TStringList.Create();
  FDump.LineBreak := #0;
  FDump.Values['Types'] := {$i lape_type_imports.inc}
  FDump.Values['Math'] := {$i lape_math_imports.inc}
  FDump.Values['Time & Date'] := {$i lape_date_time_imports.inc}
  FDump.Values['String'] := {$i lape_string_imports.inc}
  FDump.Values['Variant'] := {$i lape_variant_imports.inc}
  FDump.Values['System'] := {$i lape_system_imports.inc}
  FDump.Values['File'] := {$i lape_file_imports.inc}

  Import(nil);
end;

destructor TSimbaScript_CompilerDump.Destroy;
begin
  FDump.Free();

  inherited Destroy;
end;

end.

