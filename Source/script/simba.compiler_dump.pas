{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Overrides import methods to gather declarations.
}
unit simba.compiler_dump;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

procedure DumpCompiler(FileName: String);

implementation

uses
  lpparser, lptypes, lpvartypes, lptree,
  simba.script_compiler;

type
  TSimbaCompilerDump = class(TSimbaScript_Compiler)
  protected
    FList: TStringList;

    procedure InitBaseVariant; override;
    procedure InitBaseMath; override;
    procedure InitBaseDefinitions; override;
    procedure InitBaseDateTime; override;
    procedure InitBaseFile; override;

    procedure IterateDump(Item: String; const Key: string; var Continue: Boolean);

    procedure addMethodDump(Header: String);
    procedure addTypeDump(Name, Str: String);
    procedure addCodeDump(Str: String);
  public
    procedure addBaseDefine(Define: lpString; Value: lpString = ''); override;

    function addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;
    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method; override;
    function addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;

    procedure DumpToFile(FileName: String);
  end;

procedure DumpCompiler(FileName: String);
begin
  with TSimbaCompilerDump.Create(TLapeTokenizerString.Create('begin end')) do
  try
    DumpToFile(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaCompilerDump.InitBaseVariant;
begin
  { nothing, we import our own variant }
end;

procedure TSimbaCompilerDump.InitBaseMath;
begin
  ImportingSection := 'Math';
  inherited InitBaseMath();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.InitBaseDefinitions;
begin
  ImportingSection := 'System';
  inherited InitBaseDefinitions();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.InitBaseDateTime;
begin
  ImportingSection := 'DateTime';
  inherited InitBaseDateTime();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.InitBaseFile;
begin
  ImportingSection := 'File';
  inherited InitBaseFile();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.IterateDump(Item: String; const Key: string; var Continue: Boolean);
begin
  FList.Values[Item] := FList.Values[Item] + Key + LineEnding;
end;

procedure TSimbaCompilerDump.addMethodDump(Header: String);
begin
  Header := Header.Trim();
  if not Header.EndsWith(';') then
    Header := Header + ';';
  Header := Header + ' external;';

  FDump.Add(Header, ImportingSection);
end;

procedure TSimbaCompilerDump.addTypeDump(Name, Str: String);
begin
  if Name.StartsWith('!') then
    Exit;

  Str := 'type ' + Name + ' = ' + Str;
  if not Str.EndsWith(';') then
    Str := Str + ';';

  FDump.Add(Str, ImportingSection);
end;

procedure TSimbaCompilerDump.addCodeDump(Str: String);
begin
  FDump.Add(Str, ImportingSection);
end;

procedure TSimbaCompilerDump.addBaseDefine(Define: lpString; Value: lpString);
begin
  inherited addBaseDefine(Define, Value);

  addCodeDump('{$DEFINE ' + Define + '}');
end;

function TSimbaCompilerDump.addDelayedCode(ACode: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(ACode, AFileName, AfterCompilation, IsGlobal);

  addCodeDump(ACode);
end;

function TSimbaCompilerDump.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);

  addMethodDump(Header);
end;

function TSimbaCompilerDump.addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method;
begin
  Result := inherited addGlobalFunc(Header, Body);

  addMethodDump(Header);
end;

function TSimbaCompilerDump.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  Result := inherited addGlobalType(Typ, AName, ACopy);

  addTypeDump(AName, Typ.Name);
end;

function TSimbaCompilerDump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  addTypeDump(AName, Str);
end;

procedure TSimbaCompilerDump.DumpToFile(FileName: String);
var
  BaseType: ELapeBaseType;
  Decl: TLapeDeclaration;
begin
  Import();

  // Base types
  for BaseType in ELapeBaseType do
    if (FBaseTypes[BaseType] <> nil) then
      FDump['type %s = %s;'.Format([LapeTypeToString(BaseType), LapeTypeToString(BaseType)])] := 'System';

  // Variables & Constants
  for Decl in FGlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue) do
    with TLapeGlobalVar(Decl) do
    begin
      if (DocPos.FileName = '') or (DocPos.FileName[1] = '!') then
        Continue;
      if (Name = '') or (Name = 'nil') or (VarType.Name = '') or (BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
        Continue;

      if isConstant then
        FDump['const %s = %s;'.Format([Name.ToUpper(), VarType.Name])] := DocPos.FileName
      else
        FDump['var %s: %s;'.Format([Name, VarType.Name])] := DocPos.FileName;
    end;

  // add internals
  FDump['procedure Delete(A: array; Index: Int32; Count: Int32 = Length(A)); external;'] := 'System';
  FDump['procedure Insert(Item: Anything; A: array; Index: Int32); external;'] := 'System';
  FDump['procedure Copy(A: array; Index: Int32 = 0; Count: Int32 = Length(A)); overload; external;'] := 'System';
  FDump['procedure SetLength(A: array; Length: Int32); overload; external;'] := 'System';
  FDump['function Low(A: array): Int32; external;'] := 'System';
  FDump['function High(A: array): Int32; external;'] := 'System';
  FDump['function Length(A: array): Int32; overload; external;'] := 'System';
  FDump['procedure WriteLn(Args: Anything); external;'] := 'System';
  FDump['procedure Write(Args: Anything); external;'] := 'System';
  FDump['procedure Swap(var A, B: Anything); external;'] := 'System';
  FDump['function SizeOf(A: Anything): Int32; external;'] := 'System';
  FDump['function ToString(A: Anything): String; external;'] := 'System';
  FDump['function ToStr(A: Anything): String; external;'] := 'System';
  FDump['function Inc(var X: Ordinal; Amount: SizeInt = 1): Ordinal; overload; external;'] := 'System';
  FDump['function Dec(var X: Ordinal; Amount: SizeInt = 1): Ordinal; overload; external;'] := 'System';
  FDump['function Ord(X: Ordinal): Int32; external;'] := 'System';
  FDump['function WaitUntil(Condition: Expression; Interval, Timeout: Int32): Boolean; external;'] := 'System';
  FDump['function Default(T: AnyType): AnyType; external;'] := 'System';
  FDump['procedure Sort(var A: array); overload; external;'] := 'System';
  FDump['procedure Sort(var A: array; CompareFunc: function(constref L, R: Anything): Int32); overload; external;'] := 'System';
  FDump['procedure Sort(var A: array; Weights: TIntegerArray; LowToHigh: Boolean); overload; external;'] := 'System';
  FDump['procedure Sort(var A: array; Weights: TExtendedArray; LowToHigh: Boolean); overload; external;'] := 'System';
  FDump['function Sorted(const A: array): array; overload; external;'] := 'System';
  FDump['function Sorted(const A: array; CompareFunc: function(constref L, R: Anything): Int32): array; overload; external;'] := 'System';
  FDump['function Sorted(const A: array; Weights: TIntegerArray; LowToHigh: Boolean): array; overload; external;'] := 'System';
  FDump['function Sorted(const A: array; Weights: TExtendedArray; LowToHigh: Boolean): array; overload; external;'] := 'System';
  FDump['function Unique(const A: array): array; external;'] := 'System';
  FDump['procedure Reverse(var A: array); external;'] := 'System';
  FDump['function Reversed(const A: array): array; external;'] := 'System';
  FDump['function IndexOf(const Item: T; const A: array): Integer; external;'] := 'System';
  FDump['function IndicesOf(const Item: T; const A: array): TIntegerArray; external;'] := 'System';
  FDump['function Contains(const Item: T; const A: array): Boolean; external;'] := 'System';

  FDump['function RTTIFields(constref AnyRecord): TRTTIFields; external;']      := 'System';
  FDump['function RTTIClassFields(constref AnyRecord): TRTTIFields; external;'] := 'System';

  FDump['function GetCallerAddress: Pointer; external;']    := 'System';
  FDump['function GetCallerName: String; external;']        := 'System';
  FDump['function GetCallerLocation: Pointer; external;']   := 'System';
  FDump['function GetCallerLocationStr: String; external;'] := 'System';

  FDump['function GetExceptionLocation: Pointer; external;']   := 'System';
  FDump['function GetExceptionLocationStr: String; external;'] := 'System';
  FDump['function GetExceptionMessage: String; external;']     := 'System';

  FDump['function GetScriptMethodName(Address: Pointer): String; external;'] := 'System';
  FDump['function DumpCallStack(Start: Integer = 0): String; external;']     := 'System';

  // Move lape stuff to better sections
  FDump['function Random(min, max: Int64): Int64; overload; external;']       := 'Random';
  FDump['function Random(min, max: Extended): Extended; overload; external;'] := 'Random';
  FDump['function Random(l: Int64): Int64; overload; external;']              := 'Random';
  FDump['function Random: Extended; overload; external;']                     := 'Random';
  FDump['procedure Randomize; external;']                                     := 'Random';
  FDump['var RandSeed: UInt32;']                                              := 'Random';

  FDump['function GetTickCount: UInt64; external;']                           := 'Timing';
  FDump['procedure Sleep(MilliSeconds: UInt32); external;']                   := 'Timing';

  FList := TStringList.Create();
  FList.LineBreak := #0;

  FDump.Iterate(@IterateDump);

  FList.SaveToFile(FileName);
  FList.Free();
end;

end.

