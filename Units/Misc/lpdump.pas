unit LPDump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpvartypes, lpparser, lptree;

type
  TLPCompiler = class(TLapeCompiler)
  private
    FItems: TStringList;
  public
    constructor Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    ); override;
    destructor Destroy; override;

    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; override;
    function addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Val: Int32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;
    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addDelayedCode(ACode: lpString; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;
    procedure getInfo(aItems: TStrings);
  end;

implementation

function AddLeadingSemiColon(x: string): string;
begin
  Result := Trim(x);
  if (Result[Length(Result)] <> ';') then
    Result += ';';
end;

constructor TLPCompiler.Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True; AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True);
begin
  FItems := TStringList.Create();

  inherited Create(ATokenizer, ManageTokenizer, AEmitter, ManageEmitter);
end;

destructor TLPCompiler.Destroy;
begin
  FItems.Free();

  inherited;
end;

function TLPCompiler.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := inherited;
  //if (Length(AName) > 0) and (AName[1] <> '!') then
    //FItems.Add(AddLeadingSemiColon('const ' + AName + ' = ' + AVar.AsString));
end;

function TLPCompiler.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;
  if (Length(AName) > 0) and (AName[1] <> '!') and (Lowercase(AName) <> Lowercase(Typ)) then
    FItems.Add(AddLeadingSemiColon('var ' + AName + ': ' + Typ));
end;

function TLPCompiler.addGlobalVar(Val: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;
  if (Length(AName) > 0) and (AName[1] <> '!') then
    FItems.Add(AddLeadingSemiColon('var ' + AName + ': ' + IntToStr(Val)));
end;

function TLPCompiler.addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType;
begin
  Result := inherited;
  if (Length(AName) > 0) and (AName[1] <> '!') and (Lowercase(AName) <> Lowercase(Typ.Name)) then
    FItems.Add(AddLeadingSemiColon('type ' + AName + ' = ' + Typ.Name));
end;

function TLPCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited;
  if (Length(AName) > 0) and (AName[1] <> '!') and (Lowercase(AName) <> Lowercase(Str)) then
    FItems.Add(AddLeadingSemiColon('type ' + AName + ' = ' + Str));
end;

function TLPCompiler.addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited;
  FItems.Add(AddLeadingSemiColon(AHeader) + ' forward;');
end;

function TLPCompiler.addDelayedCode(ACode: lpString; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base;
begin
  Result := inherited;
  FItems.Add(ACode);
end;

procedure TLPCompiler.getInfo(aItems: TStrings);
begin
  aItems.Clear();
  aItems.addStrings(FItems);
end;

end.

