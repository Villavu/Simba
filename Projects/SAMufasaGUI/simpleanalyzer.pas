unit simpleanalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SynEdit, SynHighlighterPas, Clipbrd;

type

  TScriptVar = record
    VarName, VarType : string;
  end;

  { TScriptMethod }

  TScriptMethod = class(TObject)
    BeginPos, EndPos : integer;
    isFunction : Boolean;
    Name : string;
    Parameters,Vars : array of TScriptVar;
    ParameterLen,VarLen : integer;
    Returns : String;
    Methods : Array of TScriptMethod;
    MethodLen : Integer;
    function AddMethod( iszehfunction : boolean; TheName : string): TScriptMethod;
    procedure AddVar( TheName, TheType : string);
    procedure AddParam( TheName : string);
    constructor create( iszehfunction : Boolean; TheName : string);
    function CreateMethodStr : string;
    Destructor  Destroy; override;
  end;

  { TScriptAnalyzer }

  TScriptAnalyzer = class(TObject)
    ScriptToAnalyze : string;
    ScriptName : string;
    Methods : Array of TScriptMethod;
    MethodLen : Integer;
    Main : TScriptMethod;
    HasMain : boolean;
    Vars : Array of TScriptVar;
    VarLen : integer;
    Function AddMethod( iszehfunction : boolean; Name : string): TScriptMethod;
    procedure AddVar( TheName, TheType : string);
    procedure analyze;
    constructor create;
    Destructor  Destroy; override;
  end;



implementation

uses
  mPasLex,strutils;



destructor TScriptAnalyzer.Destroy;
var
  i : integer;
begin;
  for i := 0 to MethodLen - 1 do
    Self.Methods[i].Free;
  Main.Free;
  inherited;
end;

destructor TScriptMethod.Destroy;
var
  i : integer;
begin;
  for i := 0 to MethodLen - 1 do
    Self.Methods[i].Free;
  inherited;
end;
constructor TScriptMethod.create( iszehfunction : Boolean; TheName : string);
begin;
  inherited Create;
  Self.isFunction := iszehfunction;
  Self.Name := TheName;
end;

function TScriptMethod.CreateMethodStr: string;
var
  i : integer;
  LastType : string;
begin
  if isFunction then
    result := 'function ' + Name
  else
    result := 'procedure '+  name;
  if ParameterLen > 0 then
  begin;
    result := result + '(';
    LastType := '';
    for i := 0 to ParameterLen - 1 do
    begin;
      if lasttype <> '' then
        if lowercase(lasttype) <> lowercase(Parameters[i].VarType) then
          result := result + ' : ' + LastType + '; '
        else
          result := result + ', ';
        result := result + Parameters[i].VarName;
      lasttype := Parameters[i].VarType;
    end;
    Result := result + ' : ' + Parameters[ParameterLen - 1].VarType + ')';
  end;
  if isFunction then
    result := result + ' : ' + Returns
  else
    result := result + ';';
end;

constructor TScriptAnalyzer.create;
begin;
  inherited create;
  ScriptName := 'Default';
  Main := TScriptMethod.create(false,'!main');
end;
procedure TScriptMethod.addVar( TheName, TheType : string);
begin;
  inc(Self.VarLen);
  SetLength(Self.Vars, self.varlen);
  Self.Vars[Self.VarLen - 1].VarName := TheName;
  Self.Vars[Self.VarLen - 1].VarType := TheType;
end;
procedure TScriptAnalyzer.AddVar( TheName, TheType : string);
begin;
  inc(Self.VarLen);
  SetLength(Self.Vars, self.VarLen);
  Self.Vars[Self.VarLen - 1].VarName := TheName;
  Self.Vars[Self.VarLen - 1].VarType := TheType;
end;
function TScriptMethod.AddMethod( iszehfunction : boolean; TheName : string) : TScriptMethod;
begin;
  inc(Self.MethodLen);
  SetLength(Self.Methods, Self.MethodLen);
  Self.Methods[Self.Methodlen - 1] := TScriptMethod.create(iszehfunction,TheName);
  Result :=Self.Methods[Self.Methodlen - 1];
end;



function TScriptAnalyzer.AddMethod( iszehfunction : boolean; Name : string) : TScriptMethod;
begin;
  inc(Self.MethodLen);
  SetLength(Self.Methods, Self.MethodLen);
  Self.Methods[Self.Methodlen - 1] := TScriptMethod.create(iszehfunction,Name);
  Result :=Self.Methods[Self.Methodlen - 1];
end;


procedure TScriptMethod.AddParam(TheName: string);
begin;
  inc(Self.ParameterLen);
  SetLength(Self.Parameters, self.ParameterLen);
  Self.Parameters[Self.ParameterLen - 1].VarName := TheName;
end;


procedure TScriptAnalyzer.analyze;
var
  LastTk : TTokenKind;
  StartPos,LastPos: integer;

  LastTkString : string;
  I : integer;
  InMethod : Boolean;
  ExpectingType : boolean; //Params and result
  WaitingForResult : boolean;
  StartParam : integer;
  StartVar : integer;
  Method : TScriptMethod;
  InTypes : Boolean;
  TempName : string;
  BeginCount : integer;
  InGlobal : boolean;
  InParams : boolean;
  InVarSection : Boolean;
  Lex : TmwPasLex;
begin
  Lex := TmwPasLex.Create;
  Lex.Origin := PChar(Self.ScriptToAnalyze);
  InTypes := False;
  InMethod := False;
  InParams := False;
  Method := nil;
  BeginCount := 0;
  ExpectingType := False;
  WaitingForResult := False;
  StartParam := 0;
  InVarSection := False;
  while (Lex.TokenID <> tkNull) do
  begin;
    LastTk := Lex.TokenID;
    LastPos := Lex.RunPos;
    Lex.NextNoJunk;
    case LastTk of
//      tkInclude : ShowMessage(Lex.Token);
//        if (FParser.Token[Length(FParser.Token)] = '}') then
//          FIncludes.Add(StringReplace(LowerCase(Trim(Copy(FParser.Token, 11, Length(FParser.Token) - 11))), '/', '\', [rfReplaceAll]));
      tkProgram : if Lex.TokenID = tkIdentifier then
                    Self.ScriptName := Lex.Token;
      tkRoundOpen: begin;
                     LastTkString := Lex.Token;
                     if InMethod and InParams then
                       StartParam := 0;
                   end;
      tkType      : InVarSection := False;
      tkBegin     : begin;

                      InVarSection := False;
                      if InMethod then
                        inc(BeginCount)
                      else if BeginCount = 0 then
                      begin;
                        InVarSection := false;
                        InMethod := true;
                        HasMain := True;
                        Method := Main;
                        Main.BeginPos := LastPos - 4;
                        Inc(BeginCount);
                      end;
                    end;
      tkCase      : begin;
                      if InMethod then
                        inc(BeginCount);
                    end;
      tkEnd       : if InMethod then
                    begin;
                      Dec(BeginCount);
                      if BeginCount = 0 then
                      begin;
                        Method.EndPos := LastPos;
                        InMethod := False;
                      end;
                    end;
      tkIdentifier: begin;
                      if (InMethod and InParams) or (InVarSection) then
                      begin;

{                        if ExpectingType then
                        begin;
                          FormAnalyzer.SynEdit1.Lines.add(copy( FormAnalyzer.SynEdit1.Lines.Text,StartPos, LastPos - StartPos));
                          LastTKString := copy( FormAnalyzer.SynEdit1.Lines.GetText,StartPos, Lex.TokenPos - StartPos);
                          for i := StartParam to Method.ParameterLen - 1 do
                            Method.Parameters[i].VarType := LastTkString;
                        end
                        else
                          Method.AddParam(LastTkString);    }
                        if not ExpectingType and InVarSection then
                          Method.AddVar(LastTKString,'') else
                        if not ExpectingType then
                          Method.AddParam(LastTKString);
                      end;
                    end;
      tkVar       : begin;
                      if InMethod and Not InParams then
                        InVarSection := True;
                      if not InMethod then
                      begin;
                        Method := Main;
                        InGlobal := True;
                        InVarSection := True;

                      end;
                      if InVarSection then
                        StartVar := Method.VarLen;
                      LastTKString := Lex.Token;
                    end;
      tkConst     : begin;
                      if not InMethod then
                        InVarSection := false;
                      LastTKString := Lex.token;
                    end;
      tkComma     : begin;
                      LastTKString := Lex.Token;
                    end;
      tkColon     : begin;
                      LastTkString := Lex.Token;
                      if (InMethod and InParams) or InVarSection then
                      begin;
                        ExpectingType := True;
                        StartPos := LastPos;
                      end
                      else if InMethod and WaitingForresult then
                      begin;
                        Method.Returns := Lex.Token;
                        WaitingForResult := False;
                      end;
                    end;
      tkSemiColon : begin;
                      if (InMethod and InParams) or (InVarSection) then
                      begin;
                        if ExpectingType then
                        begin;
                          LastTKString := Trim(copy( ScriptToAnalyze,StartPos + 1, LastPos - StartPos - 1));
                          if (InParams and InMethod) then
                            for i := StartParam to Method.ParameterLen - 1 do
                              Method.Parameters[i].VarType := LastTkString
                          else
                            for i := StartVar to Method.VarLen - 1 do
                              Method.Vars[i].VarType := LastTKString;
                          ExpectingType := False;
                        end;
                        LastTkString := Lex.Token;
                        StartParam := Method.ParameterLen;
                        StartVar := Method.VarLen;
                      end;
                    end;
      tkRoundClose: if InMethod and InParams then
                    begin;
                      if ExpectingType then
                      begin;
                        LastTKString := Trim(copy(ScriptToAnalyze,StartPos + 2, LastPos - StartPos - 2));
                        for i := StartParam to Method.ParameterLen - 1 do
                          Method.Parameters[i].VarType := LastTkString;
                        ExpectingType := False;
                      end;
                      InParams := False;
                    end;
      tkProcedure,
      tkFunction : begin;
                      if not InMethod then
                        InVarSection := False;
                      if (not InTypes) and (not InVarSection) then
                      begin;
                        WaitingForResult := LastTK = tkFunction;
                        if Lex.TokenID <> tkIdentifier then
                        begin;
                          ShowMessage('No method name -> exiting');
                          exit;
                        end;
                        TempName := Lex.Token;
                        Lex.NextNoJunk;
                        if Lex.TokenID = tkRoundOpen then
                          InParams := True
                        else if Lex.TokenID = tkPoint then
                        begin;
//                          FormAnalyzer.SynEdit2.Lines.add('In class definition *cough*');
                          Lex.NextNoJunk;
                          TempName := Lex.Token;
                        end else if ((Lex.TokenID = tkSemicolon) xor WaitingForResult) or ((Lex.TokenID = tkColon) xor WaitingForResult)  then
                        begin;
                          InParams := False;
                        end else
                        begin;
                          ShowMessage('Your missing some stuff in the procedure declaration');
                          Exit;
                        end;
                        if InMethod then
                          Method := Method.AddMethod(WaitingForResult,TempName)
                        else
                          Method := Self.AddMethod(WaitingForResult,TempName);
                        InMethod := true;
                        Method.BeginPos := LastPos - 5;

                      end;
                    end;

    end;
//    SynEdit2.Lines.Add(TokeToString(Lex.TokenID) + '-' + Lex.Token);
  end;
end;

end.

