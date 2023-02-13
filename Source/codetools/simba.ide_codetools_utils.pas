{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_utils;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  mPasLexTypes, mPasLex;

type
  TExpressionItem = record
    IsLastItem: Boolean;

    Text: String;
    Dimensions: Integer;

    DerefDimension: Boolean;
    DerefText: Boolean;
  end;
  TExpressionItems = array of TExpressionItem;

function StringToExpression(const Str: String): TExpressionItems;

function FindInclude(Sender: TmwBasePasLex): String;
function FindPluginExports(FileName: String): String;

implementation

uses
  simba.mufasatypes, simba.files, simba.process;

function StringToExpression(const Str: String): TExpressionItems;
var
  Lex: TmwPasLex;
  InSquare, InRound: Integer;
  LastToken: TptTokenKind;
begin
  Result := nil;

  InSquare := 0;
  InRound := 0;

  Lex := TmwPasLex.Create(Str);
  try
    Lex.NextNoJunk();
    if (Lex.TokenID <> tokIdentifier) then
      Exit;

    LastToken := Lex.TokenID;
    while (Lex.TokenID <> tokNull) do
    begin
      case Lex.TokenID of
        tokIdentifier:
          begin
            if (InRound = 0) and (InSquare = 0) then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)].Text := Lex.Token;
            end;
          end;
        tokSquareOpen:
          begin
            Inc(InSquare);
          end;
        tokSquareClose:
          begin
            if (InSquare > 0) and (InRound = 0) and (Length(Result) > 0) then
              Inc(Result[High(Result)].Dimensions);

            Dec(InSquare);
          end;
        tokComma:
          begin
            if (InSquare > 0) and (InRound = 0) and (Length(Result) > 0) then
              Inc(Result[High(Result)].Dimensions);
          end;
        tokRoundOpen:
          begin
            Inc(InRound);
          end;
        tokRoundClose:
          begin
            Dec(InRound);
          end;
        tokPointerSymbol:
          begin
            if (InRound = 0) and (InSquare = 0) and (Length(Result) > 0) then
            begin
              if (LastToken = tokSquareClose) then
                Result[High(Result)].DerefDimension := True
              else
                Result[High(Result)].DerefText := True;
            end;
          end;
      end;

      LastToken := Lex.TokenID;
      Lex.NextNoJunk();
    end;
  finally
    Lex.Free();
  end;

  if (Length(Result) > 0) then
    Result[High(Result)].IsLastItem := True;
end;

function FindInclude(Sender: TmwBasePasLex): String;
var
  FileName: String;
begin
  Result := '';

  FileName := Sender.DirectiveParamAsFileName;

  case Sender.TokenID of
    tokLibraryDirect:
      begin
        if FindPlugin(FileName, [ExtractFileDir(Sender.FileName), GetPluginPath(), GetSimbaPath()]) then
          Result := FileName;
      end;

    tokIncludeDirect, tokIncludeOnceDirect:
      begin
        if FindFile(FileName, '', [ExtractFileDir(Sender.FileName), GetIncludePath(), GetSimbaPath()]) then
          Result := FileName;
      end;
  end;
end;

function FindPluginExports(FileName: String): String;
var
  List: TStringList;
begin
  Result := '';

  List := nil;
  try
    List := SimbaProcess.RunDump(HashFile(FileName), ['--dumpplugin=' + FileName]);

    Result := List.Text;
  except
    on E: Exception do
      DebugLn(E.Message);
  end;

  if (List <> nil) then
    List.Free();
end;

end.
