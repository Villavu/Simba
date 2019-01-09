unit scriptcommenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TScriptCommenter }

  TScriptCommenter = class
  private
    FSelStart: integer;
    FSelEnd: integer;
    FLines: TStrings;
    procedure Clear;
  public
    constructor Create(const SStart, SSEnd: integer);
    property SelStart: integer read FSelStart write FSelStart;
    property SelEnd: integer read FSelEnd write FSelEnd;
    property Lines: TStrings read FLines write FLines;

    procedure Process;

  end;

implementation

{ TScriptCommenter }

procedure TScriptCommenter.Clear;
begin
  FSelStart := -1;
  FSelEnd := -1;
end;

constructor TScriptCommenter.Create(const SStart, SSEnd: integer);
begin
  Clear;
  SelStart := SStart;
  SelEnd := SSEnd;
end;

procedure TScriptCommenter.Process;
var
  i: integer;
  Str: string;
begin
  for i := SelStart to SelEnd do
  begin
    Str := Lines.Strings[i];
    if ((Length(Str) > 3) and (Str[1] = '/') and (Str[2] = '/')) then
    begin
      Str := copy(Str, 3, length(Str) - 2);
    end
    else if (Length(Str) > 1) then
      Str := '//' + Str;
    Lines.Strings[i] := str;
  end;
end;

end.
