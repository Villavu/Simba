unit scriptcommenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit;

type

  { TScriptCommenter }

  TScriptCommenter = class
  public
    class procedure Process(Syn: TSynEdit);static;

  end;

implementation

{ TScriptCommenter }

class procedure TScriptCommenter.Process(Syn: TSynEdit);
var
  i, SelStart, SelEnd, temp: integer;
begin
  SelStart := Syn.BlockBegin.y;
  SelEnd := Syn.BlockEnd.y;
  if(SelStart > SelEnd) then
  begin
    temp := SelStart;
    SelStart := SelEnd;
    SelEnd := temp;
  end;

  Syn.BeginUpdate;
  try
    Syn.BeginUndoBlock;
    try
      if Syn.Lines[SelStart - 1].StartsWith('//') then
      begin
        for i := SelStart to SelEnd do
            if Syn.Lines[i - 1].StartsWith('//') then
              Syn.TextBetweenPoints[Point(0, i), Point(3, i)] := '';
      end
      else
          for i := SelStart to SelEnd do
              Syn.TextBetweenPoints[Point(0, i), Point(0, i)] := '//';
    finally
      Syn.EndUndoBlock;
    end;
  finally
    Syn.EndUpdate;
  end;
end;

end.
