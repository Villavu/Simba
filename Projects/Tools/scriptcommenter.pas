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
  i, SelStart, SelEnd, ColStart, temp: integer;
  StartStr: String;

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
      StartStr := Syn.Lines[SelStart - 1].TrimLeft();
      ColStart := Syn.Lines[SelStart - 1].IndexOf(StartStr) + 1;

      if StartStr.StartsWith('//') then
      begin
        for i := SelStart to SelEnd do
          if Syn.Lines[i - 1].TrimLeft().StartsWith('//') then
            Syn.TextBetweenPoints[Point(ColStart, i), Point(ColStart + 2, i)] := '';
      end
      else
        for i := SelStart to SelEnd do
          Syn.TextBetweenPoints[Point(ColStart, i), Point(ColStart, i)] := '//';
    finally
      Syn.EndUndoBlock;
    end;
  finally
    Syn.EndUpdate;
  end;
end;

end.
