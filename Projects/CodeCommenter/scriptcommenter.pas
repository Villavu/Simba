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
      Fis_C_Code: boolean;
      procedure Clear;
    public
      Constructor Create(const SStart, SSEnd: integer);
      property SelStart: integer read FSelStart write FSelStart;
      property SelEnd: integer read FSelEnd write FSelEnd;
      property Lines: TStrings read FLines write FLines;
      property IsCCode: boolean read FIs_C_Code write FIs_C_Code;

      procedure Process;

  end;

implementation

{ TScriptCommenter }

procedure TScriptCommenter.Clear;
begin
  FSelStart := -1;
  FSelEnd := -1;
  Fis_C_Code:= false;
end;

constructor TScriptCommenter.Create(const SStart, SSEnd: integer);
begin
  Clear;
  SelStart := SStart;
  SelEnd := SSEnd;
end;

procedure TScriptCommenter.Process;
var
  b: Boolean;
  i, j, k: integer;
begin
  if selend < selstart then
  begin
    i := selstart;
    selstart := selend;
    selend := i;
  end;
  if SelStart = SelEnd then
  begin
    for i := Selstart downto 1 do
      if Lines.text[i] = #10 then
      begin
        j := i + 1;
        while lines.text[j] = ' ' do
          inc(j);
        if (lines.text[j] = '/') and (lines.text[j + 1] = '/') then
        begin
          Lines.text := copy(Lines.text, 1, j - 1) + copy(lines.text, j + 2, length(lines.text));
          exit;
        end
        else
        begin
          Lines.text := copy(Lines.text, 1, i) + '//' + copy(lines.text, i + 1, length(lines.text));
          exit;
        end;
      end;
  end
  else
  begin
    b := false;
    for i := SelStart to SelEnd do
      if lines.text[i] = #10 then
      begin
        b := True;
        break;
      end;
    if b then
    begin
      i := selstart;
      while lines.text[i] <> #10 do
        dec(i);
      inc(i);
      while lines.text[i] = ' ' do
        inc(i);
      if (lines.text[i] = '/') and (lines.text[i + 1] = '/') then
      begin
        i := SelEnd;
        j := selstart;
        while i >= j do
        begin
          if lines.text[i] = #10 then
          begin
            k := i + 1;
            while lines.text[k] = ' ' do
              inc(k);
            if (Lines.text[k] = '/') and (Lines.text[k + 1] = '/') then
              lines.text := copy(lines.text, 1, k - 1) + copy(lines.text, k + 2, length(lines.text));
          end;
          dec(i);
        end;
        while (lines.text[i] <> #10) and (i >= 1) do
          dec(i);
        k := i + 1;
        while lines.text[k] = ' ' do
          inc(k);
        if (Lines.text[k] = '/') and (Lines.text[k + 1] = '/') then
          lines.text := copy(lines.text, 1, k - 1) + copy(lines.text, k + 2, length(lines.text));
      end
      else
      begin
        i := SelEnd;
        j := selstart;
        while i >= j do
        begin
          if lines.text[i] = #10 then
            lines.text := copy(lines.text, 1, i) + '//' + copy(lines.text, i + 1, length(lines.text));
          dec(i);
        end;
        i := j;
        while (lines.text[i] <> #10) and (i >= 1) do
          dec(i);
        lines.text := copy(lines.text, 1, i) + '//' + copy(lines.text, i + 1, length(lines.text));
      end;
    end;
  end;
end;


end.

