{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  TRGB32Helper = record Helper for TRGB32
  public
    class function Create(const Red, Green, Blue: Byte): TRGB32; static; overload; inline;
    class function Create(const Red, Green, Blue, Alpha: Byte): TRGB32; static; overload; inline;

    function ToString: String;
    function Equals(const Other: TRGB32): Boolean; inline;
    function EqualsIgnoreAlpha(const Other: TRGB32): Boolean; inline;
  end;
{$ENDIF}

{$IFDEF BODY}
class function TRGB32Helper.Create(const Red, Green, Blue: Byte): TRGB32;
begin
  Result.B := Blue;
  Result.G := Green;
  Result.R := Red;
  Result.A := 0;
end;

class function TRGB32Helper.Create(const Red, Green, Blue, Alpha: Byte): TRGB32;
begin
  Result.B := Blue;
  Result.G := Green;
  Result.R := Red;
  Result.A := Alpha;
end;

function TRGB32Helper.ToString: String;
begin
  Result := '[' + IntToStr(B) + ',' + IntToStr(G) + ',' + IntToStr(R) + ',' + IntToStr(A) + ']';
end;

function TRGB32Helper.Equals(const Other: TRGB32): Boolean;
begin
  Result := AsInteger = Other.AsInteger;
end;

function TRGB32Helper.EqualsIgnoreAlpha(const Other: TRGB32): Boolean;
begin
  Result := (AsInteger and $FFFFFF) = (Other.AsInteger and $FFFFFF);
end;
{$ENDIF}

