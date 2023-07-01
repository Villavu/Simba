{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  MACRO_HELPER_NAME = type helper for MACRO_MATRIX_NAME
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
  end;
{$ENDIF}

{$IFDEF BODY}
function MACRO_HELPER_NAME.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function MACRO_HELPER_NAME.Height: Integer;
begin
  Result := Length(Self);
end;

function MACRO_HELPER_NAME.Area: Integer;
begin
  Result := Width * Height;
end;

function MACRO_HELPER_NAME.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure MACRO_HELPER_NAME.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;
{$ENDIF}
