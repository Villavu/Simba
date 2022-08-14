{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  TDoubleMatrixHelper = type helper for TDoubleMatrix
  public
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
  end;

  TByteMatrixHelper = type helper for TByteMatrix
  public
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
  end;

  TBooleanMatrixHelper = type helper for TBooleanMatrix
  public
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
  end;
{$ENDIF}

{$IFDEF BODY}
function TDoubleMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TDoubleMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TDoubleMatrixHelper.Area: Integer;
begin
  Result := Width * Height;
end;

function TDoubleMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TDoubleMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

function TByteMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TByteMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TByteMatrixHelper.Area: Integer;
begin
  Result := Width * Height;
end;

function TByteMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TByteMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

function TBooleanMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TBooleanMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TBooleanMatrixHelper.Area: Integer;
begin
  Result := Width * Height;
end;

function TBooleanMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TBooleanMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;
{$ENDIF}




