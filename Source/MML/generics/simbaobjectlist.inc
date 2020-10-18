{%MainUnit simba.generics}

procedure TSimbaObjectList.Clear;
var
  i: Int32;
begin
 if FManageObjects then
   for i := 0 to FCount - 1 do
     TObject(FItems[i]).Free();

  inherited Clear;
end;

function TSimbaObjectList.Delete(Index: Integer): _T;
begin
  Result := inherited Delete(Index);

  if FManageObjects then
    TObject(Result).Free();
end;

constructor TSimbaObjectList.Create(ManageObjects: Boolean);
begin
  inherited Create(TObject(nil), dupAccept, False);

  FManageObjects := ManageObjects;
end;

destructor TSimbaObjectList.Destroy;
begin
  if FManageObjects then
     Clear();

  inherited Destroy;
end;
