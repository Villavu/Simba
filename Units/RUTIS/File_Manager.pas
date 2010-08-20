unit File_Manager;

interface

uses SysUtils;

Type
  TFileItem = Record
    Item : Pointer;
    Name : String;
    FName: String;
  End;

  TFileLoadFunc = Function(FileName: String; var Item: Pointer): Boolean;
  TFreeDataFunc = procedure(Item: Pointer);
  TObjFileLoadFunc = Function(FileName: String; var Item: Pointer): Boolean of Object;
  TObjFreeDataFunc = procedure(Item: Pointer) of Object;

  TFileManager = class
    constructor Create;
    Destructor Destroy; override;
  private
    Function SearchFileInPath(Name, Path: String): String;
    Function GetItem(Name: String): Pointer;
    Function IsFail(Name: String): Boolean;
  public
    fItems        : Array Of TFileItem;
    Paths         : Array Of String;
    FileExts      : Array Of String;
    NotFoundFiles : Array Of String;
    LoadFunc      : TFileLoadFunc;
    FreeFunc      : TFreeDataFunc;
    ObjLoadFunc   : TObjFileLoadFunc;
    ObjFreeFunc   : TObjFreeDataFunc;
    property  Items[Name: string]: Pointer read GetItem;
    Procedure Clear;
    Procedure AddExtension(Ext: String);
    Procedure AddPath(Path: String);
    Function  AddItem(Name: String): Boolean;
    Function  SearchFile(Name: String): String;
    Function  IndexOf(Name: String): Integer;
  end;


Implementation


//==============================================================================
//==================== File Procedures =========================================
//==============================================================================

constructor TFileManager.Create;
begin
  //AddPath(ExtractFileDir(ParamStr(0)));
end;

Destructor TFileManager.Destroy;
begin
  Clear;
end;

Procedure TFileManager.Clear;
var i: Integer;
begin
  If Assigned(FreeFunc) then
    for i := 0 to high(fItems) do
      FreeFunc(fItems[i].Item);
  If Assigned(ObjFreeFunc) then
    for i := 0 to high(fItems) do
      ObjFreeFunc(fItems[i].Item);
  SetLength(fItems, 0);
  SetLength(NotFoundFiles, 0);
end;

Function TFileManager.SearchFileInPath(Name, Path: String): String;
Var SR: TSearchRec;
    i: Integer;
Begin
  If FindFirst(Path + Name + '.*', faAnyFile, SR) = 0 Then
  Begin
    Repeat
      If (SR.Name <> '.') And (SR.Name <> '..') And Not ((sr.Attr And faDirectory) <> 0) Then
      Begin
        Result := lowerCase(ExtractFileExt(SR.Name));
        For i := 0 To high(FileExts) Do
          If Result = FileExts[i] Then
          Begin
            Result := Path + SR.Name;
            exit;
          End;
      End;
    Until FindNext(SR) <> 0;
    FindClose(SR);
  End;
  Result := '';
End;

Function TFileManager.SearchFile(Name: String): String;
Var i: Integer;
Begin
  For i := 0 To high(Paths) Do
  Begin
    If length(Paths[i]) = 0 then Continue;
    If Paths[i][length(Paths[i])] <> '\' Then Paths[i] := Paths[i] + '\';
    Result := SearchFileInPath(Name, Paths[i]);
    If Result <> '' Then exit;
  End;
End;

Procedure TFileManager.AddPath(Path: String);
Begin
  setLength(Paths, length(Paths) + 1);
  Paths[high(Paths)] := Path;
End;

Procedure TFileManager.AddExtension(Ext: string);
begin
  If Ext[1] <> '.' then
    Ext := '.' + Ext; 
  SetLength(FileExts,Length(FileExts)+1);
  FileExts[high(FileExts)] := LowerCase(Ext);
end;

Function TFileManager.IndexOf(Name: String): Integer;
Var i: Integer;
Begin
  Name := LowerCase(Name);
  Result := -1;
  For i := 0 To high(fItems) Do
    If fItems[i].Name = Name Then
    Begin
      Result := i;
      exit;
    End;
End;

Function TFileManager.IsFail(Name: String): Boolean;
Var i: Integer;
Begin
  Result := true;
  For i := 0 To length(NotFoundFiles) - 1 Do
    If NotFoundFiles[i] = Name Then exit;
  Result := false;
End;

Function TFileManager.AddItem(Name: String): Boolean;
Var fn: String;
Begin
  Name := LowerCase(Name);
  Result := false;
  If (Assigned(LoadFunc) or Assigned(ObjLoadFunc)) and not
     (Assigned(FreeFunc) or Assigned(ObjFreeFunc)) then exit;
  If IsFail(Name) Then exit;
  fn := SearchFile(Name);
  If fn = '' Then
  Begin
    setLength(NotFoundFiles, length(NotFoundFiles) + 1);
    NotFoundFiles[high(NotFoundFiles)] := Name;
    exit;
  End;
  setLength(fItems, length(fItems) + 1);
  fItems[high(fItems)].Name := Name;
  fItems[high(fItems)].FName := FN;
  If Assigned(LoadFunc) then
  begin
    If Not LoadFunc(fn, fItems[high(fItems)].Item) Then
    Begin
      setLength(fItems, length(fItems)-1);
      setLength(NotFoundFiles, length(NotFoundFiles) + 1);
      NotFoundFiles[high(NotFoundFiles)] := Name;
      exit;
    End;
  end
  else
    If Assigned(ObjLoadFunc) then
      If Not ObjLoadFunc(fn, fItems[high(fItems)].Item) Then
      Begin
        setLength(fItems, length(fItems)-1);
        setLength(NotFoundFiles, length(NotFoundFiles) + 1);
        NotFoundFiles[high(NotFoundFiles)] := Name;
        exit;
      End;
  Result := true;
End;

Function TFileManager.GetItem(Name: String): Pointer;
Var index: Integer;
Begin
  Result := nil;
  If (Assigned(LoadFunc) or Assigned(ObjLoadFunc)) and not
     (Assigned(FreeFunc) or Assigned(ObjFreeFunc)) then exit;
  Name := LowerCase(Name);
  index := IndexOf(Name);
  If Index>=0 then
  begin
    Result := fItems[index].Item;
    exit;
  end;
  If not AddItem(Name) Then exit;
  index := high(fItems);
  Result := fItems[index].Item;
End;

End.
