unit extensionmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,virtualextension,psextension,mufasabase,mufasatypes;

type
    TExtension = TVirtualSimbaExtension;
    (**
      TExtensionManager holds a list of TExtension, and
      has functions to easily handle hooks.
    *)

    { TExtensionManager }

    TExtensionManager = class(TObject)
    private
      FOnChange: TNotifyEvent;
      procedure SetOnchange(const AValue: TNotifyEvent);
    public
      constructor Create;
      destructor Destroy; override;
    public
      Extensions: TList;
      StartDisabled : boolean;
      property OnChange : TNotifyEvent read FOnChange write SetOnchange;
      function GetExtensionIndex(Filename : string) : integer;
      function LoadPSExtension(Filename : string; enabled : boolean=false) : boolean;
      function LoadPSExtensionsDir(Directory,ext : string) : boolean;
      function HandleHook(const HookName: String; var Args: TVariantArray): Variant;
    end;

var
  ExtManager : TExtensionManager;

implementation
uses
  TestUnit, settingssandbox,simbasettings;

procedure TExtensionManager.SetOnchange(const AValue: TNotifyEvent);
var
  i : integer;
begin
  for i := 0 to Extensions.Count - 1 do
    TExtension(Extensions[i]).OnChange := AValue;;
  FOnChange:=AValue;
end;

constructor TExtensionManager.Create;
begin
  inherited Create;
  Extensions := TList.Create;
  StartDisabled := True;
end;

destructor TExtensionManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Extensions.Count - 1 do
    TExtension(Extensions.Items[i]).Free;
  Extensions.Free;
  inherited Destroy;
end;

function TExtensionManager.GetExtensionIndex(Filename: string): integer;
var
  i : integer;
begin
  for i := 0 to Extensions.Count - 1 do
    if CompareText(TExtension(Extensions[i]).Filename,filename) = 0 then
      exit(i);
  result := -1;
end;

function TExtensionManager.LoadPSExtension(Filename: string; enabled: boolean): boolean;
var
  Ext : TExtension;
begin
  if GetExtensionIndex(filename) <> -1 then
    exit(true);
  Result := False;
  try
    Ext := TSimbaPSExtension.Create(filename,True);
//    result := TSimbaPSExtension(ext).Working;
    Extensions.Add(ext);
    ext.Settings := TMMLSettingsSandbox.Create(SettingsForm.Settings);
    ext.Settings.Prefix := format('Extensions/Extension%d/Settings/',[Extensions.Count - 1]);
    if enabled then
      ext.Enabled := true;
    ext.OnChange:= FOnChange;
    if assigned(FOnChange) then
      FOnChange(Self);
    Result := True;
  except
    on e : exception do
      formWritelnex(format('Error in LoadPSExtension(%s): %s',[FileName, e.message]));
  end;
end;

function GetFiles(Path, Ext: string): TstringArray;
var
  SearchRec : TSearchRec;
  c : integer;
begin
  c := 0;
  if FindFirst(Path + '*.' + ext, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      inc(c);
      SetLength(Result,c);
      Result[c-1] := SearchRec.Name;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function TExtensionManager.LoadPSExtensionsDir(Directory, ext: string): boolean;
var
  Files : TstringArray;
  i : integer;
  tempevent : TNotifyEvent;
begin
  result := false;
  if not DirectoryExists(directory) then
    exit;
  tempevent := FOnChange;
  FOnChange := nil;
  Directory := IncludeTrailingPathDelimiter(directory);
  Files := GetFiles(Directory,ext);
  for i := 0 to high(Files) do
    result := LoadPSExtension(Directory + files[i],not StartDisabled) or result;
  FOnChange := Tempevent;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

// How do we return more than one result?
function TExtensionManager.HandleHook(const HookName: String;var Args: TVariantArray): Variant;
var
  i: Integer;
begin
  for i := 0 to Extensions.Count -1 do
    with TExtension(Extensions[i]) do
      if Enabled then
        if HookExists(HookName) then
          if ExecuteHook(HookName, Args, Result) <> 0 then
          begin
            mDebugLn('Execute hook failed: Hookname: %s',[hookname]);
            // Not succesfull.
          end;
end;

initialization
  ExtManager := TExtensionManager.Create;
  ExtManager.StartDisabled := True;
finalization
  if ExtManager <> nil then
    FreeAndNil(ExtManager);

end.

