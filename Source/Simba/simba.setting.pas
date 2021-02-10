unit simba.setting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, generics.collections;

type
  TSimbaSettingBase = class(TObject);
  TSimbaSettingList = specialize TObjectList<TSimbaSettingBase>;
  TSimbaSettingManager = class
  protected
    FIniFile: TIniFile;
    FSettings: TSimbaSettingList;
  public
    property INIFile: TIniFile read FIniFile;
    property Settings: TSimbaSettingList read FSettings;

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

  generic TSimbaSetting<_T> = class(TSimbaSettingBase)
  protected
  type
    TSelf = specialize TSimbaSetting<_T>;

    TChangeEvent = procedure(Value: _T) of object;
    TChangeHandlerList = specialize TList<TChangeEvent>;
  protected
    FIniFile: TINIFile;
    FSection: String;
    FName: String;
    FChangeHandlers: TChangeHandlerList;
    FDefaultValue: _T;

    function GetValue: _T; virtual; abstract;
    procedure SetValue(AValue: _T); virtual; abstract;
    procedure SetDefaultValue(AValue: _T); virtual;
  public
    property DefaultValue: _T read FDefaultValue write SetDefaultValue;
    property Value: _T read GetValue write SetValue;

    function AddOnChangeHandler(Handler: TChangeEvent): TSelf;
    procedure RemoveOnChangeHandler(Handler: TChangeEvent);

    procedure Changed;

    constructor Create(Manager: TSimbaSettingManager; Section, Name: String);
    destructor Destroy; override;
  end;

  TSimbaSetting_Int64 = class(specialize TSimbaSetting<Int64>)
  protected
    function GetValue: Int64; override;
    procedure SetValue(AValue: Int64); override;
  end;

  TSimbaSetting_Boolean = class(specialize TSimbaSetting<Boolean>)
  protected
    function GetValue: Boolean; override;
    procedure SetValue(AValue: Boolean); override;
  end;

  TSimbaSetting_String = class(specialize TSimbaSetting<String>)
  protected
    function GetValue: String; override;
    procedure SetValue(AValue: String); override;
  end;

  TSimbaSetting_CompressedString = class(TSimbaSetting_String)
  protected
    function GetValue: String; override;
    procedure SetValue(AValue: String); override;
  end;

  TSimbaSetting_File = class(TSimbaSetting_String)
  protected
    function GetValue: String; override;
  end;

implementation

uses
  simba.settings, simba.stringutil;

constructor TSimbaSettingManager.Create(FileName: String);
begin
  inherited Create();

  FIniFile := TIniFile.Create(FileName);
  FIniFile.CacheUpdates := True;

  FSettings := TSimbaSettingList.Create();
end;

destructor TSimbaSettingManager.Destroy;
begin;
  FIniFile.Free();
  FSettings.Free();

  inherited Destroy();
end;

function TSimbaSetting.AddOnChangeHandler(Handler: TChangeEvent): TSelf;
begin
  FChangeHandlers.Add(Handler);

  Result := Self;
end;

procedure TSimbaSetting.RemoveOnChangeHandler(Handler: TChangeEvent);
begin
  FChangeHandlers.Remove(Handler);
end;

constructor TSimbaSetting.Create(Manager: TSimbaSettingManager; Section, Name: String);
begin
  FChangeHandlers := TChangeHandlerList.Create();

  FIniFile := Manager.INIFile;
  FSection := Section;
  FName := Name;

  Manager.Settings.Add(Self);
end;

destructor TSimbaSetting.Destroy;
begin
  FChangeHandlers.Free();

  inherited Destroy();
end;

procedure TSimbaSetting.Changed;
var
  i: Int32;
begin
  for i := 0 to FChangeHandlers.Count - 1 do
    FChangeHandlers[i](Value);
end;

procedure TSimbaSetting.SetDefaultValue(AValue: _T);
begin
  FDefaultValue := AValue;
  if (not FIniFile.ValueExists(FSection, FName)) then
    Value := FDefaultValue;
end;

function TSimbaSetting_Boolean.GetValue: Boolean;
begin
  Result := FIniFile.ReadBool(FSection, FName, DefaultValue);
end;

procedure TSimbaSetting_Boolean.SetValue(AValue: Boolean);
begin
  FIniFile.WriteBool(FSection, FName, AValue);

  Changed();
end;

function TSimbaSetting_Int64.GetValue: Int64;
begin
  Result := FIniFile.ReadInt64(FSection, FName, DefaultValue);
end;

procedure TSimbaSetting_Int64.SetValue(AValue: Int64);
begin
  FIniFile.WriteInt64(FSection, FName, AValue);

  Changed();
end;

function TSimbaSetting_CompressedString.GetValue: String;
begin
  Result := FIniFile.ReadString(FSection, FName, DefaultValue);
  if Result <> DefaultValue then
    Result := DecompressString(Base64Decode(Result));
end;

procedure TSimbaSetting_CompressedString.SetValue(AValue: String);
begin
  FIniFile.WriteString(FSection, FName, Base64Encode(CompressString(AValue)));

  Changed();
end;

function TSimbaSetting_String.GetValue: String;
begin
  Result := FIniFile.ReadString(FSection, FName, DefaultValue);
end;

procedure TSimbaSetting_String.SetValue(AValue: String);
begin
  FIniFile.WriteString(FSection, FName, AValue);

  Changed();
end;

function TSimbaSetting_File.GetValue: String;
begin
  Result := inherited GetValue;

  if (FDefaultValue <> '') and (not FileExists(Result)) then
  begin
    Value := FDefaultValue;

    Result := inherited GetValue();
  end;
end;

end.

