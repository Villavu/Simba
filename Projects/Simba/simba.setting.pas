unit simba.setting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles,
  simba.generics;

type
  TSimbaSetting = class
  protected
    FINI: TINIFile;
    FSection: String;
    FName: String;
  public
    procedure Changed; virtual; abstract;

    constructor Create(INI: TINIFile; Section: String; Name: String); virtual;
  end;

  { TSimbaSetting_Generic }

  generic TSimbaSetting_Generic<_T> = class(TSimbaSetting)
  public type
    TChangeEvent = procedure(Value: _T) of object;
    TChangeHandlerList = specialize TSimbaList<TChangeEvent>;
  protected
    FChangeHandlers: TChangeHandlerList;
    FDefaultValue: _T;

    function GetValue: _T; virtual; abstract;
    procedure SetValue(AValue: _T); virtual; abstract;
    procedure SetDefaultValue(AValue: _T); virtual;
  public
    property DefaultValue: _T read FDefaultValue write SetDefaultValue;
    property Value: _T read GetValue write SetValue;

    procedure AddHandlerOnChange(Handler: TChangeEvent; CallHandler: Boolean = True);
    procedure RemoveHandlerOnChange(Handler: TChangeEvent);

    procedure Changed; override;

    constructor Create(INI: TINIFile; Section: String; Name: String); override;
    destructor Destroy; override;
  end;

  TSimbaSetting_Int64 = class(specialize TSimbaSetting_Generic<Int64>)
  protected
    function GetValue: Int64; override;
    procedure SetValue(AValue: Int64); override;
  end;

  TSimbaSetting_Boolean = class(specialize TSimbaSetting_Generic<Boolean>)
  protected
    function GetValue: Boolean; override;
    procedure SetValue(AValue: Boolean); override;
  end;

  TSimbaSetting_String = class(specialize TSimbaSetting_Generic<String>)
  protected
    function GetValue: String; override;
    procedure SetValue(AValue: String); override;
  end;

  TSimbaSetting_CompressedString = class(TSimbaSetting_String)
  protected
    function GetValue: String; override;
    procedure SetValue(AValue: String); override;
  end;

  TSimbaSetting_Directory = class(TSimbaSetting_String)
  protected
    procedure SetValue(AValue: String); override;
    function GetValue: String; override;
  end;

  TSimbaSetting_File = class(TSimbaSetting_String)
  protected
    function GetValue: String; override;
  end;

  TSimbaSettingsList = specialize TSimbaObjectList<TSimbaSetting>;

implementation

uses
  simba.settings, simba.stringutil;

constructor TSimbaSetting.Create(INI: TINIFile; Section: String; Name: String);
begin
  FINI := INI;
  FSection := Section;
  FName := Name;
end;

procedure TSimbaSetting_Generic.Changed;
var
  i: Int32;
begin
  for i := 0 to FChangeHandlers.Count - 1 do
    FChangeHandlers[i](Value);
end;

procedure TSimbaSetting_Generic.SetDefaultValue(AValue: _T);
begin
  FDefaultValue := AValue;
  if (not FINI.ValueExists(FSection, FName)) then
    Value := FDefaultValue;
end;

procedure TSimbaSetting_Generic.AddHandlerOnChange(Handler: TChangeEvent; CallHandler: Boolean);
begin
  FChangeHandlers.Add(Handler);
  if CallHandler then
    Handler(Value);
end;

procedure TSimbaSetting_Generic.RemoveHandlerOnChange(Handler: TChangeEvent);
begin
  FChangeHandlers.DeleteItem(Handler);
end;

constructor TSimbaSetting_Generic.Create(INI: TINIFile; Section: String; Name: String);
begin
  inherited Create(INI, Section, Name);

  FChangeHandlers := TChangeHandlerList.Create();
end;

destructor TSimbaSetting_Generic.Destroy;
begin
  FChangeHandlers.Free();

  inherited Destroy();
end;

function TSimbaSetting_Boolean.GetValue: Boolean;
begin
  Result := FINI.ReadBool(FSection, FName, DefaultValue);
end;

procedure TSimbaSetting_Boolean.SetValue(AValue: Boolean);
begin
  FINI.WriteBool(FSection, FName, AValue);

  Changed();
end;

function TSimbaSetting_Int64.GetValue: Int64;
begin
  Result := FINI.ReadInt64(FSection, FName, DefaultValue);
end;

procedure TSimbaSetting_Int64.SetValue(AValue: Int64);
begin
  FINI.WriteInt64(FSection, FName, AValue);

  Changed();
end;

function TSimbaSetting_CompressedString.GetValue: String;
begin
  Result := FINI.ReadString(FSection, FName, DefaultValue);
  if Result <> DefaultValue then
    Result := DecompressString(Base64Decode(Result));
end;

procedure TSimbaSetting_CompressedString.SetValue(AValue: String);
begin
  FINI.WriteString(FSection, FName, Base64Encode(CompressString(AValue)));

  Changed();
end;

function TSimbaSetting_String.GetValue: String;
begin
  Result := FINI.ReadString(FSection, FName, DefaultValue);
end;

procedure TSimbaSetting_String.SetValue(AValue: String);
begin
  FINI.WriteString(FSection, FName, AValue);

  Changed();
end;

procedure TSimbaSetting_Directory.SetValue(AValue: String);
begin
  inherited SetValue(AValue);

  ForceDirectories(AValue);
end;

function TSimbaSetting_Directory.GetValue: String;
begin
  Result := inherited GetValue;

  if (FDefaultValue <> '') and (not DirectoryExists(Result)) then
  begin
    Value := FDefaultValue;

    Result := inherited GetValue();
  end;
end;

function TSimbaSetting_File.GetValue: _T;
begin
  Result := inherited GetValue;

  if (FDefaultValue <> '') and (not FileExists(Result)) then
  begin
    Value := FDefaultValue;

    Result := inherited GetValue();
  end;
end;

end.

