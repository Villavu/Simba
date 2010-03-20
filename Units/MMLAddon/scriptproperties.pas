unit scriptproperties;

{$mode objfpc}

interface

uses
  Classes, SysUtils;
type
    TScriptProperties = class(TObject)
    public
            constructor Create;
            destructor Destroy; override;
            function SetProp(Name: String; Value: String): Boolean;
    public
            WriteTimeStamp: Boolean;
    end;

implementation

constructor TScriptProperties.Create;
begin
  inherited;

  WriteTimeStamp := False;

  { set default values }
end;

destructor TScriptProperties.Destroy;
begin
  inherited Destroy;
end;

function TScriptProperties.SetProp(Name: String; Value: String): Boolean;

begin
  {
    Fucking hell. We can't use a String in case statement, and we cannot define
    hash maps as constants, and we also cannot use variables in case statements.
  }
  Name := LowerCase(Name);
  if Name = 'writetimestamp' then
  begin
    WriteTimeStamp := LowerCase(Value) = 'true';
    Exit(True);
  end;
  {more if bla }
  Result := False;
end;

end.

