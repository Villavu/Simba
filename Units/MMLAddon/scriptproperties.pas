unit scriptproperties;

{$mode objfpc}

interface

uses
  Classes, SysUtils;
type

    { TScriptProperties }

    TSP_Property = (
      SP_WriteTimeStamp //Writes the time infront of a writeln
    );
    TSP_Properties = set of TSP_Property;


    TScriptProperties = class(TObject)
    private
      FProperties : TSP_Properties;
      FWriteTimeStamp : boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function GetProperty(name: ansistring; var Prop : TSP_Property): boolean;
      function GetProp(name: ansistring; var Value : ansistring) : Boolean;overload;
      function GetProp(Prop : TSP_Property; var Value : ansistring) : Boolean;overload;
      function SetProp(Name: ansistring; Value: ansistring): Boolean;overload;
      function SetProp(Prop : TSP_Property; Value: ansistring): Boolean;overload;
    public
      property WriteTimeStamp : boolean read FWriteTimeStamp;
    end;


implementation

constructor TScriptProperties.Create;
begin
  inherited;

  FWriteTimeStamp := False;
  FProperties := [];

  { set default values }
end;

destructor TScriptProperties.Destroy;
begin
  inherited Destroy;
end;

function TScriptProperties.GetProperty(name: ansistring; var Prop : TSP_Property): boolean;
const
  Names : array[TSP_Property] of ansistring = ('writetimestamp');
var
  i : integer;
begin
  Result := false;
  for i := 0 to length(names)- 1 do
    if lowercase(name) = Names[TSP_Property(i)] then
    begin
      Prop := (TSP_Property(i));
      Exit(true);
    end;
end;

function TScriptProperties.GetProp(name: ansistring; var Value: ansistring): Boolean;
var
  Prop : TSP_Property;
begin
  Result := false;
  if GetProperty(name,prop) then
    Result := (GetProp(Prop,value));
end;

function TScriptProperties.GetProp(Prop: TSP_Property; var Value: ansistring
  ): Boolean;
begin
  Result := true;
  case Prop of
    SP_WriteTimeStamp : Value := BoolToStr(Prop in FProperties,true);
  end;
end;

function TScriptProperties.SetProp(Name: ansistring; Value: ansistring): Boolean;
var
  Prop : TSP_Property;
begin
  Result := false;
  if GetProperty(name,prop) then
    Result := (SetProp(Prop,value));
end;

function TScriptProperties.SetProp(Prop: TSP_Property; Value: ansistring): Boolean;
begin
  case Prop of
    SP_WriteTimeStamp : if lowercase(value) = 'true' then
                     begin
                       FWriteTimeStamp:= True;
                       FProperties := FProperties + [Prop];
                     end else
                     begin
                       FWriteTimeStamp := False;
                       FProperties := FProperties - [Prop];
                     end;
  end;
end;

end.

