unit internets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetPage(URL: String): String;

implementation
uses
  httpsend;

{ OTHER }
function GetPage(URL: String): String;
var
  s: TStringList;
begin
  s:=TStringList.Create;
  HttpGetText(URL, s);
  result := String(s.GetText);
  s.Free;
end;

end.

