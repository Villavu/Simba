unit sm_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sm_settings;
//equal 2 strings
function Eq(aValue1, aValue2: string): boolean;
function SM_StrToDate(str: string): TDateTime;
function GetPackageUrl(s: string):string;
function GetScriptName(s: string):string;
//function convert path like {$SIMBA_SPS}/RUNESCAPE_OTHER
//to X:/Simba/Include/SPS/Img/RUNESCAPE_OTHER/
function ConvertPath(s: string; Opt: TOption): string;

const

ReservedWords : array[0..5] of string[20] = ('{$SIMBA}','{$SIMBA_SCRIPTS}','{$SIMBA_SPS}','{$SIMBA_SRL}','{$SIMBA_FONTS}','{$SIMBA_PLUGINS}');


implementation
function Eq(aValue1, aValue2: string): boolean;
//--------------------------------------------------------
begin
  Result := AnsiCompareText(Trim(aValue1),Trim(aValue2))=0;
end;
function SM_StrToDate(str: string): TDateTime;
var frmstg: TFormatSettings;
begin
  //GetLocaleFormatSettings(0, frmstg);
  frmstg.DateSeparator := '.';
  frmstg.ShortDateFormat := 'yyyy.mm.dd';
  frmstg.TimeSeparator := '.';
  frmstg.LongTimeFormat := 'hh.nn';

  if not TryStrToDateTime(str, Result, frmstg) then
    Result := Now();
end;
function GetPackageUrl(s: string): string;
var
  c: char;
  i, k: Integer;
begin
  k := 0;
  SetLength(Result, Length(s));
  for i := 0 to Length(s) - 1 do
  begin
    c := s[i + 1];
    if c in [ 'a'..'z', 'A'..'Z' ] then
    begin
      Inc(k);
      Result[k] := c;
    end;
  end;
  SetLength(Result, k);
  result:=lowercase(result+'.tar.bz2');
end;

function GetScriptName(s: string): string;
var
  c: char;
  i, k: Integer;
begin
  k := 0;
  SetLength(Result, Length(s));
  for i := 0 to Length(s) - 1 do
  begin
    c := s[i + 1];
    if c in [ 'a'..'z', 'A'..'Z' ] then
    begin
      Inc(k);
      Result[k] := c;
    end;
  end;
  SetLength(Result, k);
  result:=lowercase(result+'.simba');
end;

function ConvertPath(s: string; Opt: TOption): string;
  function GetPathType(path: string; var res: string): integer;  {very shitty code. Somebody, rewrote me please!!!}
    var
      i: integer;
      j: integer;
    begin
      result:=-1;
      res:='';
      for i:=0 to 5 do
        begin
            j:=pos(trim(reservedWords[i]),path);
             if j <> 0 then
              begin
              res:=path;
              delete(res,j,length(trim(reservedWords[i])));
              result:=i;
              end
         end;
      end;
var
  res: string;
  ptype: integer;
begin
  ptype:=GetPathType(s,res);
   if ptype > -1 then
     case ptype of
       0:res:=opt.Simba+res;
       1:res:=opt.Simba_Scripts+res;
       2:res:=opt.Simba_SPS+res;
       3:res:=opt.Simba_SRL+res;
       4:res:=opt.Simba_Fonts+res;
       5:res:=opt.Simba_Plugins+res;
     end;
   result:=res;
end;

end.

