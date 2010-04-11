{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Other.inc for the Mufasa Macro Library
}

function ps_SetScriptProp(prop : TSP_Property; Value: TVariantArray): boolean; extdecl;
begin
  Exit(CurrThread.Prop.SetProp(prop, Value));
end;

function ps_GetScriptProp(prop : TSP_Property; var Value : TVariantArray) : boolean; extdecl;
begin;
  exit(CurrThread.Prop.GetProp(prop,value));
end;

procedure ps_Wait(t: Integer); extdecl;
{$ifdef MSWINDOWS}
begin
  Sleep(t);
end;
{$else}
var
  EndTime : DWord;
begin
  if t > 50 then
  begin;
    EndTime := GetTickCount + t;
    while {(CurrThread.PSScript.Exec.Status = isRunning) and }(GetTickCount < EndTime) do
      Sleep(16);
  end else
  begin
    sleep(t);
  end;
end;
{$endif}

procedure ps_PlaySound( Sound : string); extdecl;
begin
  {$ifdef MSWINDOWS}
  sndPlaySound(PChar(sound),SND_ASYNC or SND_NODEFAULT);
  {$else}
  psWriteln(Format('Playing sound %s (not supported yet on Linux)',[sound]));
  {$endif}
end;

Procedure ps_StopSound; extdecl;
begin
  {$ifdef MSWINDOWS}
  sndPlaySoundW(nil,0);
  {$else}
  psWriteln('Stopping sound is not supported yet on Linux');
  {$endif}
end;

procedure ps_ClearDebug; extdecl;
begin
  if Assigned(CurrThread.DebugClear) then
    CurrThread.DebugClear();
end;

procedure ps_SetSupressExceptions(Supress : boolean);extdecl;
begin
  CurrThread.Client.MFinder.WarnOnly:= Supress;
end;

procedure ps_SaveScreenshot(FileName: string); extdecl;
var
  w,h : integer;
  bmp: TMufasaBitmap;
begin;
  CurrThread.Client.IOManager.GetDimensions(w,h);
  bmp := TMufasaBitmap.Create;
  bmp.CopyClientToBitmap(CurrThread.Client.IOManager,True,0,0,w-1,h-1);
  bmp.SaveToFile(FileName);
  bmp.Free;
end;



procedure ps_DisplayDebugImgWindow(w,h : integer); extdecl;
begin;
  CurrThread.DebugImg.DispSize^ := Classes.Point(w,h);
  CurrThread.Synchronize( CurrThread.DebugImg.ShowForm);
end;

procedure ps_DrawBitmapDebugImg(bmp : integer); extdecl;
begin;
  CurrThread.DebugImg.ToDrawBitmap^ := CurrThread.Client.MBitmaps[bmp];
  CurrThread.Synchronize(CurrThread.DebugImg.DrawBitmap);
end;

function ps_GetDebugBitmap : integer; extdecl;
begin;
  result := CurrThread.Client.MBitmaps.CreateBMP(0,0);
  CurrThread.DebugImg.GetDebugBitmap^ := CurrThread.Client.MBitmaps[result];
  CurrThread.Synchronize(CurrThread.DebugImg.GetBitmap);
end;

procedure ps_TerminateScript; extdecl;
begin;
  CurrThread.Terminate;
end;
function ps_GetTimeRunning: LongWord; extdecl;
begin;
  result := GetTickCount - CurrThread.StartTime;
end;

procedure ps_ConvertTime(Time : integer; var h,m,s : integer); extdecl;
var
  x : integer;
begin;
  x := time;
  h := x div(1000 * 60 * 60);
  x := x mod (1000 * 60 * 60);
  m := x div (1000 * 60);
  x := x mod (1000 * 60);
  s := x div (1000);
end;
procedure ps_DecodeDate ( const SourceDate  : TDateTime; out Year, Month, Day  : Word ); extdecl;
begin
  decodedate(sourcedate,year,month,day);
end;
procedure ps_DecodeTime(DateTime : TDateTime; var Hour,Min,Sec,MSec : word); extdecl;
begin
  decodetime(datetime,hour,min,sec,msec);
end;
function ps_Now : TDateTime; extdecl;
begin
  result := now;
end;
function ps_Date : TDateTime; extdecl;
begin
  result := date;
end;
function ps_GetTickCount : Longword; extdecl;
begin
  result := GetTickCount;
end;
procedure ps_HakunaMatata; extdecl;
begin;
  OpenWebPage('http://www.youtube.com/v/ejEVczA8PLU&hl=en&fs=1&autoplay=1');
end;

function ps_Random(Int: integer): integer; extdecl;
begin
  result := Random(int);
end;
