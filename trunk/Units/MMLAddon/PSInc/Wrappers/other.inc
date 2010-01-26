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

procedure psWait(t: Integer); ps_decl;
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

procedure ClearDebug; ps_decl;
begin
  if Assigned(CurrThread.DebugMemo) then
    CurrThread.Synchronize(@CurrThread.DebugMemo.Clear);
end;

procedure SaveScreenshot(FileName: string); ps_decl;
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



procedure DisplayDebugImgWindow(w,h : integer); ps_decl;
begin;
  CurrThread.DebugImg.DispSize^ := Classes.Point(w,h);
  CurrThread.Synchronize( CurrThread.DebugImg.ShowForm);
end;

procedure DrawBitmapDebugImg(bmp : integer); ps_decl;
begin;
  CurrThread.DebugImg.ToDrawBitmap^ := CurrThread.Client.MBitmaps[bmp];
  CurrThread.Synchronize(CurrThread.DebugImg.DrawBitmap);
end;

function GetDebugBitmap : integer; ps_decl;
begin;
  result := CurrThread.Client.MBitmaps.CreateBMP(0,0);
  CurrThread.DebugImg.GetDebugBitmap^ := CurrThread.Client.MBitmaps[result];
  CurrThread.Synchronize(CurrThread.DebugImg.GetBitmap);
end;

procedure TerminateScript; ps_decl;
begin;
  CurrThread.Terminate;
end;
function GetTimeRunning: LongWord; ps_decl;
begin;
  result := GetTickCount - CurrThread.StartTime;
end;

procedure ConvertTime(Time : integer; var h,m,s : integer); ps_decl;
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
procedure ps_DecodeDate ( const SourceDate  : TDateTime; out Year, Month, Day  : Word ); ps_decl;
begin
  decodedate(sourcedate,year,month,day);
end;
procedure ps_DecodeTime(DateTime : TDateTime; var Hour,Min,Sec,MSec : word); ps_decl;
begin
  decodetime(datetime,hour,min,sec,msec);
end;
function ps_Now : TDateTime; ps_decl;
begin
  result := now;
end;
function ps_Date : TDateTime; ps_decl;
begin
  result := date;
end;
procedure HakunaMatata; ps_decl;
begin;
  OpenWebPage('http://www.youtube.com/v/ejEVczA8PLU&hl=en&fs=1&autoplay=1');
end;
