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

procedure psWait(t: Integer);
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
    while (CurrThread.PSScript.Exec.Status = isRunning) and (GetTickCount < EndTime) do
      Sleep(16);
  end else
  begin
    sleep(t);
  end;
end;
{$endif}

procedure ClearDebug;
begin
  if Assigned(CurrThread.DebugMemo) then
    CurrThread.Synchronize(@CurrThread.DebugMemo.Clear);
end;

procedure SaveScreenshot(FileName: string);
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

function MinE(a, b: Extended): Extended;
begin;
  result := min(a,b);
end;

function IntToBox(x1,y1,x2,y2 : integer) : TBox;
begin;
  result.x1 := x1;
  result.y1 := y1;
  result.x2 := x2;
  result.y2 := y2;
end;

function Distance(x1, y1, x2, y2: Integer): Integer;
begin;
  Result := Round(Sqrt(Sqr(x2-x1) + Sqr(y2-y1)));
end;

procedure DisplayDebugImgWindow(w,h : integer);
begin;
  CurrThread.DebugImg.DispSize^ := Classes.Point(w,h);
  CurrThread.Synchronize( CurrThread.DebugImg.ShowForm);
end;

procedure DrawBitmapDebugImg(bmp : integer);
begin;
  CurrThread.DebugImg.ToDrawBitmap^ := CurrThread.Client.MBitmaps.Bmp[bmp];
  CurrThread.Synchronize(CurrThread.DebugImg.DrawBitmap);
end;

function GetDebugBitmap : integer;
begin;
  result := CurrThread.Client.MBitmaps.CreateBMP(0,0);
  CurrThread.DebugImg.GetDebugBitmap^ := CurrThread.Client.MBitmaps.Bmp[result];
  CurrThread.Synchronize(CurrThread.DebugImg.GetBitmap);
end;

procedure TerminateScript;
begin;
  CurrThread.PSScript.Stop;
end;
function GetTimeRunning: LongWord;
begin;
  result := GetTickCount - CurrThread.StartTime;
end;

procedure ConvertTime(Time : integer; var h,m,s : integer);
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

procedure HakunaMatata;
begin;
  OpenWebPage('http://www.youtube.com/v/ejEVczA8PLU&hl=en&fs=1&autoplay=1');
end;
