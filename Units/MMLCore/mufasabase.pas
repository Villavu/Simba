{
  This file is part of the Mufasa Macro Library (MML)
  Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

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

    MufasaBase for the Mufasa Macro Library
}

unit mufasabase;

{$mode objfpc}{$H+}

interface
{$undef mDebug}

uses
  Classes, SysUtils{$ifdef MSWindows},windows{$endif};


const
    SimbaVersion = 980;
    SimbaMajor = 980; // this should be 980 even if SimbaVersion is 981, etc

procedure mDebugLn( s : string);overload;
procedure mDebugLn( s : string; f : array of const);overload;
procedure InitmDebug;
procedure FreemDebug;
implementation

var
  CanDebug : boolean = false;

procedure mDebugLn(s: string);
begin
  if CanDebug then
    Writeln(s);
end;

procedure mDebugLn(s: string; f: array of const); overload;
begin
  mDebugLn(format(s,f));
end;

procedure InitmDebug;
begin
  CanDebug := true;
end;

procedure FreemDebug;
begin
  CanDebug := false;
end;

end.

