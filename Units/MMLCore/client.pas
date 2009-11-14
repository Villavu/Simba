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

    Client class for the Mufasa Macro Library
}


unit Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes,
  Window, Input, Files, Finder, Bitmaps, dtm, ocr;

{
TClient is a full-blown instance of the MML.
It binds all the components together.
}

type
    TClient = class(TObject)
        constructor Create;
        destructor Destroy; override;

        public
            MWindow: TMWindow;
            MInput: TMInput;
            MFiles: TMFiles;
            MFinder: TMFinder;
            MBitmaps : TMBitmaps;
            MDTM: TMDTM;
            MOCR: TMOCR;

    end;

implementation

// Possibly pass arguments to a default window.
constructor TClient.Create;
begin
  inherited Create;

  MWindow := TMWindow.Create;
  MInput := TMInput.Create(Self);
  MFiles := TMFiles.Create;
  MFinder := TMFinder.Create(Self);
  MBitmaps := TMBitmaps.Create(self);
  MDTM := TMDTM.Create(self);
  MOCR := TMOCR.Create(self);
end;

destructor TClient.Destroy;
begin
  MOCR.Free;
  MDTM.Free;
  MBitmaps.Free;
  MFinder.Free;
  MFiles.Free;
  MInput.Free;
  MWindow.Free;

  inherited;
end;

end.

