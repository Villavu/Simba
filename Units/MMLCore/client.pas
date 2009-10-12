{
	This file is part of the Mufasa Macro Library (MML)

	Copyright (c) 2009 by Raymond van Venentie and Merlijn Wajer

	See the file COPYING, included in this distribution,
	for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

}


unit Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes, Window, Input, Files, Finder, Bitmaps, dtm;

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
end;

destructor TClient.Destroy;
begin
  MDTM.Free;
  MBitmaps.Free;
  MFinder.Free;
  MFiles.Free;
  MInput.Free;
  MWindow.Free;



  inherited;
end;

end.

