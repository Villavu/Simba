{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Images for components.
  Images are stored as resouces added with "Project options > Resources"
}
unit simba.component_images;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics;

type
  TSimbaImages = class(TImageList)
  public
    SIMBA: Integer;
    DOCUMENT: Integer;
    FOLDER: Integer;
    FOLDER_RECENT: Integer;
    ARROW_RIGHT: Integer;
    ARROW_DOWN: Integer;
    ARROW_UP_GREEN: Integer;
    ARROW_DOWN_RED: Integer;
    TICK: Integer;
    CLOSE: Integer;
    CLOSE_ALL: Integer;
    COMPILE: Integer;
    PLAY: Integer;
    PAUSE: Integer;
    STOP: Integer;
    POWER: Integer;
    NEW: Integer;
    SAVE: Integer;
    SAVE_AS: Integer;
    SAVE_ALL: Integer;
    COLOR_PICKER: Integer;
    AREA_SELECTOR: Integer;
    TARGET_SELECTOR: Integer;
    ERASER: Integer;
    PACKAGE: Integer;
    SETTINGS: Integer;
    COLORS: Integer;
    SHAPES: Integer;
    COPY: Integer;
    CUT: Integer;
    PASTE: Integer;
    UNDO: Integer;
    REDO: Integer;
    UPPERCASE: Integer;
    LOWERCASE: Integer;
    SELECT_ALL: Integer;
    SELECT_LINE: Integer;
    SELECT_WORD: Integer;

    FIND: Integer;
    FIND_NEXT: Integer;
    FIND_PREV: Integer;
    FIND_REPLACE: Integer;
    FIND_FILES: Integer;
    PROP: Integer;
    METHOD: Integer;
    TYPE_DECL: Integer;
    VARIABLE: Integer;
    ENUM: Integer;
    ANCHOR: Integer;
    CONSTANT: Integer;
    EYE: Integer;
    WRITE_BUG: Integer;
    GITHUB: Integer;
    INFO: Integer;
    SECTION: Integer;
    BOOK: Integer;
  end;

var
  SimbaImages: TSimbaImages;

implementation

uses
  LCLType, ImgList, Forms,
  simba.initializations,
  simba.ide_utils;

type
  TImageListHelper = class helper for TImageList
    procedure DoWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
  end;

procedure TImageListHelper.DoWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := ImageWidthForDPI(Screen.PixelsPerInch);
end;

procedure CreateSimbaImages;

  function ImageFromResource(Name: String): TCustomBitmap;
  var
    Stream: TStream;
  begin
    Stream := TResourceStream.Create(HINSTANCE, Name, RT_RCDATA);
    try
      Result := TPortableNetworkGraphic.Create();
      Result.LoadFromStream(Stream);
    finally
      Stream.Free();
    end;
  end;

  function Add(const name: String): Integer;
  var
    bmp16, bmp24, bmp32: TCustomBitmap;
  begin
    bmp16 := ImageFromResource(name);
    bmp24 := ImageFromResource(name + '_150');
    bmp32 := ImageFromResource(name + '_200');

    Result := SimbaImages.AddMultipleResolutions([
      bmp16, bmp24, bmp32
    ]);

    bmp16.Free();
    bmp24.Free();
    bmp32.Free();
  end;

begin
  SimbaImages := TSimbaImages.Create(Application);
  SimbaImages.RegisterResolutions([16,24,32]);
  SimbaImages.OnGetWidthForPPI := @SimbaImages.DoWidthForPPI;

  SimbaImages.SIMBA         := Add('SIMBA');
  SimbaImages.DOCUMENT      := Add('DOCUMENT');
  SimbaImages.FOLDER        := Add('FOLDER');
  SimbaImages.FOLDER_RECENT := Add('FOLDER_RECENT');

  SimbaImages.ARROW_RIGHT     := Add('ARROW_RIGHT');
  SimbaImages.ARROW_DOWN      := Add('ARROW_DOWN');
  SimbaImages.ARROW_UP_GREEN  := Add('ARROW_UP_GREEN');
  SimbaImages.ARROW_DOWN_RED  := Add('ARROW_DOWN_RED');
  SimbaImages.TICK            := Add('TICK');
  SimbaImages.CLOSE           := Add('CLOSE');
  SimbaImages.CLOSE_ALL       := Add('CLOSE_ALL');
  SimbaImages.COMPILE         := Add('COMPILE');
  SimbaImages.PLAY            := Add('RUN');
  SimbaImages.PAUSE           := Add('PAUSE');
  SimbaImages.STOP            := Add('STOP');
  SimbaImages.POWER           := Add('POWER');
  SimbaImages.NEW             := Add('NEW');

  SimbaImages.SAVE            := Add('SAVE');
  SimbaImages.SAVE_AS         := Add('SAVE_AS');
  SimbaImages.SAVE_ALL        := Add('SAVE_ALL');
  SimbaImages.COLOR_PICKER    := Add('PIPETTE');
  SimbaImages.TARGET_SELECTOR := Add('TARGET');
  SimbaImages.AREA_SELECTOR   := Add('ALIGN');
  SimbaImages.ERASER          := Add('ERASER');
  SimbaImages.PACKAGE         := Add('PACKAGE');
  SimbaImages.SETTINGS        := Add('SETTINGS');
  SimbaImages.COLORS          := Add('COLOR');
  SimbaImages.SHAPES          := Add('SHAPE');

  SimbaImages.COPY            := Add('COPY');
  SimbaImages.CUT             := Add('CUT');
  SimbaImages.PASTE           := Add('PASTE');
  SimbaImages.UNDO            := Add('UNDO');
  SimbaImages.REDO            := Add('REDO');
  SimbaImages.UPPERCASE       := Add('UPPERCASE');
  SimbaImages.LOWERCASE       := Add('LOWERCASE');
  SimbaImages.SELECT_ALL      := Add('SELECT_ALL');
  SimbaImages.SELECT_LINE     := Add('SELECT_LINE');
  SimbaImages.SELECT_WORD     := Add('SELECT_WORD');

  SimbaImages.FIND            := Add('SEARCH_FIND');
  SimbaImages.FIND_NEXT       := Add('FIND_NEXT');
  SimbaImages.FIND_PREV       := Add('FIND_PREVIOUS');
  SimbaImages.FIND_FILES      := Add('FIND_IN_FILES');
  SimbaImages.FIND_REPLACE    := Add('SEARCH_REPLACE');

  SimbaImages.PROP      := Add('PROP');
  SimbaImages.METHOD    := Add('METHOD');
  SimbaImages.TYPE_DECL := Add('TYPE');
  SimbaImages.VARIABLE  := Add('VARIABLE');
  SimbaImages.ENUM      := Add('ENUM');
  SimbaImages.ANCHOR    := Add('ANCHOR');
  SimbaImages.CONSTANT  := Add('CONSTANT');

  SimbaImages.EYE       := Add('EYE');
  SimbaImages.WRITE_BUG := Add('BUG_WRITE');
  SimbaImages.GITHUB    := Add('MARK');
  SimbaImages.INFO      := Add('INFO');
  SimbaImages.SECTION   := Add('SECTION');
  SimbaImages.BOOK      := Add('BOOK');
end;

initialization
  CreateSimbaImages();

end.

