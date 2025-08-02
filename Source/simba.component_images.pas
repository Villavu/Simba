{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Images for our components.
  Must be here so the components work in a script processes since SimbaMainForm.Images isn't available

  Images are stored in Project options > Resources
}
unit simba.component_images;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, ImgList, Types, Forms;

type
  TSimbaImageComponents = class(TImageList)
  public
    ARROW_RIGHT: Integer;
    ARROW_DOWN: Integer;
    TICK: Integer;
  end;

var
  SimbaComponentImages: TSimbaImageComponents;

implementation

uses
  simba.initializations, simba.ide_utils;

type
  TImageListHelper = class helper for TImageList
    procedure DoWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
  end;

procedure TImageListHelper.DoWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := ImageWidthForDPI(Screen.PixelsPerInch);
end;

procedure CreateSimbaComponentImages;

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

  function Add(img16, img24, img32: String): Integer;
  var
    bmp16, bmp24, bmp32: TCustomBitmap;
  begin
    bmp16 := ImageFromResource(img16);
    bmp24 := ImageFromResource(img24);
    bmp32 := ImageFromResource(img32);

    Result := SimbaComponentImages.AddMultipleResolutions([
      bmp16, bmp24, bmp32
    ]);

    bmp16.Free();
    bmp24.Free();
    bmp32.Free();
  end;

begin
  SimbaComponentImages := TSimbaImageComponents.Create(Application);
  SimbaComponentImages.RegisterResolutions([16,24,32]);
  SimbaComponentImages.OnGetWidthForPPI := @SimbaComponentImages.DoWidthForPPI;

  SimbaComponentImages.ARROW_RIGHT := Add('ARROW_RIGHT', 'ARROW_RIGHT_150', 'ARROW_RIGHT_200');
  SimbaComponentImages.ARROW_DOWN  := Add('ARROW_DOWN', 'ARROW_DOWN_150', 'ARROW_DOWN_200');
  SimbaComponentImages.TICK        := Add('TICK', 'TICK_150', 'TICK_200');
end;

initialization
  CreateSimbaComponentImages();

end.

