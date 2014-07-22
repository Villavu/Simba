unit lplclgraphics;

{$mode objfpc}{$H+}
{$I Simba.inc}
interface

uses
  Classes, SysUtils,Graphics, lpcompiler, lptypes, lpClassHelper;

type
  PStream = ^TStream;
  PHandle = ^THandle;
  PNotifyEvent = ^TNotifyEvent;
  PGraphicsObject = ^TGraphicsObject;
  //TFont
  PFontStyles = ^TFontStyles;
  PFont = ^TFont;
  PFontPitch = ^TFontPitch;
  PCopyMode = ^TCopyMode;
  PFontQuality = ^TFontQuality;
  //TPen
  PPen = ^TPen;
  PPenStyle = ^TPenStyle;
  PPenMode = ^TPenMode;
  //TBrush
  PBrush = ^TBrush;
  PBrushStyle = ^TBrushStyle;
  //TCanvas
  PCanvas = ^TCanvas;
  PFillStyle = ^TFillStyle;
  PRect = ^TRect;
  PPoint = ^TPoint;
  PPPoint = ^PPoint;
  //TGraphics
   PGraphic = ^TGraphic;
  //TBitmap
  PBitmap = ^Tbitmap;
  PTransparentMode =^TTransparentMode;
  //TPicture
  PPicture = ^TPicture;

procedure RegisterLCLGraphics(Compiler: TLapeCompiler);

implementation

uses
  MufasaTypes,LCLType,lplclsystem, stringutil;

type
  PHbitmap = ^HBitmap;
  PHPalette = ^HPalette;

  {TGraphicsObject}
//Read: property OnChanging: TNotifyEvent read OnChanging write OnChanging;
procedure TGraphicsObject_OnChanging_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PGraphicsObject(Params^[0])^.OnChanging;
end;

//Write: property OnChanging: TNotifyEvent read OnChanging write OnChanging;
procedure TGraphicsObject_OnChanging_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphicsObject(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TGraphicsObject_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PGraphicsObject(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TGraphicsObject_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphicsObject(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure TGraphicsObject_Init(const Params: PParamArray); lape_extdecl
begin
  PGraphicsObject(Params^[0])^ := TGraphicsObject.Create();
end;

//procedure Free();
procedure TGraphicsObject_Free(const Params: PParamArray); lape_extdecl
begin
  PGraphicsObject(Params^[0])^.Free();
end;

procedure Register_TGraphicsObject(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGraphicsObject', 'TPersistent');

    addClassVar('TGraphicsObject', 'OnChanging', 'TNotifyEvent', @TGraphicsObject_OnChanging_Read, @TGraphicsObject_OnChanging_Write);
    addClassVar('TGraphicsObject', 'OnChange', 'TNotifyEvent', @TGraphicsObject_OnChange_Read, @TGraphicsObject_OnChange_Write);
    addGlobalFunc('procedure TGraphicsObject.Init();', @TGraphicsObject_Init);
    addGlobalFunc('procedure TGraphicsObject.Free();', @TGraphicsObject_Free);
  end;
end;

{TFont}
//constructor Create;
procedure TFont_Init(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^ := TFont.Create();
end;

//procedure Assign(Source: TPersistent);
procedure TFont_Assign(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//procedure BeginUpdate;
procedure TFont_BeginUpdate(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.BeginUpdate();
end;

//procedure EndUpdate;
procedure TFont_EndUpdate(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.EndUpdate();
end;

//function HandleAllocated: boolean;
procedure TFont_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PFont(Params^[0])^.HandleAllocated();
end;

//Read: property Handle: THandle read Handle write Handle;
procedure TFont_Handle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PFont(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read Handle write Handle;
procedure TFont_Handle_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//function IsDefault: boolean;
procedure TFont_IsDefault(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PFont(Params^[0])^.IsDefault();
end;

//function IsEqual(AFont: TFont): boolean;
procedure TFont_IsEqual(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PFont(Params^[0])^.IsEqual(PFont(Params^[1])^);
end;

//Read: property IsMonoSpace: boolean read GetIsMonoSpace;
procedure TFont_IsMonoSpace_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PFont(Params^[0])^.IsMonoSpace;
end;

//procedure SetDefault;
procedure TFont_SetDefault(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.SetDefault();
end;

//Read: property PixelsPerInch: Integer read PixelsPerInch write PixelsPerInch;
procedure TFont_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PFont(Params^[0])^.PixelsPerInch;
end;

//Write: property PixelsPerInch: Integer read PixelsPerInch write PixelsPerInch;
procedure TFont_PixelsPerInch_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.PixelsPerInch := PInteger(Params^[1])^;
end;

//Read: property Color: TColor read Color write Color;
procedure TFont_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PFont(Params^[0])^.Color;
end;

//Write: property Color: TColor read Color write Color;
procedure TFont_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Color := PColor(Params^[1])^;
end;

//Read: property Height: Integer read Height write Height;
procedure TFont_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PFont(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure TFont_Height_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Name: string read Name write Name;
procedure TFont_Name_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PFont(Params^[0])^.Name;
end;

//Write: property Name: string read Name write Name;
procedure TFont_Name_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Name := PlpString(Params^[1])^;
end;

//Read: property Orientation: Integer read Orientation write Orientation;
procedure TFont_Orientation_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PFont(Params^[0])^.Orientation;
end;

//Write: property Orientation: Integer read Orientation write Orientation;
procedure TFont_Orientation_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Orientation := PInteger(Params^[1])^;
end;

//Read: property Pitch: Byte read Pitch write Pitch;
procedure TFont_Pitch_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFontPitch(Result)^ := PFont(Params^[0])^.Pitch;
end;

//Write: property Pitch: Byte read Pitch write Pitch;
procedure TFont_Pitch_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Pitch := PFontPitch(Params^[1])^;
end;

//Read: property Size: Integer read Size write Size;
procedure TFont_Size_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PFont(Params^[0])^.Size;
end;

//Write: property Size: Integer read Size write Size;
procedure TFont_Size_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Size := PInteger(Params^[1])^;
end;

//Read: property Style: TFontStyles read Style write Style;
procedure TFont_Style_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFontStyles(Result)^ := PFont(Params^[0])^.Style;
end;

//Write: property Style: TFontStyles read Style write Style;
procedure TFont_Style_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Style := PFontStyles(Params^[1])^;
end;

//Read Quality: TFontQuality read FQuality write SetQuality default fqDefault;
procedure TFont_Quality_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFontQuality(Result)^ := PFont(Params^[0])^.Quality;
end;

//Write: property Quality: TFontQuality read FQuality write SetQuality default fqDefault;
procedure TFont_Quality_Write(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Quality := PFontQuality(Params^[1])^;
end;

//procedure Free();
procedure TFont_Free(const Params: PParamArray); lape_extdecl
begin
  PFont(Params^[0])^.Free();
end;

procedure Register_TFont(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TFont', 'TGraphicsObject');

    addGlobalFunc('procedure TFont.Init();', @TFont_Init);
    addGlobalFunc('procedure TFont.Assign(Source: TPersistent);', @TFont_Assign);
    addGlobalFunc('procedure TFont.BeginUpdate();', @TFont_BeginUpdate);
    addGlobalFunc('procedure TFont.EndUpdate();', @TFont_EndUpdate);
    addGlobalFunc('function TFont.HandleAllocated(): boolean;', @TFont_HandleAllocated);
    addClassVar('TFont', 'Handle', 'THandle', @TFont_Handle_Read, @TFont_Handle_Write);
    addGlobalFunc('function TFont.IsDefault(): boolean;', @TFont_IsDefault);
    addGlobalFunc('function TFont.IsEqual(AFont: TFont): boolean;', @TFont_IsEqual);
    addClassVar('TFont', 'IsMonoSpace', 'boolean', @TFont_IsMonoSpace_Read);
    addGlobalFunc('procedure TFont.SetDefault();', @TFont_SetDefault);
    addClassVar('TFont', 'PixelsPerInch', 'Integer', @TFont_PixelsPerInch_Read, @TFont_PixelsPerInch_Write);
    addClassVar('TFont', 'Color', 'TColor', @TFont_Color_Read, @TFont_Color_Write);
    addClassVar('TFont', 'Height', 'Integer', @TFont_Height_Read, @TFont_Height_Write);
    addClassVar('TFont', 'Name', 'string', @TFont_Name_Read, @TFont_Name_Write);
    addClassVar('TFont', 'Orientation', 'Integer', @TFont_Orientation_Read, @TFont_Orientation_Write);
    addClassVar('TFont', 'Pitch', 'TFontPitch', @TFont_Pitch_Read, @TFont_Pitch_Write);
    addClassVar('TFont', 'Size', 'Integer', @TFont_Size_Read, @TFont_Size_Write);
    addClassVar('TFont', 'Style', 'TFontStyles', @TFont_Style_Read, @TFont_Style_Write);
    addClassVar('TFont', 'Quality', 'TFontQuality', @TFont_Quality_Read, @TFont_Quality_Write);
    addGlobalFunc('procedure TFont.Free();', @TFont_Free);
  end;
end;

{TPen}
//constructor Create;
procedure TPen_Init(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^ := TPen.Create();
end;

//procedure Assign(Source: TPersistent);
procedure TPen_Assign(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//Read: property Handle: THandle read Handle write Handle;
procedure TPen_Handle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PPen(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read Handle write Handle;
procedure TPen_Handle_Write(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//Read: property Color: Integer read Color write Color ;
procedure TPen_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PPen(Params^[0])^.Color;
end;

//Write: property Color: Integer read Color write Color ;
procedure TPen_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Color := PInteger(Params^[1])^;
end;

//Read: property Cosmetic: Boolean read Cosmetic write Cosmetic;
procedure TPen_Cosmetic_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PPen(Params^[0])^.Cosmetic;
end;

//Write: property Cosmetic: Boolean read Cosmetic write Cosmetic;
procedure TPen_Cosmetic_Write(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Cosmetic := PBoolean(Params^[1])^;
end;

//Read: property Mode: TPenMode read Mode Write Mode;
procedure TPen_Mode_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPenMode(Result)^ := PPen(Params^[0])^.Mode;
end;

//Write: property Mode: TPenMode read Mode Write Mode;
procedure TPen_Mode_Write(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Mode := PPenMode(Params^[1])^;
end;

//Read: property Style: TPenStyle read Style Write Style;
procedure TPen_Style_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPenStyle(Result)^ := PPen(Params^[0])^.Style;
end;

//Write: property Style: TPenStyle read Style Write Style;
procedure TPen_Style_Write(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Style := PPenStyle(Params^[1])^;
end;

//Read: property Width: integer read Width write Width;
procedure TPen_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PPen(Params^[0])^.Width;
end;

//Write: property Width: integer read Width write Width;
procedure TPen_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

//procedure Free();
procedure TPen_Free(const Params: PParamArray); lape_extdecl
begin
  PPen(Params^[0])^.Free();
end;

procedure Register_TPen(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TPen', 'TGraphicsObject');

    addGlobalFunc('procedure TPen.Init();', @TPen_Init);
    addGlobalFunc('procedure TPen.Assign(Source: TPersistent);', @TPen_Assign);
    addClassVar('TPen', 'Handle', 'THandle', @TPen_Handle_Read, @TPen_Handle_Write);
    addClassVar('TPen', 'Color', 'Integer', @TPen_Color_Read, @TPen_Color_Write);
    addClassVar('TPen', 'Cosmetic', 'Boolean', @TPen_Cosmetic_Read, @TPen_Cosmetic_Write);
    addClassVar('TPen', 'Mode', 'TPenMode', @TPen_Mode_Read, @TPen_Mode_Write);
    addClassVar('TPen', 'Style', 'TPenStyle', @TPen_Style_Read, @TPen_Style_Write);
    addClassVar('TPen', 'Width', 'integer', @TPen_Width_Read, @TPen_Width_Write);
    addGlobalFunc('procedure TPen.Free();', @TPen_Free);
  end;
end;

{TBrush}
//procedure Assign(Source: TPersistent);
procedure TBrush_Assign(const Params: PParamArray); lape_extdecl
begin
  PBrush(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//constructor Create;
procedure TBrush_Init(const Params: PParamArray); lape_extdecl
begin
  PBrush(Params^[0])^ := TBrush.Create();
end;

//Read: property Color: Integer read Color write Color;
procedure TBrush_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PBrush(Params^[0])^.Color;
end;

//Write: property Color: Integer read Color write Color;
procedure TBrush_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PBrush(Params^[0])^.Color := PInteger(Params^[1])^;
end;

//Read: property Style:TBrushStyle read Style write Style;
procedure TBrush_Style_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBrushStyle(Result)^ := PBrush(Params^[0])^.Style;
end;

//Write: property Style:TBrushStyle read Style write Style;
procedure TBrush_Style_Write(const Params: PParamArray); lape_extdecl
begin
  PBrush(Params^[0])^.Style := PBrushStyle(Params^[1])^;
end;

//procedure Free();
procedure TBrush_Free(const Params: PParamArray); lape_extdecl
begin
  PBrush(Params^[0])^.Free();
end;

procedure Register_TBrush(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TBrush', 'TGraphicsObject');

    addGlobalFunc('procedure TBrush.Assign(Source: TPersistent);', @TBrush_Assign);
    addGlobalFunc('procedure TBrush.Init();', @TBrush_Init);
    addClassVar('TBrush', 'Color', 'Integer', @TBrush_Color_Read, @TBrush_Color_Write);
    addClassVar('TBrush', 'Style', 'TBrushStyle', @TBrush_Style_Read, @TBrush_Style_Write);
    addGlobalFunc('procedure TBrush.Free();', @TBrush_Free);
  end;
end;

{TCanvas}
//procedure Lock;
procedure TCanvas_Lock(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Lock();
end;

//function TryLock: Boolean;
procedure TCanvas_TryLock(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.TryLock();
end;

//procedure Unlock; ;
procedure TCanvas_Unlock(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Unlock();
end;

//procedure Refresh;
procedure TCanvas_Refresh(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Refresh();
end;

//procedure Changing;
procedure TCanvas_Changing(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Changing();
end;

//procedure Changed;
procedure TCanvas_Changed(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Changed();
end;

//procedure SaveHandleState;
procedure TCanvas_SaveHandleState(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.SaveHandleState();
end;

//procedure RestoreHandleState;
procedure TCanvas_RestoreHandleState(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.RestoreHandleState();
end;

//procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
procedure TCanvas_Arc(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Arc(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//procedure Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer); virtual;
procedure TCanvas_Chord(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Chord(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//procedure CopyRect(Dest: TRect; SrcCanvas: TCanvas;Source: TRect);
procedure TCanvas_CopyRect(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.CopyRect(PRect(Params^[1])^, PCanvas(Params^[2])^, PRect(Params^[3])^);
end;

//procedure DrawFocusRect(ARect: TRect);
procedure TCanvas_DrawFocusRect(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.DrawFocusRect(PRect(Params^[1])^);
end;

//procedure Ellipse(x1, y1, x2, y2: Integer);
procedure TCanvas_Ellipse(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Ellipse(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure FillRect(X1,Y1,X2,Y2: Integer);
procedure TCanvas_FillRect(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.FillRect(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure FloodFill(X, Y: Integer; FillColor: TColor;FillStyle: TFillStyle);
procedure TCanvas_FloodFill(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.FloodFill(PInteger(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PFillStyle(Params^[4])^);
end;

//procedure RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);
procedure TCanvas_RadialPie(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.RadialPie(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

//procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2, StartX,StartY,EndX,EndY: Integer);
procedure TCanvas_Pie(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Pie(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//procedure PolyBezier(Points: PPoint; NumPts: Integer; Filled: boolean;Continuous: boolean);
procedure TCanvas_PolyBezier(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.PolyBezier(PPPoint(Params^[1])^, PInteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure PolyBezier( Points: TPointArray;Filled: boolean; Continuous: boolean);
procedure TCanvas_PolyBezierEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.PolyBezier(PPointArray(Params^[1])^, Pboolean(Params^[2])^, Pboolean(Params^[3])^);
end;

//procedure Polygon( Points: TPointArray;Winding: Boolean;StartIndex: Integer; NumPts: Integer);
procedure TCanvas_Polygon(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Polygon(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure Polygon(Points: PPoint; NumPts: Integer;Winding: boolean);
procedure TCanvas_PolygonEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Polygon(PPPoint(Params^[1])^, PInteger(Params^[2])^, Pboolean(Params^[3])^);
end;

//procedure Polygon( Points: TPointArray);
procedure TCanvas_PolygonExEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Polygon(PPointArray(Params^[1])^);
end;

//procedure Polyline( Points: TPointArray;StartIndex: Integer;NumPts: Integer);
procedure TCanvas_Polyline(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Polyline(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure Polyline(Points: PPoint; NumPts: Integer);
procedure TCanvas_PolylineEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Polyline(PPPoint(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure Polyline( Points: TPointArray);
procedure TCanvas_PolylineExEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Polyline(PPointArray(Params^[1])^);
end;

//procedure Rectangle(X1,Y1,X2,Y2: Integer);
procedure TCanvas_Rectangle(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Rectangle(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure Rectangle( ARect: TRect);
procedure TCanvas_RectangleEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Rectangle(PRect(Params^[1])^);
end;

//procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer);
procedure TCanvas_RoundRect(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.RoundRect(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

//procedure RoundRect( Rect: TRect; RX,RY: Integer);
procedure TCanvas_RoundRectEx(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.RoundRect(PRect(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure TextOut(X,Y: Integer;  Text: String);
procedure TCanvas_TextOut(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.TextOut(PInteger(Params^[1])^, PInteger(Params^[2])^, PlpString(Params^[3])^);
end;

//procedure TextRect( ARect: TRect; X, Y: integer;  Text: string);
procedure TCanvas_TextRect(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.TextRect(PRect(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, PlpString(Params^[4])^);
end;

//function TextHeight( Text: string): Integer;
procedure TCanvas_TextHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCanvas(Params^[0])^.TextHeight(PlpString(Params^[1])^);
end;

//function TextWidth( Text: string): Integer;
procedure TCanvas_TextWidth(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCanvas(Params^[0])^.TextWidth(PlpString(Params^[1])^);
end;

//function HandleAllocated: boolean; virtual;
procedure TCanvas_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCanvas(Params^[0])^.HandleAllocated();
end;

//Read: property AutoRedraw: Boolean read AutoRedraw write AutoRedraw;
procedure TCanvas_AutoRedraw_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.AutoRedraw;
end;

//Write: property AutoRedraw: Boolean read AutoRedraw write AutoRedraw;
procedure TCanvas_AutoRedraw_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.AutoRedraw := PBoolean(Params^[1])^;
end;

//Read: property Brush: TBrush read Brush write Brush;
procedure TCanvas_Brush_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBrush(Result)^ := PCanvas(Params^[0])^.Brush;
end;

//Write: property Brush: TBrush read Brush write Brush;
procedure TCanvas_Brush_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Brush := PBrush(Params^[1])^;
end;

//Read: property CopyMode: TCopyMode read FCopyMode write FCopyMode;
procedure TCanvas_CopyMode_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCopyMode(Result)^ := PCanvas(Params^[0])^.CopyMode;
end;

//Write: property CopyMode: TCopyMode read FCopyMode write FCopyMode;
procedure TCanvas_CopyMode_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.CopyMode := PCopyMode(Params^[1])^;
end;

//Read: property Font: TFont read Font write Font;
procedure TCanvas_Font_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFont(Result)^ := PCanvas(Params^[0])^.Font;
end;

//Write: property Font: TFont read Font write Font;
procedure TCanvas_Font_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property Height: integer read Height;
procedure TCanvas_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCanvas(Params^[0])^.Height;
end;

//Read: property Pen: TPen read Pen write Pen;
procedure TCanvas_Pen_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPen(Result)^ := PCanvas(Params^[0])^.Pen;
end;

//Write: property Pen: TPen read Pen write Pen;
procedure TCanvas_Pen_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Pen := PPen(Params^[1])^;
end;

//Read: property Width: integer read Width;
procedure TCanvas_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCanvas(Params^[0])^.Width;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCanvas_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCanvas(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCanvas_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
procedure TCanvas_OnChanging_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCanvas(Params^[0])^.OnChanging;
end;

//Write: property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
procedure TCanvas_OnChanging_Write(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure TCanvas_Get_Pixel(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PCanvas(Params^[0])^.Pixels[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure TCanvas_Set_Pixel(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Pixels[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

procedure TCanvas_Set_Pixels(const Params: PParamArray); lape_extdecl
var
  i, l: integer;
begin
  l := length(PPointArray(Params^[1])^);

  for i := 0 to (l - 1) do
    PCanvas(Params^[0])^.Pixels[PPointArray(Params^[1])^[i].x, PPointArray(Params^[1])^[i].y] := PColor(Params^[2])^;
end;

//procedure Draw(X,Y: Integer; SrcGraphic: TGraphic);
procedure TCanvas_Draw(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Draw(PInteger(Params^[1])^, PInteger(Params^[2])^, PGraphic(Params^[3])^);
end;

//constructor Create();
procedure TCanvas_Init(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^ := TCanvas.Create();
end;

//procedure Free();
procedure TCanvas_Free(const Params: PParamArray); lape_extdecl
begin
  PCanvas(Params^[0])^.Free();
end;

procedure Register_TCanvas(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCanvas', 'TPersistent');

    addGlobalFunc('procedure TCanvas.Lock();', @TCanvas_Lock);
    addGlobalFunc('function TCanvas.TryLock(): Boolean;', @TCanvas_TryLock);
    addGlobalFunc('procedure TCanvas.Unlock();', @TCanvas_Unlock);
    addGlobalFunc('procedure TCanvas.Refresh();', @TCanvas_Refresh);
    addGlobalFunc('procedure TCanvas.Changing();', @TCanvas_Changing);
    addGlobalFunc('procedure TCanvas.Changed();', @TCanvas_Changed);
    addGlobalFunc('procedure TCanvas.SaveHandleState();', @TCanvas_SaveHandleState);
    addGlobalFunc('procedure TCanvas.RestoreHandleState();', @TCanvas_RestoreHandleState);
    addGlobalFunc('procedure TCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);', @TCanvas_Arc);
    addGlobalFunc('procedure TCanvas.Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer);', @TCanvas_Chord);
    addGlobalFunc('procedure TCanvas.CopyRect(Dest: TRect; SrcCanvas: TCanvas;Source: TRect);', @TCanvas_CopyRect);
    addGlobalFunc('procedure TCanvas.Draw(X,Y: Integer; SrcGraphic: TGraphic);', @TCanvas_Draw);
    addGlobalFunc('procedure TCanvas.DrawFocusRect(ARect: TRect);', @TCanvas_DrawFocusRect);
    addGlobalFunc('procedure TCanvas.Ellipse(x1, y1, x2, y2: Integer);', @TCanvas_Ellipse);
    addGlobalFunc('procedure TCanvas.FillRect(X1,Y1,X2,Y2: Integer);', @TCanvas_FillRect);
    addGlobalFunc('procedure TCanvas.FloodFill(X, Y: Integer; FillColor: TColor;FillStyle: TFillStyle);', @TCanvas_FloodFill);
    addGlobalFunc('procedure TCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);', @TCanvas_RadialPie);
    addGlobalFunc('procedure TCanvas.Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2, StartX,StartY,EndX,EndY: Integer);', @TCanvas_Pie);
    addGlobalFunc('procedure TCanvas.PolyBezier(Points: PPoint; NumPts: Integer; Filled: boolean;Continuous: boolean);', @TCanvas_PolyBezier);
    addGlobalFunc('procedure TCanvas.PolyBezier( Points: TPointArray;Filled: boolean; Continuous: boolean); overload;', @TCanvas_PolyBezierEx);
    addGlobalFunc('procedure TCanvas.Polygon( Points: TPointArray;Winding: Boolean;StartIndex: Integer; NumPts: Integer);', @TCanvas_Polygon);
    addGlobalFunc('procedure TCanvas.Polygon(Points: PPoint; NumPts: Integer;Winding: boolean); overload;', @TCanvas_PolygonEx);
    addGlobalFunc('procedure TCanvas.Polygon( Points: TPointArray); overload;', @TCanvas_PolygonExEx);
    addGlobalFunc('procedure TCanvas.Polyline( Points: TPointArray;StartIndex: Integer;NumPts: Integer);', @TCanvas_Polyline);
    addGlobalFunc('procedure TCanvas.Polyline(Points: PPoint; NumPts: Integer); overload;', @TCanvas_PolylineEx);
    addGlobalFunc('procedure TCanvas.Polyline( Points: TPointArray); overload;', @TCanvas_PolylineExEx);
    addGlobalFunc('procedure TCanvas.Rectangle(X1,Y1,X2,Y2: Integer);', @TCanvas_Rectangle);
    addGlobalFunc('procedure TCanvas.Rectangle( ARect: TRect); overload;', @TCanvas_RectangleEx);
    addGlobalFunc('procedure TCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer);', @TCanvas_RoundRect);
    addGlobalFunc('procedure TCanvas.RoundRect( Rect: TRect; RX,RY: Integer); overload;', @TCanvas_RoundRectEx);
    addGlobalFunc('procedure TCanvas.TextOut(X,Y: Integer;  Text: String);', @TCanvas_TextOut);
    addGlobalFunc('procedure TCanvas.TextRect( ARect: TRect; X, Y: integer;  Text: string);', @TCanvas_TextRect);
    addGlobalFunc('function TCanvas.TextHeight( Text: string): Integer;', @TCanvas_TextHeight);
    addGlobalFunc('function TCanvas.TextWidth( Text: string): Integer;', @TCanvas_TextWidth);
    addGlobalFunc('function TCanvas.HandleAllocated(): boolean;', @TCanvas_HandleAllocated);
    addGlobalFunc('function TCanvas.GetPixel(x, y: integer): TColor', @TCanvas_Set_Pixel);
    addGlobalFunc('procedure TCanvas.SetPixel(x, y: integer; colour: TColor);', @TCanvas_Set_Pixel);
    addGlobalFunc('procedure TCanvas.SetPixels(tpa: TPointArray; colour: TColor);', @TCanvas_Set_Pixels);
    addClassVar('TCanvas', 'AutoRedraw', 'Boolean', @TCanvas_AutoRedraw_Read, @TCanvas_AutoRedraw_Write);
    addClassVar('TCanvas', 'Brush', 'TBrush', @TCanvas_Brush_Read, @TCanvas_Brush_Write);
    addClassVar('TCanvas', 'CopyMode', 'TCopyMode', @TCanvas_CopyMode_Read, @TCanvas_CopyMode_Write);
    addClassVar('TCanvas', 'Font', 'TFont', @TCanvas_Font_Read, @TCanvas_Font_Write);
    addClassVar('TCanvas', 'Height', 'integer', @TCanvas_Height_Read);
    addClassVar('TCanvas', 'Pen', 'TPen', @TCanvas_Pen_Read, @TCanvas_Pen_Write);
    addClassVar('TCanvas', 'Width', 'integer', @TCanvas_Width_Read);
    addClassVar('TCanvas', 'OnChange', 'TNotifyEvent', @TCanvas_OnChange_Read, @TCanvas_OnChange_Write);
    addClassVar('TCanvas', 'OnChanging', 'TNotifyEvent', @TCanvas_OnChanging_Read, @TCanvas_OnChanging_Write);
    addGlobalFunc('procedure TCanvas.Init();', @TCanvas_Init);
    addGlobalFunc('procedure TCanvas.Free();', @TCanvas_Free);
  end;
end;
{TGraphics}

//procedure Assign(ASource: TPersistent);
procedure TGraphic_Assign(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//constructor Create;
procedure TGraphic_Init(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^ := TGraphic.Create();
end;

//procedure Clear;
procedure TGraphic_Clear(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Clear();
end;

//procedure LoadFromFile(const Filename: string);
procedure TGraphic_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure SaveToFile(const Filename: string);
procedure TGraphic_SaveToFile(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//Read: property Empty: Boolean read Empty;
procedure TGraphic_Empty_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Empty;
end;

//Read: property Height: Integer read Height write Height;
procedure TGraphic_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure TGraphic_Height_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Modified: Boolean read Modified write Modified;
procedure TGraphic_Modified_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Modified;
end;

//Write: property Modified: Boolean read Modified write Modified;
procedure TGraphic_Modified_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TGraphic_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PGraphic(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TGraphic_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property Palette: Integer read GetPalette write SetPalette;
procedure TGraphic_Palette_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Palette;
end;

//Write: property Palette: Integer read GetPalette write SetPalette;
procedure TGraphic_Palette_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Palette := PInteger(Params^[1])^;
end;

//Read: property PaletteModified: Boolean read PaletteModified write PaletteModified;
procedure TGraphic_PaletteModified_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.PaletteModified;
end;

//Write: property PaletteModified: Boolean read PaletteModified write PaletteModified;
procedure TGraphic_PaletteModified_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.PaletteModified := PBoolean(Params^[1])^;
end;

//Read: property Transparent: Boolean read Transparent write Transparent;
procedure TGraphic_Transparent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read Transparent write Transparent;
procedure TGraphic_Transparent_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//Read: property Width: Integer read Width write Width;
procedure TGraphic_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Width;
end;

//Write: property Width: Integer read Width write Width;
procedure TGraphic_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//procedure Free();
procedure TGraphic_Free(const Params: PParamArray); lape_extdecl
begin
  PGraphic(Params^[0])^.Free();
end;

procedure Register_TGraphic(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGraphic', 'TPersistent');

    addGlobalFunc('procedure TGraphic.Assign(ASource: TPersistent);', @TGraphic_Assign);
    addGlobalFunc('procedure TGraphic.Init();', @TGraphic_Init);
    addGlobalFunc('procedure TGraphic.Clear();', @TGraphic_Clear);
    addGlobalFunc('procedure TGraphic.LoadFromFile(const Filename: string);', @TGraphic_LoadFromFile);
    addGlobalFunc('procedure TGraphic.SaveToFile(const Filename: string);', @TGraphic_SaveToFile);
    addClassVar('TGraphic', 'Empty', 'Boolean', @TGraphic_Empty_Read);
    addClassVar('TGraphic', 'Height', 'Integer', @TGraphic_Height_Read, @TGraphic_Height_Write);
    addClassVar('TGraphic', 'Modified', 'Boolean', @TGraphic_Modified_Read, @TGraphic_Modified_Write);
    addClassVar('TGraphic', 'OnChange', 'TNotifyEvent', @TGraphic_OnChange_Read, @TGraphic_OnChange_Write);
    addClassVar('TGraphic', 'Palette', 'Integer', @TGraphic_Palette_Read, @TGraphic_Palette_Write);
    addClassVar('TGraphic', 'PaletteModified', 'Boolean', @TGraphic_PaletteModified_Read, @TGraphic_PaletteModified_Write);
    addClassVar('TGraphic', 'Transparent', 'Boolean', @TGraphic_Transparent_Read, @TGraphic_Transparent_Write);
    addClassVar('TGraphic', 'Width', 'Integer', @TGraphic_Width_Read, @TGraphic_Width_Write);
    addGlobalFunc('procedure TGraphic.Free();', @TGraphic_Free);
  end;
end;
{TBitmap}
//constructor Create;
procedure TBitmap_Init(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^ := TBitmap.Create();
end;

//procedure BeginUpdate(ACanvasOnly: Boolean);
procedure TBitmap_BeginUpdate(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.BeginUpdate(PBoolean(Params^[1])^);
end;

//procedure EndUpdate(AStreamIsValid: Boolean);
procedure TBitmap_EndUpdate(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.EndUpdate(PBoolean(Params^[1])^);
end;

//procedure FreeImage;
procedure TBitmap_FreeImage(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.FreeImage();
end;

//function BitmapHandleAllocated: boolean;
procedure TBitmap_BitmapHandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.BitmapHandleAllocated();
end;

//function MaskHandleAllocated: boolean;
procedure TBitmap_MaskHandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.MaskHandleAllocated();
end;

//function PaletteAllocated: boolean;
procedure TBitmap_PaletteAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.PaletteAllocated();
end;

//procedure LoadFromStream(AStream: TStream);
procedure TBitmap_LoadFromStream(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure LoadFromStream(AStream: TStream; ASize: Cardinal);
procedure TBitmap_LoadFromStreamEx(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.LoadFromStream(PStream(Params^[1])^, PCardinal(Params^[2])^);
end;

//procedure SaveToStream(AStream: TStream); override;
procedure TBitmap_SaveToStream(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//procedure GetSize(var AWidth, AHeight: Integer);
procedure TBitmap_GetSize(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure Mask(ATransparentColor: TColor);
procedure TBitmap_Mask(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.Mask(PColor(Params^[1])^);
end;

//function ReleaseBitmapHandle: HBITMAP;
procedure TBitmap_ReleaseBitmapHandle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHBITMAP(Result)^ := PBitmap(Params^[0])^.ReleaseBitmapHandle();
end;

//function ReleaseMaskHandle: HBITMAP;
procedure TBitmap_ReleaseMaskHandle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHBITMAP(Result)^ := PBitmap(Params^[0])^.ReleaseMaskHandle();
end;

//function ReleasePalette: HPALETTE;
procedure TBitmap_ReleasePalette(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHPALETTE(Result)^ := PBitmap(Params^[0])^.ReleasePalette();
end;

//Read: property Canvas: TCanvas read GetCanvas;
procedure TBitmap_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PBitmap(Params^[0])^.Canvas;
end;

//function HandleAllocated: boolean;
procedure TBitmap_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.HandleAllocated();
end;

//Read: property BitmapHandle: HBITMAP read BitmapHandle write BitmapHandle;
procedure TBitmap_BitmapHandle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHBITMAP(Result)^ := PBitmap(Params^[0])^.BitmapHandle;
end;

//Write: property BitmapHandle: HBITMAP read BitmapHandle write BitmapHandle;
procedure TBitmap_BitmapHandle_Write(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.BitmapHandle := PHBITMAP(Params^[1])^;
end;

//Read: property Masked: Boolean read Masked write Masked;
procedure TBitmap_Masked_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.Masked;
end;

//Write: property Masked: Boolean read Masked write Masked;
procedure TBitmap_Masked_Write(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.Masked := PBoolean(Params^[1])^;
end;

//Read: property MaskHandle: HBITMAP read MaskHandle write MaskHandle;
procedure TBitmap_MaskHandle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHBITMAP(Result)^ := PBitmap(Params^[0])^.MaskHandle;
end;

//Write: property MaskHandle: HBITMAP read MaskHandle write MaskHandle;
procedure TBitmap_MaskHandle_Write(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.MaskHandle := PHBITMAP(Params^[1])^;
end;

//Read: property TransparentColor: TColor read TransparentColor write TransparentColor;
procedure TBitmap_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PBitmap(Params^[0])^.TransparentColor;
end;

//Write: property TransparentColor: TColor read TransparentColor write TransparentColor;
procedure TBitmap_TransparentColor_Write(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.TransparentColor := PColor(Params^[1])^;
end;

//Read: property TransparentMode: TTransparentMode read TransparentMode write TransparentMode;
procedure TBitmap_TransparentMode_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTransparentMode(Result)^ := PBitmap(Params^[0])^.TransparentMode;
end;

//Write: property TransparentMode: TTransparentMode read TransparentMode write TransparentMode;
procedure TBitmap_TransparentMode_Write(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.TransparentMode := PTransparentMode(Params^[1])^;
end;

//procedure Free();
procedure TBitmap_Free(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.Free();
end;

procedure TBitmap_Transparent_Write(const Params: PParamArray); lape_extdecl
begin
  PBitmap(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure TBitmap_Transparent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.Transparent;
end;

procedure TBitmap_ToString(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  b: TBitmap;
  x, y, w, h: integer;
  Addition, Data: string;
begin
  PBitmap(Params^[0])^.GetSize(w, h);
  Data := '';

  for x := 0 to (w - 1) do
    for y := 0 to (h - 1) do
    begin
      Addition := IntToHex(PBitmap(Params^[0])^.Canvas.Pixels[x, y], 1);

      while (length(Addition) < 6) do
        Addition := '0' + Addition;

      Data := (Data + Addition);
    end;

  PLPString(Result)^ := Format('%d, %d, ''', [w, h]) + Data + '''';
end;

procedure TBitmap_LoadFromString(const Params: PParamArray); lape_extdecl
var
  x, y, w, h: integer;
begin
  PBitmap(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);

  for x := (PInteger(Params^[1])^ - 1) downto 0 do
    for y := (PInteger(Params^[2])^ -1) downto 0 do
      PBitmap(Params^[0])^.Canvas.Pixels[x, y] := StrToInt('$' + Copy(PLPString(Params^[4])^, y * 6 + x * PInteger(Params^[2])^ * 6 + 1, 6));

  PBitmap(Params^[0])^.Mask(PInteger(Params^[3])^);
end;

procedure Register_TBitmap(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TBitmap', 'TGraphic');

    addGlobalFunc('procedure TBitmap.Init();', @TBitmap_Init);
    addGlobalFunc('procedure TBitmap.BeginUpdate(ACanvasOnly: Boolean);', @TBitmap_BeginUpdate);
    addGlobalFunc('procedure TBitmap.EndUpdate(AStreamIsValid: Boolean);', @TBitmap_EndUpdate);
    addGlobalFunc('procedure TBitmap.FreeImage();', @TBitmap_FreeImage);
    addGlobalFunc('function TBitmap.BitmapHandleAllocated(): boolean;', @TBitmap_BitmapHandleAllocated);
    addGlobalFunc('function TBitmap.MaskHandleAllocated(): boolean;', @TBitmap_MaskHandleAllocated);
    addGlobalFunc('function TBitmap.PaletteAllocated(): boolean;', @TBitmap_PaletteAllocated);
    addGlobalFunc('procedure TBitmap.LoadFromStream(AStream: TStream); ', @TBitmap_LoadFromStream);
    addGlobalFunc('procedure TBitmap.LoadFromStream(AStream: TStream; ASize: Cardinal);overload;', @TBitmap_LoadFromStreamEx);
    addGlobalFunc('procedure TBitmap.SaveToStream(AStream: TStream);', @TBitmap_SaveToStream);
    addGlobalFunc('procedure TBitmap.GetSize(var AWidth, AHeight: Integer);', @TBitmap_GetSize);
    addGlobalFunc('procedure TBitmap.Mask(ATransparentColor: TColor);', @TBitmap_Mask);
    addGlobalFunc('function TBitmap.ReleaseBitmapHandle(): HBITMAP;', @TBitmap_ReleaseBitmapHandle);
    addGlobalFunc('function TBitmap.ReleaseMaskHandle(): HBITMAP;', @TBitmap_ReleaseMaskHandle);
    addGlobalFunc('function TBitmap.ReleasePalette(): HPALETTE;', @TBitmap_ReleasePalette);
    addGlobalFunc('function TBitmap.ToString(): string;', @TBitmap_ToString);
    addGlobalFunc('procedure TBitmap.LoadFromString(w, h, TransparentColor: integer; data: string);', @TBitmap_LoadFromString);
    addClassVar('TBitmap', 'Canvas', 'TCanvas', @TBitmap_Canvas_Read);
    addGlobalFunc('function TBitmap.HandleAllocated(): boolean;', @TBitmap_HandleAllocated);
    addClassVar('TBitmap', 'BitmapHandle', 'HBITMAP', @TBitmap_BitmapHandle_Read, @TBitmap_BitmapHandle_Write);
    addClassVar('TBitmap', 'Masked', 'Boolean', @TBitmap_Masked_Read, @TBitmap_Masked_Write);
    addClassVar('TBitmap', 'MaskHandle', 'HBITMAP', @TBitmap_MaskHandle_Read, @TBitmap_MaskHandle_Write);
    addClassVar('TBitmap', 'TransparentColor', 'TColor', @TBitmap_TransparentColor_Read, @TBitmap_TransparentColor_Write);
    addClassVar('TBitmap', 'TransparentMode', 'TTransparentMode', @TBitmap_TransparentMode_Read, @TBitmap_TransparentMode_Write);
    addClassVar('TBitmap', 'Transparent', 'Boolean', @TBitmap_TransparentMode_Read, @TBitmap_Transparent_Write);
    addGlobalFunc('procedure TBitmap.Free();', @TBitmap_Free);
  end;
end;
{TPicture}
//constructor Create;
procedure TPicture_Init(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^ := TPicture.Create();
end;

//procedure Clear;
procedure TPicture_Clear(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.Clear();
end;

//procedure LoadFromFile(const Filename: string);
procedure TPicture_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure LoadFromStream(Stream: TStream);
procedure TPicture_LoadFromStream(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure LoadFromStreamWithFileExt(Stream: TStream; const FileExt: string);
procedure TPicture_LoadFromStreamWithFileExt(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.LoadFromStreamWithFileExt(PStream(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure SaveToFile(const Filename: string; const FileExt: string = '');
procedure TPicture_SaveToFile(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.SaveToFile(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure SaveToStream(Stream: TStream);
procedure TPicture_SaveToStream(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//procedure SaveToStreamWithFileExt(Stream: TStream; const FileExt: string);
procedure TPicture_SaveToStreamWithFileExt(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.SaveToStreamWithFileExt(PStream(Params^[1])^, PlpString(Params^[2])^);
end;

//Read: property Bitmap: TBitmap read Bitmap write Bitmap;
procedure TPicture_Bitmap_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBitmap(Result)^ := PPicture(Params^[0])^.Bitmap;
end;

//Write: property Bitmap: TBitmap read Bitmap write Bitmap;
procedure TPicture_Bitmap_Write(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

//Read: property Graphic: TGraphic read FGraphic write SetGraphic;
procedure TPicture_Graphic_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGraphic(Result)^ := PPicture(Params^[0])^.Graphic;
end;

//Write: property Graphic: TGraphic read FGraphic write SetGraphic;
procedure TPicture_Graphic_Write(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.Graphic := PGraphic(Params^[1])^;
end;

//Read: property Height: Integer read Height;
procedure TPicture_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PPicture(Params^[0])^.Height;
end;

//Read: property Width: Integer read Width;
procedure TPicture_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PPicture(Params^[0])^.Width;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TPicture_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PPicture(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TPicture_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure TPicture_Free(const Params: PParamArray); lape_extdecl
begin
  PPicture(Params^[0])^.Free();
end;

procedure Register_TPicture(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TPicture', 'TPersistent');

    addGlobalFunc('procedure TPicture.Init();', @TPicture_Init);
    addGlobalFunc('procedure TPicture.Clear();', @TPicture_Clear);
    addGlobalFunc('procedure TPicture.LoadFromFile(const Filename: string);', @TPicture_LoadFromFile);
    addGlobalFunc('procedure TPicture.LoadFromStream(Stream: TStream);', @TPicture_LoadFromStream);
    addGlobalFunc('procedure TPicture.LoadFromStreamWithFileExt(Stream: TStream; const FileExt: string);', @TPicture_LoadFromStreamWithFileExt);
    addGlobalFunc('procedure TPicture.SaveToFile(const Filename: string; const FileExt: string);', @TPicture_SaveToFile);
    addGlobalFunc('procedure TPicture.SaveToStream(Stream: TStream);', @TPicture_SaveToStream);
    addGlobalFunc('procedure TPicture.SaveToStreamWithFileExt(Stream: TStream; const FileExt: string);', @TPicture_SaveToStreamWithFileExt);
    addClassVar('TPicture', 'Bitmap', 'TBitmap', @TPicture_Bitmap_Read, @TPicture_Bitmap_Write);
    addClassVar('TPicture', 'Graphic', 'TGraphic', @TPicture_Graphic_Read, @TPicture_Graphic_Write);
    addClassVar('TPicture', 'Height', 'Integer', @TPicture_Height_Read);
    addClassVar('TPicture', 'Width', 'Integer', @TPicture_Width_Read);
    addClassVar('TPicture', 'OnChange', 'TNotifyEvent', @TPicture_OnChange_Read, @TPicture_OnChange_Write);
    addGlobalFunc('procedure TPicture.Free();', @TPicture_Free);
  end;
end;

{ALL}
 procedure RegisterLCLGraphics(Compiler: TLapeCompiler);
 begin
   with Compiler do
     begin
       AddGlobalType('record Left,Top,Right,Bottom : Longint;end;','TRect');
       AddGlobalType('(fsBold, fsItalic, fsStrikeOut, fsUnderline)','TFontStyle');
       AddGlobalType('(fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqCleartype, fqCleartypeNatural)', 'TFontQuality');
       AddGlobalType('set of TFontStyle','TFontStyles');   
       AddGlobalType('(fpDefault, fpVariable, fpFixed)','TFontPitch');
       AddGlobalType('integer','TCopyMode');
       AddGlobalType('(psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,psClear)','TPenStyle');
       AddGlobalType('(pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor)','TPenMode');
       AddGlobalType('(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern)','TBrushStyle');
       AddGlobalType('(fsSurface,fsBorder)','TFillStyle');
       //AddGlobalType('^TPointArray','PPoint');
       AddGlobalType('integer','HBITMAP');
       AddGlobalType('integer','HPALETTE');
       AddGlobalType('(tmAuto,tmFixed)','TTransparentMode');

       //register graphics
       Register_TGraphicsObject(Compiler);
       Register_TFont(Compiler);
       Register_TPen(Compiler);
       Register_TBrush(Compiler);
       Register_TGraphic(Compiler);
       Register_TCanvas(Compiler);
       Register_TBitmap(Compiler);
       Register_TPicture(Compiler);
     end;
 end;

end.

