unit design_frm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, LCLType,LCLIntf, Dialogs,
  TypInfo, Math, cselectonruntime, LMessages, ComCtrls, ExtCtrls, Menus,bitmaps,Graphics ;
type
 THControl = Class(TControl);
  { TDsgnForm }

  TDsgnForm = class(TForm)
    FontDialog1: TFontDialog;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    sor: TSelectOnRunTime;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);

    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure sorBeforeSelect(Sender: TObject; Selected: TControl;
      var Select: Boolean);
    procedure OnResizeCtrl(Sender: TObject);
    procedure OnMoveCtrl(Sender: TObject);
    procedure ChooseImg(Sender: TObject);
    procedure sorCanMove(Sender: TObject; Control: TControl;
      var CanMove: Boolean; var CanMoveOutParent: Boolean);
  private
    { private declarations }
    _ControlsCreated:Integer;
    _comp:TControl;
     FMode: integer;{0: script form; 1:form in smart; 2: progress report}
    function CreateComponent(Sender: TObject; X, Y: Integer):TControl;
    function ResolveFileType(AStream: TStream): Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure SetScriptMode();
    procedure SetProgressMode();
    procedure SetSMARTMode();
  public
    procedure DeleteComponent();
    procedure SetMode(i: integer);
    function GetMode():integer;
    { public declarations }
  end; 

var
  DsgnForm: TDsgnForm;
  CurCom:TControl;
  imgdialog: TopenDialog;
  pathtoimg: string;
 { flag: integer;//0 - component,1: image,2: listbox, 3: combobox; }
implementation

{$R *.lfm}
uses frmdesigner;
{Image functions}
function JpegToBitmap(jpeg: string):TBitmap;
var
 jpg:TJpegImage;
 bmp:TBitmap;
 begin
   bmp := TBitmap.Create;
   jpg:=TJpegImage.Create;
   try
   jpg.LoadFromFile(jpeg);
   Bmp.Height      := jpg.Height;
   Bmp.Width       := jpg.Width;
   Bmp.PixelFormat := pf24bit;
   Bmp.Canvas.Draw(0, 0, jpg);
   bmp.Assign(jpg);
   result:=bmp;
   finally
     jpg.Free;
   end;
 end;
function PngToBitmap(png: string):TBitmap;
var
  bmp: TBitmap;
  pic: TPortableNetworkGraphic;
begin
   pic:=TPortableNetworkGraphic.Create;
   bmp := TBitmap.Create;
   try
   pic.LoadFromFile(png);
   bmp.Assign(pic);
   result:=bmp;
   finally
     pic.Free;
   end;
end;

{ TDsgnForm }

procedure TDsgnForm.FormCreate(Sender: TObject);
begin
  compForm.SetControl(sender);
  imgdialog:=TOpenDialog.Create(self);
  Self.BorderStyle:=bsToolWindow;
  Self.FormStyle:=fsStayOnTop;
  Self.ShowInTaskBar:=stNever;
  SetMode(0);
end;

procedure TDsgnForm.FormChangeBounds(Sender: TObject);
begin
 compform.SetControl(sender);
end;


procedure TDsgnForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  ctl:TControl;
begin
  if (Key = VK_ESCAPE) then begin
    if (sor.Selected) and (Assigned(sor.SelectControl)) then begin
      ctl := sor.SelectControl.Parent;
      if Assigned(ctl) then begin
        sor.SelectControl := ctl;
      end;
    end;
  end;

end;

procedure TDsgnForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _comp := CreateComponent(Sender, X, Y);
end;

procedure TDsgnForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TDsgnForm.FormPaint(Sender: TObject);
var
  i,j:Integer;
begin
  inherited;
  i := 0;
  j := 0;
  while (i < Self.Width) do begin
    while (j < Self.Height) do begin
      Self.Canvas.Pixels[i, j] := clMaroon;
      j := j + 8{GridSize};
    end;
    j := 0;
    i := i + 8{GridSize};
  end;

end;

procedure TDsgnForm.FormResize(Sender: TObject);
begin
  compform.SetControl(DsgnForm);
end;

procedure TDsgnForm.FormShow(Sender: TObject);
begin
end;

procedure TDsgnForm.MenuItem1Click(Sender: TObject);
begin
  if fontdialog1.Execute then
  begin
     SetPropValue(CurComp.Font,'Size',FontDialog1.Font.Size);
     SetPropValue(CurComp.Font,'Name',FontDialog1.Font.Name);
     SetPropValue(CurComp.Font,'Color',FontDialog1.Font.Color);
  end;

end;

procedure TDsgnForm.sorBeforeSelect(Sender: TObject; Selected: TControl;
  var Select: Boolean);
var
  m: TMenuItem;
begin
   if assigned(CurComp) then CurComp:=nil;
   compform.SetControl(Selected);
   CurComp:=Selected;
   CurComp.OnResize:=@OnResizeCtrl;
   CurComp.OnChangeBounds:=@OnMoveCtrl;
   CurComp.PopupMenu:=PopupMenu1;
   popupmenu1.Items[0].Visible:=true;
   popupmenu1.Items[1].Visible:=false;
   if (CompareText(selected.ClassName,'TImage'))=0 then
    begin
      if assigned(popupmenu1.Items[1]) then
       begin
      popupmenu1.Items[1].OnClick:=@ChooseImg;
      popupmenu1.Items[0].Visible:=false;
      popupmenu1.Items[1].Visible:=true;
      end;
      end;
      if (CompareText(CurComp.ClassName,'TListBox'))=0 then
      begin
      end;
      if (CompareText(CurComp.ClassName,'TComboBox'))=0 then
      begin
      end;
end;

procedure TDsgnForm.OnResizeCtrl(Sender: TObject);
begin
  compform.SetControl(sender);
end;

procedure TDsgnForm.OnMoveCtrl(Sender: TObject);
begin
  compform.SetControl(sender);
end;

procedure TDsgnForm.ChooseImg(Sender: TObject);
var
  imgstream: TMemoryStream;
  i: integer;
begin
  imgdialog.Filter:='Bitmap|*.bmp|Jpeg|*.jpg|png|*.png';
  if imgdialog.Execute then
   begin
   imgstream:=TMemoryStream.Create();
   imgstream.LoadFromFile(imgdialog.FileName);
   pathtoimg:=imgdialog.FileName;
   //imgstream.Position:=0;
   i:=ResolveFileType(imgstream);
   case i of
   1: begin TImage(CurComp).AutoSize:=true; TImage(CurComp).Picture.LoadFromFile(pathtoimg);end;
   3: begin TImage(CurComp).AutoSize:=true; TImage(CurComp).Picture.Bitmap:=JpegToBitmap(pathtoimg);end;
   4: begin TImage(CurComp).AutoSize:=true; TImage(CurComp).Picture.Bitmap:=PngToBitmap(pathtoimg);end;
   end;
   imgstream.Free;
   end;
end;

procedure TDsgnForm.sorCanMove(Sender: TObject; Control: TControl;
  var CanMove: Boolean; var CanMoveOutParent: Boolean);
begin
  compform.SetControl(Control);
end;

function TDsgnForm.CreateComponent(Sender: TObject; X, Y: Integer): TControl;
var
  comp:TComponent;
  none:Boolean;
  CreateClass:TComponentClass;
  cname:String;
  pInfo: PTypeInfo;
  //----------------------------------------------------------
  function __CreateComp():TComponent;
  begin
    Result := nil;
    if Assigned(CreateClass) then begin
      comp := CreateClass.Create(Self);
      pInfo := comp.ClassInfo;
      cname := pInfo^.Name;
      if (comp is TWinControl) then begin
        TWinControl(comp).ParentWindow := TWinControl(Self).Handle;
      //  if comparetext(TWinControl(comp).ClassName,'TForm') =0  then
        TWinControl(comp).Parent := TWinControl(Self); //else TWinControl(comp).Parent:=DsgnForm;
        TWinControl(comp).Name := cname + IntToStr(_ControlsCreated);
        TWinControl(comp).Left := X;
        TWinControl(comp).Top := Y;
      end
      else if (comp is TControl) then begin
       // if comparetext(TControl(Sender).ClassName,'TForm')=0 then
        Tcontrol(comp).Parent := TWinControl(Self);// else exit;
        TControl(comp).Name := cname + IntToStr(_ControlsCreated);
        TControl(comp).Left := X;
        TControl(comp).Top := Y;
      end;
      _ControlsCreated := _ControlsCreated + 1;
      Result := comp;
    end;
  end;
  //----------------------------------------------------------
  procedure __UpButton();
  begin
    if Assigned(CompForm.ButtonDown) then begin
      TToolBar(CompForm.ButtonDown.Parent).Buttons[0].Down := True;
      CompForm.ButtonDown := nil;
    end;
  end;
  //----------------------------------------------------------
  procedure __TrackEvent();
  begin
    if Assigned(comp) then begin
      THControl(comp).OnMouseDown :=@FormMouseDown;
    end;
  end;
begin
  comp := nil;
  none := False;
  CreateClass := nil;
  if (CompForm.ButtonIndex <> -1) then begin
      CreateClass := CompForm.ControlsClassPStd[CompForm.ButtonIndex];
    comp := __CreateComp();
    __UpButton();
    __TrackEvent();
  end
  else begin
    none := True;
  end;
  if (none) then begin
    if (Sender <> Self) then begin
      sor.SelectControl := TControl(Sender);
    end
    else begin
      sor.Selected := False;
      sor.SelectControl := nil;
    end;
  end
  else begin
    sor.SelectControl := TControl(comp);
  end;
  Result := TControl(comp);
end;

function TDsgnForm.ResolveFileType(AStream: TStream): Integer;
var
  p: PChar;
begin
  Result := 0;
  if not Assigned(AStream) then
    Exit;
  GetMem(p, 10);
  try
    AStream.Position := 0;
    AStream.Read(p[0], 10);
    {bitmap format}
    if (p[0] = #66) and (p[1] = #77) then
      Result := 1;
    {tiff format}
    if ((p[0] = #73) and (p[1] = #73) and (p[2] = #42) and (p[3] = #0)) or
      ((p[0] = #77) and (p[1] = #77) and (p[2] = #42) and (p[3] = #0)) then
      Result := 2;
    {jpg format}
    if (p[6] = #74) and (p[7] = #70) and (p[8] = #73) and (p[9] = #70) then
      Result := 3;
    {png format}
    if (p[0] = #137) and (p[1] = #80) and (p[2] = #78) and (p[3] = #71) and
      (p[4] = #13) and (p[5] = #10) and (p[6] = #26) and (p[7] = #10) then
      Result := 4;
    {dcx format}
    if (p[0] = #177) and (p[1] = #104) and (p[2] = #222) and (p[3] = #58) then
      Result := 5;
    {pcx format}
    if p[0] = #10 then
      Result := 6;
    {emf format}
    if (p[0] = #215) and (p[1] = #205) and (p[2] = #198) and (p[3] = #154) then
      Result := 7;
    {emf format}
    if (p[0] = #1) and (p[1] = #0) and (p[2] = #0) and (p[3] = #0) then
      Result := 7;
  finally
    Freemem(p);
  end;
end;

procedure TDsgnForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_CAPTION or WS_SIZEBOX or WS_SYSMENU;
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TDsgnForm.CreateWnd;
begin
  inherited CreateWnd;
 // SendMessage(Self.Handle, LM_SETICON, 1, 0);
end;

procedure TDsgnForm.DeleteComponent();
begin
  if not assigned(CurComp) then exit else
    begin
      sor.Selected := False;
      sor.SelectControl := nil;
      FreeAndNil(CurComp);
    end;
end;

procedure TDsgnForm.SetMode(i: integer);
begin
  case i of
  0: begin Self.FMode:=i; SetScriptMode; end;
  1: begin exit; end;
  2: begin Self.FMode:=i; SetProgressMode; end;
  end;
end;

function TDsgnForm.GetMode(): integer;
begin
  result:=FMode;
end;

procedure TDsgnForm.Paint;
begin
  inherited Paint;
end;

procedure TDsgnForm.SetScriptMode();
begin
  //Self.BorderStyle:=BsSizeable;
  Self.Width:=320;
  Self.Height:=240;
  Self.Caption:=Self.Name;
end;

procedure TDsgnForm.SetProgressMode();
begin
 // Self.BorderStyle:=BsDialog;
  Self.Width:=775;
  Self.Height:=563;
  Self.Caption:='Progress form design mode (Some settings are not considered)';
end;

procedure TDsgnForm.SetSMARTMode();
begin
 //TO DO
end;

end.

