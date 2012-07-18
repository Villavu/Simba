unit design_frm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, LCLType, Dialogs,
  TypInfo, Math, cselectonruntime, Messages, ComCtrls, ExtCtrls, Menus;

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
    function CreateComponent(Sender: TObject; X, Y: Integer):TControl;
    //procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure Paint; override;
  public

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
{ TDsgnForm }

procedure TDsgnForm.FormCreate(Sender: TObject);
begin
  compForm.SetControl(sender);
  imgdialog:=TOpenDialog.Create(self);
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
begin
  imgdialog.Filter:='Bitmap files only|*.bmp';
  if imgdialog.Execute then
   begin
   pathtoimg:=imgdialog.FileName;
   TImage(CurComp).AutoSize:=true;
   TImage(CurComp).Picture.LoadFromFile(pathtoimg);
   TImage(CurComp).Tag:=LongInt(PString(pathtoimg));
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
        TWinControl(comp).ParentWindow := TWinControl(Sender).Handle;
        TWinControl(comp).Parent := TWinControl(Sender);
        TWinControl(comp).Name := cname + IntToStr(_ControlsCreated);
        TWinControl(comp).Left := X;
        TWinControl(comp).Top := Y;
      end
      else if (comp is TControl) then begin
        TControl(comp).Parent := TWinControl(Sender);
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

procedure TDsgnForm.Paint;
begin
  inherited Paint;
end;

end.

