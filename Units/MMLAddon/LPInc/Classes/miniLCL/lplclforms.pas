unit lplclforms;

{$mode objfpc}{$H+}
{$I Simba.inc}
{$ERROR FIXME: Incorrect...}

interface

uses
  Classes, SysUtils,StdCtrls,Forms,Controls,lpcompiler, lptypes, lpClassHelper;

type
  PForm = ^TForm;
  PCloseAction = ^TCloseAction;
  PCloseEvent = ^TCloseEvent;
  PFormBorderStyle = ^TFormBorderStyle;

procedure RegisterLCLForms(Compiler: TLapeCompiler);

implementation
 uses lplclsystem,lplclgraphics;
{TForm}
//constructor Create(TheOwner: TComponent);
procedure TForm_Init(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^ := TForm.Create(PComponent(Params^[1])^);
end;

//procedure Cascade;
procedure TForm_Cascade(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Cascade();
end;

//procedure Next;
procedure TForm_Next(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Next();
end;

//procedure Previous;
procedure TForm_Previous(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Previous();
end;

//procedure Tile;
procedure TForm_Tile(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Tile();
end;

//procedure Show;
procedure TForm_Show(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Show();
end;

//procedure Close;
procedure TForm_Close(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Close();
end;

//procedure Hide;
procedure TForm_Hide(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Hide();
end;

//Read: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure TForm_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientWidth;
end;

//Write: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure TForm_ClientWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

//Read: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure TForm_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientHeight;
end;

//Write: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure TForm_ClientHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

//Read: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure TForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCloseEvent(Result)^ := PForm(Params^[0])^.OnClose;
end;

//Write: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure TForm_OnClose_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

//Read: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure TForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnCreate;
end;

//Write: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure TForm_OnCreate_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure TForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDestroy;
end;

//Write: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure TForm_OnDestroy_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure TForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure TForm_OnHide_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

//Read: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure TForm_OnPaint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnPaint;
end;

//Write: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure TForm_OnPaint_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read OnShow write OnShow;
procedure TForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read OnShow write OnShow;
procedure TForm_OnShow_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDblClick: TNotifyEvent read OnDblClick write OnDblClick;
procedure TForm_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDblClick;
end;

//Write: property OnDblClick: TNotifyEvent read OnDblClick write OnDblClick;
procedure TForm_OnDblClick_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;


//Read: property OnEnter: TNotifyEvent read OnEnter write OnEnter;
procedure TForm_OnEnter_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnEnter;
end;

//Write: property OnEnter: TNotifyEvent read OnEnter write OnEnter;
procedure TForm_OnEnter_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

//Read: property OnExit: TNotifyEvent read FOnExit write OnExit;
procedure TForm_OnExit_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnExit;
end;

//Write: property OnExit: TNotifyEvent read FOnExit write OnExit;
procedure TForm_OnExit_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

//Read: property OnClick: TNotifyEvent read OnClick write OnClick;
procedure TForm_OnClick_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnClick;
end;

//Write: property OnClick: TNotifyEvent read OnClick write OnClick;
procedure TForm_OnClick_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

//Read: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure TForm_OnResize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnResize;
end;

//Write: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure TForm_OnResize_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure TForm_Enabled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure TForm_Enabled_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Font: TFont read Font write Font;
procedure TForm_Font_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFont(Result)^ := PForm(Params^[0])^.Font;
end;

//Write: property Font: TFont read Font write Font;
procedure TForm_Font_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure TForm_Visible_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure TForm_Visible_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//Read: property Canvas: TCanvas read Canvas write Canvas;
procedure TForm_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PForm(Params^[0])^.Canvas;
end;

//Write: property Canvas: TCanvas read Canvas write Canvas;
procedure TForm_Canvas_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

//Read: property Left: Integer read Left write Left;
procedure TForm_Left_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Left;
end;

//Write: property Left: Integer read Left write Left;
procedure TForm_Left_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Left := PInteger(Params^[1])^;
end;

//Read: property Height: Integer read Height write Height;
procedure TForm_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure TForm_Height_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Top: Integer read Top write Top;
procedure TForm_Top_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Top;
end;

//Write: property Top: Integer read Top write Top;
procedure TForm_Top_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Top := PInteger(Params^[1])^;
end;

//Read: property Width: Integer read Width write Width;
procedure TForm_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Width;
end;

//Write: property Width: Integer read Width write Width;
procedure TForm_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//Read: property Caption: string read Caption write Caption;
procedure TForm_Caption_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PForm(Params^[0])^.Caption;
end;

//Write: property Caption: string read Caption write Caption;
procedure TForm_Caption_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TForm_Free(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Free();
end;

procedure Register_TForm(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TForm', 'Pointer');

    addGlobalFunc('procedure TForm.Init(TheOwner: TComponent);', @TForm_Init);
    addGlobalFunc('procedure TForm.Cascade();', @TForm_Cascade);
    addGlobalFunc('procedure TForm.Next();', @TForm_Next);
    addGlobalFunc('procedure TForm.Previous();', @TForm_Previous);
    addGlobalFunc('procedure TForm.Tile();', @TForm_Tile);
    addGlobalFunc('procedure TForm.Show();', @TForm_Show);
    addGlobalFunc('procedure TForm.Close();', @TForm_Close);
    addGlobalFunc('procedure TForm.Hide();', @TForm_Hide);
    addClassVar(Compiler, 'TForm', 'ClientWidth', 'Integer', @TForm_ClientWidth_Read, @TForm_ClientWidth_Write);
    addClassVar(Compiler, 'TForm', 'ClientHeight', 'Integer', @TForm_ClientHeight_Read, @TForm_ClientHeight_Write);
    addClassVar(Compiler, 'TForm', 'OnClose', 'TCloseEvent', @TForm_OnClose_Read, @TForm_OnClose_Write);
    addClassVar(Compiler, 'TForm', 'OnCreate', 'TNotifyEvent', @TForm_OnCreate_Read, @TForm_OnCreate_Write);
    addClassVar(Compiler, 'TForm', 'OnDestroy', 'TNotifyEvent', @TForm_OnDestroy_Read, @TForm_OnDestroy_Write);
    addClassVar(Compiler, 'TForm', 'OnHide', 'TNotifyEvent', @TForm_OnHide_Read, @TForm_OnHide_Write);
    addClassVar(Compiler, 'TForm', 'OnPaint', 'TNotifyEvent', @TForm_OnPaint_Read, @TForm_OnPaint_Write);
    addClassVar(Compiler, 'TForm', 'OnShow', 'TNotifyEvent', @TForm_OnShow_Read, @TForm_OnShow_Write);
    addClassVar(Compiler, 'TForm', 'OnDblClick', 'TNotifyEvent', @TForm_OnDblClick_Read, @TForm_OnDblClick_Write);
    addClassVar(Compiler, 'TForm', 'OnEnter', 'TNotifyEvent', @TForm_OnEnter_Read, @TForm_OnEnter_Write);
    addClassVar(Compiler, 'TForm', 'OnExit', 'TNotifyEvent', @TForm_OnExit_Read, @TForm_OnExit_Write);
    addClassVar(Compiler, 'TForm', 'OnClick', 'TNotifyEvent', @TForm_OnClick_Read, @TForm_OnClick_Write);
    addClassVar(Compiler, 'TForm', 'OnResize', 'TNotifyEvent', @TForm_OnResize_Read, @TForm_OnResize_Write);
    addClassVar(Compiler, 'TForm', 'Enabled', 'Boolean', @TForm_Enabled_Read, @TForm_Enabled_Write);
    addClassVar(Compiler, 'TForm', 'Font', 'TFont', @TForm_Font_Read, @TForm_Font_Write);
    addClassVar(Compiler, 'TForm', 'Visible', 'Boolean', @TForm_Visible_Read, @TForm_Visible_Write);
    addClassVar(Compiler, 'TForm', 'Canvas', 'TCanvas', @TForm_Canvas_Read, @TForm_Canvas_Write);
    addClassVar(Compiler, 'TForm', 'Left', 'Integer', @TForm_Left_Read, @TForm_Left_Write);
    addClassVar(Compiler, 'TForm', 'Height', 'Integer', @TForm_Height_Read, @TForm_Height_Write);
    addClassVar(Compiler, 'TForm', 'Top', 'Integer', @TForm_Top_Read, @TForm_Top_Write);
    addClassVar(Compiler, 'TForm', 'Width', 'Integer', @TForm_Width_Read, @TForm_Width_Write);
    addClassVar(Compiler, 'TForm', 'Caption', 'string', @TForm_Caption_Read, @TForm_Caption_Write);
    addGlobalFunc('procedure TForm.Free();', @TForm_Free);
  end;
end;

procedure RegisterLCLForms(Compiler: TLapeCompiler);
begin
  with compiler do
    begin
      AddGlobalType('(caNone, caHide, caFree, caMinimize)','TCloseAction');
      AddGlobalType('procedure(Sender: TObject; var CloseAction: TCloseAction)','TCloseEvent');
      AddGlobalType('(bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow,bsSizeToolWin)','TFormBorderStyle');
      Register_TForm(compiler);
    end;
end;

end.

