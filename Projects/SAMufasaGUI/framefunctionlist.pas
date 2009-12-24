unit framefunctionlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, StdCtrls, Controls;

type

  { TFunctionListFrame }

  TFunctionListFrame = class(TFrame)
    editSearchList: TEdit;
    FunctionList: TTreeView;
    procedure editSearchListChange(Sender: TObject);
    procedure editSearchListExit(Sender: TObject);
    procedure FunctionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FunctionListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    DraggingNode : TTreeNode;
    { public declarations }
  end; 

implementation

uses
  TestUnit, Graphics;

{ TFunctionListFrame }

procedure TFunctionListFrame.editSearchListChange(Sender: TObject);
var
  I: Integer;
begin
  if(editSearchList.Text = '')then
  begin
    editSearchList.Color := clWhite;
    exit;
  end;
  for I := 0 to FunctionList.Items.Count do //WTF LOOPS FAIL.  for I := 1 to 3 do ;; would make I 4 after a successful, non-brakeing run :<
  begin
    if(I = FunctionList.Items.Count)then break;
    if(pos(lowercase(editSearchList.Text), lowercase(FunctionList.Items[I].Text)) > 0)then
      break;
  end;
  if(I = FunctionList.Items.Count)then
    editSearchList.Color := 6711039
  else
    editSearchList.Color := clWhite;
end;

procedure TFunctionListFrame.editSearchListExit(Sender: TObject);
begin
  editSearchList.Color := clWhite;
end;

procedure TFunctionListFrame.DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  Form1.MenuItemFunctionList.Checked := False;
end;

procedure TFunctionListFrame.FunctionListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   N: TTreeNode;
begin
  N := Self.FunctionList.GetNodeAt(x, y);
  if(N = nil)then
  begin
    Self.DragKind := dkDock;
    Self.BeginDrag(false, 40);
    exit;
  end;
  Self.DragKind := dkDrag;
  if(Button = mbLeft) and (N.Level > 0)then
    Self.BeginDrag(False, 10);
  DraggingNode := N;
end;

procedure TFunctionListFrame.FunctionListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  F: ^TCustomDockForm;
begin
  if(Self.Parent is TCustomDockForm)then
  begin
    F := @Self.Parent; //can't typecast parent as a TCustomDockForm
    F^.Caption := 'Function List';
    F^.BorderStyle := bsSizeable;
    F^.OnClose := @DockFormOnClose;
    Form1.Splitter1.Hide;
  end;
end;

initialization
  {$I framefunctionlist.lrs}

end.

