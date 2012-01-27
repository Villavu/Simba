unit simbasettingssimple;

{$mode objfpc}{$H+}

{

Settings:
    -   Paths:
        Includes
        Plugins
        Fonts
        Extensions
        Scripts

    -   Code Tools:
        -   Automatically show hints
        -   Automatically show completion

    -   Updater:
        -   Check for updates
        -   Check every X minutes
        -   URLs

    -   Tab options:
        -   Open Next on Close
        -   Open Script in new Tab
        -   Check tabs for open script before opening

    -   Colour Picker:
        -   Show history on pick
        -   Add to history on pick

    -   Source Editor
        -   LazColors (boolean)
        -   Default Script (Path)

    -   Interpreter
        -   Lape / PS / others

    -   Tray:
        -   Always Visible

    -   Function List:
        -   ShowOnStart

    -   Show Command Prompt (Windows only)
}

{$I Simba.inc}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TSettingsSimpleForm }

  TSettingsSimpleForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    ImageList1: TImageList;
    SettingsTabsList: TListView;
    PageControl1: TPageControl;
    SettingsTabsPanel: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure CheckGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SettingsTabsListAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure SettingsTabsListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure SettingsTabsListClick(Sender: TObject);
    procedure SettingsTabsListMouseLeave(Sender: TObject);
    procedure SettingsTabsListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SwitchSettingsTab(NewTab: Integer);
    procedure HighlightSettingsTab(NewTab: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SettingsSimpleForm: TSettingsSimpleForm;
  SettingsTabState: Array of Integer;
  ClickSettingsTab: Boolean;

implementation

{$R *.lfm}

{ TSettingsSimpleForm }

procedure TSettingsSimpleForm.CheckGroup1Click(Sender: TObject);
begin

end;

procedure TSettingsSimpleForm.FormCreate(Sender: TObject);
begin
  SetLength(SettingsTabState, SettingsTabsList.Items.Count);
  SettingsTabState[0] := 1;
end;

procedure TSettingsSimpleForm.SwitchSettingsTab(NewTab: Integer);
var i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
    if i = NewTab then
      SettingsTabState[i] := 1
    else
    begin
      SettingsTabState[i] := 0;
    end;

  SettingsTabsList.Refresh;
end;

procedure TSettingsSimpleForm.HighlightSettingsTab(NewTab: Integer);
var i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
  begin
    if SettingsTabState[i] = 1 then
      Continue;
    if i = NewTab then
      SettingsTabState[i] := 2
    else
    begin
      SettingsTabState[i] := 0;
    end;
  end;

  SettingsTabsList.Refresh;
end;



procedure TSettingsSimpleForm.Button1Click(Sender: TObject);
begin

end;

procedure TSettingsSimpleForm.SettingsTabsListAdvancedCustomDraw(Sender: TCustomListView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
   {Sender.Canvas.Rectangle(ARect);
   Sender.Canvas.Brush.Color := clBlack;
   Sender.Canvas.FillRect(ARect);  }
end;

procedure TSettingsSimpleForm.SettingsTabsListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  ItemBoundsRect, IconRect, LabelRect: TRect;
begin

  if stage = cdPostPaint then
  begin

    ItemBoundsRect := item.DisplayRect(drBounds);

    case SettingsTabState[Item.Index] of
        0:  begin
              Sender.Canvas.Brush.Color := clWhite;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
        1:  begin
              Sender.Canvas.Brush.Color := $00EED2C1;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
        2:  begin
              Sender.Canvas.Brush.Color := $00F6E8E0;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
    end;

    IconRect := item.DisplayRect(drIcon);
    LabelRect := item.DisplayRect(drlabel);


    ImageList1.Draw(Sender.Canvas, ItemBoundsRect.Left + (((ItemBoundsRect.Right - ItemBoundsRect.Left) div 2) - 16), IconRect.Top+3, Item.ImageIndex);
    Sender.Canvas.TextOut(LabelRect.Left+2, LabelRect.Top, Item.Caption);


  //drbounds, dricon, drlabel, dr
  end;

end;

procedure TSettingsSimpleForm.SettingsTabsListClick(Sender: TObject);
var
  x, y, i: Integer;
  tmpRect: TRect;
begin
  x := ScreenToClient(Mouse.CursorPos).x;
  y := ScreenToClient(Mouse.CursorPos).y;
  for i := 0 to (SettingsTabsList.Items.Count - 1) do
    begin
      tmpRect := SettingsTabsList.Items.Item[i].DisplayRect(drBounds);
      if (tmpRect.Left <= x) and (x <= tmpRect.Right) and (tmpRect.Top <= y) and (y <= tmpRect.Bottom) then
      begin
        SwitchSettingsTab(i);


        Break;
      end;

    end;
end;

procedure TSettingsSimpleForm.SettingsTabsListMouseLeave(Sender: TObject);
var i: integer;
begin
  for i := 0 to High(SettingsTabState) do
  begin
    if SettingsTabState[i] = 1 then
      Continue;
    SettingsTabState[i] := 0;
  end;

  SettingsTabsList.Repaint;
end;

procedure TSettingsSimpleForm.SettingsTabsListMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  tmpRect: TRect;
  i: Integer;
begin
  for i := 0 to (SettingsTabsList.Items.Count - 1) do
    begin
      tmpRect := SettingsTabsList.Items.Item[i].DisplayRect(drBounds);
      if (tmpRect.Left <= x) and (x <= tmpRect.Right) and (tmpRect.Top <= y) and (y <= tmpRect.Bottom) then
      begin
        HighlightSettingsTab(i);

        Break;
      end;

    end;
end;





end.

