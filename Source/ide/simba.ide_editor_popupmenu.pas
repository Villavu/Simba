{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor_popupmenu;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Menus, Graphics,
  simba.base,
  simba.ide_events;

type
  TSimbaTabPopupMenu = class(TPopupMenu)
  protected
    FFindDecl: TMenuItem;
    FUndo: TMenuItem;
    FRedo: TMenuItem;
    FPaste: TMenuItem;
    FCut: TMenuItem;
    FCopy: TMenuItem;
    FCopyFile: TMenuItem;
    FCopyDir: TMenuItem;
    FFind: TMenuItem;
    FReplace: TMenuItem;
    FDocComment: TMenuItem;
    FSelectAll: TMenuItem;

    procedure DoClick(Sender: TObject);

    procedure DoCodetoolsSymbols(Sender: TObject);
    procedure DoCodetoolsCache(Sender: TObject);

    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  LCLType,
  simba.component_images,
  simba.ide_editor, simba.nativeinterface,
  simba.ide_tab, simba.form_scripttabs, simba.ide_utils, simba.ide_codetools_debug,
  simba.ide_editor_commands;

type
  TSimbaTabPopupMenuHelper = class helper for TSimbaTabPopupMenu
    function ScriptTab: TSimbaScriptTab;
  end;

function TSimbaTabPopupMenuHelper.ScriptTab: TSimbaScriptTab;
begin
  if (not (Owner is TSimbaScriptTab)) then
    SimbaException('TSimbaTabPopupMenu.ScriptTab: Invalid owner');

  Result := TSimbaScriptTab(Owner);
end;

procedure TSimbaTabPopupMenu.DoClick(Sender: TObject);
begin
  SimbaEvents.Post(ESimbaEvent(TComponent(Sender).Tag), Owner);
end;

procedure TSimbaTabPopupMenu.DoCodetoolsSymbols(Sender: TObject);
begin
  DebugSymbolTable(ScriptTab.Script, ScriptTab.ScriptFileName);
end;

procedure TSimbaTabPopupMenu.DoCodetoolsCache(Sender: TObject);
begin
  DebugCache();
end;

procedure TSimbaTabPopupMenu.DoPopup(Sender: TObject);
begin
  with TSimbaScriptTab(Owner).Editor do
  begin
    FFindDecl.Caption := IfThen(GetWordAtRowCol(CaretXY) <> '', 'Find Declaration of "' + GetWordAtRowCol(CaretXY) + '"', 'Find Declaration');
    FFindDecl.Enabled := IfThen(GetWordAtRowCol(CaretXY) <> '', True, False);

    FUndo.Enabled   := CanUndo;
    FRedo.Enabled   := CanRedo;
    FPaste.Enabled  := CanPaste;
    FCut.Enabled    := SelAvail;
    FCopy.Enabled   := SelAvail;
  end;
end;

constructor TSimbaTabPopupMenu.Create(AOwner: TComponent);

  procedure AddLine;
  begin
    Items.Add(NewLine());
  end;

  function Add(ACaption: String; AImageIndex: Integer; Shortcut: TShortCut; Event: ESimbaEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.ImageIndex := AImageIndex;
    Result.Tag := Ord(Event);
    Result.ShortCut := Shortcut;
    Result.OnClick := @DoClick;

    Items.Add(Result);
  end;

begin
  inherited Create(AOwner);

  Images := SimbaImages;

  FFindDecl := Add('Find Declaration', -1, scNone, ESimbaEvent.ACTION_FIND_DECL_AT_CARET);
  AddLine();

  FUndo := Add('Undo', SimbaImages.UNDO, ShortCut(VK_Z, [ssCtrl]),          ESimbaEvent.ACTION_UNDO);
  FRedo := Add('Redo', SimbaImages.REDO, ShortCut(VK_Z, [ssShift, ssCtrl]), ESimbaEvent.ACTION_REDO);
  AddLine();

  FCut := Add('Cut', SimbaImages.CUT, ShortCut(VK_X, [ssCtrl]), ESimbaEvent.ACTION_CUT);
  FCopy := Add('Copy', SimbaImages.COPY, ShortCut(VK_C, [ssCtrl]), ESimbaEvent.ACTION_COPY);
  FPaste := Add('Paste', SimbaImages.PASTE, ShortCut(VK_V, [ssCtrl]), ESimbaEvent.ACTION_PASTE);
  FSelectAll := Add('Select All', SimbaImages.SELECT_ALL, ShortCut(VK_A, [ssCtrl]), ESimbaEvent.ACTION_SELECT_ALL);
  AddLine();

  FCopyFile := Add('Copy Filename', -1, scNone, ESimbaEvent.ACTION_COPY_FILENAME);
  FCopyDir := Add('Open Directory', -1, scNone, ESimbaEvent.ACTION_OPEN_DIRECTORY);
  AddLine();

  FFind := Add('Find', SimbaImages.FIND, ShortCut(VK_F, [ssCtrl]), ESimbaEvent.ACTION_FIND);
  FReplace := Add('Replace', SimbaImages.FIND_REPLACE, ShortCut(VK_R, [ssCtrl]), ESimbaEvent.ACTION_REPLACE);
  AddLine();

  FDocComment := Add('Add Documentation Comment', -1, ShortCut(VK_D, [ssCtrl]), ESimbaEvent.ACTION_DOC_COMMENT);
  //AddLine();

  //Add('Codetools Symbols', -1, 0, @DoCodetoolsSymbols);
  //Add('Codetools Cache', -1, 0, @DoCodetoolsCache);
end;

end.

