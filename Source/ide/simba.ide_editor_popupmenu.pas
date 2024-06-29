{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor_popupmenu;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Menus, Graphics;

type
  TSimbaTabPopupMenu = class(TPopupMenu)
  protected
    FFindDecl: TMenuItem;
    FUndo: TMenuItem;
    FRedo: TMenuItem;
    FPaste: TMenuItem;
    FCut: TMenuItem;
    FCopy: TMenuItem;
    FDelete: TMenuItem;
    FCopyFile: TMenuItem;
    FCopyDir: TMenuItem;
    FFind: TMenuItem;
    FReplace: TMenuItem;
    FDocComment: TMenuItem;
    FSelectAll: TMenuItem;
    FCodetoolsSymbols: TMenuItem;

    procedure DoFindDeclaration(Sender: TObject);
    procedure DoOpenFileDir(Sender: TObject);
    procedure DoCopyFileName(Sender: TObject);

    procedure DoUndo(Sender: TObject);
    procedure DoRedo(Sender: TObject);

    procedure DoCut(Sender: TObject);
    procedure DoCopy(Sender: TObject);
    procedure DoPaste(Sender: TObject);

    procedure DoDelete(Sender: TObject);
    procedure DoSelectAll(Sender: TObject);

    procedure DoFind(Sender: TObject);
    procedure DoReplace(Sender: TObject);
    procedure DoDocComment(Sender: TObject);

    procedure DoCodetoolsSymbols(Sender: TObject);

    procedure DoPopup(Sender: TObject); override;
    procedure DoMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  LCLType,
  simba.base, simba.form_main, simba.ide_editor, simba.ide_editor_docgenerator, simba.nativeinterface,
  simba.ide_tab, simba.form_tabs, simba.ide_utils, simba.ide_codetools_debug;

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

procedure TSimbaTabPopupMenu.DoFindDeclaration(Sender: TObject);
begin
  ScriptTab.FindDeclarationAtCaret();
end;

procedure TSimbaTabPopupMenu.DoOpenFileDir(Sender: TObject);
begin
  SimbaNativeInterface.OpenDirectory(ExtractFileDir(ScriptTab.ScriptFileName));
end;

procedure TSimbaTabPopupMenu.DoCopyFileName(Sender: TObject);
begin
  ScriptTab.Editor.DoCopyToClipboard(ScriptTab.ScriptFileName);
end;

procedure TSimbaTabPopupMenu.DoUndo(Sender: TObject);
begin
  ScriptTab.Editor.Undo();
end;

procedure TSimbaTabPopupMenu.DoRedo(Sender: TObject);
begin
  ScriptTab.Editor.Redo();
end;

procedure TSimbaTabPopupMenu.DoCut(Sender: TObject);
begin
  ScriptTab.Editor.CutToClipboard();
end;

procedure TSimbaTabPopupMenu.DoCopy(Sender: TObject);
begin
  ScriptTab.Editor.CopyToClipboard();
end;

procedure TSimbaTabPopupMenu.DoPaste(Sender: TObject);
begin
  ScriptTab.Editor.PasteFromClipboard();
end;

procedure TSimbaTabPopupMenu.DoDelete(Sender: TObject);
begin
  ScriptTab.Editor.ClearSelection();
end;

procedure TSimbaTabPopupMenu.DoSelectAll(Sender: TObject);
begin
  ScriptTab.Editor.SelectAll();
end;

procedure TSimbaTabPopupMenu.DoFind(Sender: TObject);
begin
  SimbaTabsForm.Find();
end;

procedure TSimbaTabPopupMenu.DoReplace(Sender: TObject);
begin
  SimbaTabsForm.Replace();
end;

procedure TSimbaTabPopupMenu.DoDocComment(Sender: TObject);
begin
  ScriptTab.Editor.ExecuteSimpleCommand(TSimbaEditorPlugin_DocGenerator.EditorCommand);
end;

procedure TSimbaTabPopupMenu.DoCodetoolsSymbols(Sender: TObject);
begin
  DebugSymbolTable(ScriptTab.Script, ScriptTab.ScriptFileName);
end;

procedure TSimbaTabPopupMenu.DoPopup(Sender: TObject);
begin
  with ScriptTab.Editor do
  begin
    FFindDecl.Caption := IfThen(GetWordAtRowCol(CaretXY) <> '', 'Find Declaration of "' + GetWordAtRowCol(CaretXY) + '"', 'Find Declaration');
    FFindDecl.Enabled := IfThen(GetWordAtRowCol(CaretXY) <> '', True, False);

    FUndo.Enabled   := CanUndo;
    FRedo.Enabled   := CanRedo;
    FPaste.Enabled  := CanPaste;
    FCut.Enabled    := SelAvail;
    FCopy.Enabled   := SelAvail;
    FDelete.Enabled := SelAvail;
  end;
end;

procedure TSimbaTabPopupMenu.DoMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

constructor TSimbaTabPopupMenu.Create(AOwner: TComponent);

  procedure AddLine;
  begin
    Items.Add(NewLine());
  end;

  function Add(ACaption: String; AImageIndex: Integer; Shortcut: TShortCut; AOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.ImageIndex := AImageIndex;
    Result.OnClick := AOnClick;
    Result.ShortCut := Shortcut;

    Items.Add(Result);
  end;

begin
  inherited Create(AOwner);
  OnMeasureItem := @DoMeasureItem;
  Images := SimbaMainForm.Images;

  FFindDecl   := Add('Find Declaration',          IMG_NONE,       0,                                       @DoFindDeclaration);
                 AddLine();

  FUndo       := Add('Undo',                      IMG_UNDO,       ShortCut(VK_Z,       [ssCtrl]),          @DoUndo);
  FRedo       := Add('Redo',                      IMG_REDO,       ShortCut(VK_Z,       [ssShift, ssCtrl]), @DoRedo);
                 AddLine();

  FCut        := Add('Cut',                       IMG_CUT,        ShortCut(VK_X,       [ssCtrl]),          @DoCut);
  FCopy       := Add('Copy',                      IMG_COPY,       ShortCut(VK_C,       [ssCtrl]),          @DoCopy);
  FPaste      := Add('Paste',                     IMG_PASTE,      ShortCut(VK_V,       [ssCtrl]),          @DoPaste);
  FDelete     := Add('Delete',                    IMG_CLOSE,      ShortCut(VK_UNKNOWN, []),                @DoDelete);
  FSelectAll  := Add('Select All',                IMG_SELECT_ALL, ShortCut(VK_A,       [ssCtrl]),          @DoSelectAll);
                 AddLine();

  FCopyFile   := Add('Copy Filename',             IMG_NONE,       ShortCut(VK_UNKNOWN, []),                @DoCopyFileName);
  FCopyDir    := Add('Open Directory',            IMG_NONE,       ShortCut(VK_UNKNOWN, []),                @DoOpenFileDir);
                 AddLine();

  FFind       := Add('Find',                      IMG_FIND,       ShortCut(VK_F,       [ssCtrl]),          @DoFind);
  FReplace    := Add('Replace',                   IMG_REPLACE,    ShortCut(VK_R,       [ssCtrl]),          @DoReplace);
                 AddLine();

  FDocComment := Add('Add Documentation Comment', IMG_NONE,       ShortCut(VK_D,       [ssCtrl]),          @DoDocComment);

  FCodetoolsSymbols  := Add('View Codetools symbols', IMG_NONE, 0, @DoCodetoolsSymbols);
end;

end.

