unit simba.editor_popupmenu;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Menus, Forms, Graphics, LCLType;

type
  TSimbaEditorPopupMenu = class(TPopupMenu)
  protected
    procedure DoPopup(Sender: TObject);
    procedure DoClick(Sender: TObject);
    procedure DoScriptTabChange(Sender: TObject);
    procedure DoMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
  public
    FindDeclaration: TMenuItem;
    Undo: TMenuItem;
    Redo: TMenuItem;
    Cut: TMenuItem;
    Copy: TMenuItem;
    Paste: TMenuItem;
    Delete: TMenuItem;
    SelectAll: TMenuItem;
    CopyFileName: TMenuItem;
    OpenFileDir: TMenuItem;
    Find: TMenuItem;
    Replace: TMenuItem;
    Documentation: TMenuItem;

    constructor Create(AOwner: TComponent); override;
  end;

function GetSimbaEditorPopupMenu: TPopupMenu;

implementation

uses
  simba.main, simba.editor, simba.editor_docgenerator, simba.nativeinterface,
  simba.scripttab, simba.scripttabsform,
  simba.ide_events, simba.ide_utils;

var
  SimbaEditorPopupMenu: TSimbaEditorPopupMenu;

function GetSimbaEditorPopupMenu: TPopupMenu;
begin
  if (SimbaEditorPopupMenu = nil) then
  begin
    SimbaEditorPopupMenu := TSimbaEditorPopupMenu.Create(Application.MainForm);
    SimbaIDEEvents.RegisterMethodOnScriptTabChange(@SimbaEditorPopupMenu.DoScriptTabChange);
  end;

  Result := SimbaEditorPopupMenu;
end;

procedure TSimbaEditorPopupMenu.DoPopup(Sender: TObject);
var
  Editor: TSimbaEditor;
begin
  if (not (PopupComponent is TSimbaEditor)) then
    Close();
  Editor := TSimbaEditor(PopupComponent);

  if (Editor.GetWordAtRowCol(Editor.CaretXY) <> '') then
  begin
    FindDeclaration.Caption := 'Find Declaration of "' + Editor.GetWordAtRowCol(Editor.CaretXY) + '"';
    FindDeclaration.Enabled := True;
  end else
  begin
    FindDeclaration.Caption := 'Find Declaration';
    FindDeclaration.Enabled := False;
  end;

  Undo.Enabled   := Editor.CanUndo;
  Redo.Enabled   := Editor.CanRedo;
  Paste.Enabled  := Editor.CanPaste;
  Cut.Enabled    := Editor.SelAvail;
  Copy.Enabled   := Editor.SelAvail;
  Delete.Enabled := Editor.SelAvail;
end;

procedure TSimbaEditorPopupMenu.DoClick(Sender: TObject);
var
  Editor: TSimbaEditor;
begin
  if (PopupComponent is TSimbaEditor) then
  begin
    Editor := TSimbaEditor(PopupComponent);

    if (Sender = FindDeclaration) then Editor.OnClickLink(Self, mbLeft, [], Editor.CaretX, Editor.CaretY);
    if (Sender = Undo)            then Editor.Undo();
    if (Sender = Redo)            then Editor.Redo();
    if (Sender = Cut)             then Editor.CutToClipboard();
    if (Sender = Copy)            then Editor.CopyToClipboard();
    if (Sender = Paste)           then Editor.PasteFromClipboard();
    if (Sender = Delete)          then Editor.ClearSelection();
    if (Sender = SelectAll)       then Editor.SelectAll();
    if (Sender = Documentation)   then Editor.ExecuteSimpleCommand(TSimbaEditorPlugin_DocGenerator.EditorCommand);
    if (Sender = CopyFileName)    then Editor.DoCopyToClipboard(Editor.FileName);

    if (Sender = Find)            then SimbaScriptTabsForm.Find();
    if (Sender = Replace)         then SimbaScriptTabsForm.Replace();

    if (Sender = OpenFileDir)     then SimbaNativeInterface.OpenDirectory(ExtractFileDir(Editor.FileName));
  end;
end;

procedure TSimbaEditorPopupMenu.DoScriptTabChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    PopupComponent := TSimbaScriptTab(Sender).Editor;
end;

procedure TSimbaEditorPopupMenu.DoMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

constructor TSimbaEditorPopupMenu.Create(AOwner: TComponent);

  function Add(ACaption: String; AImageIndex: Integer; Shortcut: TShortCut; AppendLine: Boolean): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.ImageIndex := AImageIndex;
    Result.OnClick := @DoClick;
    Result.ShortCut := Shortcut;

    Items.Add(Result);

    if AppendLine then
      Items.Add(NewLine());
  end;

begin
  inherited Create(AOwner);

  OnPopup := @DoPopup;
  OnMeasureItem := @DoMeasureItem;

  Images := SimbaForm.Images;

  FindDeclaration     := Add('Find Declaration',          IMG_NONE,       ShortCut(VK_UNKNOWN, []),                True );
  Undo                := Add('Redo',                      IMG_UNDO,       ShortCut(VK_Z,       [ssCtrl]),          False);
  Redo                := Add('Undo',                      IMG_REDO,       ShortCut(VK_Z,       [ssShift, ssCtrl]), True );
  Cut                 := Add('Cut',                       IMG_CUT,        ShortCut(VK_X,       [ssCtrl]),          False);
  Copy                := Add('Copy',                      IMG_COPY,       ShortCut(VK_C,       [ssCtrl]),          False);
  Paste               := Add('Paste',                     IMG_PASTE,      ShortCut(VK_V,       [ssCtrl]),          False);
  Delete              := Add('Delete',                    IMG_CLOSE,      ShortCut(VK_UNKNOWN, []),                False);
  SelectAll           := Add('Select All',                IMG_SELECT_ALL, ShortCut(VK_A,       [ssCtrl]),          True );
  CopyFileName        := Add('Copy Filename',             IMG_NONE,       ShortCut(VK_UNKNOWN, []),                False);
  OpenFileDir         := Add('Open Directory',            IMG_NONE,       ShortCut(VK_UNKNOWN, []),                True );
  Find                := Add('Find',                      IMG_FIND,       ShortCut(VK_F,       [ssCtrl]),          False);
  Replace             := Add('Replace',                   IMG_REPLACE,    ShortCut(VK_R,       [ssCtrl]),          True );
  Documentation       := Add('Add Documentation Comment', IMG_NONE,       ShortCut(VK_D,       [ssCtrl]),          False);
end;

end.

