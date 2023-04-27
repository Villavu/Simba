unit simba.editor_popupmenu;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Menus, Forms, LCLType;

type
  TSimbaEditorPopupMenu = class(TPopupMenu)
  protected
    procedure DoPopup(Sender: TObject);
    procedure DoClick(Sender: TObject);
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
  simba.main, simba.editor, simba.editor_docgenerator, simba.nativeinterface, simba.scripttabsform;

var
  SimbaEditorPopupMenu: TSimbaEditorPopupMenu;

function GetSimbaEditorPopupMenu: TPopupMenu;
begin
  if (SimbaEditorPopupMenu = nil) then
    SimbaEditorPopupMenu := TSimbaEditorPopupMenu.Create(Application.MainForm);

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

  Images := SimbaForm.Images;

  FindDeclaration     := Add('Find Declaration',     IMAGE_NONE,   ShortCut(VK_UNKNOWN, []),                True );
  Undo                := Add('Redo',                 IMAGE_UNDO,   ShortCut(VK_Z,       [ssCtrl]),          False);
  Redo                := Add('Undo',                 IMAGE_REDO,   ShortCut(VK_Z,       [ssShift, ssCtrl]), True );
  Cut                 := Add('Cut',                  IMAGE_CUT,    ShortCut(VK_X,       [ssCtrl]),          False);
  Copy                := Add('Copy',                 IMAGE_COPY,   ShortCut(VK_C,       [ssCtrl]),          False);
  Paste               := Add('Paste',                IMAGE_PASTE,  ShortCut(VK_V,       [ssCtrl]),          False);
  Delete              := Add('Delete',               IMAGE_NONE,   ShortCut(VK_UNKNOWN, []),                False);
  SelectAll           := Add('Select All',           IMAGE_NONE,   ShortCut(VK_A,       [ssCtrl]),          True );
  CopyFileName        := Add('Copy Filename',        IMAGE_NONE,   ShortCut(VK_UNKNOWN, []),                False);
  OpenFileDir         := Add('Open Directory',       IMAGE_NONE,   ShortCut(VK_UNKNOWN, []),                True );
  Find                := Add('Find',                 IMAGE_SEARCH, ShortCut(VK_F,       [ssCtrl]),          False);
  Replace             := Add('Replace',              IMAGE_NONE,   ShortCut(VK_R,       [ssCtrl]),          True );
  Documentation       := Add('Insert Documentation', IMAGE_FONT,   ShortCut(VK_D,       [ssCtrl]),          False);
end;

end.

