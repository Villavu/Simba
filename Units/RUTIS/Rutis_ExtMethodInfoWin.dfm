object FExtMethodListInfo: TFExtMethodListInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'EXT Methods Info'
  ClientHeight = 305
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ExtList: TListBox
    Left = 0
    Top = 25
    Width = 526
    Height = 280
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnDblClick = ExtListDblClick
    OnMouseMove = ExtListMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 526
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object BtnSortDefault: TButton
      Left = 0
      Top = 1
      Width = 80
      Height = 25
      Caption = 'Default'
      TabOrder = 1
      OnClick = BtnSortDefaultClick
    end
    object BtnSortAlphabetical: TButton
      Left = 80
      Top = 1
      Width = 80
      Height = 25
      Caption = 'Alphabetical'
      TabOrder = 2
      OnClick = BtnSortAlphabeticalClick
    end
    object EdSearch: TEdit
      Left = 165
      Top = 3
      Width = 357
      Height = 21
      TabOrder = 0
      OnChange = EdSearchChange
      OnMouseDown = EdSearchMouseDown
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 208
    Top = 120
    object Insertintocode1: TMenuItem
      Caption = 'Insert into code'
      OnClick = Insertintocode1Click
    end
  end
end
