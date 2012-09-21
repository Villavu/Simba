unit cSelectOnRunTime;
{
TSelectOnRunTime  Component Version 1.4 - Suite GLib
Copyright (©) 2005,  by Germбn Estйvez (Neftalн)

  Permite seleccionar componentes visuales que haya en un form de forma visual
  como se hace con las imбgenes en los programas de diseсo o con los controles
  en el IDE de Delphi. Ademбs se pueden mover y redimensionar los controles
  seleccionados
  Basta con soltar el control en el formulario, asignarle el control que se desea
  seleccionar/mover/redimensionar y activarlo.
  Se pueden distinguir quй controles se seleccionan utilizando la propiedad
  SelectTag/SelectTaggedControls.

=========================================================================
IMPORTANTE PROGRAMADORES: Por favor, si tienes comentarios, mejoras, ampliaciones,
  errores y/o cualquier otro tipo de sugerencia envнame un mail a:
  german_ral@hotmail.com

IMPORTANT PROGRAMMERS: please, if you have comments, improvements, enlargements,
errors and/or any another type of suggestion send a mail to:
german_ral@hotmail.com
=========================================================================

@author Germбn Estйvez (Neftalн)
@cat Package GLib
ported for lazarus by Cynic
}
{
=========================================================================

  CSelectOnRunTime.pas

  Componente

========================================================================
  Historia de las Versiones
------------------------------------------------------------------------
  09/07/2012 (v.1.6a) Ported for lazarus by Cynic
  26/10/2010 (v.1.5b)
    * (Thanks to Bill) Corrected bug on positioning Selection panels on
      runtime creted components.

  20/06/2007  (v.1.4)
    * Corregidos problemas con controles que estбn alineados alClient, AlTop,...
    * Modificada la posiciуn donde se colocan las marcas.
  10/09/2006
	  * Corregidos problemas al mover componentes que estбn colocados dentro
		  de otros.
	  * Corregido bug al pintar las marcas de selecciуn cuando un control estб
		  dentro de otro.
	  * Bugs varios.
  24/03/2006
    * Aсadidas propiedades nuevas para definir quй controles se seleccionan o no.
    * SelectTaggedControls y SelectTag (thks. Jose)
    * Corregido buq en la propiedad Selected cuando se asignaba por cуdigo.
    * En diseсo las marcas no deben ser visibles.
  17/03/2006
    * Aсadido parйmetro CanMoveOutParent a los eventos de movimmiento para
      restringir el movimiento de un contro fuera de su padre.
  02/11/2005
    * Revisiуn de los mйtodos de selecciуn.
    * Ideas: Delphi About
    * Propiedad de color.
  03/10/2005
    * Implementado el movimiento de los controles y de las marcas de selecciуn.
  30/09/2005
    * Creaciуn.

=========================================================================

  Errores detectados no corregidos

=========================================================================
}
//{$mode objfpc}{$H+}
{$MODE Delphi}
interface

uses
  Controls, LCLIntf, LCLType, SysUtils, Classes, ExtCtrls, Graphics, Contnrs;

type

  //: Ampliaciуn de la clase.
  TExtControl = class(TControl);

  {: Tipo definido para el evento OnBeforeSelect.}
  TBeforeSelectEvent = procedure(Sender: TObject; Selected:TControl;
                                 var Select:Boolean) of object;
  {: Tipo definido para el evento CanMove. }
  TCanMoveEvent = procedure (Sender: TObject; Control:TControl;
                             var CanMove: Boolean;
                             var CanMoveOutParent:Boolean) of object;

  {: Tipo definido para el evento de redimensionar}
  TCanResizeControlEvent = procedure(Sender: TObject; var CanResize: Boolean) of object;

  {: Clase para definir el componente de seleccion.}
  TSelectOnRunTime = class(TComponent)
  private
    _InReposition: boolean;       // Se estб recolocando un control
    _NodePositioning: Boolean;    // Si se estб recolocando una marca
    _OldPos: TPoint;              // posicion anterior
    _Nodes: TObjectList;          // Lista de marcas
    _CurrentNodeControl: TControl;   // Control actual
    _Owner: TComponent;                 // El formulario que contiene el control
    _OldMouseDown, _OldMouseUp:TMouseEvent;
    _OldMouseMove:TMouseMoveEvent;
    _OldResize: TNotifyEvent;
    _CapturedEvents:Boolean;            // Si estбn capturados ya los eventos del control
    _OldOwnerResize: TNotifyEvent;   // OnResize del Owner

    FSelected:Boolean;
    FSelectControl:TControl;
    FActive:Boolean;
    FMarkColor: TColor;
    FMarkers3D: Boolean;
    FOnBeforeSelect: TBeforeSelectEvent;
    FOnCanMove: TCanMoveEvent;
    FOnCanResize: TCanResizeControlEvent;
    FSelectTaggedControls: Boolean;
    FSelectTag: Integer;

    // Eventos al seleccionar un control.
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlResize (Sender: TObject);
    // procedimientos para los eventos al seleccionar una marca
    procedure NodeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // Onresize del Owner
    procedure OwnerResize(Sender: TObject);

    // Capturar los eventos del control.
    procedure _CaptureEvents();
    // liberar los eventos del control (restaurarlos anteriores)
    procedure _FreeEvents();

    procedure SetActive(const Value: Boolean);
    procedure SetSelectControl(Value: TControl);
    //: Colocar las marcas de seleccion segun el control.
    procedure PositionNodes(AroundControl: TControl; AVisible:Boolean=True);
    //: Visualizar/Ocultar las marcas.
    procedure NodesVisible(AVisible: Boolean);
    //: Crea los nodos que visalizan la selecciуn.
    procedure CreateNodes;

    procedure SetMarkColor(const Value: TColor);
    procedure SetMarkers3D(const Value: Boolean);
    procedure SetSelected(const Value: Boolean);

  protected


  public
    // Redefnimos el constructor.
    constructor Create(AOwner: TComponent); override;
    // Redefnimos el destructor.
    destructor Destroy; override;
    //: Define si hay algo seleccionado o no.
    property Selected:Boolean read FSelected write SetSelected;

  published
    //: Control que vamos a seleccionar
    property SelectControl:TControl read FSelectControl write SetSelectControl;
    //: Activar el componente
    property Active:Boolean read FActive write SetActive;
    //: Propiedades del pintado de las markas
    property MarkColor:TColor read FMarkColor write SetMarkColor;
    //: Si las marcas las queremos en 3D
    property Markers3D: Boolean read FMarkers3D write SetMarkers3D;
    //: Activa que sуlo se seleccionen los controles con un determinado TAG
    property SelectTaggedControls:Boolean read FSelectTaggedControls
        write FSelectTaggedControls default False;
    // TAG para los controles a seleccionar (depende de la prop. SelectTaggedControls)
    property SelectTag:Integer read FSelectTag write FSelectTag default 999;

    // EVENTOS
    // =========================================================================
    property OnBeforeSelect: TBeforeSelectEvent read FOnBeforeSelect write FOnBeforeSelect;
    property OnCanMove: TCanMoveEvent read FOnCanMove write FOnCanMove;
    property OnCanResize:TCanResizeControlEvent read FOnCanResize write FOnCanResize;

  end;

// constantes
const
  MARK_WIDTH = 4;


// procedimiento de registro del componente.
procedure Register;

//=========================================================================
//
// I M P L E M E N T A T I O N
//
//=========================================================================
implementation

uses
  Forms, Dialogs;

// procedimiento de registro del componente.
procedure Register;
begin
  RegisterComponents('Glib', [TSelectOnRunTime]);
end;


{ TSelectOnRunTime }
{============================================================================}


//: Construictor de la clase
procedure TSelectOnRunTime.ControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  _CanSelect:Boolean;
begin

  if (Self.FActive) AND (Sender is TControl) then begin

    _CanSelect := True;
    // Asignado el evento antes de asignar?
    if Assigned(Self.FOnBeforeSelect) then begin
      Self.FOnBeforeSelect(Self, TControl(Sender), _CanSelect);
    end;

    // No se debe seleccionar o estamos en modo TagControl y no coincide
    if (not _CanSelect) or
       (Self.FSelectTaggedControls) and (TControl(Sender).Tag <> FSelectTag) then begin
      Exit;
    end;

    Self._InReposition:=True;
    Self.FSelected := True;

    // Es un TWinControl?
    if (Sender is TWinControl) then begin
      SetCapture(TWinControl(Sender).Handle);
      Self.FSelectControl := TWinControl(Sender);
    end;
    GetCursorPos(_OldPos);
    // Colocar las marcas segun el control
    PositionNodes(TControl(Sender));
  end;
end;

procedure TSelectOnRunTime.ControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  minWidth = 20;
  minHeight = 20;
var
  newPos: TPoint;
  frmPoint : TPoint;
  _CanMove, _CanMoveOutParent:Boolean;
  newLeft, newTop, newRight, newBottom:Integer;
  parentRight, parentBootom:Integer;
begin
  // Se estб reposicionando?
  if (Self._InReposition) then begin

    _CanMove := True;
    _CanMoveOutParent := False;

    // Estб asignado el evento
    if Assigned(Self.FOnCanMove) then begin
      Self.FOnCanMove(Self, TControl(Sender), _CanMove, _CanMoveOutParent);
    end;
    // No se puede redimensionar?
    if not(_CanMove) then begin
      Exit;
    end;

    // El control es correcto?
    with TControl(Sender) do begin
      // posicion
      GetCursorPos(newPos);
      // Modificar el cuursor
      Screen.Cursor := crSize;

      // Nueva posicion
      newLeft := Left - _OldPos.X + newPos.X;
      newTop := Top - _OldPos.Y + newPos.Y;
      newRight := newLeft + Width;
      newBottom := newTop + Height;

      if Assigned(TControl(Sender).Parent) then begin
        parentRight := TControl(Sender).Parent.Width;
        parentBootom := TControl(Sender).Parent.Height;
      end;

      // Estб fuera de lнmites y no se puede (Left)
      if ((newLeft < 0) and (_CanMoveOutParent)) or (newLeft >= 0) then begin
        // Posicion
        Left := newLeft;
      end;

      // Estб fuera de lнmites y no se puede (Right)
      if ((newRight > parentRight) and (_CanMoveOutParent)) or (newRight <= parentRight) then begin
        // Posicion
        Left := newLeft;
      end;

      // Estб fuera de lнmites y no se puede (Bottom)
      if ((newBottom > parentBootom) and (_CanMoveOutParent)) or (newBottom <= parentBootom) then begin
        Top := newTop;
      end;

      // Estб fuera de lнmites y no se puede (Top)
      if ((newTop < 0) and (_CanMoveOutParent)) or (newTop >= 0) then begin
        Top := newTop;
      end;

      _OldPos := newPos;
    end;
    // Reporsicionar las marcas
    PositionNodes(TControl(Sender));
  end;

  // Seleccionado?  ==> Cambiar el Cursor
  if FSelected then begin
    TControl(Sender).Cursor := crSize;
  end;

end;

procedure TSelectOnRunTime.ControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Recolocando?
  if (Self._InReposition) then begin
    // Dejar el cursor como estaba
    Screen.Cursor := crDefault;
    // Liberar
    ReleaseCapture;
    // Dejar de recolocar
    Self._InReposition := False;
  end;

end;


procedure TSelectOnRunTime.ControlResize(Sender: TObject);
begin
  // Reporsicionar las marcas
  PositionNodes(TControl(Sender));
end;

constructor TSelectOnRunTime.Create(AOwner: TComponent);
begin

  // Mйtodo heredado
  inherited;
  // Owner del control
  Self._Owner := AOwner;

  // Capturar el evento OnResize del owner
  if (Self._Owner is TControl) then begin
    // Nos quedamos con la antigua
    _OldOwnerResize := TExtControl(Self._Owner).OnResize;
    // La nueva
    TExtControl(Self._Owner).OnResize := OwnerResize;
  end;

  // otras propiedades por defecto
  Self.FMarkColor := clBlack;
  Self.FMarkers3D := False;
  Self._CapturedEvents := False;
  Self.FSelectTag := 9999;

  // Version
  // Self.FVersion := 'v1.4 - by Germбn Estйvez';

  // Lista de nodos
  _Nodes := TObjectList.Create(False);
  // Crearlos
  CreateNodes;
end;

//: destructor de la clase;
procedure TSelectOnRunTime.CreateNodes();
var
  Node: Integer;
  Panel: TPanel;
begin
  // Para las 8 Marcas.
  for Node := 0 to 7 do  begin
    // Crear un panel
    Panel := TPanel.Create(nil);
    // Aсadirlo
    _Nodes.Add(Panel);
    // Para cada uno
    with Panel do  begin

      // No marcas en 3D?
      if not (Self.FMarkers3D) then begin
        BevelOuter := bvNone;
      end;
      // Otras
      Color := FMarkColor;
      Brush.Color := FMarkColor;
      Name := 'Node' + IntToStr(Node);
      Width := MARK_WIDTH{4}{5};
      Height := MARK_WIDTH{4}{5};
      Parent := TWinControl(Self._Owner);
      Visible := False;

      // Segun el nodo
      case Node of
        0,4: Cursor := crSizeNWSE;
        1,5: Cursor := crSizeNS;
        2,6: Cursor := crSizeNESW;
        3,7: Cursor := crSizeWE;
      end;
      // Asignar eventos
      OnMouseDown := NodeMouseDown;
      OnMouseMove := NodeMouseMove;
      OnMouseUp := NodeMouseUp;
    end;
  end;

end;

//: Destructor de la clase
destructor TSelectOnRunTime.Destroy;
begin

  // Liberar objetos creados
  _Nodes.Free;
  // mйtopdo heredado
  inherited;

end;

//: Mйtodo de acceso apare escritura a la prop. active.
procedure TSelectOnRunTime.NodeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Estб activo y el Sender es correcto
  if (Self.FActive) AND (Sender is TControl) then begin
    // Reposicionando nodo
    Self._NodePositioning:=True;
    // Es un TWinControl?
    if (Sender is TWinControl) then begin
      // Capturar el control
      SetCapture(TWinControl(Sender).Handle);
    end;
    // Gusrdar la posicion
    GetCursorPos(_OldPos);
  end;
end;

procedure TSelectOnRunTime.NodeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  minWidth = 20;
  minHeight = 20;
var
  newPos: TPoint;
  frmPoint : TPoint;
  OldRect: TRect;
  AdjL,AdjR,AdjT,AdjB: Boolean;
  _CanResize:Boolean;
begin
  // Se estб reposicionando un nodo?
  if Self._NodePositioning then begin

    _CanResize := True;
    // Estб asignado el evento
    if Assigned(Self.FOnCanResize) then begin
      Self.FOnCanResize(Sender, _CanResize);
    end;
    // No se puede redimensionar?
    if not(_CanResize) then begin
      Exit;
    end;

    Application.ProcessMessages;
    begin
      with TControl(Sender) do
      begin
      GetCursorPos(newPos);
      with Self._CurrentNodeControl do  begin
        //resize
        frmPoint := Self._CurrentNodeControl.Parent.ScreenToClient(Mouse.CursorPos);
        OldRect := Self._CurrentNodeControl.BoundsRect;
        AdjL := False;
        AdjR := False;
        AdjT := False;
        AdjB := False;
        case Self._Nodes.IndexOf(TControl(Sender)) of
          0: begin
               AdjL := True;
               AdjT := True;
             end;
          1: begin
               AdjT := True;
             end;
          2: begin
               AdjR := True;
               AdjT := True;
             end;
          3: begin
               AdjR := True;
             end;
          4: begin
               AdjR := True;
               AdjB := True;
             end;
          5: begin
               AdjB := True;
             end;
          6: begin
               AdjL := True;
               AdjB := True;
             end;
          7: begin
               AdjL := True;
             end;
        end;

        if AdjL then
          OldRect.Left := frmPoint.X;
        if AdjR then
          OldRect.Right := frmPoint.X;
        if AdjT then
          OldRect.Top := frmPoint.Y;
        if AdjB then
          OldRect.Bottom := frmPoint.Y;



        SetBounds(OldRect.Left,
                  OldRect.Top,
                  OldRect.Right - OldRect.Left,
                  OldRect.Bottom - OldRect.Top);
      end;
      Left := Left - _OldPos.X + newPos.X;
      Top := Top - _OldPos.Y + newPos.Y;
      _OldPos := newPos;
      end;
    end;
    PositionNodes(Self._CurrentNodeControl);
  end;

end;

procedure TSelectOnRunTime.NodeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Recolocando nodo?
  if (Self._NodePositioning) then begin
    // Cursor estandard
    Screen.Cursor := crDefault;
    // Liberar captura
    ReleaseCapture;
    // poner a false
    Self._NodePositioning := False;
  end;
end;

//: Colocar las marcas de seleccion segun el control.
procedure TSelectOnRunTime.PositionNodes(AroundControl: TControl; AVisible:Boolean=True);
var
  Node,T,L,CT,CL,FR,FB,FT,FL: Integer;
  TopLeft: TPoint;
begin
  // Inicializar
  Self._CurrentNodeControl := nil;
  // Para cada nodo
  for Node := 0 to 7 do  begin
    // Con el control seleccionado
    with AroundControl do
    begin
      CL := (Width div 2) + Left - (MARK_WIDTH div 2);
      CT := (Height div 2) + Top - (MARK_WIDTH div 2);
      FR := Left + Width - MARK_WIDTH - 2;
      FB := Top + Height - MARK_WIDTH - 2;
      FT := Top{ + 2} + 2;
      FL := Left{ - 2} + 2;
      case Node of
        0: begin
             T := FT;      // ARR-IZQ
             L := FL;
           end;
        1: begin
             T := FT;      // ARR-CENTR
             L := CL;
           end;
        2: begin
             T := FT;       // ARR-DER
             L := FR;
           end;
        3: begin
             T := CT;       // CENT -DER
             L := FR;
           end;
        4: begin
             T := FB;
             L := FR;
           end;
        5: begin
             T := FB;
             L := CL;
           end;
        6: begin
             T := FB;
             L := FL;
           end;
        7: begin
             T := CT;
             L := FL;
           end;
        else
          T := 0;
          L := 0;
      end;
      TopLeft := Parent.ClientToScreen(Point(L,T));
    end;
    // Para cada marca
    with TPanel(Self._Nodes[Node]) do
    begin
      TopLeft := Parent.ScreenToClient(TopLeft);
      Top := TopLeft.Y;
      Left := TopLeft.X;
      BringToFront;
    end;
  end;
  // Control actual
  Self._CurrentNodeControl := AroundControl;

  // Visualizar?
  if (AVisible) then begin
    NodesVisible(True);
  end;
end;

//: Procedimiento de escritura a la propiedad.
procedure TSelectOnRunTime.SetSelected(const Value: Boolean);
begin
  // Asignamos el valor
  Self.FSelected := Value and Assigned(Self.FSelectControl);

  // desactivar
  if not (Value) then begin
    // Asignado?
    if Assigned(Self.FSelectControl) then begin
      Self.FSelectControl.Cursor := crDefault;
    end;
    Self.SelectControl := nil;
    Exit;
  end
  else begin
    // Asignado?
    if Assigned(Self.FSelectControl) then begin
      PositionNodes(TControl(Self.FSelectControl));
    end;
  end;

  // Ocular nodos
  Self.NodesVisible(Self.FSelected);

end;

//: Procedimiento de escritura a la propiedad.
procedure TSelectOnRunTime.SetActive(const Value: Boolean);
var
  i:Integer;
begin

  // No ha cambiado?
  if (Self.FActive = Value) then begin
    Exit;
  end;

  // Activar?
  if (Value) then begin
    // Se va a seleccionar uno?
    if Assigned(Self.FSelectControl) and not (_CapturedEvents) then begin
      Self._CaptureEvents();
    end;
  end
  else begin
    // Se va a seleccionar uno?
    if Assigned(Self.FSelectControl) and (_CapturedEvents) then begin
      Self._FreeEvents();
    end;
  end;

  // Activar/desactivar
  Self.FActive := Value;
  // Deselecconar
  Self.Selected := False;

end;



//: Mйtodo de escritura de la propiedad SelectControl.
procedure TSelectOnRunTime.SetMarkColor(const Value: TColor);
var
  Node: Integer;
begin
  FMarkColor := Value;
  for Node := 0 to 7 do
    TPanel(Self._Nodes.Items[Node]).Color := Value;
end;

//: Visualizar/Ocultar las marcas.
procedure TSelectOnRunTime.NodesVisible(AVisible: Boolean);
var
  Node: Integer;
  wc:TWinControl;
  repos:Boolean;
begin

  // Destruyendo el componente?
  if (csDestroying in Self.ComponentState) then begin
    Exit;
  end;

  // Visible?
  if (AVisible) then begin
    // Hay que cambiar el PArent
    if Assigned(Self.SelectControl) then begin
      // Parent
      wc := Self.SelectControl.Parent;
      // Asignado?
      if Assigned(wc) then begin
        // Es Diferente?
        if (TControl(Self._Nodes.Items[0]).Parent <> wc) then begin
          repos := True;
        end;
      end;
    end;
  end;

  // Para cada marca.
  for Node := 0 to 7 do begin
    // Visible y no en diseсo
    TControl(Self._Nodes.Items[Node]).Visible :=
      AVisible{ and (not (csDesigning in Self.ComponentState))};
    if (repos) then begin
      TControl(Self._Nodes.Items[Node]).Parent := wc;
    end;

    // No visible?
    if (not AVisible) then begin
      TControl(Self._Nodes.Items[Node]).Left := 0;
      TControl(Self._Nodes.Items[Node]).Top := 0;
    end;
  end;

  // reposicionar?
  if (repos) then begin
    PositionNodes(Self.SelectControl, False);
  end;

end;

procedure TSelectOnRunTime.SetSelectControl(Value: TControl);
var
  _CanSelect:Boolean;
begin

  // Es un form, frame o el Owner
  if (Value is TForm) or (Value is TFrame) or (Value = Self.Owner) then begin
    Self.SelectControl := nil;
    Exit;
  end;

  // No ha cambiado?
  if (Self.FSelectControl = Value) then begin
    Exit;
  end;

  // En diseсo?
  if (csDesigning in Self.ComponentState) then begin
    // Asignar
    Self.FSelectControl := Value;
    NodesVisible(False);
    Exit;
  end;

  // Se pasa a nil
  if not Assigned(Value) then begin
    Self.FSelectControl.Cursor := crDefault;
    // Estбn capturados los eventos y activo
    if (_CapturedEvents) and (Self.FActive) then begin
      Self._FreeEvents();
    end;
  end
  else begin
    _CanSelect := True;
    // Asignado el evento antes de asignar?
    if Assigned(Self.FOnBeforeSelect) then begin
      Self.FOnBeforeSelect(Self, Value, _CanSelect);
    end;

    // No se debe seleccionar o estamos en modo TagControl y no coincide
    if (not _CanSelect) or
       (Self.FSelectTaggedControls) and (Value.Tag <> FSelectTag) then begin
      // Actualizar Selected
      Self.Selected := False;
      Exit;
    end;
  end;

  // Asignar
  Self.FSelectControl := Value;
  // Actualizar Selected
  Self.Selected := Assigned(Value);

  // Se va a seleccionar uno?
  if Assigned(Value) and (Self.FActive) then begin
    Self._CaptureEvents();
  end;
end;

// "desenganchar" los eventos del control
procedure TSelectOnRunTime._FreeEvents();
begin
  _CapturedEvents := False;
  // Restaurar los eventos
  TExtControl(Self.FSelectControl).OnMouseDown := _OldMouseDown;
  TExtControl(Self.FSelectControl).OnMouseMove := _OldMouseMove;
  TExtControl(Self.FSelectControl).OnMouseUp := _OldMouseUp;
end;

// "senganchar" los eventos del control
procedure TSelectOnRunTime._CaptureEvents();
begin
  _CapturedEvents := True;
  // Guardar las anteriores configuraciones
  _OldMouseDown := TExtControl(Self.FSelectControl).OnMouseDown;
  _OldMouseMove := TExtControl(Self.FSelectControl).OnMouseMove;
  _OldMouseUp := TExtControl(Self.FSelectControl).OnMouseUp;
  _OldResize := TExtControl(Self.FSelectControl).OnResize;

  // "Cach" del evento
  TExtControl(Self.FSelectControl).OnMouseDown := ControlMouseDown;
  TExtControl(Self.FSelectControl).OnMouseMove := ControlMouseMove;
  TExtControl(Self.FSelectControl).OnMouseUp := ControlMouseUp;
  TExtControl(Self.FSelectControl).OnResize := ControlResize;

end;



procedure TSelectOnRunTime.SetMarkers3D(const Value: Boolean);
var
  Node: Integer;
begin
  FMarkers3D := Value;
  for Node := 0 to 7 do begin
    if (Value) then begin
      TPanel(Self._Nodes.Items[Node]).BevelOuter := bvRaised;
    end
    else begin
      TPanel(Self._Nodes.Items[Node]).BevelOuter := bvNone;
    end;
  end;
end;



procedure TSelectOnRunTime.OwnerResize(Sender: TObject);
begin

  Exit;
  // Diseсo?
  if (csDesigning in Self.ComponentState) or
     (csDestroying in Self.ComponentState) then begin
    Exit;
  end;

  // Onresize del Owner
  NodesVisible(False);
  // Llamar al aniguo
  if Assigned(_OldOwnerResize) then begin
    _OldOwnerResize(Self);
  end;

end;




end.
