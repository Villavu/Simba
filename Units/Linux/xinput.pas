  { $Xorg: XInput.h,v 1.4 2001/02/09 02:03:23 xorgcvs Exp $  }
  {***********************************************************

  Copyright 1989, 1998  The Open Group

  Permission to use, copy, modify, distribute, and sell this software and its
  documentation for any purpose is hereby granted without fee, provided that
  the above copyright notice appear in all copies and that both that
  copyright notice and this permission notice appear in supporting
  documentation.

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Except as contained in this notice, the name of The Open Group shall not be
  used in advertising or otherwise to promote the sale, use or other dealings
  in this Software without prior written authorization from The Open Group.

  Copyright 1989 by Hewlett-Packard Company, Palo Alto, California.

  			All Rights Reserved

  Permission to use, copy, modify, and distribute this software and its
  documentation for any purpose and without fee is hereby granted,
  provided that the above copyright notice appear in all copies and that
  both that copyright notice and this permission notice appear in
  supporting documentation, and that the name of Hewlett-Packard not be
  used in advertising or publicity pertaining to distribution of the
  software without specific, written prior permission.

  HEWLETT-PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
  HEWLETT-PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
  SOFTWARE.

  ******************************************************* }
unit xinput;

interface

uses
  x, xlib, xi, ctypes;

type
  PXEventClass = ^XEventClass;
  PpXEventClass = ^PXEventClass;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  _deviceKeyPress = 0;
  _deviceKeyRelease = 1;
  _deviceButtonPress = 0;
  _deviceButtonRelease = 1;
  _deviceMotionNotify = 0;
  _deviceFocusIn = 0;
  _deviceFocusOut = 1;
  _proximityIn = 0;
  _proximityOut = 1;
  _deviceStateNotify = 0;
  _deviceMappingNotify = 1;
  _changeDeviceNotify = 2;
  { Space of 3 between is necessary! Reserved for DeviceKeyStateNotify,
  DeviceButtonStateNotify, DevicePresenceNotify (essentially unused). This
  code has to be in sync with FixExtensionEvents() in xserver/Xi/extinit.c  }
  _propertyNotify = 6;

  { We need the declaration for DevicePresence.  }

  function _XiGetDevicePresenceNotifyEvent(dpy:pDisplay):cint; cdecl; external;

  {**************************************************************
   *
   * DeviceKey events.  These events are sent by input devices that
   * support input class Keys.
   * The location of the X pointer is reported in the coordinate
   * fields of the x,y and x_root,y_root fields.
   *
    }
type
  TXDeviceKeyEvent = record
    _type : cint;                     { of event  }
    serial : culong;                  { # of last request processed  }
    send_event : cbool;               { true if from SendEvent request  }
    display : PDisplay;               { Display the event was read from  }
    window : TWindow;                 { "event" window reported relative to  }
    deviceid : TXID;
    root : TWindow;                   { root window event occured on  }
    subwindow : TWindow;              { child window  }
    time : TTime;                     { milliseconds  }
    x : cint;                         { x, y coordinates in event window  }
    y : cint;
    x_root : cint;                    { coordinates relative to root  }
    y_root : cint;                    { coordinates relative to root  }
    state : cuint;                    { key or button mask  }
    keycode : cuint;                  { detail  }
    same_screen : cbool;              { same screen flag  }
    device_state : cuint;             { device key or button mask  }
    axes_count : cuchar;
    first_axis : cuchar;
    axis_data : array[0..5] of cint;
  end;

  TXDeviceKeyPressedEvent = TXDeviceKeyEvent;

  TXDeviceKeyReleasedEvent = TXDeviceKeyEvent;
  {******************************************************************
   *
   * DeviceButton events.  These events are sent by extension devices
   * that support input class Buttons.
   *
    }

  TXDeviceButtonEvent = record
    _type : cint;                        { of event  }
    serial : culong;                     { # of last request processed by server  }
    send_event : cbool;                  { true if from a SendEvent request  }
    display : PDisplay;                  { Display the event was read from  }
    window : TWindow;                    { "event" window reported relative to  }
    deviceid : TXID;
    root : TWindow;                      { root window that the event occured on  }
    subwindow : TWindow;                 { child window  }
    time : TTime;                        { milliseconds  }
    x : cint;                            { x, y coordinates in event window  }
    y : cint;
    x_root : cint;                       { coordinates relative to root  }
    y_root : cint;                       { coordinates relative to root  }
    state : cuint;                       { key or button mask  }
    button : cuint;                      { detail  }
    same_screen : cbool;                 { same screen flag  }
    device_state : cuint;                { device key or button mask  }
    axes_count : cuchar;
    first_axis : cuchar;
    axis_data : array[0..5] of cint;
  end;

  TXDeviceButtonPressedEvent = TXDeviceButtonEvent;

  TXDeviceButtonReleasedEvent = TXDeviceButtonEvent;
  {******************************************************************
   *
   * DeviceMotionNotify event.  These events are sent by extension devices
   * that support input class Valuators.
   *
    }
  TXDeviceMotionEvent = record
    _type : cint;                 { of event  }
    serial : culong;              { # of last request processed by server  }
    send_event : cbool;           { true if from a SendEvent request  }
    display : PDisplay;           { Display the event was read from  }
    window : TWindow;             { "event" window reported relative to  }
    deviceid : TXID;
    root : TWindow;               { root window that the event occured on  }
    subwindow : TWindow;          { child window  }
    time : TTime;                 { milliseconds  }
    x,y : cint;                   { x, y coordinates in event window  }
    x_root : cint;                { coordinates relative to root  }
    y_root : cint;                { coordinates relative to root  }
    state : cuint;                { key or button mask  }
    is_hint : char;               { detail  }
    same_screen : cbool;          { same screen flag  }
    device_state : cuint;         { device key or button mask  }
    axes_count : cuchar;
    first_axis : cuchar;
    axis_data : array[0..5] of cint;
  end;

  {******************************************************************
   *
   * DeviceFocusChange events.  These events are sent when the focus
   * of an extension device that can be focused is changed.
   *
    }
  TXDeviceFocusChangeEvent = record
    _type : cint;        { of event  }
    serial : culong;     { # of last request processed by server  }
    send_event : cbool;  { true if from a SendEvent request  }
    display : PDisplay;  { Display the event was read from  }
    window : TWindow;    { "event" window reported relative to  }
    deviceid : TXID;
    mode : cint;         { NotifyNormal, NotifyGrab, NotifyUngrab  }
    detail : cint;
    {  * NotifyAncestor, NotifyVirtual, NotifyInferior,
    	 * NotifyNonLinear,NotifyNonLinearVirtual, NotifyPointer,
  	   * NotifyPointerRoot, NotifyDetailNone  }
    time : TTime;
  end;

  TXDeviceFocusInEvent = TXDeviceFocusChangeEvent;

  TXDeviceFocusOutEvent = TXDeviceFocusChangeEvent;
  {******************************************************************
  *
  * ProximityNotify events.  These events are sent by those absolute
  * positioning devices that are capable of generating proximity information.
  *
  }

  TXProximityNotifyEvent = record
    _type : cint;         { ProximityIn or ProximityOut  }
    serial : culong;      { # of last request processed by server  }
    send_event : cbool;   { true if this came from a SendEvent request  }
    display : PDisplay;   { Display the event was read from  }
    window : TWindow;
    deviceid : TXID;
    root : TWindow;
    subwindow : TWindow;
    time : TTime;
    x : cint;
    y : cint;
    x_root : cint;
    y_root : cint;
    state : cuint;
    same_screen : cbool;
    device_state : cuint;  { device key or button mask  }
    axes_count : cuchar;
    first_axis : cuchar;
    axis_data : array[0..5] of cint;
  end;

  TXProximityInEvent = TXProximityNotifyEvent;

  TXProximityOutEvent = TXProximityNotifyEvent;
  {******************************************************************
   *
   * DeviceStateNotify events are generated on EnterWindow and FocusIn
   * for those clients who have selected DeviceState.
   *
    }

  TXInputClass = record
    c_class : cuchar;
    _length : cuchar;
  end;
  PXInputClass = ^TXInputClass;

  TXDeviceStateNotifyEvent = record
    _type : cint;
    serial : culong;    { # of last request processed by server  }
    send_event : cbool; { true if this came from a SendEvent request  }
    display : PDisplay; { Display the event was read from  }
    window : TWindow;
    deviceid : TXID;
    time : TTime;
    num_classes : cint;
    data : array[0..63] of char;
  end;

  TXValuatorStatus = record
    c_class : cuchar;
    _length : cuchar;
    num_valuators : cuchar;
    mode : cuchar;
    valuators : array[0..5] of cint;
  end;

  TXKeyStatus = record
    c_class : cuchar;
    _length : cuchar;
    num_keys : cshort;
    keys : array[0..31] of char;
  end;

  TXButtonStatus = record
    c_class : cuchar;
    _length : cuchar;
    num_buttons : cshort;
    buttons : array[0..31] of char;
  end;

  {******************************************************************
   *
   * DeviceMappingNotify event.  This event is sent when the key mapping,
   * modifier mapping, or button mapping of an extension device is changed.
   *
    }
  TXDeviceMappingEvent = record
    _type : cint;
    serial : culong;      { # of last request processed by server  }
    send_event : cbool;   { true if this came from a SendEvent request  }
    display : PDisplay;   { Display the event was read from  }
    window : TWindow;     { unused  }
    deviceid : TXID;
    time : TTime;
    request : cint;       { one of MappingModifier, MappingKeyboard, MappingPointer  }
    first_keycode : cint; { first keycode  }
    count : cint;         { defines range of change w. first_keycode }
  end;
  {******************************************************************
   *
   * ChangeDeviceNotify event.  This event is sent when an
   * XChangeKeyboard or XChangePointer request is made.
   *
    }
  TXChangeDeviceNotifyEvent = record
    _type : cint;
    serial : culong;    { # of last request processed by server  }
    send_event : cbool; { true if this came from a SendEvent request  }
    display : PDisplay; { Display the event was read from  }
    window : TWindow;   { unused  }
    deviceid : TXID;
    time : TTime;
    request : cint; { NewPointer or NewKeyboard  }
  end;

  {******************************************************************
   *
   * DevicePresenceNotify event.  This event is sent when the list of
   * input devices changes, in which case devchange will be false, and
   * no information about the change will be contained in the event;
   * the client should use XListInputDevices() to learn what has changed.
   *
   * If devchange is true, an attribute that the server believes is
   * important has changed on a device, and the client should use
   * XGetDeviceControl to examine the device.  If control is non-zero,
   * then that control has changed meaningfully.
    }

  TXDevicePresenceNotifyEvent = record
    _type : cint;
    serial : culong;      { # of last request processed by server  }
    send_event : cbool;   { true if this came from a SendEvent request  }
    display : PDisplay;   { Display the event was read from  }
    window : TWindow;     { unused  }
    time : TTime;
    devchange : cbool;
    deviceid : TXID;
    control : TXID;
  end;

  {
   * Notifies the client that a property on a device has changed value. The
   * client is expected to query the server for updated value of the property.
    }

  TXDevicePropertyNotifyEvent = record
    _type : cint;
    serial : culong;     { # of last request processed by server  }
    send_event : cbool;  { true if this came from a SendEvent request  }
    display : PDisplay;  { Display the event was read from  }
    window : TWindow;    { unused  }
    time : TTime;
    deviceid : TXID;     { id of the device that changed  }
    atom : TAtom;        { the property that changed  }
    state : cint;        { PropertyNewValue or PropertyDeleted  }
  end;

  {******************************************************************
   *
   * Control structures for input devices that support input class
   * Feedback.  These are used by the XGetFeedbackControl and
   * XChangeFeedbackControl functions.
   *
    }

  TXFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
  end;
  PXFeedbackState = ^TXFeedbackState;

  TXKbdFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    click : cint;
    percent : cint;
    pitch : cint;
    duration : cint;
    led_mask : cint;
    global_auto_repeat : cint;
    auto_repeats : array[0..31] of char;
  end;

  TXPtrFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    accelNum : cint;
    accelDenom : cint;
    threshold : cint;
  end;

  TXIntegerFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    resolution : cint;
    minVal : cint;
    maxVal : cint;
  end;

  TXStringFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    max_symbols : cint;
    num_syms_supported : cint;
    syms_supported : PKeySym;
  end;

  TXBellFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    percent : cint;
    pitch : cint;
    duration : cint;
  end;

  TXLedFeedbackState = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    led_values : cint;
    led_mask : cint;
  end;

  TXFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
  end;
  PXFeedbackControl = ^TXFeedbackControl;

  TXPtrFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    accelNum : cint;
    accelDenom : cint;
    threshold : cint;
  end;

  TXKbdFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    click : cint;
    percent : cint;
    pitch : cint;
    duration : cint;
    led_mask : cint;
    led_value : cint;
    key : cint;
    auto_repeat_mode : cint;
  end;

  TXStringFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    num_keysyms : cint;
    syms_to_display : PKeySym;
  end;

  TXIntegerFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    int_to_display : cint;
  end;

  TXBellFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    percent : cint;
    pitch : cint;
    duration : cint;
  end;

  TXLedFeedbackControl = record
    c_class : TXID;
    _length : cint;
    id : TXID;
    led_mask : cint;
    led_values : cint;
  end;

  {******************************************************************
   *
   * Device control structures.
   *
    }

  TXDeviceControl = record
    control : TXID;
    _length : cint;
  end;
  PXDeviceControl = ^TXDeviceControl;

  TXDeviceResolutionControl = record
    control : TXID;
    _length : cint;
    first_valuator : cint;
    num_valuators : cint;
    resolutions : ^cint;
  end;

  TXDeviceResolutionState = record
    control : TXID;
    _length : cint;
    num_valuators : cint;
    resolutions : ^cint;
    min_resolutions : ^cint;
    max_resolutions : ^cint;
  end;

  TXDeviceAbsCalibControl = record
    control : TXID;
    _length : cint;
    min_x : cint;
    max_x : cint;
    min_y : cint;
    max_y : cint;
    flip_x : cint;
    flip_y : cint;
    rotation : cint;
    button_threshold : cint;
  end;
  TXDeviceAbsCalibState = TXDeviceAbsCalibControl;

  TXDeviceAbsAreaControl = record
    control : TXID;
    _length : cint;
    offset_x : cint;
    offset_y : cint;
    width : cint;
    height : cint;
    screen : cint;
    following : TXID;
  end;
  TXDeviceAbsAreaState = TXDeviceAbsAreaControl;

  TXDeviceCoreControl = record
    control : TXID;
    _length : cint;
    status : cint;
  end;

  TXDeviceCoreState = record
    control : TXID;
    _length : cint;
    status : cint;
    iscore : cint;
  end;

  TXDeviceEnableControl = record
    control : TXID;
    _length : cint;
    enable : cint;
  end;
  TXDeviceEnableState = TXDeviceEnableControl;

  {******************************************************************
   *
   * An array of XDeviceList structures is returned by the
   * XListInputDevices function.  Each entry contains information
   * about one input device.  Among that information is an array of
   * pointers to structures that describe the characteristics of
   * the input device.
   *
    }

  _XAnyClassinfo = record
    c_class : TXID;
    _length : cint;
  end;
  TXAnyClassInfo = _XAnyClassinfo;
  PXAnyClassInfo = ^TXAnyClassInfo;

  _XDeviceInfo = record
    id : TXID;
    _type : TAtom;
    name : ^char;
    num_classes : cint;
    use : cint;
    inputclassinfo : PXAnyClassInfo;
  end;
  TXDeviceInfo = _XDeviceInfo;
  PXDeviceInfo = ^TXDeviceInfo;

  _XKeyInfo = record
    c_class : TXID;
    _length : cint;
    min_keycode : cushort;
    max_keycode : cushort;
    num_keys : cushort;
  end;
  TXKeyInfo = _XKeyInfo;
  PXKeyInfo = ^TXKeyInfo;

  _XButtonInfo = record
    c_class : TXID;
    _length : cint;
    num_buttons : cshort;
  end;
  TXButtonInfo = _XButtonInfo;
  PXButtonInfo = ^TXButtonInfo;

  _XAxisInfo = record
    resolution : cint;
    min_value : cint;
    max_value : cint;
  end;
  TXAxisInfo = _XAxisInfo;
  PXAxisInfo = ^TXAxisInfo;

  _XValuatorInfo = record
    c_class : TXID;
    _length : cint;
    num_axes : cuchar;
    mode : cuchar;
    motion_buffer : culong;
    axes : PXAxisInfo;
  end;
  TXValuatorInfo = _XValuatorInfo;
  PXValuatorInfo = ^TXValuatorInfo;
  {******************************************************************
   *
   * An XDevice structure is returned by the XOpenDevice function.
   * It contains an array of pointers to XInputClassInfo structures.
   * Each contains information about a class of input supported by the
   * device, including a pointer to an array of data for each type of event
   * the device reports.
   *
    }

  TXInputClassInfo = record
    input_class : cuchar;
    event_type_base : cuchar;
  end;
  PXInputClassInfo = ^TXInputClassInfo;

  TXDevice = record
    device_id : TXID;
    num_classes : cint;
    classes : PXInputClassInfo;
  end;
  PXDevice = ^TXDevice;

  {******************************************************************
   *
   * The following structure is used to return information for the
   * XGetSelectedExtensionEvents function.
   *
    }

  TXEventList = record
    event_type : XEventClass;
    device : TXID;
  end;
  {******************************************************************
   *
   * The following structure is used to return motion history data from
   * an input device that supports the input class Valuators.
   * This information is returned by the XGetDeviceMotionEvents function.
   *
    }

  TXDeviceTimeCoord = record
    time : TTime;
    data : pcint;
  end;
  PXDeviceTimeCoord = ^TXDeviceTimeCoord;

  {******************************************************************
   *
   * Device state structure.
   * This is returned by the XQueryDeviceState request.
   *
    }

  TXDeviceState = record
    device_id : TXID;
    num_classes : cint;
    data : PXInputClass;
  end;
  PXDeviceState = ^TXDeviceState;

  {******************************************************************
   *
   * Note that the mode field is a bitfield that reports the Proximity
   * status of the device as well as the mode.  The mode field should
   * be OR'd with the mask DeviceMode and compared with the values
   * Absolute and Relative to determine the mode, and should be OR'd
   * with the mask ProximityState and compared with the values InProximity
   * and OutOfProximity to determine the proximity state.
   *
    }

  TXValuatorState = record
    c_class : cuchar;
    _length : cuchar;
    num_valuators : cuchar;
    mode : cuchar;
    valuators : pcint;
  end;

  TXKeyState = record
    c_class : cuchar;
    _length : cuchar;
    num_keys : cshort;
    keys : array[0..31] of char;
  end;

  TXButtonState = record
    c_class : cuchar;
    _length : cuchar;
    num_buttons : cshort;
    buttons : array[0..31] of char;
  end;

  {******************************************************************
   *
   * Function definitions.
   *
    }

  function XChangeKeyboardDevice(display:PDisplay; device:PXDevice):cint;cdecl; external;

  function XChangePointerDevice(display:PDisplay; device:pXDevice; xaxis:cint; yaxis:cint):cint;cdecl; external;

  function XGrabDevice(display:pDisplay; device:pXDevice; grab_window:TWindow; ownerEvents:cbool; event_count:cint;
             event_list:PXEventClass; this_device_mode:cint; other_devices_mode:cint; time:TTime):cint;cdecl; external;

  function XUngrabDevice(display:pDisplay; device:pXDevice; time:TTime):cint;cdecl; external;

  function XGrabDeviceKey(display:pDisplay; device:pXDevice; key:cuint; modifiers:cuint; modifier_device:pXDevice;
             grab_window:TWindow; owner_events:cbool; event_count:cuint; event_list:pXEventClass; this_device_mode:cint;
             other_devices_mode:cint):cint;cdecl; external;

  function XUngrabDeviceKey(display:pDisplay; device:pXDevice; key:cuint; modifiers:cuint; modifier_dev:pXDevice;
             grab_window:TWindow):cint;cdecl; external;

  function XGrabDeviceButton(display:pDisplay; device:pXDevice; button:cuint; modifier:cuint; modifier_device:pXDevice;
             grab_window:TWindow; owner_events:cbool; event_count:cuint; event_list:pXEventClass; this_device_mode:cint;
             other_devices_mode:cint):cint;cdecl; external;

  function XUngrabDeviceButton(display:pDisplay; device:pXDevice; button:cuint; modifiers:cuint; modifier_dev:pXDevice;
             grab_window:TWindow):cint;cdecl; external;

  function XAllowDeviceEvents(display:pDisplay; device:pXDevice; event_mode:cint; time:TTime):cint;cdecl;external;

  function XGetDeviceFocus(display:pDisplay; device:pXDevice; focus:pWindow; revert_to:pcint; time:pTime):cint;cdecl;external;

  function XSetDeviceFocus(display:pDisplay; device:pXDevice; focus:TWindow; revert_to:cint; time:TTime):cint;cdecl;external;

  function XGetFeedbackControl(display:pDisplay; device:pXDevice; num_feedbacks:pcint): PXFeedbackState;cdecl;external;

  procedure XFreeFeedbackList(list:pXFeedbackState);cdecl;external;

  function XChangeFeedbackControl(display:pDisplay; device:pXDevice; mask:culong; f:pXFeedbackControl):cint;cdecl;external;

  function XDeviceBell(display:pDisplay; device:pXDevice; feedbackclass:TXID; feedbackid:TXID; percent:cint):cint;cdecl;external;

  function XGetDeviceKeyMapping(display:pDisplay; device:pXDevice; first:cuint; keycount:cint; syms_per_code:pcint):PKeySym;cdecl;external;

  function XChangeDeviceKeyMapping(display:pDisplay; device:pXDevice; first:cint; syms_per_code:cint; keysyms:pKeySym;
             count:cint):cint;cdecl;external;

  function XGetDeviceModifierMapping(display:pDisplay; device:pXDevice):PXModifierKeymap;cdecl;external;

  function XSetDeviceModifierMapping(display:pDisplay; device:pXDevice; modmap:pXModifierKeymap):cint;cdecl;external;

  function XSetDeviceButtonMapping(display:pDisplay; device:pXDevice; map:array of pcuchar; nmap:cint):cint;cdecl;external;

  function XGetDeviceButtonMapping(display:pDisplay; device:pXDevice; map:array of pcuchar; nmap:cuint):cint;cdecl;external;

  function XQueryDeviceState(display:pDisplay; device:pXDevice):PXDeviceState;cdecl;external;

  procedure XFreeDeviceState(list:pXDeviceState);cdecl;external;

(* Const before type ignored *)
  function XGetExtensionVersion(display:pDisplay; name:pchar):PXExtensionVersion;cdecl;external;

  function XListInputDevices(display:pDisplay; ndevices:pcint):PXDeviceInfo;cdecl;external;

  procedure XFreeDeviceList(list:pXDeviceInfo);cdecl;external;

  function XOpenDevice(display:pDisplay; id:TXID):PXDevice;cdecl;external;

  function XCloseDevice(display:pDisplay; device:pXDevice):cint;cdecl;external;

  function XSetDeviceMode(display:pDisplay; device:pXDevice; mode:cint):cint;cdecl;external;

  function XSetDeviceValuators(display:pDisplay; device:pXDevice; valuators:pcint; first_valuator:cint; num_valuators:cint):cint;cdecl;external;

  function XGetDeviceControl(display:pDisplay; device:pXDevice; control:cint):PXDeviceControl;cdecl;external;

  function XChangeDeviceControl(display:pDisplay; device:pXDevice; control:cint; d:pXDeviceControl):cint;cdecl;external;

  function XSelectExtensionEvent(display:pDisplay; w:TWindow; event_list:pXEventClass; count:cint):cint;cdecl;external;

  function XGetSelectedExtensionEvents(display:pDisplay; w:TWindow; this_client_count:pcint; this_client_list:PpXEventClass; all_clients_count:pcint;
             all_clients_list:PpXEventClass):cint;cdecl;external;

  function XChangeDeviceDontPropagateList(display:pDisplay; window:TWindow; count:cint; events:pXEventClass; mode:cint):cint;cdecl;external;

  function XGetDeviceDontPropagateList(display:pDisplay; window:TWindow; count:pcint):PXEventClass;cdecl;external;

  function XSendExtensionEvent(display:pDisplay; device:pXDevice; dest:TWindow; prop:cbool; count:cint;
             list:pXEventClass; event:pXEvent):TStatus;cdecl;external;

  function XGetDeviceMotionEvents(display:pDisplay; device:pXDevice; start:TTime; stop:TTime; nEvents:pcint;
             mode:pcint; axis_count:pcint):PXDeviceTimeCoord;cdecl;external;

  procedure XFreeDeviceMotionEvents(events:pXDeviceTimeCoord);cdecl;external;

  procedure XFreeDeviceControl(control:pXDeviceControl);cdecl;external;


type
  TXIPropertyInfo = record
    pending : cbool;
    range : cbool;
    immutable : cbool;
    fromClient : cbool;
    num_values : cint;
    values : ^longint;
  end;

  function XListDeviceProperties(dpy:pDisplay; dev:pXDevice; nprops_return:pcint):PAtom;cdecl;external;

(* Const before type ignored *)
  procedure XChangeDeviceProperty(dpy:pDisplay; dev:pXDevice; _property:TAtom; _type:TAtom; format:cint;
              mode:cint; data:pcuchar; nelements:cint);cdecl;external;

  procedure XDeleteDeviceProperty(dpy:pDisplay; dev:pXDevice; _property:TAtom);cdecl;external;

  function XGetDeviceProperty(dpy:pDisplay; dev:pXDevice; _property:TAtom; offset:longint; _length:longint;
             _delete:cbool; req_type:TAtom; actual_type:pAtom; actual_format:pcint; nitems:pculong;
             bytes_after:pculong; prop:Ppcuchar):TStatus;cdecl;external;
  procedure FindTypeAndClass(d: PXDevice; _type, _class, classid: cint; offset: cint); inline;
  procedure DeviceKeyPress(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceKeyRelease(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButtonPress(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButtonRelease(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceMotionNotify(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceFocusIn(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceFocusOut(d: PXDevice; _type, _class: cint); inline;
  procedure ProximityIn(d: PXDevice; _type, _class: cint); inline;
  procedure ProximityOut(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceStateNotify(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceMappingNotify(d: PXDevice; _type, _class: cint); inline;
  procedure ChangeDeviceNotify(d: PXDevice; _type, _class: cint); inline;
  procedure DevicePropertyNotify(d: PXDevice; _type, _class: cint); inline;
  procedure DevicePointerMotionHint(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButton1Motion(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButton2Motion(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButton3Motion(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButton4Motion(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButton5Motion(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButtonMotion(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceOwnerGrabButton(d: PXDevice; _type, _class: cint); inline;
  procedure DeviceButtonPressGrab(d: PXDevice; _type, _class: cint); inline;

implementation

// Most important macro
procedure FindTypeAndClass(d: PXDevice; _type, _class, classid: cint; offset: cint); inline;
var
   _i: integer;
   _ip: PXInputClassInfo;
begin
  _type := 0;
  _class := 0;
  _ip  := d^.classes;
  for _i := 0 to d^.num_classes do
  begin
    if _ip^.input_class = classid then
    begin
      _type := _ip^.event_type_base + offset;
      _class := d^.device_id shl 8 or _type;
    end;
    Inc(_ip);
  end;
end;

 {
  #define FindTypeAndClass(d,type,_class,classid,offset) \
       int _i; XInputClassInfo *_ip; \
      type = 0; _class = 0; \
      for (_i=0, _ip= ((XDevice *) d)->classes; \
  	 _i< ((XDevice *) d)->num_classes; \
  	 _i++, _ip++) \
  	if (_ip->input_class == classid) \
  	    type =  _ip->event_type_base + offset; \
  	     _class =  ((XDevice *) d)->device_id << 8 | type;           }

procedure DeviceKeyPress(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, KeyClass, _deviceKeyPress);
end;

procedure DeviceKeyRelease(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, KeyClass, _deviceKeyRelease);
end;

procedure DeviceButtonPress(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, ButtonClass, _deviceButtonPress);
end;

procedure DeviceButtonRelease(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, ButtonClass, _deviceButtonRelease);
end;

procedure DeviceMotionNotify(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, ValuatorClass, _deviceMotionNotify);
end;

procedure DeviceFocusIn(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, FocusClass, _deviceFocusIn);
end;

procedure DeviceFocusOut(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, FocusClass, _deviceFocusOut);
end;

procedure ProximityIn(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, ProximityClass, _proximityIn);
end;

procedure ProximityOut(d: PXDevice; _type, _class: cint); inline;
begin
 FindTypeAndClass(d, _type, _class, ProximityClass, _proximityOut);
end;

procedure DeviceStateNotify(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, OtherClass, _deviceStateNotify);
end;

procedure DeviceMappingNotify(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, OtherClass, _deviceMappingNotify);
end;

procedure ChangeDeviceNotify(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, OtherClass, _changeDeviceNotify);
end;

procedure DevicePropertyNotify(d: PXDevice; _type, _class: cint); inline;
begin
  FindTypeAndClass(d, _type, _class, OtherClass, _propertyNotify);
end;

procedure DevicePointerMotionHint(d: PXDevice; _type, _class: cint); inline;
begin
  _class :=  d^.device_id shl 8 or _devicePointerMotionHint;
end;

procedure DeviceButton1Motion(d: PXDevice; _type, _class: cint); inline;
begin  
  _class :=  d^.device_id shl 8 or _deviceButton1Motion;
end;

procedure DeviceButton2Motion(d: PXDevice; _type, _class: cint); inline;
begin    
  _class :=  d^.device_id shl 8 or _deviceButton2Motion;
end;

procedure DeviceButton3Motion(d: PXDevice; _type, _class: cint); inline;
begin
  _class :=  d^.device_id shl 8 or _deviceButton3Motion;
end;

procedure DeviceButton4Motion(d: PXDevice; _type, _class: cint); inline;
begin 
  _class :=  d^.device_id shl 8 or _deviceButton4Motion;
end;

procedure DeviceButton5Motion(d: PXDevice; _type, _class: cint); inline;
begin       
  _class :=  d^.device_id shl 8 or _deviceButton5Motion;
end;

procedure DeviceButtonMotion(d: PXDevice; _type, _class: cint); inline;
begin       
  _class :=  d^.device_id shl 8 or _deviceButtonMotion;
end;

procedure DeviceOwnerGrabButton(d: PXDevice; _type, _class: cint); inline;
begin
  _class :=  d^.device_id shl 8 or _deviceOwnerGrabButton;
end;
procedure DeviceButtonPressGrab(d: PXDevice; _type, _class: cint); inline;
begin
  _class :=  d^.device_id shl 8 or _deviceButtonGrab;
end;

end.
