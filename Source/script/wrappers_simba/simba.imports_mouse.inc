pushSection('Mouse');

addGlobalFunc('procedure MoveMouse(X, Y: Integer)', @_LapeMoveMouse);
addGlobalFunc('procedure ScrollMouse(X, Y: Integer; Clicks: Integer)', @_LapeScrollMouse);
addGlobalFunc('procedure GetMousePos(var X, Y: Integer)', @_LapeGetMousePos);
addGlobalFunc('procedure HoldMouse(X, Y: Integer; ClickType: Integer)', @_LapeHoldMouse);
addGlobalFunc('procedure ReleaseMouse(X, Y: Integer; ClickType: Integer)', @_LapeReleaseMouse);
addGlobalFunc('procedure ClickMouse(X, Y: Integer; ClickType: Integer)', @_LapeClickMouse);
addGlobalFunc('function IsMouseButtonDown(Button: Integer): boolean', @_LapeIsMouseButtonDown);

addGlobalVar(Integer(MOUSE_RIGHT),   'MOUSE_RIGHT').isConstant := True;
addGlobalVar(Integer(MOUSE_LEFT),    'MOUSE_LEFT').isConstant := True;
addGlobalVar(Integer(MOUSE_MIDDLE),  'MOUSE_MIDDLE').isConstant := True;
addGlobalVar(Integer(MOUSE_EXTRA_1), 'MOUSE_EXTRA_1').isConstant := True;
addGlobalVar(Integer(MOUSE_EXTRA_2), 'MOUSE_EXTRA_2').isConstant := True;

popSection();

