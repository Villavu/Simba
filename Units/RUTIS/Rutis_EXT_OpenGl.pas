Unit Rutis_EXT_OpenGl;

Interface

Uses
  Windows, Controls, SysUtils,
  Rutis_Engine, Rutis_Defs,
  dglOpenGl,
  TFrustumClass,
  OpenGl_Texture_Manager,
  OpenGl_Types,
  OpenGl_Functions,
  OpenGl_Speed_Lists;

Var
  Rogl_Display      : TWinControl;
  Rogl_DC, Rogl_RC  : HDC;
  Rogl_CamPos,
  Rogl_CamRot       : TVector3f;
  Rogl_NearClipping,
  Rogl_FarClipping  : Single;

Procedure RegisterEXTMethods(Engine : TRutisEngine);
Procedure ROgl_SetViewport(WinControl : TWinControl);
Procedure DestroyRoglContext;
Procedure ResizeRoglContext;

Implementation

Var
  ROGL_Width, ROGL_Height  : Integer;
 //==============================================================================
 //==============================================================================

Function CreateContext : Integer;
Begin
  Result := 0;
  If not InitOpenGl Then exit;
  Result := CreateRenderingContext(Rogl_DC, [opDoubleBuffered], 32, 24, 0, 32, 0, 0);
End;

Procedure DestroyRoglContext;
Begin
  SetLength(TexManager_TexturePaths, 0);
  AddTexturePath('.\');
  AddTexturePath(ExtractFileDir(ParamStr(0)));
  FreeTextures;
  DeleteSpeedLists;
  Rogl_NearClipping := 1;
  Rogl_FarClipping  := 1000;
  If Rogl_DC <> 0 Then
  Begin
    If Rogl_RC <> 0 Then
    Begin
      DeactivateRenderingContext;
      DestroyRenderingContext(Rogl_RC);
      Rogl_RC := 0;
    End;
    Try
      ReleaseDC(Rogl_Display.Handle, Rogl_DC);
    Except
    End;
    Rogl_DC     := 0;
    Rogl_CamPos := nullvect;
    Rogl_CamRot := nullvect;
    sleep(1);
  End;
End;

Procedure ResizeRoglContext;
Begin
  If (Rogl_DC <> 0) and (Rogl_RC <> 0) Then
  Begin
    glViewport(0, 0, Rogl_Display.Width, Rogl_Display.Height);
    {glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    gluPerspective(45.0, ClientWidth/ClientHeight, NearClipping, FarClipping);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;}
  End;
End;

Procedure ROgl_SetViewport(WinControl : TWinControl);
Begin
  If not (WinControl is TWinControl) Then exit;
  Rogl_Display := WinControl;

  DestroyRoglContext;

  If not (Rogl_Display is TWinControl) Then
    Rogl_Display := Rogl_Display.Parent;

  Rogl_DC := GetDC(Rogl_Display.Handle);
  Rogl_RC := CreateContext;
  ActivateRenderingContext(Rogl_DC, Rogl_RC);

  sleep(1);

  glClearColor(0, 0, 0, 0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
End;

//==============================================================================
//==============================================================================
{$REGION 'OpenGl-Extension glx'}

Procedure _glxSetViewportControl(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  ROgl_SetViewport(TWinControl(PPointer(Params^[0].Data)^));
End;

Procedure _InitOpenGL(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  If Rogl_Display = nil Then exit;

  DestroyRoglContext;

  Rogl_DC := GetDC(Rogl_Display.Handle);
  //If not InitOpenGl then exit;
  Rogl_RC := CreateContext;
  ActivateRenderingContext(Rogl_DC, Rogl_RC);

  sleep(1);

  glClearColor(0, 0, 0, 0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_CULL_FACE);
End;

Procedure _glxNewFrame(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  If Rogl_RC = 0 Then _InitOpenGL(nil, nil);
  If Rogl_RC = 0 Then exit;

  SwapBuffers(Rogl_DC);

  If (Rogl_Display.Width <> ROGL_Width) or (Rogl_Display.Height <> ROGL_Height) Then
  Begin
    ROGL_Width := Rogl_Display.Width;
    ROGL_Width := Rogl_Display.Height;
    glViewport(0, 0, Rogl_Display.Width, Rogl_Display.Height);
  End;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, Rogl_Display.Width / Rogl_Display.Height, Rogl_NearClipping, Rogl_FarClipping);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glRotateV(Vector(-Rogl_CamRot.X, -Rogl_CamRot.Y, -Rogl_CamRot.Z));
  glTranslatef(-Rogl_CamPos.X, -Rogl_CamPos.Y, -Rogl_CamPos.Z);
End;

Procedure _glxSwapBuffers(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SwapBuffers(Rogl_DC);
End;

Procedure _glxSetNearFarClipping(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Rogl_NearClipping := PSingle(Params^[0].Data)^;
  Rogl_FarClipping  := PSingle(Params^[1].Data)^;
End;

Procedure _glxGetTexture(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PInteger(Result^.Data)^ := ApplyTexture(PAnsiString(Params^[0].Data)^, False);
End;

Procedure _glxApplyTexture(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  ApplyTexture(PAnsiString(Params^[0].Data)^);
End;

Procedure _glxCamPos(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Rogl_CamPos.X := PSingle(Params^[0].Data)^;
  Rogl_CamPos.Y := PSingle(Params^[1].Data)^;
  Rogl_CamPos.Z := PSingle(Params^[2].Data)^;
End;

Procedure _glxCamRot(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Rogl_CamRot.X := PSingle(Params^[0].Data)^;
  Rogl_CamRot.Y := PSingle(Params^[1].Data)^;
  Rogl_CamRot.Z := PSingle(Params^[2].Data)^;
End;

Procedure _glxCalculateFrustum(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Frustum.Calculate;
End;

Procedure _glxIsPointInFrustum(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Frustum.IsPointWithin(
    PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^);
End;

Procedure _glxIsSphereInFrustum(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Frustum.IsSphereWithin(
    PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^);
End;

Procedure _glxIsBoxInFrustum(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Frustum.IsBoxWithin(
    PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^,
    PSingle(Params^[4].Data)^,
    PSingle(Params^[5].Data)^);
End;

Procedure _glxIsPointInFrustumV(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Frustum.IsPointWithin(
    PVector3f(Params^[0].Data)^.x,
    PVector3f(Params^[0].Data)^.y,
    PVector3f(Params^[0].Data)^.z);
End;

Procedure _glxIsSphereInFrustumV(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Frustum.IsSphereWithin(
    PPVector3f(Params^[0].Data)^^.x,
    PPVector3f(Params^[0].Data)^^.y,
    PPVector3f(Params^[0].Data)^^.z,
    PSingle(Params^[1].Data)^);
End;

Procedure _glxIsBoxInFrustumV(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Frustum.IsBoxWithin(
    PVector3f(Params^[0].Data)^.x,
    PVector3f(Params^[0].Data)^.y,
    PVector3f(Params^[0].Data)^.z,
    PVector3f(Params^[1].Data)^.x,
    PVector3f(Params^[1].Data)^.y,
    PVector3f(Params^[1].Data)^.z);
End;

Procedure _glSphere(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  slSphere(PInteger(Params^[0].Data)^{,Params^[1]});
End;
Procedure _glCube(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  slCube(1, 1, 1, PInteger(Params^[0].Data)^);
End;

{$ENDREGION}

//==============================================================================
//====================== OPENGL native functions ===============================
//==============================================================================

{$REGION 'Other OpenGl Functions'}

Procedure _glBegin(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glBegin(PCardinal(Params^[0].Data)^);
End;

Procedure _glEnd(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glEnd;
End;

Procedure _glClear(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glClear(PCardinal(Params^[0].Data)^);
End;

Procedure _glFlush(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glFlush;
End;

{$ENDREGION}
//==============================================================================

{$REGION 'OpenGl-States'}

Procedure _glEnable(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glEnable(PInteger(Params^[0].Data)^);
End;

Procedure _glDisable(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glDisable(PInteger(Params^[0].Data)^);
End;

Procedure _glPushAttrib(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glPushAttrib(PInteger(Params^[0].Data)^);
End;

Procedure _glPopAttrib(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glPopAttrib;
End;

//==============================================================================

Procedure _glCullFace(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glCullFace(PCardinal(Params^[0].Data)^);
End;

Procedure _glViewport(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glViewport(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^);
End;

Procedure _glLineWidth(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glLineWidth(PSingle(Params^[0].Data)^);
End;

Procedure _glBlendFunc(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glBlendFunc(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^);
End;

Procedure _glClearColor(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glClearColor(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^);
End;

Procedure _glAccum(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glAccum(PCardinal(Params^[0].Data)^, PSingle(Params^[1].Data)^);
End;

Procedure _glReadBuffer(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glReadBuffer(PCardinal(Params^[0].Data)^);
End;

Procedure _glPolygonMode(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glPolygonMode(PCardinal(Params^[0].Data)^, PCardinal(Params^[1].Data)^);
End;

Procedure _glPolygonOffset(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glPolygonOffset(PSingle(Params^[0].Data)^, PSingle(Params^[1].Data)^);
End;

{$ENDREGION}
//==============================================================================

{$REGION 'Matrix'}

Procedure _glMatrixMode(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glMatrixMode(PCardinal(Params^[0].Data)^);
End;

Procedure _glPushMatrix(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glPushMatrix;
End;

Procedure _glPopMatrix(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glPopMatrix;
End;

Procedure _glLoadIdentity(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glLoadIdentity;
End;

Procedure _glOrtho(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glOrtho(PSingle(Params^[0].Data)^, PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^, PSingle(Params^[3].Data)^,
    PSingle(Params^[4].Data)^, PSingle(Params^[5].Data)^);
End;

Procedure _gluOrtho2D(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  gluOrtho2D(PSingle(Params^[0].Data)^, PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^, PSingle(Params^[3].Data)^);
End;

Procedure _gluPerspective(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  gluPerspective(PSingle(Params^[0].Data)^, PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^, PSingle(Params^[3].Data)^);
End;

//==============================================================================

Procedure _glTranslatef(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glTranslatef(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^);
End;

Procedure _glTranslatefv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glTranslatef(PPVector3f(Params^[0].Data)^^.X,
    PPVector3f(Params^[0].Data)^^.Y,
    PPVector3f(Params^[0].Data)^^.Z);
End;

//==============================================================================

Procedure _glRotatef(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glRotatef(PSingle(Params^[0].Data)^, PSingle(Params^[1].Data)^, PSingle(Params^[2].Data)^, PSingle(Params^[3].Data)^);
End;

Procedure _glRotate3f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glRotatef(PSingle(Params^[0].Data)^, 1, 0, 0);
  glRotatef(PSingle(Params^[1].Data)^, 0, 1, 0);
  glRotatef(PSingle(Params^[2].Data)^, 0, 0, 1);
End;

Procedure _glRotate3fvect(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glRotatef(PVector3f(Params^[0].Data)^.X, 1, 0, 0);
  glRotatef(PVector3f(Params^[0].Data)^.Y, 0, 1, 0);
  glRotatef(PVector3f(Params^[0].Data)^.Z, 0, 0, 1);
End;

Procedure _glRotate3fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glRotatef(PPVector3f(Params^[0].Data)^^.X, 1, 0, 0);
  glRotatef(PPVector3f(Params^[0].Data)^^.Y, 0, 1, 0);
  glRotatef(PPVector3f(Params^[0].Data)^^.Z, 0, 0, 1);
End;

//==============================================================================

Procedure _glScalef(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glScalef(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^);
End;

Procedure _glScalefv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glScalef(PPVector3f(Params^[0].Data)^^.X,
    PPVector3f(Params^[0].Data)^^.Y,
    PPVector3f(Params^[0].Data)^^.Z);
End;

{$ENDREGION}
//==============================================================================

{$REGION 'Textures'}

Procedure _glGenTexture(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glGenTextures(1, PGLUInt(Result^.Data));
End;

Procedure _glGenTextures(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glGenTextures(PInteger(Params^[0].Data)^, PGLUInt(PPointer(Params^[1].Data)^));
End;

Procedure _glBindTexture(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glBindTexture(PCardinal(Params^[0].Data)^, PCardinal(Params^[1].Data)^);
End;

Procedure _glTexImage2D(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glTexImage2D(PCardinal(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^,
    PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^,
    PCardinal(Params^[6].Data)^, PCardinal(Params^[7].Data)^, PPointer(Params^[8].Data)^);
End;

Procedure _glCopyTexImage2d(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glCopyTexImage2d(PCardinal(Params^[0].Data)^, PInteger(Params^[1].Data)^, PCardinal(Params^[2].Data)^,
    PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^,
    PInteger(Params^[7].Data)^);
End;

Procedure _glTexParameteri(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glTexParameteri(PCardinal(Params^[0].Data)^, PCardinal(Params^[1].Data)^, PInteger(Params^[2].Data)^);
End;

{$ENDREGION}
//==============================================================================

{$REGION 'Colors'}

Procedure _glColor3f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glColor3f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^);
End;

Procedure _glColor3fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glColor3fv(PGLFloat(PPointer(Params^[0].Data)^));
End;

Procedure _glColor4f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glColor4f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^);
End;

Procedure _glColor4fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glColor4fv(PGLFloat(PPointer(Params^[0].Data)^));
End;

{$ENDREGION}
//==============================================================================

{$REGION 'Light'}

Procedure _glLighti(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glLighti({GL_LIGHT0 + }PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
End;

Procedure _glLightf(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glLightf({GL_LIGHT0 + }PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PSingle(Params^[2].Data)^);
End;

Procedure _glLight3f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var vec4f  : TVector4f;
Begin
  vec4f := Vector(PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^,
    PSingle(Params^[4].Data)^,
    1);
  glLightfv({GL_LIGHT0 + }PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, @vec4f);
End;

Procedure _glLight4f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  vec4f  : TVector4f;
Begin
  vec4f := Vector(PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^,
    PSingle(Params^[4].Data)^,
    PSingle(Params^[5].Data)^);
  glLightfv({GL_LIGHT0 + }PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, @vec4f);
End;

{$ENDREGION}
//==============================================================================

{$REGION 'Fog'}

Procedure _glFogi(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glFogi(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^);
End;

Procedure _glFogf(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glFogf(PInteger(Params^[0].Data)^, PSingle(Params^[1].Data)^);
End;

Procedure _glFog3f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var vec4f  : TVector4f;
Begin
  vec4f := Vector(PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^,
    1);
  glFogfv(PInteger(Params^[0].Data)^, @vec4f);
End;

Procedure _glFog4f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  vec4f  : TVector4f;
Begin
  vec4f := Vector(PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^,
    PSingle(Params^[4].Data)^);
  glFogfv(PInteger(Params^[0].Data)^, @vec4f);
End;

{$ENDREGION}
//==============================================================================

{$REGION 'DisplayLists'}

Procedure _glGenLists(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PCardinal(Result^.Data)^ := glGenLists(PCardinal(Params^[0].Data)^);
End;

Procedure _glNewList(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glNewList(PCardinal(Params^[0].Data)^, PCardinal(Params^[1].Data)^);
End;

Procedure _glEndList(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glEndList;
End;

Procedure _glCallList(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glCallList(PCardinal(Params^[0].Data)^);
End;

Procedure _glListBase(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glListBase(PCardinal(Params^[0].Data)^);
End;

{$ENDREGION}
//==============================================================================

{$REGION 'Vectors'}

Procedure _glVertex2f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glVertex2f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^);
End;

Procedure _glVertex3f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glVertex3f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^);
End;

Procedure _glVertex4f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glVertex4f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^,
    PSingle(Params^[3].Data)^);
End;

Procedure _glNormal3f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glNormal3f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^,
    PSingle(Params^[2].Data)^);
End;

//==============================================================================

Procedure _glVertex2fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glVertex2fv(PGLFloat(PPointer(Params^[0].Data)^));
End;

Procedure _glVertex3fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glVertex3fv(PGLFloat(PPointer(Params^[0].Data)^));
End;

Procedure _glVertex4fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glVertex4fv(PGLFloat(PPointer(Params^[0].Data)^));
End;

Procedure _glNormal3fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glNormal3fv(PGLFloat(PPointer(Params^[0].Data)^));
End;

//==============================================================================

Procedure _glTexCoord2f(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glTexCoord2f(PSingle(Params^[0].Data)^,
    PSingle(Params^[1].Data)^);
End;

Procedure _glTexCoord2fv(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  glTexCoord2fv(PGLFloat(PCardinal(Params^[0].Data)^));
End;

{$ENDREGION}
//==============================================================================

//==============================================================================
//==============================================================================

Procedure RegisterEXTMethods(Engine : TRutisEngine);
Begin
  //==============================================================================
  //====================== OPENGL ================================================
  //==============================================================================

  {$REGION 'gl-Extension glx'}
  Engine.RegExtMethod('glxInitOpenGL',{$IfDef FPC}@{$EndIf}_InitOpenGL, [], '');
  Engine.RegExtMethod('glxSetViewportControl',{$IfDef FPC}@{$EndIf}_glxSetViewportControl, ['Pointer'], '');
  Engine.RegExtMethod('glxNewFrame',{$IfDef FPC}@{$EndIf}_glxNewFrame, [], '');
  Engine.RegExtMethod('glxSwapBuffers',{$IfDef FPC}@{$EndIf}_glxSwapBuffers, [], '');
  Engine.RegExtMethod('glxSetNearFarClipping',{$IfDef FPC}@{$EndIf}_glxSetNearFarClipping, ['Single', 'Single'], '');
  Engine.RegExtMethod('glxApplyTexture',{$IfDef FPC}@{$EndIf}_glxApplyTexture, ['String'], '');
  Engine.RegExtMethod('glxGetTexture',{$IfDef FPC}@{$EndIf}_glxGetTexture, ['String'], 'Integer');
  Engine.RegExtMethod('glxCamPos',{$IfDef FPC}@{$EndIf}_glxCamPos, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glxCamRot',{$IfDef FPC}@{$EndIf}_glxCamRot, ['Single', 'Single', 'Single'], '');

  Engine.RegExtMethod('glxCalculateFrustum',{$IfDef FPC}@{$EndIf}_glxCalculateFrustum, [], '');
  Engine.RegExtMethod('glxIsPointInFrustum',{$IfDef FPC}@{$EndIf}_glxIsPointInFrustum, ['Single', 'Single', 'Single'], 'Boolean',
    '#1: X' + sLineBreak +
    '#2: Y' + sLineBreak +
    '#3: Z' + sLineBreak);
  Engine.RegExtMethod('glxIsSphereInFrustum',{$IfDef FPC}@{$EndIf}_glxIsSphereInFrustum, ['Single', 'Single', 'Single', 'Single'], 'Boolean',
    '#1: X' + sLineBreak +
    '#2: Y' + sLineBreak +
    '#3: Z' + sLineBreak +
    '#4: Radius' + sLineBreak);
  Engine.RegExtMethod('glxIsBoxInFrustum',{$IfDef FPC}@{$EndIf}_glxIsBoxInFrustum, ['Single', 'Single', 'Single', 'Single', 'Single', 'Single'], 'Boolean',
    '#1: X' + sLineBreak +
    '#2: Y' + sLineBreak +
    '#3: Z' + sLineBreak +
    '#4: X' + sLineBreak +
    '#5: Size-Y' + sLineBreak +
    '#6: Size-Z' + sLineBreak);
  Engine.RegExtMethod('glxIsPointInFrustumV',{$IfDef FPC}@{$EndIf}_glxIsPointInFrustumV, ['PVector3f'], 'Boolean',
    '#1: XYZ' + sLineBreak);
  Engine.RegExtMethod('glxIsSphereInFrustumV',{$IfDef FPC}@{$EndIf}_glxIsSphereInFrustumV, ['PVector3f', 'Single'], 'Boolean',
    '#1: XYZ' + sLineBreak +
    '#2: Radius' + sLineBreak);
  Engine.RegExtMethod('glxIsBoxInFrustumV',{$IfDef FPC}@{$EndIf}_glxIsBoxInFrustumV, ['PVector3f', 'PVector3f'], 'Boolean',
    '#1: XYZ' + sLineBreak +
    '#2: Size-XYZ');
  {$ENDREGION}


  {$REGION 'OpenGl not-native functions'}
  Engine.RegExtMethod('glxSwapBuffers',{$IfDef FPC}@{$EndIf}_glxSwapBuffers, [], '');
  Engine.RegExtMethod('glxApplyTexture',{$IfDef FPC}@{$EndIf}_glxApplyTexture, ['String'], '');
  Engine.RegExtMethod('glxGetTexture',{$IfDef FPC}@{$EndIf}_glxGetTexture, ['String'], 'Integer');
  Engine.RegExtMethod('glxCamPos',{$IfDef FPC}@{$EndIf}_glxCamPos, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glxCamRot',{$IfDef FPC}@{$EndIf}_glxCamRot, ['Single', 'Single', 'Single'], '');

  Engine.RegExtMethod('glSphere',{$IfDef FPC}@{$EndIf}_glSphere, ['Integer'], '');
  Engine.RegExtMethod('glCube',{$IfDef FPC}@{$EndIf}_glCube, ['Integer'], '');
  {$ENDREGION}

  //==============================================================================
  //====================== OPENGL native functions ===============================
  //==============================================================================

  {$REGION 'Other OpenGl Functions'}
  Engine.RegExtMethod('glBegin',{$IfDef FPC}@{$EndIf}_glBegin, ['Cardinal'], '');
  Engine.RegExtMethod('glEnd',{$IfDef FPC}@{$EndIf}_glEnd, [], '');
  Engine.RegExtMethod('glClear',{$IfDef FPC}@{$EndIf}_glClear, ['Cardinal'], '');
  Engine.RegExtMethod('glFlush',{$IfDef FPC}@{$EndIf}_glFlush, [], '');
  {$ENDREGION}
  {$REGION 'OpenGl-States'}
  Engine.RegExtMethod('glEnable',{$IfDef FPC}@{$EndIf}_glEnable, ['Integer'], '');
  Engine.RegExtMethod('glDisable',{$IfDef FPC}@{$EndIf}_glDisable, ['Integer'], '');
  Engine.RegExtMethod('glPushAttrib',{$IfDef FPC}@{$EndIf}_glPushAttrib, ['Integer'], '');
  Engine.RegExtMethod('glPopAttrib',{$IfDef FPC}@{$EndIf}_glPopAttrib, [], '');

  Engine.RegExtMethod('glCullFace',{$IfDef FPC}@{$EndIf}_glCullFace, ['Cardinal'], '');
  Engine.RegExtMethod('glViewport',{$IfDef FPC}@{$EndIf}_glViewport, ['Integer', 'Integer', 'Integer', 'Integer'], '',
    'procedure(x, y, width, height: Integer)');
  Engine.RegExtMethod('glLineWidth',{$IfDef FPC}@{$EndIf}_glLineWidth, ['Single'], '');
  Engine.RegExtMethod('glBlendFunc',{$IfDef FPC}@{$EndIf}_glBlendFunc, ['Cardinal', 'Cardinal'], '',
    'procedure(sfactor, dfactor: Cardinal)');
  Engine.RegExtMethod('glClearColor',{$IfDef FPC}@{$EndIf}_glClearColor, ['Single', 'Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glAccum',{$IfDef FPC}@{$EndIf}_glAccum, ['Cardinal', 'Single'], '',
    'procedure(op: Cardinal; value: Single)');
  Engine.RegExtMethod('glReadBuffer',{$IfDef FPC}@{$EndIf}_glReadBuffer, ['Cardinal'], '');
  Engine.RegExtMethod('glPolygonMode',{$IfDef FPC}@{$EndIf}_glPolygonMode, ['Cardinal','Cardinal'], '',
    'procedure(face: Cardinal; mode: Cardinal)');
  Engine.RegExtMethod('glPolygonOffset',{$IfDef FPC}@{$EndIf}_glPolygonOffset, ['Single','Single'], '',
    'procedure(factor, units: Single)');
  {$ENDREGION}
  {$REGION 'Matrix'}
  Engine.RegExtMethod('glMatrixMode',{$IfDef FPC}@{$EndIf}_glMatrixMode, ['Cardinal'], '');
  Engine.RegExtMethod('glPushMatrix',{$IfDef FPC}@{$EndIf}_glPushMatrix, [], '');
  Engine.RegExtMethod('glPopMatrix',{$IfDef FPC}@{$EndIf}_glPopMatrix, [], '');
  Engine.RegExtMethod('glLoadIdentity',{$IfDef FPC}@{$EndIf}_glLoadIdentity, [], '');
  //Engine.RegExtMethod('glMultMatrixf',{$IfDef FPC}@{$EndIf}_glMultMatrixf, ['TMatrix4f'], '');

  Engine.RegExtMethod('glOrtho',{$IfDef FPC}@{$EndIf}_glOrtho, ['Single', 'Single', 'Single', 'Single', 'Single', 'Single'], '',
    'procedure glOrtho(left, right, bottom, top, zNear, zFar: Single);');
  Engine.RegExtMethod('gluOrtho2D',{$IfDef FPC}@{$EndIf}_gluOrtho2D, ['Single', 'Single', 'Single', 'Single'], '',
    'procedure gluOrtho2D(left, right, bottom, top: Single);');
  Engine.RegExtMethod('gluPerspective',{$IfDef FPC}@{$EndIf}_gluPerspective, ['Single', 'Single', 'Single', 'Single'], '',
    'procedure gluPerspective(fovy, aspect, zNear, zFar: Single);');

  Engine.RegExtMethod('glTranslatef',{$IfDef FPC}@{$EndIf}_glTranslatef, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glTranslatefv',{$IfDef FPC}@{$EndIf}_glTranslatefv, ['PVector3f'], '');
  Engine.RegExtMethod('glRotatef',{$IfDef FPC}@{$EndIf}_glRotatef, ['Single', 'Single', 'Single', 'Single'], '',
    'procedure glRotatef(angle, x, y, z: Single);');
  Engine.RegExtMethod('glRotate3f',{$IfDef FPC}@{$EndIf}_glRotate3f, ['Single', 'Single', 'Single'], '',
    'procedure glRotate3f(x,y,z: Single);');
  Engine.RegExtMethod('glRotate3fv',{$IfDef FPC}@{$EndIf}_glRotate3fv, ['PVector3f'], '');
  Engine.RegExtMethod('glRotate3fvect',{$IfDef FPC}@{$EndIf}_glRotate3fvect, ['TVector3f'], '');
  Engine.RegExtMethod('glScalef',{$IfDef FPC}@{$EndIf}_glScalef, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glScalefv',{$IfDef FPC}@{$EndIf}_glScalefv, ['PVector3f'], '');
  {$ENDREGION}
  {$REGION 'Textures'}
  Engine.RegExtMethod('glGenTexture',{$IfDef FPC}@{$EndIf}_glGenTexture, [], 'Cardinal');
  Engine.RegExtMethod('glGenTextures',{$IfDef FPC}@{$EndIf}_glGenTextures, ['Integer', 'PCardinal'], '');
  Engine.RegExtMethod('glBindTexture',{$IfDef FPC}@{$EndIf}_glBindTexture, ['Cardinal', 'Cardinal'], '');
  Engine.RegExtMethod('glTexImage2D',{$IfDef FPC}@{$EndIf}_glTexImage2D, ['Cardinal', 'Integer',
    'Integer', 'Integer', 'Integer', 'Integer', 'Cardinal', 'Cardinal', 'Pointer'], '',
    'procedure(target: Cardinal; level, internalformat, width, height, border: Integer; format, _type: Cardinal; pixels: Pointer)');
  Engine.RegExtMethod('glCopyTexImage2d',{$IfDef FPC}@{$EndIf}_glCopyTexImage2d, ['Cardinal', 'Integer',
    'Cardinal', 'Integer', 'Integer', 'Integer', 'Integer', 'Integer'], '',
    'procedure(target: Cardinal; level: Integer; internalformat: Cardinal; x, y, width, height, border: Integer)');
  Engine.RegExtMethod('glTexParameteri',{$IfDef FPC}@{$EndIf}_glTexParameteri, ['Cardinal', 'Cardinal', 'Integer'], '',
    'procedure(target, pname: Cardinal; param: Integer)');
  //Engine.RegExtMethod('glMaterialf',{$IfDef FPC}@{$EndIf}_glMaterialf, ['Single'], '');
  //Engine.RegExtMethod('glMaterial4f',{$IfDef FPC}@{$EndIf}_glMaterial4f, ['Single','Single','Single','Single'], '');
  {$ENDREGION}
  {$REGION 'Colors'}
  Engine.RegExtMethod('glColor3f',{$IfDef FPC}@{$EndIf}_glColor3f, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glColor4f',{$IfDef FPC}@{$EndIf}_glColor4f, ['Single', 'Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glColor3fv',{$IfDef FPC}@{$EndIf}_glColor3fv, ['PVector3f'], '');
  Engine.RegExtMethod('glColor4fv',{$IfDef FPC}@{$EndIf}_glColor3fv, ['PVector4f'], '');
  {$ENDREGION}
  {$REGION 'Light'}
  Engine.RegExtMethod('glLighti',{$IfDef FPC}@{$EndIf}_glLighti, ['Integer', 'Integer', 'Integer'], '');
  Engine.RegExtMethod('glLightf',{$IfDef FPC}@{$EndIf}_glLightf, ['Integer', 'Integer', 'Single'], '');
  Engine.RegExtMethod('glLight3f',{$IfDef FPC}@{$EndIf}_glLight3f, ['Integer', 'Integer', 'Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glLight4f',{$IfDef FPC}@{$EndIf}_glLight4f, ['Integer', 'Integer', 'Single', 'Single', 'Single', 'Single'], '');
  {$ENDREGION}
  {$REGION 'Fog'}
  Engine.RegExtMethod('glFogi',{$IfDef FPC}@{$EndIf}_glFogi, ['Integer', 'Integer'], '');
  Engine.RegExtMethod('glFogf',{$IfDef FPC}@{$EndIf}_glFogf, ['Integer', 'Single'], '');
  Engine.RegExtMethod('glFog3f',{$IfDef FPC}@{$EndIf}_glFog3f, ['Integer', 'Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glFog4f',{$IfDef FPC}@{$EndIf}_glFog4f, ['Integer', 'Single', 'Single', 'Single', 'Single'], '');
  {$ENDREGION}
  {$REGION 'DisplayLists'}
  Engine.RegExtMethod('glGenLists',{$IfDef FPC}@{$EndIf}_glGenLists, ['Cardinal'], 'Cardinal');
  Engine.RegExtMethod('glNewList',{$IfDef FPC}@{$EndIf}_glNewList, ['Cardinal', 'Cardinal'], '');
  Engine.RegExtMethod('glEndList',{$IfDef FPC}@{$EndIf}_glEndList, [], '');
  Engine.RegExtMethod('glCallList',{$IfDef FPC}@{$EndIf}_glCallList, ['Cardinal'], '');
  Engine.RegExtMethod('glListBase',{$IfDef FPC}@{$EndIf}_glListBase, ['Cardinal'], '');
  {$ENDREGION}
  {$REGION 'Vectors'}
  Engine.RegExtMethod('glVertex2f',{$IfDef FPC}@{$EndIf}_glVertex2f, ['Single', 'Single'], '');
  Engine.RegExtMethod('glVertex3f',{$IfDef FPC}@{$EndIf}_glVertex3f, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glVertex4f',{$IfDef FPC}@{$EndIf}_glVertex4f, ['Single', 'Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glNormal3f',{$IfDef FPC}@{$EndIf}_glNormal3f, ['Single', 'Single', 'Single'], '');
  Engine.RegExtMethod('glVertex2fv',{$IfDef FPC}@{$EndIf}_glVertex2fv, ['PVector2f'], '');
  Engine.RegExtMethod('glVertex3fv',{$IfDef FPC}@{$EndIf}_glVertex3fv, ['PVector3f'], '');
  Engine.RegExtMethod('glVertex4fv',{$IfDef FPC}@{$EndIf}_glVertex4fv, ['PVector4f'], '');
  Engine.RegExtMethod('glNormal3fv',{$IfDef FPC}@{$EndIf}_glNormal3fv, ['PVector3f'], '');
  Engine.RegExtMethod('glTexCoord2f',{$IfDef FPC}@{$EndIf}_glTexCoord2f, ['Single', 'Single'], '');
  Engine.RegExtMethod('glTexCoord2fv',{$IfDef FPC}@{$EndIf}_glTexCoord2fv, ['PVector2f'], '');
  {$ENDREGION}
End;



End.

{
glGenLists
glDeleteLists
glCallList
glCallLists
glEndList
glListBase
glColorMaterial
glFrontFace
glPointSize
glDeleteTextures
glBindTexture
glDeleteTextures
glTexCoord2f
glTexCoord3f
glTexCoord4f
glTexGeni
glTexGenfv (4f)
glTexParameteri
glTexParameterf (4f)
glCopyTexImage2D
glCopyTexSubImage2D
glActiveTexture
glMultiTexCoord2f
glMultiTexCoord3f
glMultiTexCoord4f
glViewport

gluLookAt

glIsEnabled
glIsList
glIsTexture





glBlendFuncSeparate
glBlendColor
glBlendEquation
glBlendEquationSeparate
glClearDepth
glClearStencil
glClipPlane
glColorMask
glDepthFunc
glDepthMask
glDepthRange
glFinish
glFlush
glFrustum

glInitNames
glLoadName
glPushName
glPopName
glLightModelf (4f)
glLogicOp
glNewList
glRasterPos2f
glRasterPos3f
glWindowPos2f
glWindowPos3f
glRectf  (x1,y1,x2,y2)
glRenderMode
glSelectBuffer
glShadeModel
glStencilFunc
glStencilFuncSeparate
glStencilMask
glStencilMaskSeparate
glStencilOp
glStencilOpSeparate
glTexImage2D
glTexSubImage2D
glTexImage3D
glTexSubImage3D
glFogCoordf
glGenQueries
glDeleteQueries
glIsQuery
glBeginQuery
glEndQuery
glGetQueryiv
glGetQueryObjectiv

gluPickMatrix
gluLookAt
gluProject
gluUnProject
gluScaleImage
gluBuild2DMipmaps

  gluNewQuadric: TgluNewQuadric;
  gluDeleteQuadric: TgluDeleteQuadric;
  gluQuadricNormals: TgluQuadricNormals;
  gluQuadricTexture: TgluQuadricTexture;
  gluQuadricOrientation: TgluQuadricOrientation;
  gluQuadricDrawStyle: TgluQuadricDrawStyle;
  gluCylinder: TgluCylinder;
  gluDisk: TgluDisk;
  gluPartialDisk: TgluPartialDisk;
  gluSphere: TgluSphere;
  gluQuadricCallback: TgluQuadricCallback;
  gluNewTess: TgluNewTess;
  gluDeleteTess: TgluDeleteTess;
  gluTessBeginPolygon: TgluTessBeginPolygon;
  gluTessBeginContour: TgluTessBeginContour;
  gluTessVertex: TgluTessVertex;
  gluTessEndContour: TgluTessEndContour;
  gluTessEndPolygon: TgluTessEndPolygon;
  gluTessProperty: TgluTessProperty;
  gluTessNormal: TgluTessNormal;
  gluTessCallback: TgluTessCallback;
  gluGetTessProperty: TgluGetTessProperty;
  gluNewNurbsRenderer: TgluNewNurbsRenderer;
  gluDeleteNurbsRenderer: TgluDeleteNurbsRenderer;
  gluBeginSurface: TgluBeginSurface;
  gluBeginCurve: TgluBeginCurve;
  gluEndCurve: TgluEndCurve;
  gluEndSurface: TgluEndSurface;
  gluBeginTrim: TgluBeginTrim;
  gluEndTrim: TgluEndTrim;
  gluPwlCurve: TgluPwlCurve;
  gluNurbsCurve: TgluNurbsCurve;
  gluNurbsSurface: TgluNurbsSurface;
  gluLoadSamplingMatrices: TgluLoadSamplingMatrices;
  gluNurbsProperty: TgluNurbsProperty;
  gluGetNurbsProperty: TgluGetNurbsProperty;
  gluNurbsCallback: TgluNurbsCallback;
  gluBeginPolygon: TgluBeginPolygon;
  gluNextContour: TgluNextContour;
  gluEndPolygon: TgluEndPolygon;

  glGetBooleanv: TglGetBooleanv;
  glGetClipPlane: TglGetClipPlane;
  glGetDoublev: TglGetDoublev;
  glGetError: TglGetError;
  glGetFloatv: TglGetFloatv;
  glGetIntegerv: TglGetIntegerv;
  glGetLightfv: TglGetLightfv;
  glGetLightiv: TglGetLightiv;
  glGetMapdv: TglGetMapdv;
  glGetMapfv: TglGetMapfv;
  glGetMapiv: TglGetMapiv;
  glGetMaterialfv: TglGetMaterialfv;
  glGetMaterialiv: TglGetMaterialiv;
  glGetPixelMapfv: TglGetPixelMapfv;
  glGetPixelMapuiv: TglGetPixelMapuiv;
  glGetPixelMapusv: TglGetPixelMapusv;
  glGetPointerv: TglGetPointerv;
  glGetPolygonStipple: TglGetPolygonStipple;
  glGetTexEnvfv: TglGetTexEnvfv;
  glGetTexEnviv: TglGetTexEnviv;
  glGetTexGendv: TglGetTexGendv;
  glGetTexGenfv: TglGetTexGenfv;
  glGetTexGeniv: TglGetTexGeniv;
  glGetTexImage: TglGetTexImage;
  glGetTexLevelParameterfv: TglGetTexLevelParameterfv;
  glGetTexLevelParameteriv: TglGetTexLevelParameteriv;
  glGetTexParameterfv: TglGetTexParameterfv;
  glGetTexParameteriv: TglGetTexParameteriv;

}
