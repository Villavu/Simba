{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Redirects the input of 3 certain variables to the correct evaluation emitter function.
}
{%MainUnit lpvartypes.pas}

case d of
  mmpPtr: case l of
    mmpPtr: case r of
      mmpNone: Result := _Eval__PtrD_PtrL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.GlobalVar.Ptr, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PtrD_PtrL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.GlobalVar.Ptr, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PtrD_PtrL_VarR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PtrD_PtrL_StkR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.GlobalVar.Ptr, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PtrD_PtrL_PVarR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PtrD_PtrL_PStkR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.GlobalVar.Ptr, Offset, Pos);
      else e := True;
    end;
    mmpVar: case r of
      mmpNone: Result := _Eval__PtrD_VarL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PtrD_VarL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PtrD_VarL_VarR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PtrD_VarL_StkR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PtrD_VarL_PVarR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PtrD_VarL_PStkR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpStk: case r of
      mmpNone: Result := _Eval__PtrD_StkL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType], nil, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__PtrD_StkL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.GlobalVar.Ptr, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__PtrD_StkL_VarR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset + Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__PtrD_StkL_StkR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PtrD_StkL_PVarR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset, Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__PtrD_StkL_PStkR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpPVar: case r of
      mmpNone: Result := _Eval__PtrD_PVarL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PtrD_PVarL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PtrD_PVarL_VarR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PtrD_PVarL_StkR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PtrD_PVarL_PVarR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PtrD_PVarL_PStkR(AProc, Dest.VarPos.GlobalVar.Ptr, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpPStk: case r of
      mmpNone: Result := _Eval__PtrD_PStkL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PtrD_PStkL_PtrR(AProc, Dest.VarPos.GlobalVar.Ptr, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PtrD_PStkL_VarR(AProc, Dest.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PtrD_PStkL_StkR(AProc, Dest.VarPos.GlobalVar.Ptr, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -SizeOf(Pointer) - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PtrD_PStkL_PVarR(AProc, Dest.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PtrD_PStkL_PStkR(AProc, Dest.VarPos.GlobalVar.Ptr, Offset, Pos);
      else e := True;
    end;
    else e := True;
  end;
  mmpVar: case l of
    mmpPtr: case r of
      mmpNone: Result := _Eval__VarD_PtrL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, nil, Offset, Pos);
      mmpPtr: Result := _Eval__VarD_PtrL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__VarD_PtrL_VarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__VarD_PtrL_StkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__VarD_PtrL_PVarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__VarD_PtrL_PStkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Offset, Pos);
      else e := True;
    end;
    mmpVar: case r of
      mmpNone: Result := _Eval__VarD_VarL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__VarD_VarL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__VarD_VarL_VarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__VarD_VarL_StkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__VarD_VarL_PVarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__VarD_VarL_PStkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpStk: case r of
      mmpNone: Result := _Eval__VarD_StkL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], nil, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__VarD_StkL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.GlobalVar.Ptr, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__VarD_StkL_VarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset + Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__VarD_StkL_StkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__VarD_StkL_PVarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset, Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__VarD_StkL_PStkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpPVar: case r of
      mmpNone: Result := _Eval__VarD_PVarL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__VarD_PVarL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__VarD_PVarL_VarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__VarD_PVarL_StkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__VarD_PVarL_PVarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__VarD_PVarL_PStkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpPStk: case r of
      mmpNone: Result := _Eval__VarD_PStkL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__VarD_PStkL_PtrR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__VarD_PStkL_VarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__VarD_PStkL_StkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -SizeOf(Pointer) - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__VarD_PStkL_PVarR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__VarD_PStkL_PStkR(AProc, Dest.VarPos.StackVar.Offset + Dest.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    else e := True;
  end;
  mmpStk: case l of
    mmpPtr: case r of
      mmpNone: Result := _Eval__StkD_PtrL_PtrR(AProc, Left.VarPos.GlobalVar.Ptr, nil, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__StkD_PtrL_PtrR(AProc, Left.VarPos.GlobalVar.Ptr, Right.VarPos.GlobalVar.Ptr, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__StkD_PtrL_VarR(AProc, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__StkD_PtrL_StkR(AProc, Left.VarPos.GlobalVar.Ptr, LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__StkD_PtrL_PVarR(AProc, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__StkD_PtrL_PStkR(AProc, Left.VarPos.GlobalVar.Ptr, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpVar: case r of
      mmpNone: Result := _Eval__StkD_VarL_PtrR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, nil, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__StkD_VarL_PtrR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__StkD_VarL_VarR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__StkD_VarL_StkR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__StkD_VarL_PVarR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__StkD_VarL_PStkR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpStk: case r of
      mmpNone: Result := _Eval__StkD_StkL_PtrR(AProc, LapeTypeSize[Left.VarType.BaseType], nil, LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__StkD_StkL_PtrR(AProc, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.GlobalVar.Ptr, LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__StkD_StkL_VarR(AProc, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset + Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__StkD_StkL_StkR(AProc, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Left.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__StkD_StkL_PVarR(AProc, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset, Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__StkD_StkL_PStkR(AProc, LapeTypeSize[Left.VarType.BaseType], LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Left.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpPVar: case r of
      mmpNone: Result := _Eval__StkD_PVarL_PtrR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, nil, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__StkD_PVarL_PtrR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__StkD_PVarL_VarR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__StkD_PVarL_StkR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Dest.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__StkD_PVarL_PVarR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__StkD_PVarL_PStkR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpPStk: case r of
      mmpNone: Result := _Eval__StkD_PStkL_PtrR(AProc, nil, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      mmpPtr: Result := _Eval__StkD_PStkL_PtrR(AProc, Right.VarPos.GlobalVar.Ptr, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      mmpVar: Result := _Eval__StkD_PStkL_VarR(AProc, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      mmpStk: Result := _Eval__StkD_PStkL_StkR(AProc, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer) - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__StkD_PStkL_PVarR(AProc, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      mmpPStk: Result := _Eval__StkD_PStkL_PStkR(AProc, LapeTypeSize[Dest.VarType.BaseType] - SizeOf(Pointer) - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    else e := True;
  end;
  mmpPVar: case l of
    mmpPtr: case r of
      mmpNone: Result := _Eval__PVarD_PtrL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PVarD_PtrL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PVarD_PtrL_VarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PVarD_PtrL_StkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PVarD_PtrL_PVarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PVarD_PtrL_PStkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.GlobalVar.Ptr, Offset, Pos);
      else e := True;
    end;
    mmpVar: case r of
      mmpNone: Result := _Eval__PVarD_VarL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PVarD_VarL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PVarD_VarL_VarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PVarD_VarL_StkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PVarD_VarL_PVarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PVarD_VarL_PStkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpStk: case r of
      mmpNone: Result := _Eval__PVarD_StkL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], nil, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__PVarD_StkL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.GlobalVar.Ptr, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__PVarD_StkL_VarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset + Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__PVarD_StkL_StkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PVarD_StkL_PVarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset, Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__PVarD_StkL_PStkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpPVar: case r of
      mmpNone: Result := _Eval__PVarD_PVarL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PVarD_PVarL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PVarD_PVarL_VarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PVarD_PVarL_StkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PVarD_PVarL_PVarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PVarD_PVarL_PStkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpPStk: case r of
      mmpNone: Result := _Eval__PVarD_PStkL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PVarD_PStkL_PtrR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PVarD_PStkL_VarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PVarD_PStkL_StkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -SizeOf(Pointer) - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PVarD_PStkL_PVarR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PVarD_PStkL_PStkR(AProc, Dest.VarPos.StackVar.Offset, Dest.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    else e := True;
  end;
  {$IFDEF Lape_PStkD}
  mmpPStk: case l of
    mmpPtr: case r of
      mmpNone: Result := _Eval__PStkD_PtrL_PtrR(AProc, Left.VarPos.GlobalVar.Ptr, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PStkD_PtrL_PtrR(AProc, Left.VarPos.GlobalVar.Ptr, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PStkD_PtrL_VarR(AProc, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PStkD_PtrL_StkR(AProc, Left.VarPos.GlobalVar.Ptr, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PStkD_PtrL_PVarR(AProc, Left.VarPos.GlobalVar.Ptr, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PStkD_PtrL_PStkR(AProc, Left.VarPos.GlobalVar.Ptr, Offset, Pos);
      else e := True;
    end;
    mmpVar: case r of
      mmpNone: Result := _Eval__PStkD_VarL_PtrR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PStkD_VarL_PtrR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PStkD_VarL_VarR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PStkD_VarL_StkR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PStkD_VarL_PVarR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PStkD_VarL_PStkR(AProc, Left.VarPos.StackVar.Offset + Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpStk: case r of
      mmpNone: Result := _Eval__PStkD_StkL_PtrR(AProc, LapeTypeSize[Left.VarType.BaseType], nil, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPtr: Result := _Eval__PStkD_StkL_PtrR(AProc, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.GlobalVar.Ptr, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpVar: Result := _Eval__PStkD_StkL_VarR(AProc, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset + Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpStk: Result := _Eval__PStkD_StkL_StkR(AProc, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PStkD_StkL_PVarR(AProc, LapeTypeSize[Left.VarType.BaseType], Right.VarPos.StackVar.Offset, Right.VarPos.Offset, -LapeTypeSize[Left.VarType.BaseType], Offset, Pos);
      mmpPStk: Result := _Eval__PStkD_StkL_PStkR(AProc, LapeTypeSize[Left.VarType.BaseType], -LapeTypeSize[Left.VarType.BaseType] - SizeOf(Pointer), Offset, Pos);
      else e := True;
    end;
    mmpPVar: case r of
      mmpNone: Result := _Eval__PStkD_PVarL_PtrR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PStkD_PVarL_PtrR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PStkD_PVarL_VarR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PStkD_PVarL_StkR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, LapeTypeSize[Right.VarType.BaseType], -LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PStkD_PVarL_PVarR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PStkD_PVarL_PStkR(AProc, Left.VarPos.StackVar.Offset, Left.VarPos.Offset, Offset, Pos);
      else e := True;
    end;
    mmpPStk: case r of
      mmpNone: Result := _Eval__PStkD_PStkL_PtrR(AProc, nil, Offset, Pos);
      mmpPtr: Result := _Eval__PStkD_PStkL_PtrR(AProc, Right.VarPos.GlobalVar.Ptr, Offset, Pos);
      mmpVar: Result := _Eval__PStkD_PStkL_VarR(AProc, Right.VarPos.StackVar.Offset + Right.VarPos.Offset, Offset, Pos);
      mmpStk: Result := _Eval__PStkD_PStkL_StkR(AProc, LapeTypeSize[Left.VarType.BaseType] + LapeTypeSize[Right.VarType.BaseType], LapeTypeSize[Right.VarType.BaseType], -SizeOf(Pointer) - LapeTypeSize[Right.VarType.BaseType], Offset, Pos);
      mmpPVar: Result := _Eval__PStkD_PStkL_PVarR(AProc, Right.VarPos.StackVar.Offset, Right.VarPos.Offset, Offset, Pos);
      mmpPStk: Result := _Eval__PStkD_PStkL_PStkR(AProc, Offset, Pos);
      else e := True;
    end;
    else e := True;
  end;
  {$ENDIF}
  else e := True;
end;
