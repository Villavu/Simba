function ps_GetPage(S: String): String; stdcall;
begin
  result := GetPage(S);
end;
