{==============================================================================]
  Copyright © 2018, Jarl Krister Holta
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
[==============================================================================}
procedure Size(a: T2DIntArray; out W,H: Int32);
begin 
  H := Length(a); 
  if H > 0 then  W := Length(a[0])
  else           W := 0;
end;

procedure Size(a: T2DI64Array; out W,H: Int32);
begin
  H := Length(a);
  if H > 0 then  W := Length(a[0])
  else           W := 0;
end;

procedure Size(a: T2DSingleArray; out W,H: Int32);
begin 
  H := Length(a); 
  if H > 0 then  W := Length(a[0])
  else           W := 0;
end;

procedure Size(a: T2DDoubleArray; out W,H: Int32);
begin 
  H := Length(a); 
  if H > 0 then  W := Length(a[0])
  else           W := 0;
end;

procedure Size(a: T2DComplexArray; out W,H: Int32);
begin
  H := Length(a);
  if H > 0 then  W := Length(a[0])
  else           W := 0;
end;

// ----------------------------------------------------------------------------
//

function Area(a: T2DIntArray): Int32;
var H: Int32;
begin
  H := Length(a);
  if H > 0 then  Result := H*Length(a[0])
  else           Result := 0;
end;

function Area(a: T2DI64Array): Int32;
var H: Int32;
begin
  H := Length(a);
  if H > 0 then  Result := H*Length(a[0])
  else           Result := 0;
end;

function Area(a: T2DSingleArray): Int32;
var H: Int32;
begin
  H := Length(a);
  if H > 0 then  Result := H*Length(a[0])
  else           Result := 0;
end;

function Area(a: T2DDoubleArray): Int32;
var H: Int32;
begin
  H := Length(a);
  if H > 0 then  Result := H*Length(a[0])
  else           Result := 0;
end;

function Area(a: T2DComplexArray): Int32;
var H: Int32;
begin
  H := Length(a);
  if H > 0 then  Result := H*Length(a[0])
  else           Result := 0;
end;

