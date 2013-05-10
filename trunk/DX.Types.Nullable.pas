unit DX.Types.Nullable;

/// <summary>
/// DX.Types.Nullable provides a Nullable Type for Delphi.
/// </summary>
/// <remarks>
/// <para>
/// DX.Rest.Client is part of DX.Librayry<br />See:
/// http://code.google.com/p/dx-library/ See below for license
/// information of its original author:
/// </para>
/// <para>
/// Copyright (c) 2012, Stefan Glienke All rights reserved.
/// Redistribution and use in source and binary forms, with or without
/// modification, are permitted provided that the following conditions
/// are met: - Redistributions of source code must retain the above
/// copyright notice, this list of conditions and the following
/// disclaimer. - Redistributions in binary form must reproduce the above
/// copyright notice, this list of conditions and the following
/// disclaimer in the documentation and/or other materials provided with
/// the distribution. - Neither the name of this library nor the names of
/// its contributors may be used to endorse or promote products derived
/// from this software without specific prior written permission. THIS
/// SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
/// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
/// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
/// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
/// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
/// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
/// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
/// OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
/// AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
/// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
/// WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
/// POSSIBILITY OF SUCH DAMAGE.
/// </para>
/// </remarks>

interface

uses
  Classes,
  SysUtils;

type

  Nullable<T> = record
  private
    FValue: T;
    FHasValue: string;
    procedure Clear;
    function GetValue: T;
    function GetHasValue: Boolean;
  public
    constructor Create(const Value: T); overload;
    constructor Create(const Value: Variant); overload;
    function Equals(const Value: Nullable<T>): Boolean;
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(const Default: T): T; overload;

    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue;

    class operator Implicit(const Value: Nullable<T>): T;
    class operator Implicit(const Value: Nullable<T>): Variant;
    class operator Implicit(const Value: Pointer): Nullable<T>;
    class operator Implicit(const Value: T): Nullable<T>;
    class operator Implicit(const Value: Variant): Nullable<T>;
    class operator Equal(const Left, Right: Nullable<T>): Boolean;
    class operator NotEqual(const Left, Right: Nullable<T>): Boolean;
  end;

{$IF CompilerVersion = 21}

  EInvalidOpException = class(Exception);
{$IFEND}

resourcestring
  RNullableNoValue = 'Nullable type has no value';

implementation

uses
  System.Variants,
  System.Rtti,
  System.Generics.Defaults;

{ Nullable<T> }

constructor Nullable<T>.Create(const Value: T);
begin
  FValue := Value;
  FHasValue := DefaultTrueBoolStr;
end;

constructor Nullable<T>.Create(const Value: Variant);
begin
  if not VarIsNull(Value) and not VarIsEmpty(Value) then
    Create(TValue.FromVariant(Value).AsType<T>)
  else
    Clear;
end;

procedure Nullable<T>.Clear;
begin
  FValue := Default (T);
  FHasValue := '';
end;

function Nullable<T>.Equals(const Value: Nullable<T>): Boolean;
begin
  if HasValue and Value.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Self.Value, Value.Value)
  else
    Result := HasValue = Value.HasValue;
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := FHasValue <> '';
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    raise EInvalidOpException.CreateRes(@RNullableNoValue);
  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault(const Default: T): T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  Result := GetValueOrDefault(Default (T));
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): Variant;
begin
  if Value.HasValue then
    Result := TValue.From<T>(Value.Value).AsVariant
  else
    Result := Null;
end;

class operator Nullable<T>.Implicit(const Value: Pointer): Nullable<T>;
begin
  if Value = nil then
    Result.Clear
  else
    Result := Nullable<T>.Create(T(Value^));
end;

class operator Nullable<T>.Implicit(const Value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(Value);
end;

class operator Nullable<T>.Implicit(const Value: Variant): Nullable<T>;
begin
  Result := Nullable<T>.Create(Value);
end;

class operator Nullable<T>.Equal(const Left, Right: Nullable<T>): Boolean;
begin
  Result := Left.Equals(Right);
end;

class operator Nullable<T>.NotEqual(const Left, Right: Nullable<T>): Boolean;
begin
  Result := not Left.Equals(Right);
end;

end.
