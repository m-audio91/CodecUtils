unit uGenericSubtitleFile;
{ base generic class for a text based subtitle file. has some abstract methods
  for descendant classes. look at uSubripFile for example.

  Copyright (C) 2017 Mohammadreza Bahrami m.audio91@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uTimeSlice;  

type

  { TPlainSubtitleDialog }

  TPlainSubtitleDialog = record
    TimeSlice: TTimeSlice;
    Text: String;
    // any dialog type should contain at least exactly these two fields.
  end;

  { TGenericSubtitleFile }

  generic TGenericSubtitleFile<T> = class
  public
    type TGenericSubtitleDialogs = specialize TArray<T>;
  strict private
    FList: TGenericSubtitleDialogs;
  private
    FCount: Integer;
    FTimeSlice: TTimeSlice;
    procedure CheckIndex(AIndex: Integer); inline;
    procedure CheckCapacity(Amount: Integer = 250);
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
    function GetDialog(Index: Integer): T;
    procedure SetDialog(Index: Integer; AValue: T);
    function DialogsInRange(ARange: TTimeSlice): TGenericSubtitleDialogs;
    procedure SetValue(AValue: TGenericSubtitleDialogs);
    function GetValue: TGenericSubtitleDialogs;
  public
    procedure Clear;
    procedure Cleanup; virtual;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromString(const AContents: String); virtual; abstract;
    procedure SaveToFile(const AFileName: String);
    procedure SaveToString(out AContents: String); virtual; abstract;
    function NewDialog: Integer;
    function AddDialog(AValue: T): Integer;
    procedure AddDialogs(AValue: TGenericSubtitleDialogs);
    procedure RemoveDialog(AIndex: Integer);
    function Count: Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    function MakeNewFromRanges(ARanges: TTimeSliceList; AFinalStartOffset
      : Double = 0): TGenericSubtitleDialogs;
    procedure FixOverlapsForward;
    procedure FixOverlapsBackward;
  public
    property Dialogs[Index: Integer]: T read GetDialog write SetDialog; default;
    property Value: TGenericSubtitleDialogs read GetValue write SetValue;
    constructor Create; virtual;
    destructor Destroy; override;
  end; 


implementation

{ TGenericSubtitleFile }

procedure TGenericSubtitleFile.CheckIndex(AIndex: Integer);
begin
  if (AIndex < Low(FList)) or (AIndex > FCount-1) then
    raise EListError.Create('Out of bounds index');
end;

procedure TGenericSubtitleFile.CheckCapacity(Amount: Integer);
begin
  if (GetCapacity-FCount) < Amount then
    SetCapacity(GetCapacity+Amount);
end;

function TGenericSubtitleFile.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

procedure TGenericSubtitleFile.SetCapacity(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  SetLength(FList, AValue);
end;

function TGenericSubtitleFile.GetDialog(Index: Integer): T;
begin
  CheckIndex(Index);
  Result := FList[Index];
end;

procedure TGenericSubtitleFile.SetDialog(Index: Integer; AValue: T);
begin
  CheckIndex(Index);
  FList[Index] := AValue;
  FTimeSlice.Value.StartPos.Value := AValue.TimeSlice.Value.StartPos.Value;
  FTimeSlice.Value.EndPos.Value := AValue.TimeSlice.Value.EndPos.Value;
  FList[Index].TimeSlice := FTimeSlice;
end;

function TGenericSubtitleFile.DialogsInRange(ARange: TTimeSlice
  ): TGenericSubtitleDialogs;
var
  i,j: Integer;
begin
  SetLength(Result, FCount);
  j := 0;
  for i := 0 to FCount-1 do
  begin
    if ((FList[i].TimeSlice.Value.StartPos.ValueAsDouble >= ARange.Value.StartPos.ValueAsDouble)
      and (FList[i].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.EndPos.ValueAsDouble))
    or ((FList[i].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.StartPos.ValueAsDouble)
      and (FList[i].TimeSlice.Value.EndPos.ValueAsDouble > ARange.Value.StartPos.ValueAsDouble)) then
    begin
      Result[j] := FList[i];
      Inc(j);
    end;
  end;
  SetLength(Result, j);
  for i := 0 to j-1 do
  begin
    if Result[i].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.StartPos.ValueAsDouble then
      Result[i].TimeSlice.Value.StartPos.Value := ARange.Value.StartPos.Value
    else if Result[i].TimeSlice.Value.EndPos.ValueAsDouble > ARange.Value.EndPos.ValueAsDouble then
      Result[i].TimeSlice.Value.EndPos.Value := ARange.Value.EndPos.Value;
  end;
end;

procedure TGenericSubtitleFile.SetValue(AValue: TGenericSubtitleDialogs);
var
  i: Integer;
begin
  Clear;
  CheckCapacity(Length(AValue));
  for i := 0 to High(AValue) do
    AddDialog(AValue[i]);
end;

function TGenericSubtitleFile.GetValue: TGenericSubtitleDialogs;
var
  i: Integer;
begin
  SetLength(Result, FCount);
  for i := 0 to FCount-1 do
    Result[i] := FList[i];
end;

procedure TGenericSubtitleFile.Clear;
begin
  FList := nil;
  FCount := 0;
  SetCapacity(250);
end;

procedure TGenericSubtitleFile.Cleanup;
var
  i: Integer;
begin
  for i := FCount-1 downto 0 do
    if not FList[i].TimeSlice.Valid then
      RemoveDialog(i);
end;

procedure TGenericSubtitleFile.LoadFromFile(const AFileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    LoadFromString(sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TGenericSubtitleFile.SaveToFile(const AFileName: String);
var
  sl: TStringList;
  s: String;
begin
  SaveToString(s);
  if s = EmptyStr then Exit;
  sl := TStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

function TGenericSubtitleFile.NewDialog: Integer;
begin
  CheckCapacity;
  Inc(FCount);
  Result := FCount-1;
end;

function TGenericSubtitleFile.AddDialog(AValue: T): Integer;
begin
  Result := NewDialog;
  SetDialog(Result, AValue);
end;

procedure TGenericSubtitleFile.AddDialogs(AValue: TGenericSubtitleDialogs);
var
  i: Integer;
begin
  for i := 0 to High(AValue) do
    AddDialog(AValue[i]);
end;

procedure TGenericSubtitleFile.RemoveDialog(AIndex: Integer);
var
  i: Integer;
begin
  CheckIndex(AIndex);
  for i := AIndex+1 to FCount-1 do
    FList[i-1] := FList[i];
  Dec(FCount);
end;

function TGenericSubtitleFile.Count: Integer;
begin
  Result := FCount;
end;

function TGenericSubtitleFile.MakeNewFromRanges(ARanges: TTimeSliceList;
  AFinalStartOffset: Double = 0): TGenericSubtitleDialogs;
var
  dlgs, dlgsr: TGenericSubtitleDialogs;
  ts: TTimeSlice;
  i,j,k: Integer;
  Offset: Double;
begin
  Result := nil;
  if not ARanges.Valid then Exit;
  if FCount < 1 then Exit;
  SetLength(dlgs, FCount);

  k := 0;
  Offset := 0;
  for i := 0 to ARanges.Count-1 do
  begin
    dlgsr := DialogsInRange(ARanges.Values[i]);
    if Length(dlgsr) < 1 then Continue;

    ts.Reset;
    if i > 0 then
      ts.Value.StartPos.Value := ARanges.Values[i-1].Value.EndPos.Value;
    ts.Value.EndPos.Value := ARanges.Values[i].Value.StartPos.Value;
    Offset := Offset +ts.Duration.ValueAsDouble;

    for j := 0 to High(dlgsr) do
    begin
      dlgs[k] := dlgsr[j];
      dlgs[k].TimeSlice.Delay := -Offset +ARanges.Values[i].Delay;
      Inc(k);
    end;
  end;
  SetLength(dlgs, k);

  if AFinalStartOffset > 0 then
    for i := 0 to k-1 do
      dlgs[i].TimeSlice.Delay := dlgs[i].TimeSlice.Delay +AFinalStartOffset;

  for i := 0 to k-1 do
  begin
    dlgs[i].TimeSlice.Value := dlgs[i].TimeSlice.ValueWithDelay;
    dlgs[i].TimeSlice.Delay := 0;
  end;

  Result := dlgs;
end;

procedure TGenericSubtitleFile.FixOverlapsForward;
var
  i: Integer;
begin
  for i := FCount-1 downto 1 do
    if FList[i].TimeSlice.Value.StartPos.ValueAsDouble
    < FList[i-1].TimeSlice.Value.EndPos.ValueAsDouble then
      FList[i].TimeSlice.Value.StartPos.ValueAsDouble :=
        FList[i-1].TimeSlice.Value.EndPos.ValueAsDouble+0.001;
end;

procedure TGenericSubtitleFile.FixOverlapsBackward;
var
  i: Integer;
begin
  for i := 0 to FCount-2 do
    if FList[i].TimeSlice.Value.EndPos.ValueAsDouble
    > FList[i+1].TimeSlice.Value.StartPos.ValueAsDouble then
      FList[i].TimeSlice.Value.EndPos.ValueAsDouble :=
        FList[i+1].TimeSlice.Value.StartPos.ValueAsDouble-0.001;
end;

constructor TGenericSubtitleFile.Create;
begin
  FCount := 0;
end;

destructor TGenericSubtitleFile.Destroy;
begin 
  FList := nil;
  inherited Destroy;
end;

end.
