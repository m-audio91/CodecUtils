unit uGenericSubtitleFile;
{ Subrip file (srt) decoder and encoder.

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
  strict protected
    FList: TGenericSubtitleDialogs;
    FTimeSlice: TTimeSlice;
  private
    procedure CheckIndex(AIndex: Integer); inline;
    function GetDialog(Index: Integer): T;
    procedure SetDialog(Index: Integer; AValue: T);
    function DialogsInRange(ARange: TTimeSlice): TGenericSubtitleDialogs;
    procedure SetValue(AValue: TGenericSubtitleDialogs);
  public
    procedure Clear;
    procedure Cleanup; virtual;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromString(const AContents: String); virtual; abstract;
    procedure SaveToFile(const AFileName: String);
    procedure SaveToString(out AContents: String); virtual; abstract;
    function AddNew: Integer;
    function Add(AValue: T): Integer;
    procedure AddDialogs(AValue: TGenericSubtitleDialogs);
    procedure RemoveDialog(AIndex: Integer);
    function Count: Integer;
    function MakeNewFromRanges(ARanges: TTimeSliceList; AStartTime: Double
      = 0): TGenericSubtitleDialogs;
  public
    property Dialogs[Index: Integer]: T read GetDialog write SetDialog; default;
    property Value: TGenericSubtitleDialogs read FList write SetValue;
    constructor Create; virtual;
    destructor Destroy; override;
  end; 


implementation

{ TGenericSubtitleFile }

procedure TGenericSubtitleFile.CheckIndex(AIndex: Integer);
begin
  if (AIndex < Low(FList)) or (AIndex > High(FList)) then
    raise EListError.Create('Out of bounds index');
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
end;

function TGenericSubtitleFile.DialogsInRange(ARange: TTimeSlice
  ): TGenericSubtitleDialogs;
var
  i,j: Integer;
begin
  SetLength(Result, Length(FList));
  j := 0;
  for i := 0 to High(FList) do
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
  if Length(Result) < 1 then Exit;
  if Result[0].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.StartPos.ValueAsDouble then
    Result[0].TimeSlice.Value.StartPos.Value := ARange.Value.StartPos.Value;
  if Result[j-1].TimeSlice.Value.EndPos.ValueAsDouble > ARange.Value.EndPos.ValueAsDouble then
    Result[j-1].TimeSlice.Value.EndPos.Value := ARange.Value.EndPos.Value;
end;

procedure TGenericSubtitleFile.SetValue(AValue: TGenericSubtitleDialogs);
var
  i: Integer;
begin
  if Length(AValue) < 1 then Exit;
  FList := nil;
  SetLength(FList, Length(AValue));
  for i := 0 to High(AValue) do
  begin
    FTimeSlice.Value.StartPos.Value := AValue[i].TimeSLice.Value.StartPos.Value;
    FTimeSlice.Value.EndPos.Value := AValue[i].TimeSLice.Value.EndPos.Value;
    FList[i].TimeSlice := FTimeSlice;
    FList[i].Text := AValue[i].Text;
  end;
end;

procedure TGenericSubtitleFile.Clear;
begin
  FList := nil;
end;

procedure TGenericSubtitleFile.Cleanup;
var
  i: Integer;
begin
  for i := 0 to High(FList)-1 do
  begin
    if FList[i].TimeSlice.Value.EndPos.ValueAsDouble
    > FList[i+1].TimeSlice.Value.StartPos.ValueAsDouble then
      FList[i].TimeSlice.Value.EndPos.ValueAsDouble :=
        FList[i+1].TimeSlice.Value.StartPos.ValueAsDouble-0.001;
  end;
  for i := High(FList) downto 0 do
  begin
    if not FList[i].TimeSlice.Valid then
      RemoveDialog(i);
  end;
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

function TGenericSubtitleFile.AddNew: Integer;
begin
  SetLength(FList, Length(FList)+1);
  Result := High(FList);
end;

function TGenericSubtitleFile.Add(AValue: T): Integer;
begin
  Result := AddNew;
  FList[Result] := AValue;
  FTimeSlice.Value.StartPos.Value := AValue.TimeSlice.Value.StartPos.Value;
  FTimeSlice.Value.EndPos.Value := AValue.TimeSlice.Value.EndPos.Value;
  FList[Result].TimeSlice := FTimeSlice;
end;

procedure TGenericSubtitleFile.AddDialogs(AValue: TGenericSubtitleDialogs);
var
  i: Integer;
begin
  if Length(AValue) < 1 then Exit;
  for i := 0 to High(AValue) do
    Add(AValue[i]);
end;

procedure TGenericSubtitleFile.RemoveDialog(AIndex: Integer);
var
  i: Integer;
begin
  CheckIndex(AIndex);
  for i := AIndex+1 to High(FList) do
    FList[i-1] := FList[i];
  SetLength(FList, High(FList));
end;

function TGenericSubtitleFile.Count: Integer;
begin
  Result := Length(FList);
end;

function TGenericSubtitleFile.MakeNewFromRanges(ARanges: TTimeSliceList;
  AStartTime: Double): TGenericSubtitleDialogs;
var
  dlgs, dlgsr: TGenericSubtitleDialogs;
  ts: TTimeSlice;
  i,j,k: Integer;
  Offset: Double;
begin
  Result := nil;
  if not ARanges.Valid then Exit;
  if Count < 1 then Exit;
  SetLength(dlgs, Count);

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

  if AStartTime > 0 then
    for i := 0 to k-1 do
      dlgs[i].TimeSlice.Delay := dlgs[i].TimeSlice.Delay +AStartTime;

  for i := 0 to k-1 do
  begin
    dlgs[i].TimeSlice.Value := dlgs[i].TimeSlice.ValueWithDelay;
    dlgs[i].TimeSlice.Delay := 0;
  end;

  Result := dlgs;
end;

constructor TGenericSubtitleFile.Create;
begin
  FList := nil;
end;

destructor TGenericSubtitleFile.Destroy;
begin 
  FList := nil;
  inherited Destroy;
end;

end.
