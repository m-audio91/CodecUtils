unit uRawSubtitleFile;
{ base class for a text based subtitle file. leaves some abstract methods
  for descendants classes. look at uSubripFile for example.

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

  { TRawSubtitleDialog }

  TRawSubtitleDialog = record
    Text: String;
    TimeSlice: TTimeSlice;
  end;

  { TRawSubtitleDialogs }

  TRawSubtitleDialogs = array of TRawSubtitleDialog;

  { TRawSubtitleFile }

  TRawSubtitleFile = class
  strict protected
    FTimeSlice: TTimeSlice;
    FList: TRawSubtitleDialogs;
  private
    function GetDialog(Index: Integer): TRawSubtitleDialog;
    procedure SetDialog(Index: Integer; AValue: TRawSubtitleDialog);
    procedure CheckIndex(AIndex: Integer); inline;
    function DialogsInRange(ARange: TTimeSlice): TRawSubtitleDialogs;
    procedure SetValue(AValue: TRawSubtitleDialogs);
  public
    procedure Cleanup;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromString(const AContents: String); virtual; abstract;
    procedure SaveToFile(const AFileName: String);
    procedure SaveToString(out AContents: String); virtual; abstract;
    function Add(const AText: String; const ATimeSlice: TTimeSlice): Integer; overload;
    function Add(const AText, ATimeSlice: String): Integer; overload;
    procedure Delete(AIndex: Integer);
    property Dialogs[Index: Integer]: TRawSubtitleDialog read GetDialog write SetDialog; default;
    property Value: TRawSubtitleDialogs read FList write SetValue;
    function Count: Integer;
    function MakeNewFromRanges(ARanges: TTimeSliceList; AStartTime: Double
      = 0): TRawSubtitleDialogs;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TRawSubtitleFile }

function TRawSubtitleFile.GetDialog(Index: Integer): TRawSubtitleDialog;
begin
  CheckIndex(Index);
  Result := FList[Index];
end;

procedure TRawSubtitleFile.SetDialog(Index: Integer; AValue: TRawSubtitleDialog
  );
begin
  CheckIndex(Index);
  FList[Index] := AValue;
end;

procedure TRawSubtitleFile.CheckIndex(AIndex: Integer);
begin
  if (AIndex < Low(FList)) or (AIndex > High(FList)) then
    raise EListError.Create('Out of bounds index');
end;

function TRawSubtitleFile.DialogsInRange(ARange: TTimeSlice
  ): TRawSubtitleDialogs;
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
  if Result[0].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.StartPos.ValueAsDouble then
    Result[0].TimeSlice.Value.StartPos.Value := ARange.Value.StartPos.Value;
  if Result[j-1].TimeSlice.Value.EndPos.ValueAsDouble > ARange.Value.EndPos.ValueAsDouble then
    Result[j-1].TimeSlice.Value.EndPos.Value := ARange.Value.EndPos.Value;
end;

procedure TRawSubtitleFile.SetValue(AValue: TRawSubtitleDialogs);
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

procedure TRawSubtitleFile.Cleanup;
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
      Delete(i);
  end;
end;

procedure TRawSubtitleFile.LoadFromFile(const AFileName: String);
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

procedure TRawSubtitleFile.SaveToFile(const AFileName: String);
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

function TRawSubtitleFile.Add(const AText: String; const ATimeSlice: TTimeSlice
  ): Integer;
var
  i: Integer;
begin
  i := Length(FList)+1;
  SetLength(FList, i);
  FTimeSlice.Value.StartPos.Value := ATimeSlice.Value.StartPos.Value;
  FTimeSlice.Value.EndPos.Value := ATimeSlice.Value.EndPos.Value;
  FList[i-1].TimeSlice := FTimeSlice;
  FList[i-1].Text := AText;
  Result := i-1;
end;

function TRawSubtitleFile.Add(const AText, ATimeSlice: String): Integer;
var
  i: Integer;
begin
  i := Length(FList)+1;
  SetLength(FList, i);
  FTimeSlice.ValueAsString := ATimeSlice;
  FList[i-1].TimeSlice := FTimeSlice;
  FList[i-1].Text := AText;
  Result := i-1;
end;

procedure TRawSubtitleFile.Delete(AIndex: Integer);
var
  i: Integer;
begin
  CheckIndex(AIndex);
  for i := AIndex+1 to High(FList) do
    FList[i-1] := FList[i];
  SetLength(FList, High(FList));
end;

function TRawSubtitleFile.Count: Integer;
begin
  Result := Length(FList);
end;

function TRawSubtitleFile.MakeNewFromRanges(ARanges: TTimeSliceList;
  AStartTime: Double): TRawSubtitleDialogs;
var
  dlgs, dlgsr: TRawSubtitleDialogs;
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
    Offset := Offset + ts.Duration.ValueAsDouble;

    for j := 0 to High(dlgsr) do
    begin
      dlgs[k] := dlgsr[j];
      dlgs[k].TimeSlice.Delay := -Offset + ARanges.Values[i].Delay;
      Inc(k);
    end;
  end;
  SetLength(dlgs, k);

  if AStartTime > 0 then
    for i := 0 to k-1 do
      dlgs[i].TimeSlice.Delay := dlgs[i].TimeSlice.Delay + AStartTime;

  for i := 0 to k-1 do
  begin
    dlgs[i].TimeSlice.Value := dlgs[i].TimeSlice.ValueWithDelay;
    dlgs[i].TimeSlice.Delay := 0;
  end;

  Result := dlgs;
end;

constructor TRawSubtitleFile.Create;
begin
  inherited Create;
  FList := nil;
end;

destructor TRawSubtitleFile.Destroy;
begin
  FList := nil;
  inherited Destroy;
end;

end.

