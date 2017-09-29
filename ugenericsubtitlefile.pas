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
  Classes, SysUtils, uMinimalList, uTimeSlice;

type

  { TPlainSubtitleDialog }

  TPlainSubtitleDialog = record
    TimeSlice: TTimeSlice; //any subtitle dialog type should contain these field.
    Text: String;
  end;

  { TGenericSubtitleFile }

  generic TGenericSubtitleFile<T> = class
  private
    type TGenericSubtitleDialogs = specialize TArray<T>;
    type TCustomGenericSubtitleDialogsList = specialize TMinimalList<T>;
    type TGenericSubtitleDialogsList = class(TCustomGenericSubtitleDialogsList)
         private
           procedure SetItem(AIndex: Integer; AItem: T); override;
         end;
  strict private
    FDialogs: TGenericSubtitleDialogsList; 
    FTimeSlice: TTimeSlice;
  private
    function DialogsInRange(ARange: TTimeSlice): TGenericSubtitleDialogs;
  public
    procedure Cleanup; virtual;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromString(const AContents: String); virtual; abstract;
    procedure SaveToFile(const AFileName: String);
    procedure SaveToString(out AContents: String); virtual; abstract;
    function MakeNewFromRanges(ARanges: TTimeSliceList; AFinalStartOffset
      : Double = 0): TGenericSubtitleDialogs;
    procedure FixOverlapsForward;
    procedure FixOverlapsBackward;
  public
    property Dialogs: TGenericSubtitleDialogsList read FDialogs write FDialogs;
    property TimeSlice: TTimeSlice read FTimeSlice write FTimeSlice;
    constructor Create; virtual;
    destructor Destroy; override;
  end; 


implementation

{ TGenericSubtitleFile.TGenericSubtitleDialogsList }

procedure TGenericSubtitleFile.TGenericSubtitleDialogsList.SetItem(
  AIndex: Integer; AItem: T);
begin
  inherited SetItem(AIndex, AItem);
  if Assigned(Owner) then
    with (Owner as TGenericSubtitleFile) do
    begin
      FTimeSlice.Value.StartPos.Value := AItem.TimeSlice.Value.StartPos.Value;
      FTimeSlice.Value.EndPos.Value := AItem.TimeSlice.Value.EndPos.Value;
      PItems[AIndex]^.TimeSlice := FTimeSlice;
    end;
end;


{ TGenericSubtitleFile }

function TGenericSubtitleFile.DialogsInRange(ARange: TTimeSlice
  ): TGenericSubtitleDialogs;
var
  i,j: Integer;
begin
  SetLength(Result, FDialogs.Count);
  j := 0;
  for i := 0 to FDialogs.Count-1 do
  begin
    if ((FDialogs[i].TimeSlice.Value.StartPos.ValueAsDouble >= ARange.Value.StartPos.ValueAsDouble)
      and (FDialogs[i].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.EndPos.ValueAsDouble))
    or ((FDialogs[i].TimeSlice.Value.StartPos.ValueAsDouble < ARange.Value.StartPos.ValueAsDouble)
      and (FDialogs[i].TimeSlice.Value.EndPos.ValueAsDouble > ARange.Value.StartPos.ValueAsDouble)) then
    begin
      Result[j] := FDialogs[i];
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

procedure TGenericSubtitleFile.Cleanup;
var
  i: Integer;
begin
  for i := FDialogs.Count-1 downto 0 do
    if not FDialogs[i].TimeSlice.Valid then
      FDialogs.Remove(i);
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
  sl := TStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

function TGenericSubtitleFile.MakeNewFromRanges(ARanges: TTimeSliceList;
  AFinalStartOffset: Double): TGenericSubtitleDialogs;
var
  dlgs, dlgsr: TGenericSubtitleDialogs;
  ts: TTimeSlice;
  i,j,k: Integer;
  Offset: Double;
begin
  Result := nil;
  if not ARanges.Valid then Exit;
  if Dialogs.Count < 1 then Exit;
  ARanges.Initialize(FTimeSlice.TimeSliceFormat);
  SetLength(dlgs, Dialogs.Count);

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
  for i := FDialogs.Count-1 downto 1 do
    if FDialogs[i].TimeSlice.Value.StartPos.ValueAsDouble
    < FDialogs[i-1].TimeSlice.Value.EndPos.ValueAsDouble then
      FDialogs[i].TimeSlice.Value.StartPos.ValueAsDouble :=
        FDialogs[i-1].TimeSlice.Value.EndPos.ValueAsDouble+0.001;
end;

procedure TGenericSubtitleFile.FixOverlapsBackward;
var
  i: Integer;
begin
  for i := 0 to FDialogs.Count-1 do
    if FDialogs[i].TimeSlice.Value.EndPos.ValueAsDouble
    > FDialogs[i+1].TimeSlice.Value.StartPos.ValueAsDouble then
      FDialogs[i].TimeSlice.Value.EndPos.ValueAsDouble :=
        FDialogs[i+1].TimeSlice.Value.StartPos.ValueAsDouble-0.001;
end;

constructor TGenericSubtitleFile.Create;
begin
  FDialogs := TGenericSubtitleDialogsList.Create(Self);
end;

destructor TGenericSubtitleFile.Destroy;
begin
  if Assigned(FDialogs) then FDialogs.Free;
  inherited Destroy;
end;

end.
