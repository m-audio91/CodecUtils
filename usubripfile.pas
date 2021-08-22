unit uSubripFile;
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CommonStrUtils, CommonGenericLists, uTimeSlice,
  uGenericSubtitleFile;

type

  { TCustomSubripFile }

  TCustomSubripFile = specialize TGenericSubtitleFile<TPlainSubtitleEvent>;

  { TSubripFile }

  TSubripFile = class(TCustomSubripFile)
  public
    procedure LoadFromString(const AContents: String); override;
    procedure SaveToString(out AContents: String); override;
    constructor Create; override;
  end;

const
  SubripTimeSliceSep = ' --> ';

function DefaultSubripTimeSliceFormat: TTimeSliceFormatSettings;

implementation


function DefaultSubripTimeSliceFormat: TTimeSliceFormatSettings;
begin
  with Result do
  begin
    TimeCodes.MillisecondPrecision := 3;
    TimeCodes.MajorSep := ':';
    TimeCodes.MinorSep := ',';
    SourceFPS := 0;
    HasFrame := False;
    IsFrame := False;
    SliceSep := SubripTimeSliceSep;
  end;
end;

{ TSubripFile }

procedure TSubripFile.LoadFromString(const AContents: String);
var
  il: TIntegerList;
  sl: TStringList;
  Dlg: TPlainSubtitleEvent;
  i,j: Integer;
begin
  Clear;
  if IsEmptyStr(AContents) or (AContents.IndexOf(SubripTimeSliceSep)<0) then Exit;
  il := TIntegerList.Create;
  sl := TStringList.Create;
  Dlg.TimeSlice.Initialize(TimeSlice.TimeSliceFormat);
  try
    sl.Text := AContents.TrimLeft;
    if sl.Count < 3 then Exit;
    il.Capacity := sl.Count div 3;
    Events.Capacity := sl.Count div 3;
    for i:=0 to sl.Count-1 do
    begin
      if sl[i].Contains(SubripTimeSliceSep) then
      begin
        Dlg.TimeSlice.ValueAsString := sl[i];
        if Dlg.TimeSlice.Valid then
          il.Add(i);
      end;
    end;
    for i:=0 to il.Count-2 do
    begin
      if (il[i+1]-1)-il[i]>1 then
      begin
        Dlg.TimeSlice.ValueAsString := sl[il[i]];
        Dlg.Text := EmptyStr;
        for j:=il[i]+1 to il[i+1]-2 do
          Dlg.Text := Dlg.Text + sl[j] +LineEnding;
        Events.Add(Dlg);
      end;
    end;
    Dlg.TimeSlice.ValueAsString := sl[il[il.Count-1]];
    Dlg.Text := EmptyStr;
    for j:=il[il.Count-1]+1 to sl.Count-1 do
      Dlg.Text := Dlg.Text + sl[j] +LineEnding;
    Events.Add(Dlg);
  finally
    il.Free;
    sl.Free;
  end;
end;

procedure TSubripFile.SaveToString(out AContents: String);
var
  i: Integer;
begin
  AContents := EmptyStr;
  for i := 0 to Events.Count-1 do
  begin
    AContents := AContents +(i+1).ToString +LineEnding
    +Events[i].TimeSlice.ValueAsString +LineEnding
    +Events[i].Text;
  end;
end;

constructor TSubripFile.Create;
begin
  inherited Create;
  TimeSlice.Initialize(DefaultSubripTimeSliceFormat);
end;

end.

