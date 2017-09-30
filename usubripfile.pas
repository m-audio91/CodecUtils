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
  Classes, SysUtils, CommonStrUtils, uGenericSubtitleFile;

type

  { TCustomSubripFile }

  TCustomSubripFile = specialize TGenericSubtitleFile<TPlainSubtitleDialog>;

  { TSubripFile }

  TSubripFile = class(TCustomSubripFile)
  public
    procedure LoadFromString(const AContents: String); override;
    procedure SaveToString(out AContents: String); override;
    constructor Create; override;
  end;

implementation

const
  SubripTimeSliceSep = ' --> ';

{ TSubripFile }

procedure TSubripFile.LoadFromString(const AContents: String);
var
  sa: TStringArray;
  sl: TStringList;
  Dlg: TPlainSubtitleDialog;
  i,j,k: Integer;
begin
  if AContents = EmptyStr then Exit;
  if AContents.IndexOf(SubripTimeSliceSep) < 0 then Exit;
  sl := TStringList.Create;
  try
    sl.Text := AContents.TrimLeft;
    if sl.Count < 3 then Exit;
    sa := StringListToArray(sl);
  finally
    sl.Free;
  end;
  Dialogs.Capacity := Length(sa) div 3;
  i := 0;
  while i >= 0 do
  begin
    i := FindInArray(sa, SubripTimeSliceSep, i+1);
    if i >= 0 then
    begin
      TimeSlice.ValueAsString := sa[i];
      Dlg.TimeSlice := TimeSlice;
      Dlg.Text := EmptyStr;
      k := i+1;
      j := FindInArray(sa, SubripTimeSliceSep, k);
      repeat
        Dlg.Text := Dlg.Text +sa[k] +LineEnding;
        Inc(k);
      until (k > High(sa)) or (k >= j-1);
      Dialogs.Add(Dlg);
    end;
  end;
end;

procedure TSubripFile.SaveToString(out AContents: String);
var
  i: Integer;
begin
  AContents := EmptyStr;
  for i := 0 to Dialogs.Count-1 do
  begin
    AContents := AContents +(i+1).ToString +LineEnding
    +Dialogs[i].TimeSlice.ValueAsString +LineEnding
    +Dialogs[i].Text;
  end;
end;

constructor TSubripFile.Create;
begin
  inherited Create;
  TimeSlice.Initialize(3, ':', ',', SubripTimeSliceSep);
end;

end.

