unit uSubStationAlphaFile;
{ SubStationAlpha file (ssa ver 4 and 4+ (ass)) decoder, encoder and more.
  written with GUI interaction in mind.

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

{$mode objfpc}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, FPImage, strutils, Graphics, uGenericSubtitleFile,
  uTimeSlice, uMinimalList, CommonStrUtils, CommonNumeralUtils;

type

  { TCustomAlphaInfoManager }

  TCustomAlphaInfoManager = specialize TMinimalList<String>;

  { TAlphaInfoManager }

  TAlphaInfoManager = class(TCustomAlphaInfoManager)
  public
    function IndexOf(const AItem: String): Integer;
    function New: Integer; override;
    function Add(AItem: String): Integer; override;
    procedure RemoveComments;
    function ToText(IsAdvanced: Boolean = True): String;
  end;

  { TAlphaStyle }

  TAlphaStyle = record
  private
    FName: String;
    FScaleX,
    FScaleY: Integer; //0..100
    FAngle: Double; //0..360
    FBorderStyle, //1 or 3
    FOutline, //0..4
    FShadow, //0..4
    //SSA alignment is sub: 1,2,3, top(4+sub): 5,6,7, mid(8+sub): 9,10,11. ASS is (1..3 sub, 4..6 mid, 7..9 top).
    FAlignment: Integer;
    procedure SetName(AValue: String);
    procedure SetScaleX(AValue: Integer);
    procedure SetScaleY(AValue: Integer);
    procedure SetAngle(AValue: Double);
    procedure SetBorderStyle(AValue: Integer);
    procedure SetOutline(AValue: Integer);
    procedure SetShadow(AValue: Integer);
    procedure SetAlignment(AValue: Integer);
  public
    Fontname: String;
    PrimaryColour,
    SecondaryColour,
    OutlineColour,
    BackColour: TFPColor;
    Bold,
    Italic,
    Underline,
    StrikeOut: Boolean;
    Fontsize,
    Spacing,
    MarginL,
    MarginR,
    MarginV,
    AlphaLevel,
    Encoding: LongWord;
    property Name: String read FName write SetName;
    property ScaleX: Integer read FScaleX write SetScaleX;
    property ScaleY: Integer read FScaleY write SetScaleY;
    property Angle: Double read FAngle write SetAngle;
    property BorderStyle: Integer read FBorderStyle write SetBorderStyle;
    property Outline: Integer read FOutline write SetOutline;
    property Shadow: Integer read FShadow write SetShadow;
    property Alignment: Integer read FAlignment write SetAlignment;
    function ParseFromString(const AStyle: String; IsAdvanced: Boolean = True)
      : Boolean;
    function ToString(IsAdvanced: Boolean = True): String;
  end;

  { TCustomAlphaStyleManager }

  TCustomAlphaStyleManager = specialize TMinimalList<TAlphaStyle>;

  { TAlphaStyleManager }

  TAlphaStyleManager = class(TCustomAlphaStyleManager)
  private
    function GetUniqueName(const AName: String): String;
  public
    function New: Integer; override;
    function Add(AItem: TAlphaStyle): Integer; override;
    function ToText(IsAdvanced: Boolean = True): String;
  end;

  { TAlphaEventType }
  TAlphaEventType = (aeDialogue,aeComment,aePicture,aeSound,aeMovie,aeCommand);

  { TAlphaEvent }

  TAlphaEvent = record
  private
    FStyle,
    FName,
    FEffect: String;
    procedure SetStyle(const AValue: String);
    procedure SetName(const AValue: String);
    procedure SetEffect(const AValue: String);
  public
    TimeSlice: TTimeSlice;
    LayerOrMarked,
    MarginL,
    MarginR,
    MarginV: Integer;
    Text: String;
    EventType: TAlphaEventType;
    property Style: String read FStyle write SetStyle;
    property Name: String read FName write SetName;
    property Effect: String read FEffect write SetEffect;
    function ParseFromString(const AEvent: String): Boolean;
    function ToString(IsAdvanced: Boolean = True): String;
  end;

  { TCustomSubStationAlphaFile }

  TCustomSubStationAlphaFile = specialize TGenericSubtitleFile<TAlphaEvent>;

  { TSubStationAlphaFile: not a completed class. still can be extended with
    other functionalities like [Fonts] and [Graphics] handling.
    more ssa info can be found on:
    https://www.paralog.net/files/SubTitler/SSA4%20Spec.doc
    https://wiki.multimedia.cx/index.php/SubStation_Alpha
    https://www.matroska.org/technical/specs/subtitles/ssa.html }

  TSubStationAlphaFile = class(TCustomSubStationAlphaFile)
  private
    FAlphaIsAdvanced: Boolean;
    procedure ConvertToAdvancedAlpha;
    procedure ConvertToOldAlpha;
    procedure SetAdvanced(AValue: Boolean);
  public
    Infos: TAlphaInfoManager;
    Styles: TAlphaStyleManager;
    property AlphaIsAdvanced: Boolean read FAlphaIsAdvanced write SetAdvanced;
    procedure LoadFromString(const AContents: String); override;
    procedure SaveToString(out AContents: String); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

function DefaultAlphaStyle: TAlphaStyle;
function DefaultAlphaEvent: TAlphaEvent;
//to translate alignment from two ComboBoxes, one for H pos (0=left, 1=center
//, 2=right) and one for V pos(0=top, 1=center, 2=bottom) use the three following
//fuctions but beaware these are assuming advanced alpha (v4.00+) style values
//: 1..9.
function HVAlignToAlphaAlign(AHorizontalValue, AVerticalValue: Integer): Integer;
function GetHorizontalAlign(AAlphaAlign: Integer): Integer;
function GetVerticalAlign(AAlphaAlign: Integer): Integer;
//additional alignment conversion functions
function AlphaOldAlignToAdvancedAlign(A: Integer): Integer;
function AlphaAdvancedAlignToOldAlign(A: Integer): Integer;
//color translation functions from GUI and back
//AColor can be from a TColorButton etc and AALphaPercent can be from
//a TSpinEdit with it's Min=0=>transparent and it's Max=100=>visible
function FPColor(AColor: TColor; AALphaPercent: Integer): TFPColor; overload;
function AlphaPercentFromFPColor(AColor: TFPColor): Integer;


implementation

const
  AlphaSep = ',';
  AlphaSepReplacement = ';';
  AlphaInfoHeader = '[Script Info]';
  AlphaVersionMarker = 'ScriptType: ';
  AlphaVersionValue = 'v4.00+';
  OldAlphaVersionValue = 'v4.00';
  AlphaStylesHeader = '[V4+ Styles]';
  OldAlphaStylesHeader = '[V4 Styles]';
  AlphaStylesHint = 'Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour,'
    +'OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, '
    +'Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding';
  OldAlphaStylesHint = 'Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, '
    +'TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, '
    +'MarginL, MarginR, MarginV, AlphaLevel, Encoding';
  AlphaStyleFormatMarker = 'Format: ';
  AlphaStyleMarker = 'Style: ';
  AlphaEventsHeader = '[Events]';
  AlphaEventsHint = 'Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text';
  OldAlphaEventsHint = 'Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text';
  OldAlphaMarkers: array[0..1] of String = ('Marked=','AlphaLevel');
  AlphaDialogueMarker = 'Dialogue: ';
  AlphaCommentMarker = 'Comment: ';
  AlphaPictureMarker = 'Picture: ';
  AlphaSoundMarker = 'Sound: ';
  AlphaMovieMarker = 'Movie: ';
  AlphaCommandMarker = 'Command: ';
  AlphaEventMarkers: array[0..5] of String = (AlphaDialogueMarker,AlphaCommentMarker,
    AlphaPictureMarker,AlphaSoundMarker,AlphaMovieMarker,AlphaCommandMarker);
 AlphaDialogueDefaultText = 'hi there, I am a Sub Station Alpha dialog!';


function AlphaColorToFPColor(const C: String): TFPColor;
begin
  if (C.Length = 10) and (C.StartsWith('&H')) then
    Result := FPColor(
      Hex2Dec(C.Substring(8,2)),
      Hex2Dec(C.Substring(6,2)),
      Hex2Dec(C.Substring(4,2)),
      Hex2Dec(C.Substring(2,2)))
  else
    Result := FPColor(
      (C.ToInteger and $FF),
      ((C.ToInteger shr 8) and $FF),
      ((C.ToInteger shr 16) and $FF),
      0);
end;

function FPColorToAlphaColor(const C: TFPColor; IsAdvanced: Boolean)
  : String;
begin
  if IsAdvanced then
  Result := '&H'
    +Dec2Numb(C.alpha, 2, 16)
    +Dec2Numb(C.blue, 2, 16)
    +Dec2Numb(C.green, 2, 16)
    +Dec2Numb(C.red, 2, 16)
  else
    Result := Numb2Dec(
      Dec2Numb(C.blue, 2, 16)
      +Dec2Numb(C.green, 2, 16)
      +Dec2Numb(C.red, 2, 16) ,16).ToString;
end;

function AlphaOldAlignToAdvancedAlign(A: Integer): Integer;
begin
  Result := 2;
  case A of
  1..3: Result := A;
  5: Result := 4;
  6: Result := 5;
  7: Result := 6;
  9: Result := 7;
  10: Result := 8;
  11: Result := 9;
  end;
end;

function AlphaAdvancedAlignToOldAlign(A: Integer): Integer;
begin
  Result := 2;
  case A of
  1..3: Result := A;
  4: Result := 5;
  5: Result := 6;
  6: Result := 7;
  7: Result := 9;
  8: Result := 10;
  9: Result := 11;
  end;
end;

function FPColor(AColor: TColor; AALphaPercent: Integer): TFPColor;
begin
  ForceInRange(AALphaPercent,0,100);
  AALphaPercent := ConvertInRange(AALphaPercent,0,100,0,255);
  AALphaPercent := 255-AALphaPercent;
  Result := TColorToFPColor(AColor);
  Result.alpha := AALphaPercent;
end;

function AlphaPercentFromFPColor(AColor: TFPColor): Integer;
begin
  Result := AColor.alpha;
  Result := ConvertInRange(Result,0,255,0,100);
  Result := 100-Result;
end;

function DefaultAlphaStyle: TAlphaStyle;
begin
  with Result do
  begin
    Name := 'Default';
    Fontname := 'Arial';
    Fontsize := 20;
    PrimaryColour := AlphaColorToFPColor('&H00FFFFFF');
    SecondaryColour := AlphaColorToFPColor('&H000000FF');
    OutlineColour := AlphaColorToFPColor('&H00000000');
    BackColour := AlphaColorToFPColor('&H00000000');
    Bold := False;
    Italic := False;
    Underline := False;
    StrikeOut := False;
    ScaleX := 100;
    ScaleY := 100;
    Spacing := 0;
    Angle := 0;
    BorderStyle := 1;
    Outline := 2;
    Shadow := 2;
    Alignment := 2;
    MarginL := 10;
    MarginR := 10;
    MarginV := 10;
    AlphaLevel := 0;
    Encoding := 1;
  end;
end;

function DefaultAlphaEvent: TAlphaEvent;
begin
  with Result do
  begin
    TimeSlice.Reset;
    TimeSlice.Value.StartPos.ValueAsDouble := 120;
    LayerOrMarked := 0;
    MarginL := 0;
    MarginR := 0;
    MarginV := 0;
    Text := AlphaDialogueDefaultText;
    EventType := aeDialogue;
    Style := DefaultAlphaStyle.Name;
    Name := EmptyStr;
    Effect := EmptyStr;
  end;
end;

function HVAlignToAlphaAlign(AHorizontalValue, AVerticalValue: Integer): Integer;
begin
  ForceInRange(AHorizontalValue, 0, 2);
  ForceInRange(AVerticalValue, 0, 2);
  Result := 2;
  case AHorizontalValue of
  0: begin
      case AVerticalValue of
      0: Result := 7;
      1: Result := 4;
      2: Result := 1;
      end;
    end;
  1: begin
      case AVerticalValue of
      0: Result := 8;
      1: Result := 5;
      2: Result := 2;
      end;
    end;
  2: begin
      case AVerticalValue of
      0: Result := 9;
      1: Result := 6;
      2: Result := 3;
      end;
    end;
  end;
end;

function GetHorizontalAlign(AAlphaAlign: Integer): Integer;
begin
  if AAlphaAlign in [1,4,7] then
    Result := 0
  else if AAlphaAlign in [3,6,9] then
    Result := 2
  else
    Result := 1;
end;

function GetVerticalAlign(AAlphaAlign: Integer): Integer;
begin
  if AAlphaAlign in [7,8,9] then
    Result := 0
  else if AAlphaAlign in [4,5,6] then
    Result := 1
  else
    Result := 2;
end;

{ TAlphaInfoManager }

function TAlphaInfoManager.IndexOf(const AItem: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Contains(AItem) then
      Exit(i);
end;

function TAlphaInfoManager.New: Integer;
begin
  Result := inherited New;
  Items[Result] := '; ';
end;

function TAlphaInfoManager.Add(AItem: String): Integer;
var
  i: Integer;
begin
  i := IndexOf(AItem.Split(':')[0]);
  if i >= 0 then
  begin
    Items[i] := AItem;
    Result := i;
  end
  else
  begin
    Result := inherited Add(AItem);
    if not IsEmptyStr(AItem) then
      Items[Result] := AItem;
  end;
end;

procedure TAlphaInfoManager.RemoveComments;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].StartsWith(';') or Items[i].StartsWith('!:')then
      Remove(i);
end;

function TAlphaInfoManager.ToText(IsAdvanced: Boolean): String;
var
  sa: TStringArray;
  s: String;
  i: Integer;
begin
  Result := EmptyStr;
  if IsAdvanced then
    s := AlphaVersionValue
  else
    s := OldAlphaVersionValue;
  i := IndexOf(AlphaVersionMarker);
  if i >= 0 then
    Items[i] := AlphaVersionMarker +s
  else
    Add(AlphaVersionMarker +s);
  SetLength(sa, Count);
  for i := 0 to Count-1 do
    sa[i] := Items[i];
  RemoveDupsInArray(sa);
  Result := Result.Join(LineEnding,sa) +LineEnding;
end;

{ TAlphaStyle }

procedure TAlphaStyle.SetName(AValue: String);
begin
  FName := AValue.Replace(AlphaSep, AlphaSepReplacement);
end;

procedure TAlphaStyle.SetScaleX(AValue: Integer);
begin
  FScaleX := AValue;
  ForceInRange(FScaleX, 0, 100);
end;

procedure TAlphaStyle.SetScaleY(AValue: Integer);
begin
  FScaleY := AValue;
  ForceInRange(FScaleY, 0, 100);
end;

procedure TAlphaStyle.SetAngle(AValue: Double);
begin
  FAngle := AValue;
  ForceInRange(FAngle, 0, 360);
end;

procedure TAlphaStyle.SetBorderStyle(AValue: Integer);
begin
  FBorderStyle := AValue;
  if not FBorderStyle in [1,3] then
    FBorderStyle := 1;
end;

procedure TAlphaStyle.SetOutline(AValue: Integer);
begin
  FOutline := AValue;
  ForceInRange(FOutline, 0, 4);
end;

procedure TAlphaStyle.SetShadow(AValue: Integer);
begin
  FShadow := AValue;
  ForceInRange(FShadow, 0, 4);
end;

procedure TAlphaStyle.SetAlignment(AValue: Integer);
begin
  FAlignment := AValue;
  ForceInRange(FAlignment, 0, 11);
end;

function TAlphaStyle.ParseFromString(const AStyle: String; IsAdvanced: Boolean)
  : Boolean;
var
  sa,sa2: TStringArray;
  s: String;
  i: Integer;
begin
  Result := True;
  if not AStyle.StartsWith(AlphaStyleMarker) then
    Exit(False);

  if IsAdvanced then
    sa := AlphaStylesHint.Substring(AlphaStyleFormatMarker.Length).Split(',')
  else
    sa := OldAlphaStylesHint.Substring(AlphaStyleFormatMarker.Length).Split(',');
  sa2 := AStyle.Substring(AlphaStyleMarker.Length).Split(',');
  if High(sa) <> High(sa2) then Exit(False);

  Self := DefaultAlphaStyle;
  for i := 0 to High(sa) do
  begin
    s := sa2[i];
    case sa[i].Trim.ToLower of
    'name': Name := s;
    'fontname': Fontname := s;
    'fontsize': Fontsize := s.ToInteger;
    'primarycolour': PrimaryColour := AlphaColorToFPColor(s);
    'secondarycolour': SecondaryColour := AlphaColorToFPColor(s);
    'tertiarycolour': OutlineColour := AlphaColorToFPColor(s);
    'outlinecolour': OutlineColour := AlphaColorToFPColor(s);
    'backcolour': BackColour := AlphaColorToFPColor(s);
    'bold': Bold := s.ToBoolean;
    'italic': Italic := s.ToBoolean;
    'underline': Underline := s.ToBoolean;
    'strikeout': StrikeOut := s.ToBoolean;
    'scalex': ScaleX := s.ToInteger;
    'scaley': ScaleY := s.ToInteger;
    'spacing': Spacing := s.ToInteger;
    'angle': Angle := s.ToDouble;
    'borderstyle': BorderStyle := s.ToInteger;
    'outline': Outline := s.ToInteger;
    'shadow': Shadow := s.ToInteger;
    'alignment': Alignment := s.ToInteger;
    'marginl': MarginL := s.ToInteger;
    'marginr': MarginR := s.ToInteger;
    'marginv': MarginV := s.ToInteger;
    'alphalevel': AlphaLevel := s.ToInteger;
    'encoding': Encoding := s.ToInteger;
    end;
  end;
end;

function TAlphaStyle.ToString(IsAdvanced: Boolean): String;
var
  sa: TStringArray;
  i: Integer;
begin
  Result := EmptyStr;
  if IsAdvanced then
    sa := AlphaStylesHint.Substring(AlphaStyleFormatMarker.Length).Split(',')
  else
    sa := OldAlphaStylesHint.Substring(AlphaStyleFormatMarker.Length).Split(',');

  for i := 0 to High(sa) do
  begin
    case sa[i].Trim.ToLower of
    'name': sa[i] := Name;
    'fontname': sa[i] := Fontname;
    'fontsize': sa[i] := Fontsize.ToString;
    'primarycolour': sa[i] := FPColorToAlphaColor(PrimaryColour, IsAdvanced);
    'secondarycolour': sa[i] := FPColorToAlphaColor(SecondaryColour, IsAdvanced);
    'tertiarycolour': sa[i] := FPColorToAlphaColor(OutlineColour, IsAdvanced);
    'outlinecolour': sa[i] := FPColorToAlphaColor(OutlineColour, IsAdvanced);
    'backcolour': sa[i] := FPColorToAlphaColor(BackColour, IsAdvanced);
    'bold': sa[i] := Bold.ToString;
    'italic': sa[i] := Italic.ToString;
    'underline': sa[i] := Underline.ToString;
    'strikeout': sa[i] := StrikeOut.ToString;
    'scalex': sa[i] := ScaleX.ToString;
    'scaley': sa[i] := ScaleY.ToString;
    'spacing': sa[i] := Spacing.ToString;
    'angle': sa[i] := Angle.ToString;
    'borderstyle': sa[i] := BorderStyle.ToString;
    'outline': sa[i] := Outline.ToString;
    'shadow': sa[i] := Shadow.ToString;
    'alignment': sa[i] := Alignment.ToString;
    'marginl': sa[i] := MarginL.ToString;
    'marginr': sa[i] := MarginR.ToString;
    'marginv': sa[i] := MarginV.ToString;
    'alphalevel': sa[i] := AlphaLevel.ToString;
    'encoding': sa[i] := Encoding.ToString;
    end;
  end;
  Result := AlphaStyleMarker +Result.Join(AlphaSep, sa);
end;

{ TAlphaStyleManager }

function TAlphaStyleManager.GetUniqueName(const AName: String): String;
var
  i: Integer;
begin
  Result := AName;
  if Count <= 1 then Exit;
  i := 0;
  while i < Count do
  begin
    if Result.Equals(Items[i].Name) then
    begin
      Result := Result +' - Copy';
      i := 0;
      Continue;
    end
    else
      Inc(i);
  end;
end;

function TAlphaStyleManager.New: Integer;
begin
  Result := inherited New;
  Items[Result] := DefaultAlphaStyle;
  PItems[Result]^.Name := GetUniqueName(PItems[Result]^.Name);
end;

function TAlphaStyleManager.Add(AItem: TAlphaStyle): Integer;
begin
  AItem.Name := GetUniqueName(AItem.Name);
  Result := inherited Add(AItem);
end;

function TAlphaStyleManager.ToText(IsAdvanced: Boolean): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to Count-1 do
    Result := Result +Items[i].ToString(IsAdvanced) +LineEnding;
end;

{ TAlphaEvent }

procedure TAlphaEvent.SetStyle(const AValue: String);
begin
  FStyle := AValue.Replace(AlphaSep, AlphaSepReplacement);
end;

procedure TAlphaEvent.SetName(const AValue: String);
begin
  FName := AValue.Replace(AlphaSep, AlphaSepReplacement);
end;

procedure TAlphaEvent.SetEffect(const AValue: String);
begin
  FEffect := AValue.Replace(AlphaSep, AlphaSepReplacement);
end;

function TAlphaEvent.ParseFromString(const AEvent: String): Boolean;
var
  sa: TStringArray;
  i: Integer;
begin
  Result := True;
  case AEvent.Substring(0, AEvent.IndexOf(' ')+1) of
  AlphaDialogueMarker: EventType := aeDialogue;
  AlphaCommentMarker: EventType := aeComment;
  AlphaPictureMarker: EventType := aePicture;
  AlphaSoundMarker: EventType := aeSound;
  AlphaMovieMarker: EventType := aeMovie;
  AlphaCommandMarker: EventType := aeCommand;
  end;

  sa := AEvent.Substring(AEvent.IndexOf(' ')+1).Split(AlphaSep);
  if Length(sa) < 10 then Exit(False);

  if sa[0].StartsWith(OldAlphaMarkers[0]) then
    LayerOrMarked := sa[0].Substring(OldAlphaMarkers[0].Length).ToInteger
  else
    LayerOrMarked := sa[0].ToInteger;
  TimeSlice.Initialize(2, ':', '.', AlphaSep);
  TimeSlice.ValueAsString := sa[1] +AlphaSep +sa[2];
  Style := sa[3];
  Name := sa[4];
  MarginL := sa[5].ToInteger;
  MarginR := sa[6].ToInteger;
  MarginV := sa[7].ToInteger;
  Effect := sa[8];
  i := NthIndexOf(AlphaSep, AEvent, 9);
  if i > 0 then
    Text := AEvent.Substring(i+1);
end;

function TAlphaEvent.ToString(IsAdvanced: Boolean): String;
begin
  Result := LayerOrMarked.ToString;
  if not IsAdvanced then
    Result := OldAlphaMarkers[0] +Result;
  Result :=
    Result +AlphaSep +TimeSlice.ValueAsString
    +AlphaSep +Style
    +AlphaSep +Name
    +AlphaSep +StrForceLength(MarginL.ToString, 4, '0')
    +AlphaSep +StrForceLength(MarginR.ToString, 4, '0')
    +AlphaSep +StrForceLength(MarginV.ToString, 4, '0')
    +AlphaSep +Effect
    +AlphaSep +Text;
  case EventType of
  aeDialogue: Result := AlphaDialogueMarker +Result;
  aeComment: Result := AlphaCommentMarker +Result;
  aePicture: Result := AlphaPictureMarker +Result;
  aeSound: Result := AlphaSoundMarker +Result;
  aeMovie: Result := AlphaMovieMarker +Result;
  aeCommand: Result := AlphaCommandMarker +Result;
  end;
end;

{ TSubStationAlphaFile }

procedure TSubStationAlphaFile.ConvertToAdvancedAlpha;
var
  i: Integer;
begin
  if FAlphaIsAdvanced = True then Exit;
  FAlphaIsAdvanced := True;
  for i := 0 to Styles.Count-1 do
  begin
    Styles.PItems[i]^.OutlineColour := Styles[i].BackColour;
    Styles.PItems[i]^.Alignment :=
      AlphaOldAlignToAdvancedAlign(Styles.PItems[i]^.Alignment);
  end;
end;

procedure TSubStationAlphaFile.ConvertToOldAlpha;
var
  i: Integer;
begin
  if FAlphaIsAdvanced = False then Exit;
  FAlphaIsAdvanced := False;
  for i := 0 to Styles.Count-1 do
    Styles.PItems[i]^.Alignment :=
      AlphaAdvancedAlignToOldAlign(Styles.PItems[i]^.Alignment);
end;

procedure TSubStationAlphaFile.SetAdvanced(AValue: Boolean);
begin
  if AValue and not FAlphaIsAdvanced then
    ConvertToAdvancedAlpha
  else if not AValue and FAlphaIsAdvanced then
    ConvertToOldAlpha;
end;

procedure TSubStationAlphaFile.LoadFromString(const AContents: String);
var
  sa: TStringArray;
  sl: TStringList;
  ae: TAlphaEvent;
  i,j,k: Integer;
  Stl: TAlphaStyle;
begin
  Events.Clear;
  if IsEmptyStr(AContents) then Exit;

  sl := TStringList.Create;
  try
    sl.Text := AContents.Trim;
    sa := StringListToArray(sl);
  finally
    sl.Free;
  end;

  i := FindInArray(sa, AlphaInfoHeader);
  if i < 0 then Exit;
  i := FindAnyInArray(sa, [AlphaStylesHeader,OldAlphaStylesHeader], i+1);
  if i < 0 then Exit;
  i := FindInArray(sa, AlphaEventsHeader, i+1);
  if i < 0 then Exit;

  if (FindInArray(sa, OldAlphaMarkers[0]) >= 0)
  and (FindInArray(sa, OldAlphaMarkers[1]) >= 0) then
    FAlphaIsAdvanced := False
  else
    FAlphaIsAdvanced := True;

  Infos.Clear;
  i := FindInArray(sa, AlphaInfoHeader);
  j := FindAnyInArray(sa, [AlphaStylesHeader,OldAlphaStylesHeader])-2;
  for k := i+1 to j do
    Infos.Add(sa[k]);

  Styles.Clear;
  i := FindAnyInArray(sa, [AlphaStylesHeader,OldAlphaStylesHeader]);
  j := FindInArray(sa, AlphaEventsHeader)-2;
  for k := i+2 to j do
    if Stl.ParseFromString(sa[k], FAlphaIsAdvanced) then
      Styles.Add(Stl);

  Events.Clear;
  Events.Capacity := Length(sa);
  for i := 0 to High(sa) do
    if (sa[i].IndexOfAny(AlphaEventMarkers) >= 0)
    and ae.ParseFromString(sa[i]) then
      Events.Add(ae);
end;

procedure TSubStationAlphaFile.SaveToString(out AContents: String);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.Capacity := Infos.Count+Styles.Count+Events.Count+10;
  try
    sl.Add(AlphaInfoHeader);
    sl.Add(Infos.ToText(FAlphaIsAdvanced));
    if FAlphaIsAdvanced then
    begin
      sl.Add(AlphaStylesHeader);
      sl.Add(AlphaStylesHint);
    end
    else
    begin
      sl.Add(OldAlphaStylesHeader);
      sl.Add(OldAlphaStylesHint);
    end;
    sl.Add(Styles.ToText(FAlphaIsAdvanced));
    sl.Add(AlphaEventsHeader);
    if FAlphaIsAdvanced then
      sl.Add(AlphaEventsHint)
    else
      sl.Add(OldAlphaEventsHint);
    for i := 0 to Events.Count-1 do
      sl.Add(Events[i].ToString(FAlphaIsAdvanced));
  finally
    AContents := sl.Text;
    sl.Free;
  end;
end;

constructor TSubStationAlphaFile.Create;
begin
  inherited Create;
  TimeSlice.Initialize(2, ':', '.', AlphaSep);
  FAlphaIsAdvanced := True;
  Infos := TAlphaInfoManager.Create;
  Styles := TAlphaStyleManager.Create;
  Styles.New;
end;

destructor TSubStationAlphaFile.Destroy;
begin
  if Assigned(Infos) then Infos.Free;
  if Assigned(Styles) then Styles.Free;
  inherited Destroy;
end;

end.

