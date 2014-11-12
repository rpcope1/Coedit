unit ce_txtsyn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynEditTypes, ce_dlangutils;

type

  TTokenKind = (tkSym, tkTxt, tkWhi);

  TSynTxtSyn = class(TSynCustomHighlighter)
	private
    fSymAttribs: TSynHighlighterAttributes;
    fTxtAttribs: TSynHighlighterAttributes;
    fWhiAttribs: TSynHighlighterAttributes;
    fTokToAttri: array[TTokenKind] of TSynHighlighterAttributes;
    fToken: TTokenKind;
    fTokStart, fTokStop: Integer;
    fLineBuf: string;
    fCurrIdent: string;
    procedure setSymAttribs(aValue: TSynHighlighterAttributes);
    procedure setTxtAttribs(aValue: TSynHighlighterAttributes);
    procedure setWhiAttribs(aValue: TSynHighlighterAttributes);
    procedure setCurrIdent(const aValue: string);
  published
    property symbAttributes: TSynHighlighterAttributes read fSymAttribs write setSymAttribs;
    property textAttributes: TSynHighlighterAttributes read fTxtAttribs write setTxtAttribs;
    property whitAttributes: TSynHighlighterAttributes read fWhiAttribs write setWhiAttribs;
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure setLine(const NewValue: String; LineNumber: Integer); override;
    procedure next; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function GetEol: Boolean; override;
    //
    property CurrIdent: string read fCurrIdent write setCurrIdent;
  end;

  const txtSym : TCharSet = [
    '&', '~', '#', '"', #39, '(', '-', ')', '=',
    '{', '[', '|', '`', '\', '^', '@', ']', '}',
    '+', '$', '*', '%',
    '<', '>', ',', '?', ';', '.', ':', '/', '!'];

implementation

uses
  Graphics;

constructor TSynTxtSyn.create(aOwner: TComponent);
begin
  inherited;
  //
  fSymAttribs := TSynHighlighterAttributes.Create('Symbols', 'Symbols');
  fTxtAttribs := TSynHighlighterAttributes.Create('Text', 'Text');
  fWhiAttribs := TSynHighlighterAttributes.Create('White', 'White');
  //
  fSymAttribs.Foreground := clBlack;
  fSymAttribs.Style := [fsBold];
  fTxtAttribs.Foreground := clNavy;
  fWhiAttribs.Foreground := clNone;
  //
  AddAttribute(fSymAttribs);
  AddAttribute(fTxtAttribs);
  AddAttribute(fWhiAttribs);
  //
  fTokToAttri[tkSym] := fSymAttribs;
  fTokToAttri[tkTxt] := fTxtAttribs;
  fTokToAttri[tkWhi] := fWhiAttribs;
  //
  fTokStop := 1;
  Next;
end;

procedure TSynTxtSyn.setSymAttribs(aValue: TSynHighlighterAttributes);
begin
  fSymAttribs.Assign(aValue);
end;

procedure TSynTxtSyn.setTxtAttribs(aValue: TSynHighlighterAttributes);
begin
  fTxtAttribs.Assign(aValue);
end;

procedure TSynTxtSyn.setWhiAttribs(aValue: TSynHighlighterAttributes);
begin
  fWhiAttribs.Assign(aValue);
end;

procedure TSynTxtSyn.setCurrIdent(const aValue: string);
begin
  if aValue = '' then exit;
  if fCurrIdent = aValue then Exit;
  fCurrIdent := aValue;
  BeginUpdate;
    fUpdateChange := true;
  EndUpdate;
end;

procedure TSynTxtSyn.setLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLineBuf := NewValue + #10;
  fTokStop := 1;
  next;
end;

procedure TSynTxtSyn.next;
begin
  fTokStart := fTokStop;
  fTokStop  := fTokStart;

  // EOL
  if fTokStop > length(fLineBuf) then exit;

  // spaces
  if (isWhite(fLineBuf[fTokStop])) then
  begin
    fToken := tkWhi;
    while isWhite(fLineBuf[fTokStop]) do
    begin
      Inc(fTokStop);
      if fTokStop > length(fLineBuf) then exit;
    end;
    exit;
  end;

  // symbs
  if (fLineBuf[fTokStop] in txtSym) then
  begin
    fToken := tkSym;
    while(fLineBuf[fTokStop] in txtSym) do
    begin
      Inc(fTokStop);
      if fLineBuf[fTokStop] = #10 then exit;
    end;
    exit;
  end;

  // text
  fToken := tkTxt;
  while not (isSymbol(fLineBuf[fTokStop]) or isWhite(fLineBuf[fTokStop])) do
  begin
    Inc(fTokStop);
    if fLineBuf[fTokStop] = #10 then exit;
  end;

  if fLineBuf[fTokStop] = #10 then exit;
end;

function TSynTxtSyn.GetEol: Boolean;
begin
  result := fTokStop > length(fLineBuf);
end;

function TSynTxtSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  result := fTokToAttri[fToken];
  result.FrameEdges := sfeNone;
  if fCurrIdent <> '' then
    if GetToken = fCurrIdent then begin
      result.FrameColor := result.Foreground;
      result.FrameStyle := slsSolid;
      result.FrameEdges := sfeAround;
    end;
end;

function TSynTxtSyn.GetTokenPos: Integer;
begin
  result := fTokStart - 1;
end;

function TSynTxtSyn.GetToken: string;
begin
  result := copy(fLineBuf, FTokStart, fTokStop - FTokStart);
end;

procedure TSynTxtSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart  := @fLineBuf[FTokStart];
  TokenLength := fTokStop - FTokStart;
end;

function TSynTxtSyn.GetTokenKind: integer;
var
  a: TSynHighlighterAttributes;
begin
  Result := SYN_ATTR_IDENTIFIER;
  a := GetTokenAttribute;
  if a = fTxtAttribs then Result := SYN_ATTR_IDENTIFIER  else
  if a = fWhiAttribs then Result := SYN_ATTR_WHITESPACE  else
  if a = fSymAttribs then Result := SYN_ATTR_SYMBOL;
end;

function TSynTxtSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result    := fTxtAttribs;
    SYN_ATTR_IDENTIFIER: Result := fTxtAttribs;
    SYN_ATTR_KEYWORD: Result    := fTxtAttribs;
    SYN_ATTR_STRING: Result     := fTxtAttribs;
    SYN_ATTR_WHITESPACE: Result := fWhiAttribs;
    SYN_ATTR_SYMBOL: Result     := fSymAttribs;
    else Result := fTxtAttribs;
  end;
end;

end.
