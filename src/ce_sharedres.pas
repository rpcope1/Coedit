unit ce_sharedres;

interface

uses
  LResources, Controls, Buttons, Graphics;

procedure AssignPng(ctrl: TControl; const resName: string);

implementation

var
  png: TPortableNetworkGraphic;

procedure AssignPng(ctrl: TControl; const resName: string);
begin
  try
    png.LoadFromLazarusResource(resName);
    if ctrl is TCustomBitBtn then
      TCustomBitBtn(ctrl).Glyph.Assign(png)
    else if ctrl is TCustomSpeedButton then
      TCustomSpeedButton(ctrl).Glyph.Assign(png);
  except
  end;
end;

initialization
  png := TPortableNetworkGraphic.Create;
  {$I ../src/ce_icons.inc}
finalization
  png.Free;
end.

