unit ce_sharedres;

interface

uses
  LResources, Controls, Buttons, Graphics;

procedure AssignPng(ctrl: TControl; const resName: string);

implementation

procedure AssignPng(ctrl: TControl; const resName: string);
var
  png : TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    try
      png.LoadFromLazarusResource(resName);
      if ctrl is TCustomBitBtn then
        TCustomBitBtn(ctrl).Glyph.Assign(png)
      else if ctrl is TCustomSpeedButton then
        TCustomSpeedButton(ctrl).Glyph.Assign(png);
    except
    end;
  finally
    png.Free;
  end;

end;

initialization
  {$I ../src/ce_icons.inc}
end.

