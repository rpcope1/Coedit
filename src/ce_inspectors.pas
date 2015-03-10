unit ce_inspectors;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Dialogs, PropEdits;

type

  TCEPathname = type string;
  TCEFilename = type string;

  TCustomPathType = (ptFile, ptFolder);

  // base class for a property representing a path
  // additionaly to the text field, a dialog can be opened
  // to select the directory or the file.
  TCECustomPathEditor = class(TStringPropertyEditor)
  private
    fType: TCustomPathType;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TCEPathnameEditor = class(TCECustomPathEditor)
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
  end;

  TCEFilenameEditor = class(TCECustomPathEditor)
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
  end;

implementation

function TCECustomPathEditor.GetAttributes: TPropertyAttributes;
begin
  exit( inherited GetAttributes() + [paDialog]);
end;

procedure TCECustomPathEditor.Edit;
var
  newValue: string;
begin
  case fType of
    ptFile:
      with TOpenDialog.create(nil) do try
        InitialDir := ExtractFileName(GetValue);
        FileName := GetValue;
        if Execute then SetValue(FileName);
      finally
        free;
      end;
    ptFolder:
      if SelectDirectory(GetPropInfo^.Name, GetValue, newValue) then
        SetValue(newValue);
  end;
end;

constructor TCEPathnameEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited;
  fType := ptFolder;
end;

constructor TCEFilenameEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited;
  fType := ptFile;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TCEPathname), nil, '', TCEPathnameEditor);
  RegisterPropertyEditor(TypeInfo(TCEFilename), nil, '', TCEfilenameEditor);
end.

