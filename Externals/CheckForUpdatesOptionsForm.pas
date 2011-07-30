(**

  This module contains a class which represent a form for editing the Check for Updates
  settings.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Jul 2011

**)
Unit CheckForUpdatesOptionsForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ComCtrls;

Type
  (** A class to represent the form interface. **)
  TfrmCheckForUpdatesOptions = Class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    gbxLastUpdate: TGroupBox;
    pnlLastUpdate: TPanel;
    chkEnabled: TCheckBox;
    lblInterval: TLabel;
    edtInterval: TEdit;
    udInterval: TUpDown;
  Private
    { Private declarations }
    Procedure LoadSettings(strINIFileName : String);
    Procedure SaveSettings(strINIFileName : String);
  Public
    { Public declarations }
    Class Procedure Execute(strINIFileName: String);
  End;

Implementation

Uses
  IniFiles;

Const
  (** A constant string that defines the INI section where the Check for Updates
      information resides.  **)
  strSection = 'CheckForUpdates';

{$R *.dfm}

{ TForm1 }

(**

  This method is a class method for invoking the dialogue and editing the Check for
  Updates settings.

  @precon  None.
  @postcon Displays the dialogue for editing the Check for Updates settings.

  @param   strINIFileName as a String

**)
Class Procedure TfrmCheckForUpdatesOptions.Execute(strINIFileName: String);

Begin
  With TfrmCheckForUpdatesOptions.Create(Nil) Do
    Try
      LoadSettings(strINIFileName);
      If ShowModal = mrOK Then
        SaveSettings(strINIFileName);
    Finally
      Free;
    End;
End;

(**

  This method loads the Check for Updates settings from the INI file.

  @precon  None.
  @postcon Loads the Check for Updates settings from the INI file.

  @param   strINIFileName as a String

**)
Procedure TfrmCheckForUpdatesOptions.LoadSettings(strINIFileName: String);

Var
  iYear, iMonth, iday : Integer;
  dtDate : TDateTime;

Begin
  With TMemIniFile.Create(strINIFileName) Do
    Try
      iYear := ReadInteger(strSection, 'Year', 0);
      iMonth := ReadInteger(strSection, 'Month', 0);
      iDay := ReadInteger(strSection, 'Day', 0);
      If iDay > 0 Then
        Begin
          dtDate := EncodeDate(iYear, iMonth, iDay);
          pnlLastUpdate.Caption := FormatDateTime('ddd dd/mmm/yyyy', dtDate) + ', ' +
            Format('%d days ago.', [Trunc(Now - dtDate)]);
        End;
      udInterval.Position := ReadInteger(strSection, 'Interval', 28);
      chkEnabled.Checked := ReadBool(strSection, 'Enabled', False);
    Finally
      Free;
    End;
End;

(**

  This method saves the changed settings to the INI file.

  @precon  None.
  @postcon Saves the changed settings to the INI file.

  @param   strINIFileName as a String

**)
Procedure TfrmCheckForUpdatesOptions.SaveSettings(strINIFileName: String);

Begin
  With TMemIniFile.Create(strINIFileName) Do
    Try
      WriteInteger(strSection, 'Interval', udInterval.Position);
      WriteBool(strSection, 'Enabled', chkEnabled.Checked);
      UpdateFile;
    Finally
      Free;
    End;
End;

End.
