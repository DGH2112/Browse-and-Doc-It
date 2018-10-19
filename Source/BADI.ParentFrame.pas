(**

  This modulel contains a frame for the root node of the BADI Options frame in the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Oct 2018

**)
Unit BADI.ParentFrame;

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
  BADI.CustomOptionsFrame,
  StdCtrls;

Type
  (** A class to represent the options frame. **)
  TfmBADIParentFrame = Class(TFrame, IBADIOptionsFrame)
    lblBADI: TLabel;
    lblAuthor: TLabel;
    lblBuild: TLabel;
    lblPleaseSelect: TLabel;
    lblInformation: TLabel;
    lblEurekaLog: TLabel;
    lblBuildDate: TLabel;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

uses
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  {$ENDIF}
  BADI.Functions;

{$R *.dfm}

{ TfmBADIParentFrame }

(**

  This method intialises the frame with build information.

  @precon  None.
  @postcon The frame is initialised.

**)
Procedure TfmBADIParentFrame.LoadSettings;

Const
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';
  strBrowseAndDocIt = 'Browse and Doc It %d.%d%s';
  {$IFDEF DEBUG}
  strDEBUGBuild = 'DEBUG Build %d.%d.%d.%d';
  {$ELSE}
  strBuild = 'Build %d.%d.%d.%d';
  {$ENDIF}
  strBuildDateFmt = 'ddd dd/mmm/yyyy hh:nn';
  strBuildDate = 'Build Date: %s';
  {$IFDEF EUREKALOG_VER7}
  strEurekaLogStatus = 'EurekaLog is compiled into this version:'#13#10 +
    '  Installed:'#9'%s'#13#10 +
    '  Active:'#9#9'%s';
  {$ELSE}
  strEurekaLogStatus = 'EurekaLog is NOT compiled into this version.';
  {$ENDIF}

Var
  iMajor, iMinor, iBugFix, iBuild : Integer;
  dtDate : TDateTime;
  strModuleName : String;
  iSize : Integer;

Begin
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  lblBADI.Caption := Format(strBrowseAndDocIt, [iMajor, iMinor, strBugFix[Succ(iBugFix)]]);
  {$IFDEF DEBUG}
  lblBuild.Caption := Format(strDEBUGBuild, [iMajor, iMinor, iBugFix, iBuild]);
  lblBuild.Font.Color := clRed;
  {$ELSE}
  lblBuild.Caption := Format(strBuild, [iMajor, iMinor, iBugFix, iBuild]);
  {$ENDIF}
  SetLength(strModuleName, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strModuleName), MAX_PATH);
  SetLength(strModuleName, iSize);
  FileAge(strModuleName, dtDate);
  lblBuildDate.Caption := Format(strBuildDate, [FormatDateTime(strBuildDateFmt, dtDate)]);
  {$IFDEF EUREKALOG_VER7}
  lblEurekaLog.Caption := Format(strEurekaLogStatus, [
    BoolToStr(ExceptionLog7.IsEurekaLogInstalled, True),
    BoolToStr(ExceptionLog7.IsEurekaLogActive, True)
  ]);
  lblEurekaLog.Font.Color := clGreen;
  {$ELSE}
  lblEurekaLog.Caption := strEurekaLogStatus;
  lblEurekaLog.Font.Color := clRed;
  {$ENDIF}
End;

(**

  This method does nothing but is required by the interface.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TfmBADIParentFrame.SaveSettings;

Begin //FI:W519
End;

End.
