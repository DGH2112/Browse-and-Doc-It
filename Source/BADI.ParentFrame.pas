(**

  This modulel contains a frame for the root node of the BADI Options frame in the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    29 Apr 2017

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

Var
  iMajor, iMinor, iBugFix, iBuild : Integer;

Begin
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  lblBADI.Caption := Format('Browse and Doc It %d.%d%s', [iMajor, iMinor, strBugFix[iBugFix]]);
  lblBuild.Caption := Format('Build %d.%d.%d.%d', [iMajor, iMinor, iBugFix, iBuild]);
  {$IFDEF EUREKALOG_VER7}
  lblEurekaLog.Caption := 'EurekaLog Information:'#13#10 +
    '  Installed: ' + BoolToStr(ExceptionLog7.IsEurekaLogInstalled, True) + #13#10 +
    '  Active: ' + BoolToStr(ExceptionLog7.IsEurekaLogActive, True);
  {$ELSE}
  lblEurekaLog.Caption := 'EurekaLog is not compiled into this version.';
  {$ENDIF}
End;

(**

  This method does nothing but is required by the interface.

  @precon  None.
  @postcon None.

**)
Procedure TfmBADIParentFrame.SaveSettings;

Begin //FI:W519
End;

End.
