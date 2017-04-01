(**

  This module contains a class to represent a frame interface for the code browsing
  options.

  @Version 1.0
  @Author  David Hoyle
  @Date    01 Apr 2017

**)
Unit BADI.CodeBrowsingFrame;

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
  ExtCtrls,
  BADI.CustomOptionsFrame;

Type
  (** A class to represent the frame interface. **)
  TfmBADICodeBrowsingFrame = Class(TFrame, IBADIOptionsFrame)
    rgpBrowsePosition: TRadioGroup;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  BADI.Base.Module,
  BADI.Options,
  BADI.Types;

{ TfmBADICodeBrowsingFrame }

(**

  This method loads the code browsing information from the options.

  @precon  None.
  @postcon loads the code browsing information from the options.

**)
Procedure TfmBADICodeBrowsingFrame.LoadSettings;

Begin
  rgpBrowsePosition.ItemIndex := Integer(TBADIOptions.BADIOptions.BrowsePosition);
End;

(**

  This method saves the code browsing information to the options.

  @precon  None.
  @postcon Saves the code browsing information to the options.

**)
Procedure TfmBADICodeBrowsingFrame.SaveSettings;

Begin
  TBADIOptions.BADIOptions.BrowsePosition := TBrowsePosition(rgpBrowsePosition.ItemIndex);
End;

End.
