(**

  This module contains a class to represent a frame interface for the code browsing
  options.

  @Version 1.0
  @Author  David Hoyle
  @Date    21 Aug 2016

**)
Unit BADICodeBrowsingFrame;

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
  ExtCtrls;

Type
  (** A class to represent the frame interface. **)
  TfmBADICodeBrowsingFrame = Class(TFrame)
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
  BaseLanguageModule;

{ TfmBADICodeBrowsingFrame }

(**

  This method loads the code browsing information from the options.

  @precon  None.
  @postcon loads the code browsing information from the options.

**)
Procedure TfmBADICodeBrowsingFrame.LoadSettings;

Begin
  rgpBrowsePosition.ItemIndex := Integer(BrowseAndDocItOptions.BrowsePosition);
End;

(**

  This method saves the code browsing information to the options.

  @precon  None.
  @postcon Saves the code browsing information to the options.

**)
Procedure TfmBADICodeBrowsingFrame.SaveSettings;

Begin
  BrowseAndDocItOptions.BrowsePosition := TBrowsePosition(rgpBrowsePosition.ItemIndex);
End;

End.
