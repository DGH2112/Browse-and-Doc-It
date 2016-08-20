(**

  This module contains a class which represents the Browse and Doc It general Options as
  a frame that can be inserted into a form or the IDEs main optiosn dialogue.

  @Author  David Hoyle
  @Version 1.0
  @Date    20 Aug 2016

**)
Unit BADIGeneralOptionsFrame;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.CheckLst;

Type
  (** A class to represent the frame interface. **)
  TfmBADIGeneralOptions = Class(TFrame)
    clbOptions: TCheckListBox;
    IntervalPanel: TPanel;
    lblRefreshInterval: TLabel;
    lblManagedNodesLife: TLabel;
    edtUpdateInterval: TEdit;
    udUpdateInterval: TUpDown;
    edtManagedNodesLife: TEdit;
    udManagedNodesLife: TUpDown;
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

{ TfmBADIgeneralOptionsFrame }

(**

  This method loads the BADI options into the frames controls.

  @precon  None.
  @postcon The frame is initialised with the BADI options.

**)
Procedure TfmBADIGeneralOptions.LoadSettings;

Var
  i : TDocOption;

Begin
  For i := Low(TDocOption) To High(TDocOption) Do
    Begin
      clbOptions.Items.Add(DocOptionInfo[i].FDescription);
      clbOptions.Checked[Integer(i)] := i In BrowseAndDocItOptions.Options;
    End;
  udUpdateInterval.Position := BrowseAndDocItOptions.UpdateInterval;
  udManagedNodesLife.Position := BrowseAndDocItOptions.ManagedNodesLife;
End;

(**

  This method saves the settings in the frames controls back to the BADI options class.

  @precon  None.
  @postcon The frames settings are captured and stored in the BADI options class.

**)
Procedure TfmBADIGeneralOptions.SaveSettings;

Var
  i : TDocOption;

Begin
  BrowseAndDocItOptions.Options := [];
  For i := Low(TDocOption) To High(TDocOption) Do
    If clbOptions.Checked[Integer(i)] Then
      BrowseAndDocItOptions.Options := BrowseAndDocItOptions.Options + [i];
  BrowseAndDocItOptions.UpdateInterval := udUpdateInterval.Position;
  BrowseAndDocItOptions.ManagedNodesLife := udManagedNodesLife.Position;
End;

End.
