(**

  This module contains a class which represents the Browse and Doc It general Options as
  a frame that can be inserted into a form or the IDEs main optiosn dialogue.

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Oct 2018

**)
Unit BADI.GeneralOptionsFrame;

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
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  CheckLst,
  BADI.CustomOptionsFrame;

Type
  (** A class to represent the frame interface. **)
  TfmBADIGeneralOptions = Class(TFrame, IBADIOptionsFrame)
    IntervalPanel: TPanel;
    lblRefreshInterval: TLabel;
    lblManagedNodesLife: TLabel;
    edtUpdateInterval: TEdit;
    udUpdateInterval: TUpDown;
    edtManagedNodesLife: TEdit;
    udManagedNodesLife: TUpDown;
    lvOptions: TListView;
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
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Base.Module,
  BADI.Types,
  BADI.Constants,
  BADI.Options;

{ TfmBADIgeneralOptionsFrame }

(**

  This method loads the BADI options into the frames controls.

  @precon  None.
  @postcon The frame is initialised with the BADI options.

**)
Procedure TfmBADIGeneralOptions.LoadSettings;

Var
  iGroup : TDocOptionGroup;
  iOption : TDocOption;
  Item : TListItem;
  Group: TListGroup;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  For iGroup := Low(TDocOptionGroup) To High(TDocOptionGroup) Do
    Begin
      Group := lvOptions.Groups.Add;
      Group.Header := DocOptionGroups[iGroup];
      Group.GroupID := Integer(iGroup);
    End;
  lvOptions.GroupView := True;
  For iOption := Low(TDocOption) To High(TDocOption) Do
    Begin
      Item := lvOptions.Items.Add;
      Item.Caption := DocOptionInfo[iOption].FDescription;
      Item.Checked := iOption In TBADIOptions.BADIOptions.Options;
      Item.GroupID := Integer(DocOptionInfo[iOption].FGroup);
    End;
  udUpdateInterval.Position := TBADIOptions.BADIOptions.UpdateInterval;
  udManagedNodesLife.Position := TBADIOptions.BADIOptions.ManagedNodesLife;
End;

(**

  This method saves the settings in the frames controls back to the BADI options class.

  @precon  None.
  @postcon The frames settings are captured and stored in the BADI options class.

**)
Procedure TfmBADIGeneralOptions.SaveSettings;

Var
  iOption : TDocOption;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  TBADIOptions.BADIOptions.Options := [];
  For iOption := Low(TDocOption) To High(TDocOption) Do
    If lvOptions.Items[Integer(iOption)].Checked Then
      TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options + [iOption];
  TBADIOptions.BADIOptions.UpdateInterval := udUpdateInterval.Position;
  TBADIOptions.BADIOptions.ManagedNodesLife := udManagedNodesLife.Position;
  TBADIOptions.BADIOptions.SaveSettings;
End;

End.

