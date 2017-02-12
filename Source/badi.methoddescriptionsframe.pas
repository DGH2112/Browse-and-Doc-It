(**

  This module contains a frame for editing the method descrptions.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Feb 2017

**)
Unit BADI.MethodDescriptionsFrame;

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
  ComCtrls,
  BADI.CustomOptionsFrame;

Type
  (** This class represents the frame interface. **)
  TfmBADIMethodDescriptionsFrame = Class(TFrame, IBADIOptionsFrame)
    hctlMethodDescriptions: THeaderControl;
    lbxMethodDescriptions: TListBox;
    btnDeleteDesc: TBitBtn;
    btnEditDesc: TBitBtn;
    btnAddDesc: TBitBtn;
    procedure btnAddDescClick(Sender: TObject);
    procedure btnDeleteDescClick(Sender: TObject);
    procedure btnEditDescClick(Sender: TObject);
    procedure lbxMethodDescriptionsDblClick(Sender: TObject);
    procedure lbxMethodDescriptionsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
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
  BADI.BaseLanguageModule,
  BADI.MethodDescriptionForm;

{ TfmBADIMethodDescriptionsFrame }

(**


  This method is an on click event handler for the Add Description button.

  @precon  None.
  @postcon Aloows the user to add a method description to the list.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.btnAddDescClick(Sender: TObject);

Var
  strPattern, strDescription: String;

Begin
  If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
    lbxMethodDescriptions.Items.Add(Format('%s=%s', [strPattern, strDescription]));
End;

(**


  This is an on click event handler for the delete description button.

  @precon  None.
  @postcon Delete the selected item from the method description list view.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.btnDeleteDescClick(Sender: TObject);

Begin
  If lbxMethodDescriptions.ItemIndex > - 1 Then
    lbxMethodDescriptions.Items.Delete(lbxMethodDescriptions.ItemIndex);
End;

(**


  This method is an on click event handler for the Edit Description button.

  @precon  None.
  @postcon Allows the user to edit the current method description.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.btnEditDescClick(Sender: TObject);

Var
  strPattern, strDescription: String;
  iIndex: Integer;

Begin
  iIndex := lbxMethodDescriptions.ItemIndex;
  If iIndex > - 1 Then
    Begin
      strPattern := lbxMethodDescriptions.Items.Names[iIndex];
      strDescription := lbxMethodDescriptions.Items.ValueFromIndex[iIndex];
      If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
        lbxMethodDescriptions.Items[iIndex] := Format('%s=%s', [strPattern,
          strDescription]);
    End;
End;

(**


  This method is an on double click event handler for the Metho Description
  ListView.

  @precon  None.
  @postcon Edits the selected item.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.lbxMethodDescriptionsDblClick(Sender: TObject);
Begin
  btnEditDescClick(Sender);
End;

(**

  This is an on draw item event handler for the method descriptions.

  @precon  None.
  @postcon Draws the str=str information in the list box as 2 columns of
           information without the = sign.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
Procedure TfmBADIMethodDescriptionsFrame.lbxMethodDescriptionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

Var
  lb: TListBox;
  iPos: Integer;

Begin
  lb := Control As TListBox;
  lb.Canvas.FillRect(Rect);
  iPos := Pos('=', lb.Items[Index]);
  lb.Canvas.TextOut(Rect.Left + 4, Rect.Top, Copy(lb.Items[Index], 1, iPos - 1));
  lb.Canvas.TextOut(Rect.Left + 154, Rect.Top, Copy(lb.Items[Index], iPos + 1,
    Length(lb.Items[Index]) - iPos));
End;

(**

  This method loads the method description options from the BADI options class.

  @precon  None.
  @postcon The method descrption options are loaded into the frame controls.

**)
Procedure TfmBADIMethodDescriptionsFrame.LoadSettings;

Var
  j: Integer;

Begin
  For j := 0 To BrowseAndDocItOptions.MethodDescriptions.Count - 1 Do
    lbxMethodDescriptions.Items.Add(BrowseAndDocItOptions.MethodDescriptions[j]);
End;

(**

  This method saves the method description options to the BADI options class.

  @precon  None.
  @postcon The method descrption options are saved from the frame controls.

**)
Procedure TfmBADIMethodDescriptionsFrame.SaveSettings;

Var
  j: Integer;

Begin
  BrowseAndDocItOptions.MethodDescriptions.Clear;
  For j := 0 To lbxMethodDescriptions.Items.Count - 1 Do
    BrowseAndDocItOptions.MethodDescriptions.Add(lbxMethodDescriptions.Items[j]);
End;

End.
