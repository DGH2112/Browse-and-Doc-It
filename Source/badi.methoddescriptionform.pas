(**

  This module contains a class to add and edit method descriptions.

  @Author  David Hoyle
  @Version 1.0
  @Date    17 Oct 2018

**)
Unit BADI.MethodDescriptionForm;

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
  Vcl.ExtCtrls;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent the form interface. **)
  TfrmMethodDescriptions = Class(TForm)
    lblPattern: TLabel;
    edtPattern: TEdit;
    lblDescription: TLabel;
    edtDescription: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlForm: TPanel;
  Strict Private
  Strict Protected
  Public
    Class Function Execute(Var strPattern, strDescription: String): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  ToolsAPI;

(**


  This is the forms main interface method for getting the pattern and
  description.

  @precon  None.
  @postcon If the dialogue is confirmed then the pattern and description are
           returned in the var parameters and the function retuen true.


  @param   strPattern     as a String as a reference
  @param   strDescription as a String as a reference
  @return  a Boolean

**)
Class Function TfrmMethodDescriptions.Execute(Var strPattern, strDescription: String): Boolean;

Var
  F : TfrmMethodDescriptions;
  { $IFDEF DXE102
  ITS : IOTAIDEThemingServices250;
  {$ENDIF}
  
Begin
  Result := False;
  { $IFDEF DXE102
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.RegisterFormClass(TfrmMethodDescriptions);
  {$ENDIF}
  F := TfrmMethodDescriptions.Create(Nil);
  Try
    { $IFDEF DXE102
    If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
      If ITS.IDEThemingEnabled Then
        ITS.ApplyTheme(F);
    {$ENDIF}
    F.edtPattern.Text := strPattern;
    F.edtDescription.Text := strDescription;
    If F.ShowModal = mrOK Then
      Begin
        strPattern := F.edtPattern.Text;
        strDescription := F.edtDescription.Text;
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

End.
