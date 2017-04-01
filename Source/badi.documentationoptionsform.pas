(**

  This module contains a class which represent a form for defining the option
  for Documentation.

  @Version 1.0
  @Author  David Hoyle
  @Date    01 Apr 2017

**)
unit BADI.DocumentationOptionsForm;

interface

uses
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
  BADI.Documentation.Dispatcher;

type
  (** A class to represent the form interface. **)
  TfrmDocumentationOptions = class(TForm)
    rgpDocumentationOptions: TRadioGroup;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    gbxScopeOptions: TGroupBox;
    chkLocal: TCheckBox;
    chkPrivate: TCheckBox;
    chkProtected: TCheckBox;
    chkPublic: TCheckBox;
    chkPublished: TCheckBox;
    lblCSSComment: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(var ADocType : TDocType) : Boolean;
  end;

implementation

Uses
  BADI.Base.Module, BADI.Types, BADI.Options;

{$R *.dfm}

{ TfrmDocumentationOptions }

(**

  This is the forms main interface method for invokking the dialogue.

  @precon  None.
  @postcon Returns the documentation type require in the var parameter if the
           dialogue is accepted while returning true, else returns false.

  @param   ADocType as a TDocType as a Reference
  @return  a Boolean

**)
class function TfrmDocumentationOptions.Execute(var ADocType: TDocType): Boolean;

Var
  i : TDocType;

begin
  Result := False;
  With TfrmDocumentationOptions.Create(Nil) Do
    Try
      For i := Low(TDocType) to High(TDocType) Do
        rgpDocumentationOptions.Items.Add(strDocumentationTypes[i]);
      rgpDocumentationOptions.ItemIndex := Byte(ADocType);
      chkLocal.Checked := scLocal In TBADIOptions.BADIOptions.ScopesToDocument;
      chkPrivate.Checked := scPrivate In TBADIOptions.BADIOptions.ScopesToDocument;
      chkProtected.Checked := scProtected In TBADIOptions.BADIOptions.ScopesToDocument;
      chkPublic.Checked := scPublic In TBADIOptions.BADIOptions.ScopesToDocument;
      chkPublished.Checked := scPublished In TBADIOptions.BADIOptions.ScopesToDocument;
      If ShowModal = mrOK Then
        Begin
          ADocType := TDocType(rgpDocumentationOptions.ItemIndex);
          TBADIOptions.BADIOptions.ScopesToDocument := [];
          If chkLocal.Checked Then
            TBADIOptions.BADIOptions.ScopesToDocument :=
              TBADIOptions.BADIOptions.ScopesToDocument + [scLocal];
          If chkPrivate.Checked Then
            TBADIOptions.BADIOptions.ScopesToDocument :=
              TBADIOptions.BADIOptions.ScopesToDocument + [scPrivate];
          If chkProtected.Checked Then
            TBADIOptions.BADIOptions.ScopesToDocument :=
              TBADIOptions.BADIOptions.ScopesToDocument + [scProtected];
          If chkPublic.Checked Then
            TBADIOptions.BADIOptions.ScopesToDocument :=
              TBADIOptions.BADIOptions.ScopesToDocument + [scPublic];
          If chkPublished.Checked Then
            TBADIOptions.BADIOptions.ScopesToDocument :=
              TBADIOptions.BADIOptions.ScopesToDocument + [scPublished];
          Result := True;
        End;
    Finally
      Free;
    End;
end;

end.
