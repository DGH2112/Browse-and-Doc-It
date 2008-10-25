(**

  This module contains a class which represent a form for defining the option
  for Documentation.

  @Version 1.0
  @Author  David Hoyle
  @Date    25 Oct 2008

**)
unit DocumentationOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, DocumentationDispatcher;

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
  BaseLanguageModule;

{$R *.dfm}

{ TfrmDocumentationOptions }

(**

  This is the forms main interface method for invokking the dialogue.

  @precon  None.
  @postcon Returns the documentation type require in the var parameter if the
           dialogue is accepted while returning true, else returns false.

  @param   ADocType as a TDocType
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
      chkLocal.Checked := scLocal In BrowseAndDocItOptions.ScopesToDocument;
      chkPrivate.Checked := scPrivate In BrowseAndDocItOptions.ScopesToDocument;
      chkProtected.Checked := scProtected In BrowseAndDocItOptions.ScopesToDocument;
      chkPublic.Checked := scPublic In BrowseAndDocItOptions.ScopesToDocument;
      chkPublished.Checked := scPublished In BrowseAndDocItOptions.ScopesToDocument;
      If ShowModal = mrOK Then
        Begin
          ADocType := TDocType(rgpDocumentationOptions.ItemIndex);
          BrowseAndDocItOptions.ScopesToDocument := [];
          If chkLocal.Checked Then
            BrowseAndDocItOptions.ScopesToDocument :=
              BrowseAndDocItOptions.ScopesToDocument + [scLocal];
          If chkPrivate.Checked Then
            BrowseAndDocItOptions.ScopesToDocument :=
              BrowseAndDocItOptions.ScopesToDocument + [scPrivate];
          If chkProtected.Checked Then
            BrowseAndDocItOptions.ScopesToDocument :=
              BrowseAndDocItOptions.ScopesToDocument + [scProtected];
          If chkPublic.Checked Then
            BrowseAndDocItOptions.ScopesToDocument :=
              BrowseAndDocItOptions.ScopesToDocument + [scPublic];
          If chkPublished.Checked Then
            BrowseAndDocItOptions.ScopesToDocument :=
              BrowseAndDocItOptions.ScopesToDocument + [scPublished];
          Result := True;
        End;
    Finally
      Free;
    End;
end;

end.
