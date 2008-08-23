(**

  This module contains a class to add and edit method descriptions.

  @Author  David Hoyle
  @Version 1.0
  @Date    23 Aug 2008

**)
unit MethodDescriptionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  (** A class to represent the form interface. **)
  TfrmMethodDescriptions = class(TForm)
    lblPattern: TLabel;
    edtPattern: TEdit;
    lblDescription: TLabel;
    edtDescription: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(var strPattern, strDescription : String) : Boolean;
  end;

implementation

{$R *.dfm}

{ TfrmMethodDescriptions }

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
class function TfrmMethodDescriptions.Execute(var strPattern,
  strDescription: String): Boolean;
begin
  Result := False;
  With TfrmMethodDescriptions.Create(Nil) Do
    Try
      edtPattern.Text := strPattern;
      edtDescription.Text := strDescription;
      If ShowModal = mrOK Then
        Begin
          strPattern := edtPattern.Text;
          strDescription := edtDescription.Text;
          Result := True;
        End;
    Finally
      Free;
    End;
end;

end.
